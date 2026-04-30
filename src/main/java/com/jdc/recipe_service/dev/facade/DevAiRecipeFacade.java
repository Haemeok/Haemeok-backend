package com.jdc.recipe_service.dev.facade;

import com.jdc.recipe_service.dev.domain.type.image.DevImageGenModel;
import com.jdc.recipe_service.dev.service.recipe.ingredient.DevRecipeIngredientPersistService;
import com.jdc.recipe_service.domain.dto.recipe.AiRecipeRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.JobStatusDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepRequestDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.dto.user.UserSurveyDto;
import com.jdc.recipe_service.domain.entity.recipe.RecipeGenerationJob;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeGenerationJobRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.AiRecipeConcept;
import com.jdc.recipe_service.domain.type.JobStatus;
import com.jdc.recipe_service.domain.type.JobType;
import com.jdc.recipe_service.domain.type.QuotaType;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.DailyQuotaService;
import com.jdc.recipe_service.service.RecipeService;
import com.jdc.recipe_service.service.SurveyService;
import com.jdc.recipe_service.service.ai.GeminiClientService;
import com.jdc.recipe_service.service.ai.GrokClientService;
import com.jdc.recipe_service.service.image.AsyncImageService;
import com.jdc.recipe_service.util.ActionImageService;
import com.jdc.recipe_service.util.prompt.CostEffectivePromptBuilder;
import com.jdc.recipe_service.util.prompt.FineDiningPromptBuilder;
import com.jdc.recipe_service.util.prompt.IngredientFocusPromptBuilder;
import com.jdc.recipe_service.util.prompt.NutritionPromptBuilder;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionTemplate;
import org.springframework.util.CollectionUtils;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HexFormat;
import java.util.List;
import java.util.Optional;

/**
 * Dev V3 AI 레시피 생성 facade.
 *
 * 운영 V2 (AiRecipeFacade.createAiGenerationJobV2 / processAiGenerationAsyncV2) 와 동일한
 * 단계 구성 (survey 적용 → 4 concept 분기 → 단위 정규화 → step action key → recipe 저장 →
 * 이미지 생성 → 인덱싱/알림)을 그대로 따른다.
 *
 * V2와의 유일한 추가:
 *  - imageGenModel 파라미터 (whitelist 검증 후 RecipeGenerationJob.image_generation_model 저장)
 *  - asyncImageService.generateAndUploadAiImage(..., imageGenModel)로 라우팅 지정
 *
 * dev → prod swap 시 이 클래스가 AiRecipeFacade.createAiGenerationJobV2/processAiGenerationAsyncV2를
 * 그대로 대체하면 된다.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class DevAiRecipeFacade {

    /**
     * Job 생성 결과.
     *  - created=true 면 새 job + 쿼터가 차감됨 → controller가 async 호출
     *  - created=false 면 기존 job 재사용 (idempotency hit) → async 호출 금지 (중복 실행 방지)
     *  - usedToken 은 async 실패 시 정확한 환불 단위(token vs free quota)에 사용됨
     */
    public record JobCreateResult(Long jobId, boolean created, boolean usedToken) {}


    private final RecipeRepository recipeRepository;
    private final RecipeGenerationJobRepository jobRepository;

    private final GrokClientService grokClientService;
    private final GeminiClientService geminiClientService;
    private final RecipeService recipeService;
    private final DevRecipeIngredientPersistService devIngredientPersist;
    private final DailyQuotaService dailyQuotaService;
    private final SurveyService surveyService;
    private final ActionImageService actionImageService;
    private final AsyncImageService asyncImageService;
    private final TransactionTemplate transactionTemplate;

    private final IngredientFocusPromptBuilder ingredientBuilder;
    private final CostEffectivePromptBuilder costBuilder;
    private final NutritionPromptBuilder nutritionBuilder;
    private final FineDiningPromptBuilder fineDiningBuilder;

    /**
     * Phase 1: 작업 접수 (멱등성 + 일일 쿼터 + 화이트리스트 검증).
     *
     * 멱등성 키는 운영 V1/V2와 같은 single-column unique constraint
     * (`recipe_generation_jobs.idempotency_key`) 위에서 동작한다. 우발 충돌을 막기 위해
     * 저장 키를 `dev-ai:{userId}:{sha256(rawKey)}` 형식으로 만든다.
     *  - `dev-ai:` prefix : prod 코드가 raw key를 그대로 저장해도 prod와 dev 공간이 분리됨.
     *  - userId 포함     : 다른 사용자가 동일 클라이언트 키를 보내도 cross-user 충돌 없음.
     *  - sha256 hex      : 클라이언트 키 길이/특수문자에 무관하게 64자 고정 → 255자 한계와 무관.
     *
     * 장기적으로는 (user_id, idempotency_key) composite unique를 Flyway로 도입해 hash 의존을 제거할 것.
     *
     * @return JobCreateResult — created=false면 기존 job 재사용이므로 controller에서 async 호출 금지.
     */
    @Transactional
    public JobCreateResult createAiGenerationJob(RecipeWithImageUploadRequest request,
                                                 AiRecipeConcept concept,
                                                 String imageGenModel,
                                                 Long userId,
                                                 String idempotencyKey) {
        if (request.getAiRequest() == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "AI 요청 정보가 없습니다.");
        }

        // 화이트리스트 검증 (UNSUPPORTED_IMAGE_MODEL → 400)
        DevImageGenModel.fromIdentifier(imageGenModel);

        // dev 전용 namespace + userId + hash → prod와 분리, cross-user 충돌 방지, 길이 안전
        String storedKey = namespacedIdempotencyKey(userId, idempotencyKey);

        Optional<RecipeGenerationJob> existing = jobRepository.findByIdempotencyKey(storedKey);
        if (existing.isPresent()) {
            RecipeGenerationJob job = existing.get();
            log.info("♻️ [DevAi V3] 기존 작업 재사용 - storedKey={}, jobId={}, status={}",
                    storedKey, job.getId(), job.getStatus());
            // 재사용 시 쿼터 재차감 안 함, async 재실행 안 함 (controller가 created=false로 판단)
            return new JobCreateResult(job.getId(), false, false);
        }

        // consume이 token으로 fallback 됐는지 boolean으로 받아 후속 환불에 사용
        boolean usedToken = dailyQuotaService.consumeForUserOrThrow(userId, QuotaType.AI_GENERATION);

        RecipeGenerationJob job = RecipeGenerationJob.builder()
                .userId(userId)
                .jobType(JobType.AI_GENERATION)
                .status(JobStatus.PENDING)
                .progress(0)
                .idempotencyKey(storedKey)
                .imageGenerationModel(imageGenModel)
                .build();

        // saveAndFlush로 unique constraint 위반을 메서드 안에서 즉시 surface.
        // TODO(dev V3): 동시 동일-키 race 시 saveAndFlush가 DataIntegrityViolationException을 던진다.
        //   현재는 글로벌 핸들러가 409로 응답하고 @Transactional 롤백으로 quota는 자동 복구된다.
        //   클라이언트가 같은 키로 재요청하면 그때는 위 findByIdempotencyKey에서 winner job을 정상 반환한다.
        //   완전한 idempotency 보장을 원하면, 여기서 catch 후 REQUIRES_NEW 트랜잭션으로 winner를 재조회해
        //   created=false로 반환하도록 보강할 것.
        jobRepository.saveAndFlush(job);

        log.info("🆕 [DevAi V3] 신규 작업 생성 - jobId={}, imageGenModel={}, usedToken={}, storedKey={}",
                job.getId(), imageGenModel, usedToken, storedKey);

        return new JobCreateResult(job.getId(), true, usedToken);
    }

    private static final String DEV_AI_NAMESPACE = "dev-ai:";

    /**
     * Idempotency 저장 키 빌더.
     * 형식: dev-ai:{userId}:{sha256-hex(rawKey)}
     *  - prod 공간 분리 + cross-user 충돌 방지 + 64자 고정 길이.
     */
    private static String namespacedIdempotencyKey(Long userId, String clientKey) {
        return DEV_AI_NAMESPACE + userId + ":" + sha256Hex(clientKey);
    }

    private static String sha256Hex(String input) {
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            byte[] hash = md.digest(input.getBytes(StandardCharsets.UTF_8));
            return HexFormat.of().formatHex(hash);
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException("SHA-256 algorithm unavailable", e);
        }
    }

    /**
     * Phase 2: 비동기 본 처리 — 텍스트 생성 → 저장 → 이미지 생성(라우팅) → 인덱싱/알림.
     * 운영 V2와 동일한 단계. 마지막 이미지 생성에서만 imageGenModel을 추가 전달.
     *
     * @param usedToken createAiGenerationJob 시점에 token이 사용되었는지 여부 (실패 환불에 정확히 반영).
     */
    @Async("recipeExtractionExecutor")
    public void processAiGenerationAsync(Long jobId,
                                         RecipeWithImageUploadRequest request,
                                         AiRecipeConcept concept,
                                         String imageGenModel,
                                         Long userId,
                                         boolean usedToken) {
        long startTime = System.currentTimeMillis();
        RecipeGenerationJob job = jobRepository.findById(jobId)
                .orElseThrow(() -> new CustomException(ErrorCode.RESOURCE_NOT_FOUND));

        if (job.getStatus() == JobStatus.COMPLETED) return;

        try {
            updateProgress(job, JobStatus.IN_PROGRESS, 0);

            AiRecipeRequestDto aiReq = request.getAiRequest();
            aiReq.setUserId(userId);
            UserSurveyDto survey = surveyService.getSurvey(userId);
            applySurveyInfoToAiRequest(aiReq, survey);

            long textGenStart = System.currentTimeMillis();
            updateProgress(job, JobStatus.IN_PROGRESS, 10);

            RecipeCreateRequestDto generatedDto = generateTextRecipe(concept, aiReq);

            log.info("⏱️ [DevAi V3] 텍스트 생성 소요: {}ms", System.currentTimeMillis() - textGenStart);

            long dbSaveStart = System.currentTimeMillis();

            // 1.4 단위 보존: correctIngredientUnits(DB 기본 단위로 강제 변환)는 raw 보존 정책과 충돌 → 제거.
            // AI 출력의 raw 단위가 그대로 dev persist의 normalize 단계로 흘러간다.

            for (RecipeStepRequestDto step : generatedDto.getSteps()) {
                if (step.getAction() != null) {
                    String key = actionImageService.generateImageKey(concept, step.getAction());
                    step.updateImageKey(key);
                }
            }

            // 1.4 empty-then-save: 운영 호출에는 ingredients=[]/step.ingredients=[] 전달, dev persist로
            // dual-write + step relink. 원본 dto는 mutate 금지 (toBuilder + 별도 step list).
            //
            // 트랜잭션 경계 (MUST): processAiGenerationAsync는 @Transactional이 아니므로 여기서 명시 wrap.
            // recipeService.createRecipeAndGenerateUrls는 REQUIRED 라 outer tx에 join → afterCommit(OpenSearch
            // 인덱스 등)이 dev persist + relink 이후로 밀린다. wrap 안 하면 createRecipeAndGenerateUrls가
            // 자기 tx에서 즉시 commit + afterCommit fire → dev persist 실패 시 orphan recipe + 잘못된 인덱스.
            List<RecipeIngredientRequestDto> originalIngredients = generatedDto.getIngredients();
            List<RecipeStepRequestDto> originalSteps = generatedDto.getSteps();
            Integer suppliedMarketPrice = generatedDto.getMarketPrice();

            RecipeCreateRequestDto operationalDto = generatedDto.toBuilder()
                    .ingredients(List.of())
                    .steps(stripStepIngredients(originalSteps))
                    .build();

            RecipeWithImageUploadRequest processingRequest = RecipeWithImageUploadRequest.builder()
                    .aiRequest(aiReq)
                    .recipe(operationalDto)
                    .files(request.getFiles())
                    .build();

            Long recipeId = transactionTemplate.execute(status -> {
                PresignedUrlResponse savedResponse = recipeService.createRecipeAndGenerateUrls(
                        processingRequest, userId, RecipeSourceType.AI, concept
                );
                Long savedId = savedResponse.getRecipeId();

                // 같은 트랜잭션에서 Recipe 재조회 후 dev persist + step relink.
                // AI path → customByUser=false 강제 (시스템 매칭 실패는 UNRESOLVED이지 사용자 의도 CUSTOM 아님)
                Recipe savedRecipe = recipeRepository.findById(savedId)
                        .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));
                devIngredientPersist.persistAllSystemSourced(savedRecipe, originalIngredients, suppliedMarketPrice);
                devIngredientPersist.linkStepIngredients(savedRecipe, originalSteps);

                return savedId;
            });

            log.info("⏱️ [DevAi V3] DB 저장 소요: {}ms", System.currentTimeMillis() - dbSaveStart);

            long imageGenStart = System.currentTimeMillis();
            updateProgress(job, JobStatus.IN_PROGRESS, 75);

            try {
                log.info("🎨 [DevAi V3] 이미지 생성 시작 (model={})", imageGenModel);
                asyncImageService.generateAndUploadAiImage(recipeId, true, imageGenModel);
                log.info("✅ [DevAi V3] 이미지 생성 완료");
            } catch (Exception e) {
                log.warn("⚠️ [DevAi V3] 이미지 생성 중 오류 발생 (레시피는 유지): {}", e.getMessage());
            }

            log.info("⏱️ [DevAi V3] 이미지 생성 소요: {}ms", System.currentTimeMillis() - imageGenStart);

            job.setResultRecipeId(recipeId);
            updateProgress(job, JobStatus.COMPLETED, 100);

            log.info("✅ [DevAi V3] 전체 작업 총 소요: {}ms, jobId={}, recipeId={}",
                    System.currentTimeMillis() - startTime, jobId, recipeId);

        } catch (Exception e) {
            log.error("❌ [DevAi V3] 생성 실패 jobId={}, usedToken={}", jobId, usedToken, e);
            job.setErrorMessage(e.getMessage());
            updateProgress(job, JobStatus.FAILED, 0);

            // 정확한 환불: createJob 시점 결과(usedToken)를 그대로 사용
            // (V2의 항상 true 환불 버그를 V3에서는 회피)
            dailyQuotaService.refund(userId, QuotaType.AI_GENERATION, usedToken);
        }
    }

    /**
     * Phase 3: 상태 조회 (polling) — V2와 동일.
     */
    @Transactional(readOnly = true)
    public JobStatusDto getJobStatus(Long jobId) {
        RecipeGenerationJob job = jobRepository.findById(jobId)
                .orElseThrow(() -> new CustomException(ErrorCode.RESOURCE_NOT_FOUND));

        Long recipeId = job.getResultRecipeId();
        if (job.getStatus() == JobStatus.COMPLETED && recipeId != null
                && !recipeRepository.existsById(recipeId)) {
            recipeId = null;
        }

        return JobStatusDto.builder()
                .jobId(job.getId())
                .status(job.getStatus())
                .resultRecipeId(recipeId)
                .message(job.getErrorMessage())
                .progress(job.getProgress())
                .build();
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void updateProgress(RecipeGenerationJob job, JobStatus status, int progress) {
        job.updateProgress(status, progress);
        jobRepository.saveAndFlush(job);
    }

    // --- private helpers (V2와 동일) ---

    private RecipeCreateRequestDto generateTextRecipe(AiRecipeConcept concept, AiRecipeRequestDto aiReq) {
        // 모든 concept의 system prompt 끝에 dev V3 ingredient fallback override를 명시 append.
        // 운영 V2 builder의 기존 텍스트는 그대로, 마지막에 명시적 "Dev V3 override" 섹션이 우선권.
        String devOverride = com.jdc.recipe_service.dev.util.prompt.DevIngredientPromptOverride.SECTION;
        return switch (concept) {
            case INGREDIENT_FOCUS -> {
                String systemPrompt = ingredientBuilder.buildPrompt(aiReq) + devOverride;
                yield grokClientService.generateRecipeJson(systemPrompt, "위 정보를 바탕으로 레시피 JSON을 생성해줘.").join();
            }
            case COST_EFFECTIVE -> {
                String systemPrompt = costBuilder.buildPrompt(aiReq) + devOverride;
                yield grokClientService.generateRecipeJson(systemPrompt, "위 조건에 맞춰 JSON 결과만 출력해.").join();
            }
            case NUTRITION_BALANCE -> processNutritionLogic(aiReq);
            case FINE_DINING -> {
                FineDiningPromptBuilder.FineDiningPrompt fdPrompt = fineDiningBuilder.buildPrompt(aiReq);
                yield geminiClientService.generateRecipeJson(
                        fdPrompt.getSystemInstruction() + devOverride,
                        fdPrompt.getUserMessage()
                ).join();
            }
        };
    }

    private RecipeCreateRequestDto processNutritionLogic(AiRecipeRequestDto aiReq) {
        String step1System = nutritionBuilder.buildStep1Prompt(aiReq);
        String step1Json = grokClientService.generateRaw(step1System, "위 조건에 맞춰 JSON 결과만 출력해.").join();
        // step2가 최종 레시피 DTO(custom* 포함)를 생성하므로 dev V3 override는 step2 system prompt 끝에 append.
        String step2System = nutritionBuilder.buildStep2Prompt(step1Json)
                + com.jdc.recipe_service.dev.util.prompt.DevIngredientPromptOverride.SECTION;
        return grokClientService.generateRecipeJson(step2System, "위 재료와 양념을 조합하여 완벽한 레시피를 JSON으로 만들어줘.").join();
    }

    private void applySurveyInfoToAiRequest(AiRecipeRequestDto aiReq, UserSurveyDto survey) {
        if (survey == null) return;
        Optional.ofNullable(survey.getSpiceLevel()).ifPresent(aiReq::setSpiceLevel);
        aiReq.setAllergy(survey.getAllergy());
        if (CollectionUtils.isEmpty(aiReq.getTags())) {
            aiReq.setTags(new ArrayList<>(survey.getTags()));
        }
    }

    /**
     * 운영 호출용 step DTO 클론 — 각 step의 ingredients만 빈 리스트로. 원본 step list는 mutate 안 함.
     * 운영 RecipeStepService.saveAll이 step ingredient를 만들지 않게 차단 → dev linkStepIngredients가
     * 다시 연결 (1.3 패턴과 동일).
     */
    private static List<RecipeStepRequestDto> stripStepIngredients(List<RecipeStepRequestDto> originalSteps) {
        if (originalSteps == null) return null;
        return originalSteps.stream()
                .map(s -> s == null ? null : RecipeStepRequestDto.builder()
                        .stepNumber(s.getStepNumber())
                        .instruction(s.getInstruction())
                        .imageKey(s.getImageKey())
                        .action(s.getAction())
                        .timeline(s.getTimeline())
                        .ingredients(List.of())
                        .build())
                .toList();
    }
}
