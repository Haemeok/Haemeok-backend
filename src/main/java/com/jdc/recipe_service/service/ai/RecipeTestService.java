package com.jdc.recipe_service.service.ai;

import com.jdc.recipe_service.domain.dto.ai.RecipeAnalysisResponseDto;
import com.jdc.recipe_service.domain.dto.recipe.AiImageTestRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.AiPromptRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.AiRecipeRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.entity.*;
import com.jdc.recipe_service.domain.repository.*;
import com.jdc.recipe_service.domain.type.*;
import com.jdc.recipe_service.event.AiRecipeCreatedEvent;
import com.jdc.recipe_service.event.UserRecipeCreatedEvent;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.opensearch.service.RecipeIndexingService;
import com.jdc.recipe_service.service.RecipeIngredientService;
import com.jdc.recipe_service.service.RecipeStepService;
import com.jdc.recipe_service.service.RecipeTagService;
import com.jdc.recipe_service.service.image.GeminiImageService;
import com.jdc.recipe_service.service.image.NanoBananaImageService;
import com.jdc.recipe_service.util.PricingUtil;
import com.jdc.recipe_service.util.PromptBuilderV3;
import com.jdc.recipe_service.util.UnitService;
import jakarta.persistence.EntityManager;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class RecipeTestService {

    private final GrokClientService grokClientService;
    private final UnitService unitService;
    private final IngredientRepository ingredientRepo;
    private final RecipeRepository recipeRepository;
    private final PromptBuilderV3 promptBuilder;
    private final NanoBananaImageService nanoBananaImageService;
    private final UserRepository userRepository;
    private final RecipeIngredientRepository recipeIngredientRepository;
    private final RecipeStepService recipeStepService;
    private final RecipeTagService recipeTagService;
    private final RecipeIngredientService recipeIngredientService;
    private final RecipeIndexingService recipeIndexingService;
    private final RecipeAnalysisService recipeAnalysisService;
    private final ApplicationEventPublisher publisher;
    private final EntityManager em;
    private final GeminiImageService geminiImageService;
    private final RecipeImageRepository recipeImageRepository;


    private static final String FIXED_DISH_TYPE_LIST =
            "볶음, 국/찌개/탕, 구이, 무침/샐러드, 튀김/부침, 찜/조림, 오븐요리, 생식/회, 절임/피클류, 밥/면/파스타, 디저트/간식류";
    private static final int DEFAULT_MARGIN_PERCENT = 30;

    /**
     * 신규 추가 메서드: 이미지/DB 저장 없이 AI 레시피 텍스트(JSON DTO)만 생성
     * 순수한 텍스트 생성 테스트에 적합하도록 SurveyService 접근 및 DB 쿼리를 차단합니다.
     *
     * @param robotTypeParam 사용할 AI 모델 유형 (페르소나)
     * @param aiReq          AI 레시피 생성을 위한 요청 DTO
     * @return AI가 생성한 레시피 DTO (RecipeCreateRequestDto를 직접 사용)
     */
    public RecipeCreateRequestDto generateRecipeTextOnly(
            RobotType robotTypeParam,
            AiRecipeRequestDto aiReq) {

        if (aiReq == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "AI 레시피 생성을 위한 요청 정보(aiRequest)가 비어있습니다.");
        }

        if (robotTypeParam == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "AI 모드일 때는 robotType 파라미터가 필요합니다.");
        }

        aiReq.setUserId(null);
        if (aiReq.getSpiceLevel() == null) {
            aiReq.setSpiceLevel(0);
        }
        if (aiReq.getAllergy() == null || aiReq.getAllergy().isBlank()) {
            aiReq.setAllergy("없음");
        }
        if (aiReq.getTags() == null || aiReq.getTags().isEmpty()) {
            aiReq.setTags(Collections.emptyList());
        }

        String prompt = buildTestPrompt(aiReq, robotTypeParam);
        log.info("AI 레시피 텍스트 전용 생성 시작. Prompt: {}", prompt.substring(0, Math.min(200, prompt.length())) + "...");

        RecipeCreateRequestDto generatedDto = null;
        try {
            generatedDto = grokClientService.generateRecipeJson(prompt).join();
        } catch (RuntimeException e) {
            throw new CustomException(
                    ErrorCode.AI_RECIPE_GENERATION_FAILED,
                    "Grok AI 레시피 텍스트 생성에 최종 실패했습니다: " + e.getMessage(), e
            );
        }

        if (generatedDto == null) {
            throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "AI 응답이 null입니다.");
        }

        generatedDto.setIngredients(correctIngredientUnits(generatedDto.getIngredients()));

        if (generatedDto.getSteps() == null || generatedDto.getSteps().isEmpty() ||
                generatedDto.getTags() == null || generatedDto.getTags().isEmpty()) {
            throw new CustomException(
                    ErrorCode.AI_RECIPE_GENERATION_FAILED,
                    "AI가 필수 요리 단계 또는 태그 정보를 생성하지 못했습니다. 다시 시도해 주세요."
            );
        }

        log.info("AI 레시피 텍스트 생성 성공: {}", generatedDto.getTitle());

        return generatedDto;
    }

    /**
     * [TEST용] 템플릿 기반 프롬프트 생성 (완전체)
     * - 재료 미입력 시: UnitService가 로딩한 CSV 전체 재료 리스트 주입
     * - DishType: 고정 리스트 주입
     */
    public RecipeCreateRequestDto generateRecipeFromTemplate(AiPromptRequestDto templateReq) {

        AiRecipeRequestDto data = templateReq.getRequestData();
        String template = templateReq.getPrompt();

        String allowedUnits = unitService.unitsAsString();
        String unitMapping = unitService.mappingAsString();

        List<String> names = (data.getIngredients() != null) ? data.getIngredients() : Collections.emptyList();

        String marketInventoryStr;
        String ingredientsWithUnits;
        String knownListStr = "없음";
        String unknownListStr = "없음";

        if (names.isEmpty()) {
            marketInventoryStr = unitService.getMarketInventoryString();
            if (marketInventoryStr == null) marketInventoryStr = "제공된 재료 데이터 없음";

            ingredientsWithUnits = "없음 (위 [마켓 재료 리스트]에서 AI가 예산/영양에 맞춰 자율 선택)";

        } else {
            List<String> known = ingredientRepo.findAllByNameIn(names).stream()
                    .map(Ingredient::getName).collect(Collectors.toList());
            List<String> unknown = names.stream()
                    .filter(n -> !known.contains(n)).collect(Collectors.toList());

            knownListStr = known.isEmpty() ? "없음" : String.join(", ", known);
            unknownListStr = unknown.isEmpty() ? "없음" : String.join(", ", unknown);

            marketInventoryStr = "없음 (사용자가 재료를 지정했음)";
            ingredientsWithUnits = names.stream()
                    .map(name -> name + "(" + unitService.getDefaultUnit(name).orElse("g") + ")")
                    .collect(Collectors.joining(", "));
        }

        String userDishType = (data.getDishType() != null && !data.getDishType().isBlank())
                ? data.getDishType()
                : "AI 자유 선택 (위 [허용된 요리 종류] 목록 중 택1)";

        String cookingTimeText = (data.getCookingTime() != null && data.getCookingTime() > 0)
                ? String.format("- 희망 조리 시간: %d분 이내", data.getCookingTime())
                : "- 희망 조리 시간: AI 자율 판단";

        String servingsText = (data.getServings() != null && data.getServings() > 0)
                ? String.format("- 인분 수: %.1f인분", data.getServings())
                : "- 인분 수: 1인분 (기본값)";

        String tagsJson = (data.getTags() == null || data.getTags().isEmpty()) ? "[]" : "[\"" + String.join("\", \"", data.getTags()) + "\"]";
        String spiceLevel = (data.getSpiceLevel() != null) ? String.valueOf(data.getSpiceLevel()) : "0";
        String allergy = (data.getAllergy() != null && !data.getAllergy().isBlank()) ? data.getAllergy() : "없음";


        String finalPrompt = template
                .replace("{{UNIT_MAPPING}}", unitMapping)
                .replace("{{ALLOWED_UNITS}}", allowedUnits)

                .replace("{{MARKET_INVENTORY}}", marketInventoryStr)

                .replace("{{KNOWN_INGREDIENTS}}", knownListStr)
                .replace("{{UNKNOWN_INGREDIENTS}}", unknownListStr)
                .replace("{{USER_INGREDIENTS}}", ingredientsWithUnits)

                .replace("{{DISH_TYPE_LIST}}", FIXED_DISH_TYPE_LIST)
                .replace("{{DISH_TYPE}}", userDishType)

                .replace("{{TAGS}}", tagsJson)
                .replace("{{SPICE_LEVEL}}", spiceLevel)
                .replace("{{ALLERGY}}", allergy)
                .replace("{{COOKING_TIME_TEXT}}", cookingTimeText)
                .replace("{{SERVINGS_TEXT}}", servingsText);

        log.info(">>>> [TEMPLATE TEST] Prompt Generated. Length: {}", finalPrompt.length());

        try {
            RecipeCreateRequestDto result = grokClientService.generateRecipeJson(finalPrompt).join();
            if (result != null) {
                result.setIngredients(correctIngredientUnits(result.getIngredients()));
            }
            return result;
        } catch (RuntimeException e) {
            throw new CustomException(
                    ErrorCode.AI_RECIPE_GENERATION_FAILED,
                    "템플릿 테스트 실패: " + e.getMessage(), e
            );
        }
    }

    /**
     * [TEST용] 이미지 생성 후, 이미지가 포함된 레시피 객체 반환
     */
    public RecipeCreateRequestDto testImageGeneration(AiImageTestRequestDto request) {

        RecipeCreateRequestDto recipe = request.getRequestData();
        String promptTemplate = request.getPrompt();

        if (recipe == null || promptTemplate == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "레시피 데이터와 프롬프트가 필요합니다.");
        }

        String title = recipe.getTitle() != null ? recipe.getTitle() : "";
        String dishType = recipe.getDishType() != null ? recipe.getDishType() : "";
        String description = recipe.getDescription() != null ? recipe.getDescription() : "";

        String ingredients = "";
        if (recipe.getIngredients() != null && !recipe.getIngredients().isEmpty()) {
            ingredients = recipe.getIngredients().stream()
                    .map(RecipeIngredientRequestDto::getName)
                    .collect(Collectors.joining(", "));
        }

        String stepsSummary = "";
        if (recipe.getSteps() != null && !recipe.getSteps().isEmpty()) {
            stepsSummary = recipe.getSteps().stream()
                    .map(step -> step.getStepNumber() + ". " + step.getInstruction())
                    .collect(Collectors.joining(" "));
        }

        String tagsDetail = "";
        if (recipe.getTags() != null && !recipe.getTags().isEmpty()) {
            tagsDetail = String.join(", ", recipe.getTags());
        }

        String finalImagePrompt = promptTemplate
                .replace("{{TITLE}}", title)
                .replace("{{DISH_TYPE}}", dishType)
                .replace("{{DESCRIPTION}}", description)
                .replace("{{INGREDIENTS}}", ingredients)
                .replace("{{STEPS_SUMMARY}}", stepsSummary)
                .replace("{{TAGS_DETAIL}}", tagsDetail);

        log.info(">>>> [IMAGE TEST] Generated Prompt: {}", finalImagePrompt);

        try {
            long randomId = System.currentTimeMillis();
            List<String> imageUrls = nanoBananaImageService.generateImageUrls(finalImagePrompt, 0L, randomId);

            if (imageUrls.isEmpty()) {
                throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "이미지 생성 결과 없음");
            }

            String fullImageUrl = imageUrls.get(0);

            recipe.setImageKey(fullImageUrl);

            return recipe;

        } catch (Exception e) {
            throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "이미지 생성 실패: " + e.getMessage());
        }
    }

    /**
     * [REAL - FULL LOGIC]
     * 실제 RecipeService의 저장 로직을 100% 재현하여 DB에 저장하고,
     * 이미지는 커스텀 프롬프트로 생성하여 연결한 뒤,
     * 검색 인덱싱 및 이벤트 발행까지 수행합니다.
     */
    @Transactional
    public RecipeCreateRequestDto createRealRecipeWithCustomImage(Long userId, AiImageTestRequestDto request) {

        RecipeCreateRequestDto dto = request.getRequestData();
        String promptTemplate = request.getPrompt();

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));

        Recipe recipe = Recipe.builder()
                .user(user)
                .title(dto.getTitle())
                .description(dto.getDescription())
                .dishType(DishType.fromDisplayName(dto.getDishType()))
                .cookingTime(dto.getCookingTime())
                .servings(dto.getServings())
                .isAiGenerated(false)
                .isPrivate(true)
                .imageStatus(RecipeImageStatus.PENDING)
                .build();

        recipeRepository.save(recipe);

        int totalCost = recipeIngredientService.saveAll(recipe, dto.getIngredients(), RecipeSourceType.USER);
        recipe.updateTotalIngredientCost(totalCost);

        List<RecipeIngredient> savedIngredients = recipeIngredientRepository.findByRecipeId(recipe.getId());
        calculateAndSetTotalNutrition(recipe, savedIngredients);

        int marketPrice = calculateMarketPrice(dto, totalCost);
        recipe.updateMarketPrice(marketPrice);

        recipeStepService.saveAll(recipe, dto.getSteps());

        recipeTagService.saveAll(recipe, dto.getTags());

        em.flush();
        em.clear();

        String ingredientsStr = "";
        if (dto.getIngredients() != null && !dto.getIngredients().isEmpty()) {
            List<String> ingredientNames = dto.getIngredients().stream()
                    .map(RecipeIngredientRequestDto::getName)
                    .collect(Collectors.toList());

            Map<String, Ingredient> ingredientMap = ingredientRepo.findAllByNameIn(ingredientNames).stream()
                    .collect(Collectors.toMap(Ingredient::getName, ingredient -> ingredient));

            ingredientsStr = dto.getIngredients().stream()
                    .map(ri -> {
                        Ingredient dbIng = ingredientMap.get(ri.getName());

                        String name = (dbIng != null && dbIng.getEnglishName() != null && !dbIng.getEnglishName().isBlank())
                                ? dbIng.getEnglishName()
                                : ri.getName();
                        return name;
                    })
                    .collect(Collectors.joining(", "));
        }

        String stepsSummary = "";
        if (dto.getSteps() != null) {
            stepsSummary = dto.getSteps().stream()
                    .map(step -> step.getStepNumber() + ". " + step.getInstruction())
                    .collect(Collectors.joining(" "));
        }

        String tagsDetail = "";
        if (dto.getTags() != null) {
            tagsDetail = String.join(", ", dto.getTags());
        }

        String finalImagePrompt = promptTemplate
                .replace("{{TITLE}}", dto.getTitle())
                .replace("{{DISH_TYPE}}", dto.getDishType())
                .replace("{{DESCRIPTION}}", dto.getDescription())
                .replace("{{INGREDIENTS}}", ingredientsStr)
                .replace("{{STEPS_SUMMARY}}", stepsSummary)
                .replace("{{TAGS_DETAIL}}", tagsDetail);

        log.info(">>>> [REAL IMAGE GEN] Recipe ID: {}, Prompt: {}", recipe.getId(), finalImagePrompt);

        try {
            List<String> imageUrls = generateImageWithSelectedModel(
                    request.getModel(), finalImagePrompt, userId, recipe.getId()
            );

            if (!imageUrls.isEmpty()) {
                String fullUrl = imageUrls.get(0);
                String s3Key = fullUrl.substring(fullUrl.indexOf(".com/") + 5);

                Recipe savedRecipe = recipeRepository.findById(recipe.getId()).orElseThrow();
                savedRecipe.updateImageKey(s3Key);
                savedRecipe.updateImageStatus(RecipeImageStatus.READY);
                savedRecipe.updateIsPrivate(false);

                RecipeImage recipeImage = RecipeImage.builder()
                        .recipe(savedRecipe)
                        .fileKey(s3Key)
                        .slot("main")
                        .status(ImageStatus.ACTIVE)
                        .build();

                recipeImageRepository.save(recipeImage);

                dto.setImageKey(fullUrl);

                recipeRepository.save(savedRecipe);
            } else {
                throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "이미지 생성 결과 없음");
            }
        } catch (Exception e) {
            log.error("이미지 생성 실패", e);
            Recipe savedRecipe = recipeRepository.findById(recipe.getId()).orElseThrow();
            savedRecipe.updateImageStatus(RecipeImageStatus.FAILED);
            recipeRepository.save(savedRecipe);
        }

        final Long finalRecipeId = recipe.getId();

        TransactionSynchronizationManager.registerSynchronization(
                new TransactionSynchronization() {
                    @Override
                    public void afterCommit() {
                        try {
                            recipeIndexingService.indexRecipeSafelyWithRetry(finalRecipeId);
                        } catch (Exception e) {
                            log.error("인덱싱 실패", e);
                        }

                        publisher.publishEvent(new UserRecipeCreatedEvent(finalRecipeId));

                        recipeAnalysisService.analyzeRecipeAsync(finalRecipeId);

                        log.info(">>>> [REAL PROCESS COMPLETE] ID: {}", finalRecipeId);
                    }
                });

        return dto;
    }

    /**
     * DB 및 SurveyService 호출을 완전히 제거한 테스트 전용 프롬프트 빌더.
     * (기존 PromptBuilderV3 로직을 기반으로 수정됨)
     */
    public String buildTestPrompt(AiRecipeRequestDto request, RobotType type) {

        Integer spicePref = request.getSpiceLevel();
        String allergyPref = request.getAllergy();

        Set<String> themePrefs = (request.getTags() != null && !request.getTags().isEmpty())
                ? new HashSet<>(request.getTags())
                : Collections.emptySet();

        List<String> names = request.getIngredients();
        List<String> known = ingredientRepo.findAllByNameIn(names)
                .stream()
                .map(Ingredient::getName)
                .collect(Collectors.toList());
        List<String> unknown = names.stream()
                .filter(n -> !known.contains(n))
                .collect(Collectors.toList());
        String knownList = known.isEmpty() ? "없음" : String.join(", ", known);
        String unknownList = unknown.isEmpty() ? "없음" : String.join(", ", unknown);

        String allowedUnits = unitService.unitsAsString();
        String unitMapping = unitService.mappingAsString();
        String tagsJson = themePrefs.isEmpty()
                ? "[]"
                : "[\"" + String.join("\", \"", themePrefs) + "\"]";

        String ingredientsWithUnits = names.stream()
                .map(name -> name + "(" + unitService.getDefaultUnit(name).orElse("g") + ")")
                .collect(Collectors.joining(", "));

        String persona;
        switch (type) {
            case CREATIVE -> persona = "너는 매우 창의적이고 새로운 조합을 즐기는 요리 전문가야.";
            case HEALTHY -> persona = "너는 영양 균형과 건강한 조리법을 최우선으로 생각하는 요리 전문가야.";
            case GOURMET -> persona = "너는 풍부하고 깊은 맛을 탐닉하며, 프리미엄 재료로 고급스럽고 섬세한 요리를 선보이는 미식가야.";
            default -> persona = "너는 재료의 풍미와 조리 원리를 극대화하여 **가장 맛있고 풍부한 맛을 구현하는 전문 요리사야.**";
        }

        String cookingTimePart = (request.getCookingTime() != null && request.getCookingTime() > 0)
                ? String.format("- 희망 조리 시간: %d분 이내", request.getCookingTime())
                : "- 희망 조리 시간 정보가 제공되지 않았습니다. AI 모델이 자동으로 예상 조리 시간을 추정하세요.";

        String servingsPart = (request.getServings() != null && request.getServings() > 0)
                ? String.format("- 인분 수: %.1f인분", request.getServings())
                : "- 인분 수 정보가 제공되지 않았습니다. AI 모델이 적절히 판단하여 작성하세요.";

        String systemAndBaseRules = String.format("""
                [SYSTEM] 너는 요리 전문가 AI로서, 오직 하나의 완전한 JSON만 출력해야 합니다.
                설명, 주석, 마크다운, ```json 등 절대 포함하지 마세요.
                **반드시 아래 규칙을 100%% 준수하여 레시피를 생성하세요.**
                
                %s
                
                **[단위 및 DB 재료 정보]**
                - 허용 단위: [%s]
                - 다음 재료들은 반드시 기본 단위로 작성해야 합니다:
                [%s]
                ※ 'unit' 필드는 위 매핑에서 지정된 단위 외에는 절대 사용 불가합니다.
                
                - DB에 이미 있는 재료: [%s]
                - DB에 없는 재료: [%s]
                
                오직 단 하나의 JSON 객체 형태로만 출력하세요.
                """, persona, allowedUnits, unitMapping, knownList, unknownList);

        String jsonFormatRules = """
                **[JSON 출력 형식 규칙]**
                
                --- [🚨 CRITICAL WARNING: 숫자 필드 NULL/공백 절대 금지 🚨] ---
                - **모든 숫자 필드**(`quantity`, `customPrice`, `customCalories`, `customCarbohydrate`, `customProtein`, `customFat`, `customSugar`, `customSodium`, `marketPrice`, `cookingTime`, `servings`)는 **0.00 이상의 유효한 숫자만** 허용됩니다.
                - **절대로 빈 문자열("") 또는 null 값을 사용하지 마세요.** 이를 위반하면 JSON 전체가 무효화되고 에러가 발생합니다.
                ---
                
                **[JSON 세부 필드 규칙]**
                아래는 JSON 필드 각각의 세부 규칙입니다. 반드시 지켜주세요.
                
                --- "title" 필드 (제목 강화 규칙) ---
                - 제목은 **주재료 + 맛 표현 + 요리명** 형식으로 작성
                - 예: '매콤 돼지고기 김치찌개', '얼큰한 두부 김치찌개'
                - 너무 간결한 '김치찌개' 금지
                - 인분/시간 포함 가능 (예: '2인분 25분 매콤 김치찌개')
                
                --- "dishType" 필드 (요리 유형 규칙) ---
                - `dishType`은 반드시 요청된 값("%s")을 **그대로 사용**하거나 (요청에 없을 시) 아래 목록에서 하나만 선택하세요:
                  볶음, 국/찌개/탕, 구이, 무침/샐러드, 튀김/부침, 찜/조림, 오븐요리, 생식/회, 절임/피클류, 밥/면/파스타, 디저트/간식류
                - **절대 빈 문자열("")이나 공백으로 출력되어서는 안 됩니다.**
                
                --- "description" 필드 ---
                - 음식에 대한 설명과 후기를 첨부하세요.
                
                --- "ingredients" 필드 (재료 필드 강제 규칙 - 반드시 준수) ---
                - DB에 없는 재료(%s)는 **반드시** 아래 2개 필드 포함:
                  - `customPrice`: **해당 재료의 Quantity(총량)에 대한 전체 원가** (정수, 원).
                  - `customCalories`: **해당 재료의 Quantity(총량)에 대한 전체 칼로리** (소수점 포함 숫자, kcal)
                  - `customCarbohydrate`: **해당 재료의 Quantity(총량)에 대한 전체 탄수화물** (소수점 포함 숫자, g)
                  - `customProtein`: **해당 재료의 Quantity(총량)에 대한 전체 단백질** (소수점 포함 숫자, g)
                  - `customFat`: **해당 재료의 Quantity(총량)에 대한 전체 지방** (소수점 포함 숫자, g)
                  - `customSugar`: **해당 재료의 Quantity(총량)에 대한 전체 당류** (소수점 포함 숫자, g)
                  - `customSodium`: **해당 재료의 Quantity(총량)에 대한 전체 나트륨** (소수점 포함 숫자, mg)
                  - 이 필드 누락 시 출력 전체 무효
                - DB에 있는 재료는 `customPrice`, `customCalories`,'customCarbohydrate',`customProtein`,`customFat`,`customSugar`,`customSodium` **절대 포함 금지**
                - 또한 모든 재료의 quantity는 요청된 인분 수에 맞추어 자동으로 조절해야 하며, 기본 1인분 기준으로 자연스럽게 확장하거나 축소된 값으로 작성해야 합니다. 인분 수가 제공되지 않은 경우 모델이 적절한 기본 인분을 가정하여 일관성 있게 계산하세요.
                - 재료별 기본 단위 매핑: {%s}
                
                --- "steps" 필드 (단계 규칙) ---
                - "steps" 배열의 "action" 필드는 반드시 아래 19개 중 하나만 사용해야 합니다:
                  썰기, 다지기, 채썰기, 손질하기, 볶기, 튀기기, 끓이기, 찌기(스팀), 데치기, 구이, 조림, 무치기, 절이기, 담그기(마리네이드), 섞기, 젓기, 버무리기, 로스팅, 캐러멜라이즈, 부치기
                - 모든 필드는 의미 있는 한글 내용이어야 하고, 절대로 빈값("")이 될 수 없습니다.
                - "steps" 배열 안의 각 객체는 "stepNumber", "instruction", "action" 키를 모두 포함해야 합니다.
                
                --- "tags" 필드 (태그 규칙) ---
                - 요청한 태그 배열 %s의 원소를 절대로 수정·누락하지 말 것.
                - 만약 요청 태그가 []라면, AI는 아래 허용 목록 중 음식 분위기에 맞는 태그를 **최대 3개** 골라서 반환해야 합니다:
                  🏠 홈파티, 🌼 피크닉, 🏕️ 캠핑, 🥗 다이어트 / 건강식, 👶 아이와 함께, 🍽️ 혼밥, 🍶 술안주, 🥐 브런치, 🌙 야식, ⚡ 초스피드 / 간단 요리, 🎉 기념일 / 명절, 🍱 도시락, 🔌 에어프라이어, 🍲 해장
                
                [CRITICAL 태그 선택 조건] 섹션 내 전체 규칙
                - **Servings 기준:** Servings가 **1인분일 때만** '🍽️ 혼밥' 태그를 선택 가능합니다.
                - **시간 기준:** '⚡ 초스피드 / 간단 요리' 태그는 CookingTime이 **15분 이내**일 경우에만 선택 가능합니다.
                - **조리 방식 기준:** '🔌 에어프라이어' 태그는 레시피의 **`cookingTools` 필드에 '오븐' 또는 '에어프라이어'가 명시적으로 포함**되어 있거나, `dishType`이 **'구이'** 또는 **'튀김/부침'**에 해당될 경우에만 선택 가능합니다.
                - **건강 기준:** '🥗 다이어트 / 건강식' 태그는 **설탕, 튀김류, 가공육(햄/소시지)**이 주재료로 사용되지 않고, **채소나 단백질 위주**의 식단일 경우에만 선택 가능합니다.
                - **나머지 태그 (홈파티, 야식, 술안주 등):** 레시피의 분위기나 재료에 따라 AI가 자유롭게 판단하여 선택합니다.
                - **배제 규칙:** Servings가 2인분 초과일 경우 '🍽️ 혼밥' 태그를 절대 선택 불가. 지방/칼로리가 높거나 조리 시간이 20분 초과(오븐/찜 포함)일 경우 '⚡ 초스피드 / 간단 요리' 또는 '🥗 다이어트 / 건강식' 태그를 절대 선택 불가.
                
                --- "marketPrice" 필드 (배달 가격 규칙) ---
                - 레시피 전체 **실제 예상 배달 가격** (정수, 원)을 한국 배달 앱 기준으로 현실적으로 추정하세요.
                - **[CRITICAL PRICE RULE]** 배달 가격은 **원가, 인건비, 포장비, 마진**을 모두 포함해야 하므로, **절대로 저렴한 가격으로 책정해서는 안 됩니다.** 일반적인 **배달 전문점**의 메뉴판 가격(예: 1인분당 최소 9,000원 이상)을 기준으로 **충분히 현실적인 고가**로 설정하세요.           
                
                --- "cookingTips" 필드 (팁 규칙) ---
                - **서빙 / 맛 강화 / 재활용 / 보조 재료 대체 팁 3~5개**를 생성하세요.
                - 보조 재료 대체 가능하지만, 요리 본연의 맛과 취지를 해치지 않는 범위에서만 허용됩니다. (예: 고춧가루 → 청양고추 O)
                - 반드시 문장 단위로 이어서 작성하고, 숫자나 목록 표시(1, 2, 3...)는 사용하지 마세요.
                
                --- 기타 필드 ---
                - `cookingTime`, `cookingTools`, `servings`는 요청 조건과 요리 원리에 맞춰 적절히 작성하세요.
                
                
                [단계 설명 규칙 - 전문 레시피처럼 자연스럽고 품질감 있게]
                - **각 단계는 자연스럽고 논리적인 흐름으로 구성** (재료 손질 → 풍미 베이스 → 본 조리 → 마무리)
                - **[CRITICAL 단계 규칙]** 재료 손질(썰기, 다지기) 단계와 양념장/마리네이드 준비(섞기, 담그기) 단계를 **논리적으로 분리**하여 명확성을 높이세요. 재료 손질 단계를 끝낸 후 다음 단계에서 양념 준비를 시작하세요.
                - **초보자도 바로 따라할 수 있도록 (묘사 강화):** 불 세기, 시간, 재료 상태 변화를 구체적인 형용사나 부사를 사용하여 묘사하세요.
                - **문장 구성:** 2~3개 문장, 최대 150자 이내로 작성하고 끝은 '주세요', '하세요', '합니다' 등 자연스럽게 마무리하세요.
                - **보조 설명:** 조리 과정에 대한 보조 설명이나 팁은 별도로 분리하지 않고, 현재 단계의 instruction 뒤에 자연스러운 다음 문장으로 연결하여 추가하세요.
                
                
                [요리 원리 규칙]
                1. **(핵심)** 찌개·볶음·조림 요리에서는 기름에 주재료나 향신채(마늘·파 등)를 먼저 볶아 풍미의 기초를 다지는 과정을 최우선으로 고려하세요.
                2. 효율적이고 논리적인 순서로 단계를 구성하세요.
                3. 요청에 없더라도 필수 보조 재료를 자유롭게 추가하고 'ingredients'에 포함시키세요.
                4. **[CRITICAL] 각 재료의 양은 요청된 인분 수(servings)에 100%% 비례하여 조정할 것.**
                   - **(최우선 무게 기준)** AI는 모든 주재료(육류, 해산물, 생선)에 대해 **1인분당 최소 180g ~ 200g의 포만감**을 충족하도록 추정해야 합니다.
                   - **[개수-무게 계산 공식 적용]** 주재료가 '마리'나 '개' 단위일 경우, **1마리(개)당 평균 무게를 논리적으로 추정**하고, **1인분 최소 무게(180g)를 해당 무게로 나누어 필요한 개수**를 계산해야 합니다.
                   - **[CRITICAL 제외]** **양념류, 향신료, 감칠맛 재료(예: 고추장, 쯔유, 가다랑어포)**는 이 무게 기준을 따르지 않으며, **요리의 맛과 풍미를 내는 최소한의 합리적인 양**으로 설정되어야 합니다.
                   - 양념 및 부재료도 주재료의 양에 맞춰 풍미를 충분히 낼 수 있도록 비례하여 증가시킬 것.
                5. **[핵심: 맛의 완성도 목표]** 레시피는 **실제 전문 요리사가 만든 것처럼 깊은 맛**을 내야 하며, **맛의 깊이(단짠 균형, 감칠맛)**와 **풍미(향)**를 최우선 목표로 구현하세요.
                   - **(기능적 충족)** 볶음/조림 요리 시, 육류나 해산물의 **잡내를 제거**하는 재료(예: 미림, 청주)를 사용하고, 마지막에 **고소한 마무리 향**을 더하는 재료(예: 참기름, 들기름)를 **요리의 종류에 맞춰 자유롭게 선택**하세요.
                   - **(편향 제거)** AI가 자체적으로 가진 **건강이나 다이어트 편향을 버리고**, **가장 맛있고 풍부한 결과물**을 만들도록 양념을 **충분하고 복합적**으로 설정해야 합니다.
                6. **[새 재료 단위 추론 원칙]** DB에 없는 **새로운 재료**의 단위를 설정할 때, 해당 재료의 **물리적 특성**에 따라 가장 현실적인 단위를 선택해야 합니다.
                    - **액체류 (오일, 우유, 청주 등):** 무게(`g`) 대신 **부피 단위(`ml`, `큰술`, `작은술`)**를 우선 사용합니다.
                    - **분말류 (가루, 향신료):** 무게(`g`) 또는 **용량 단위(`큰술`, `작은술`)**를 사용합니다.
                    - **고형물 (채소, 고기):** 무게(`g`)를 기본으로 하되, 크기가 일정하면 **개수(`개`, `모`, `마리`)**를 사용할 수 있습니다.
                """.formatted(
                request.getDishType() != null && !request.getDishType().isBlank() ? request.getDishType() : "",
                unknownList,
                unitMapping,
                tagsJson
        );


        String fewShotExample = """
                {
                  "title": "샘플 요리 제목",
                  "dishType": "볶음",
                  "description": "이것은 JSON 구조를 보여주기 위한 샘플 레시피입니다. 내용을 복사하지 마세요.",
                  "cookingTime": 20,
                  "cookingTools": ["팬", "주걱"],
                  "servings": 2.0,
                  "ingredients": [
                    { "name": "주재료A", "quantity": "100", "unit": "g", "customPrice": 50, "customCalories": 300.0, "customCarbohydrate": 20.50, "customProtein": 35.00, "customFat": 10.00, "customSugar": 5.00, "customSodium": 150 },
                    { "name": "주재료B", "quantity": "1", "unit": "개" },
                    { "name": "보조재료C", "quantity": "1", "unit": "작은술" }
                  ],
                  "steps": [
                    { "stepNumber": 0, "instruction": "재료를 손질합니다.", "action": "손질하기" },
                    { "stepNumber": 1, "instruction": "팬에 재료를 볶습니다.", "action": "볶기" }
                  ],
                  "tags": ["🍽️ 혼밥"],
                  "marketPrice": 8000,
                   "cookingTips": "팁1. 팁2. 팁3."
                }
                """;

        String requestContext = """
                요청 조건:
                - 요리 유형: %s
                %s
                %s
                - 매운맛 선호도: %s/5
                - 알레르기 정보: %s
                - 요리 테마 선호 태그: %s
                - 주요 재료: %s
                - 태그: %s
                """.formatted(
                request.getDishType() != null && !request.getDishType().isBlank() ? request.getDishType() : "AI 자동 선택",
                cookingTimePart,
                servingsPart,
                spicePref != null ? spicePref : "기본",
                allergyPref != null && !allergyPref.isBlank() ? allergyPref : "없음",
                tagsJson,
                ingredientsWithUnits,
                tagsJson
        );

        return systemAndBaseRules
                + "\n\n--- 예시 JSON ---\n"
                + fewShotExample
                + "\n--- 예시 끝 ---\n"
                + "\n\n" + requestContext
                + "\n\n" + jsonFormatRules;
    }


    private List<RecipeIngredientRequestDto> correctIngredientUnits(List<RecipeIngredientRequestDto> ingredients) {
        return ingredients.stream()
                .map(ing -> {
                    String finalUnit = unitService.getDefaultUnit(ing.getName())
                            .orElse(ing.getCustomUnit());
                    return RecipeIngredientRequestDto.builder()
                            .name(ing.getName())
                            .quantity(ing.getQuantity())
                            .customPrice(ing.getCustomPrice())
                            .customUnit(finalUnit)
                            .customCalories(ing.getCustomCalories())
                            .customCarbohydrate(ing.getCustomCarbohydrate())
                            .customProtein(ing.getCustomProtein())
                            .customFat(ing.getCustomFat())
                            .customSugar(ing.getCustomSugar())
                            .customSodium(ing.getCustomSodium())
                            .build();
                })
                .collect(Collectors.toList());
    }

    /**
     * [테스트용] 특정 레시피 ID에 대해 분석(가격/팁/욕설)만 수행하고 결과를 반환
     * (DB 업데이트 X)
     */
    public RecipeAnalysisResponseDto analyzeRecipeTest(Long recipeId) {
        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        String prompt = promptBuilder.buildAnalysisPrompt(recipe);
        log.info(">>>> [TEST] Analysis Prompt Generated: \n{}", prompt);

        return grokClientService.analyzeRecipe(prompt).join();
    }

    private static int calculateMarketPrice(RecipeCreateRequestDto dto, int totalCost) {
        Integer providedMp = dto.getMarketPrice();
        int marketPrice = (providedMp != null && providedMp > 0)
                ? providedMp
                : (totalCost > 0
                ? PricingUtil.applyMargin(totalCost, PricingUtil.randomizeMarginPercent(DEFAULT_MARGIN_PERCENT))
                : 0);

        // AI 생성일 경우 원가보다 낮으면 강제 마진 적용
        if (marketPrice < totalCost) {
            marketPrice = PricingUtil.applyMargin(totalCost, DEFAULT_MARGIN_PERCENT);
        }
        return marketPrice;
    }

    private void calculateAndSetTotalNutrition(Recipe recipe, List<com.jdc.recipe_service.domain.entity.RecipeIngredient> ingredients) {
        java.math.BigDecimal totalCalorie = java.math.BigDecimal.ZERO;
        java.math.BigDecimal totalCarb = java.math.BigDecimal.ZERO;
        java.math.BigDecimal totalProtein = java.math.BigDecimal.ZERO;
        java.math.BigDecimal totalFat = java.math.BigDecimal.ZERO;
        java.math.BigDecimal totalSugar = java.math.BigDecimal.ZERO;
        java.math.BigDecimal totalSodium = java.math.BigDecimal.ZERO;

        for (com.jdc.recipe_service.domain.entity.RecipeIngredient ri : ingredients) {
            java.math.BigDecimal quantity = parseQuantityToBigDecimal(ri.getQuantity());

            if (ri.getIngredient() != null) {
                Ingredient ing = ri.getIngredient();
                totalCalorie = totalCalorie.add(ing.getCalorie().multiply(quantity));
                totalCarb = totalCarb.add(ing.getCarbohydrate().multiply(quantity));
                totalProtein = totalProtein.add(ing.getProtein().multiply(quantity));
                totalFat = totalFat.add(ing.getFat().multiply(quantity));
                totalSugar = totalSugar.add(ing.getSugar().multiply(quantity));
                totalSodium = totalSodium.add(ing.getSodium().multiply(quantity));
            } else {
                totalCalorie = totalCalorie.add(ri.getCustomCalorie());
                totalCarb = totalCarb.add(ri.getCustomCarbohydrate());
                totalProtein = totalProtein.add(ri.getCustomProtein());
                totalFat = totalFat.add(ri.getCustomFat());
                totalSugar = totalSugar.add(ri.getCustomSugar());
                totalSodium = totalSodium.add(ri.getCustomSodium());
            }
        }
        recipe.updateNutrition(totalProtein, totalCarb, totalFat, totalSugar, totalSodium, totalCalorie);
    }

    private java.math.BigDecimal parseQuantityToBigDecimal(String quantityStr) {
        if (quantityStr == null || quantityStr.isBlank()) return java.math.BigDecimal.ZERO;
        String cleanStr = quantityStr.replaceAll("[^0-9./]", "");
        try {
            if (cleanStr.contains("/")) {
                String[] parts = cleanStr.split("/");
                if (parts.length == 2) {
                    double num = Double.parseDouble(parts[0]);
                    double den = Double.parseDouble(parts[1]);
                    if (den == 0) return java.math.BigDecimal.ZERO;
                    return java.math.BigDecimal.valueOf(num / den);
                }
            }
            return new java.math.BigDecimal(cleanStr);
        } catch (Exception e) {
            return java.math.BigDecimal.ZERO;
        }
    }

    private List<String> generateImageWithSelectedModel(ImageGenModel model, String prompt, Long userId, Long recipeId) {
        if (model == ImageGenModel.GEMINI) {
            return geminiImageService.generateImageUrls(prompt, userId, recipeId);
        }
        return nanoBananaImageService.generateImageUrls(prompt, userId, recipeId);
    }
}
