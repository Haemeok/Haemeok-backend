package com.jdc.recipe_service.util;

import com.jdc.recipe_service.domain.dto.recipe.AiRecipeRequestDto;
import com.jdc.recipe_service.domain.dto.user.UserSurveyDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.type.RobotType;
import com.jdc.recipe_service.service.SurveyService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.stream.Collectors;

@Component
@Slf4j
public class PromptBuilder {

    private final IngredientRepository ingredientRepo;
    private final SurveyService surveyService;
    private final UnitService unitService;

    public PromptBuilder(IngredientRepository ingredientRepo, SurveyService surveyService, UnitService unitService) {
        this.ingredientRepo = ingredientRepo;
        this.surveyService = surveyService;
        this.unitService = unitService;
    }

    public String buildPrompt(AiRecipeRequestDto request, RobotType type) {
        String mappingString = unitService.mappingAsString();
        log.info("🔧 강제 단위 매핑: {}", mappingString);

        UserSurveyDto survey = surveyService.getSurvey(request.getUserId());
        Integer spicePref = (survey != null && survey.getSpiceLevel() != null) ? survey.getSpiceLevel() : request.getSpiceLevel();
        String allergyPref = (survey != null && survey.getAllergy() != null && !survey.getAllergy().isBlank()) ? survey.getAllergy() : request.getAllergy();
        Set<String> themePrefs;
        if (survey != null && survey.getTags() != null && !survey.getTags().isEmpty()) {
            themePrefs = survey.getTags();
        } else if (request.getTagNames() != null && !request.getTagNames().isEmpty()) {
            themePrefs = new HashSet<>(request.getTagNames());
        } else {
            themePrefs = Collections.emptySet();
        }
        String tagsJson = themePrefs.isEmpty() ? "[]" : "[\"" + String.join("\", \"", themePrefs) + "\"]";

        List<String> names = request.getIngredients();
        List<String> known = ingredientRepo.findAllByNameIn(names).stream().map(Ingredient::getName).collect(Collectors.toList());
        List<String> unknown = names.stream().filter(n -> !known.contains(n)).collect(Collectors.toList());
        String knownList = known.isEmpty() ? "없음" : String.join(", ", known);
        String unknownList = unknown.isEmpty() ? "없음" : String.join(", ", unknown);

        String persona;
        switch (type) {
            case CREATIVE -> persona = "너는 매우 창의적이고 새로운 조합을 즐기는 한국 요리 전문가 페르소나로 응답해줘.";
            case HEALTHY -> persona = "너는 영양 균형과 건강한 조리법을 최우선으로 생각하는 요리 전문가 페르소나로 응답해줘.";
            case GOURMET -> persona = "너는 풍부하고 깊은 맛을 탐닉하며, 프리미엄 재료로 고급스럽고 섬세한 요리를 선보이는 미식가 페르소나로 응답해줘.";
            default -> persona = "너는 '백종원'처럼 조리 원리를 잘 이해하고 맛의 깊이를 더하는 전문 한국 요리사 페르소나로 응답해줘.";
        }

        String servingsInstruction = (request.getServings() != null && request.getServings() > 0)
                ? String.format("Few-Shot 예시는 2인분 기준이다. 너는 반드시 %.1f인분에 맞게 각 재료의 quantity를 비례하여 조정해야 한다. (계산식: 예시 양 × (%.1f ÷ 2))", request.getServings(), request.getServings())
                : "인분 수가 제공되지 않았으므로, 2인분 기준으로 레시피를 생성해줘.";

        String quantityRules = """
                - quantity 필드는 오직 숫자(소수점 구분 ‘.’)만 포함하세요. 분수(1/2) 대신 0.5 형태로 변환할 것.
                 - 단위(unit) 정보는 절대로 quantity에 포함하지 말고 unit 필드에만 표기하세요.
                """;

        String unitMappingRules = String.format("""
                - DB에 저장된 재료는 반드시 아래 ‘재료별 기본 단위 매핑’에 명시된 단위만 사용하세요:
                  %s
                """, unitService.mappingAsString());

        String specialInstructions = servingsInstruction
                + "\n" + quantityRules
                + "\n" + unitMappingRules;


        return String.format("""
                        아래 요청 조건에 맞춰 레시피 JSON을 생성해줘.
                        
                        [페르소나]
                        %s
                        
                        [요청 조건]
                        - DB에 이미 있는 재료: [%s]
                        - DB에 없는 재료: [%s]
                        - 요리 유형: %s
                        - 희망 조리 시간: %s
                        - 인분 수: %s
                        - 매운맛 선호도: %s/5
                        - 알레르기 정보: %s
                        - 주요 재료: %s
                        - 요청 태그: %s
                        
                        [특별 지시]
                        %s
                        """,
                persona,
                knownList,
                unknownList,
                Optional.ofNullable(request.getDishType())
                        .filter(s -> !s.isBlank())
                        .orElse("AI가 자유롭게 결정"),
                (request.getCookingTime() != null && request.getCookingTime() > 0) ? request.getCookingTime() + "분 이내" : "AI가 자동으로 추정",
                (request.getServings() != null && request.getServings() > 0) ? request.getServings() + "인분" : "AI가 적절히 판단",
                spicePref != null ? spicePref : "기본",
                allergyPref != null && !allergyPref.isBlank() ? allergyPref : "없음",
                String.join(", ", request.getIngredients()),
                tagsJson,
                specialInstructions
        );
    }
}