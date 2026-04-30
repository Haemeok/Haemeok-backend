package com.jdc.recipe_service.dev.service.report;

import com.jdc.recipe_service.dev.service.interaction.DevRecipeAccessValidator;
import com.jdc.recipe_service.domain.dto.report.IngredientReportRequest;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.entity.RecipeIngredientReport;
import com.jdc.recipe_service.domain.repository.RecipeIngredientReportRepository;
import com.jdc.recipe_service.domain.repository.RecipeIngredientRepository;
import com.jdc.recipe_service.domain.type.ReportReason;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevReportService 분기 매트릭스 (raw-first 매칭).
 *
 * <p>운영 ReportService에 위임하지 않고 자체 매칭 로직을 사용한다 — rawName > customName >
 * ingredient.name 우선순위. 상세 화면 표시 우선순위와 동일해야 사용자가 본 row와 신고가 일치.
 */
@ExtendWith(MockitoExtension.class)
@org.mockito.junit.jupiter.MockitoSettings(strictness = org.mockito.quality.Strictness.LENIENT)
class DevReportServiceTest {

    @Mock DevRecipeAccessValidator accessValidator;
    @Mock RecipeIngredientRepository recipeIngredientRepository;
    @Mock RecipeIngredientReportRepository reportRepository;

    @InjectMocks DevReportService devReportService;

    private static final Long USER_ID = 7L;
    private static final Long RECIPE_ID = 100L;

    @BeforeEach
    void setUp() {
        // save echo로 captor 검증 가능하게
        given(reportRepository.save(org.mockito.ArgumentMatchers.any(RecipeIngredientReport.class)))
                .willAnswer(inv -> inv.getArgument(0));
    }

    private static IngredientReportRequest request(String name) {
        IngredientReportRequest r = new IngredientReportRequest();
        ReflectionTestUtils.setField(r, "ingredientName", name);
        ReflectionTestUtils.setField(r, "reason", ReportReason.WRONG_NAME);
        ReflectionTestUtils.setField(r, "memo", "test memo");
        return r;
    }

    private static Ingredient master(Long id, String name) {
        return Ingredient.builder().id(id).name(name).build();
    }

    private static RecipeIngredient row(Long id, Ingredient master, String rawName, String customName) {
        return RecipeIngredient.builder()
                .id(id)
                .ingredient(master)
                .rawName(rawName)
                .customName(customName)
                .resolutionStatus(master != null ? "MAPPED" : "UNRESOLVED")
                .build();
    }

    @Test
    @DisplayName("[report] request=null → INVALID_INPUT_VALUE, validator/repo/save 모두 미호출")
    void report_nullRequest_throws() {
        assertThatThrownBy(() -> devReportService.createReportByName(USER_ID, RECIPE_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode").isEqualTo(ErrorCode.INVALID_INPUT_VALUE);

        verifyNoInteractions(accessValidator, recipeIngredientRepository, reportRepository);
    }

    @Test
    @DisplayName("[report] ingredientName 비어 있음 → INVALID_INPUT_VALUE (validator는 통과 후 검사)")
    void report_blankName_throws() {
        // validator는 통과시키고, 이후 빈 이름 검사에서 실패해야 함
        IngredientReportRequest req = request("   ");

        assertThatThrownBy(() -> devReportService.createReportByName(USER_ID, RECIPE_ID, req))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode").isEqualTo(ErrorCode.INVALID_INPUT_VALUE);

        verify(reportRepository, never()).save(org.mockito.ArgumentMatchers.any());
    }

    @Test
    @DisplayName("[report] **🚨 운영 leak 차단**: validator throw (RESTRICTED non-owner) → 매칭 시도 없이 즉시 실패")
    void report_validatorThrowsAccessDenied_skipsReportSave() {
        willThrow(new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED))
                .given(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);

        assertThatThrownBy(() -> devReportService.createReportByName(USER_ID, RECIPE_ID, request("감자")))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode").isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);

        verifyNoInteractions(recipeIngredientRepository, reportRepository);
    }

    @Test
    @DisplayName("[report] **MUST 회귀 차단**: rawName 우선 매칭 — ingredient.name이 다르더라도 rawName이 같으면 그 row를 잡는다")
    void report_rawNameTakesPrecedenceOverIngredientName() {
        // 사용자가 상세 화면에서 본 표시 = rawName "청양고추"인데, ingredient.name은 "고추"라 다름.
        // 운영 매칭(ingredient.name 기준)은 "청양고추" 신고 시 못 찾음 — dev는 rawName으로 정확히 매칭.
        Ingredient pepper = master(1L, "고추");
        RecipeIngredient ri = row(101L, pepper, "청양고추", null);
        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(ri));

        devReportService.createReportByName(USER_ID, RECIPE_ID, request("청양고추"));

        ArgumentCaptor<RecipeIngredientReport> captor = ArgumentCaptor.forClass(RecipeIngredientReport.class);
        verify(reportRepository).save(captor.capture());
        RecipeIngredientReport saved = captor.getValue();
        assertThat(saved.getIngredientId())
                .as("rawName 매칭 성공 → 그 row의 ingredient.id 저장 (legacy 호환)")
                .isEqualTo(1L);
        assertThat(saved.getProposedName()).isEqualTo("청양고추");
    }

    @Test
    @DisplayName("[report] **MUST 회귀 차단**: customName 매칭 (C' bypass row — ingredient_id=null + customName=raw)")
    void report_customNameMatch_forBypassRow() {
        // C' bypass row: ingredient=null, customName="마늘". rawName도 "마늘"인 일반적인 케이스.
        RecipeIngredient bypass = row(102L, null, "마늘", "마늘");
        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(bypass));

        devReportService.createReportByName(USER_ID, RECIPE_ID, request("마늘"));

        ArgumentCaptor<RecipeIngredientReport> captor = ArgumentCaptor.forClass(RecipeIngredientReport.class);
        verify(reportRepository).save(captor.capture());
        RecipeIngredientReport saved = captor.getValue();
        assertThat(saved.getIngredientId())
                .as("ingredient=null 이면 ri.id 저장 (legacy 호환)")
                .isEqualTo(102L);
    }

    @Test
    @DisplayName("[report] **MUST 회귀 차단**: rawName 빈/누락이면 customName으로 fallback")
    void report_fallbackToCustomName_whenRawNameMissing() {
        RecipeIngredient ri = RecipeIngredient.builder()
                .id(103L)
                .ingredient(null)
                .rawName(null)               // legacy row — rawName 미백필
                .customName("황태머리")
                .resolutionStatus("UNRESOLVED")
                .build();
        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(ri));

        devReportService.createReportByName(USER_ID, RECIPE_ID, request("황태머리"));

        ArgumentCaptor<RecipeIngredientReport> captor = ArgumentCaptor.forClass(RecipeIngredientReport.class);
        verify(reportRepository).save(captor.capture());
        assertThat(captor.getValue().getIngredientId()).isEqualTo(103L);
    }

    @Test
    @DisplayName("[report] **MUST 회귀 차단**: rawName/customName 둘 다 없으면 ingredient.name으로 fallback")
    void report_fallbackToIngredientName_whenRawAndCustomMissing() {
        Ingredient potato = master(2L, "감자");
        RecipeIngredient ri = RecipeIngredient.builder()
                .id(104L)
                .ingredient(potato)
                .rawName(null)
                .customName(null)
                .resolutionStatus("MAPPED")
                .build();
        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(ri));

        devReportService.createReportByName(USER_ID, RECIPE_ID, request("감자"));

        ArgumentCaptor<RecipeIngredientReport> captor = ArgumentCaptor.forClass(RecipeIngredientReport.class);
        verify(reportRepository).save(captor.capture());
        assertThat(captor.getValue().getIngredientId()).isEqualTo(2L);
    }

    @Test
    @DisplayName("[report] **SHOULD 회귀 차단**: canonicalizeName 정규화 — '청 양 고추' 신고 vs row rawName='청양고추' 매칭됨")
    void report_canonicalizeAbsorbsWhitespaceVariation() {
        Ingredient pepper = master(1L, "고추");
        RecipeIngredient ri = row(101L, pepper, "청양고추", null);
        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(ri));

        // 사용자가 띄어쓰기 변형으로 신고
        devReportService.createReportByName(USER_ID, RECIPE_ID, request(" 청 양 고추 "));

        ArgumentCaptor<RecipeIngredientReport> captor = ArgumentCaptor.forClass(RecipeIngredientReport.class);
        verify(reportRepository).save(captor.capture());
        assertThat(captor.getValue().getIngredientId()).isEqualTo(1L);
    }

    @Test
    @DisplayName("[report] **MUST 회귀 차단**: rawName='청양고추' + ingredient.name='고추' row에 '고추' 신고는 안 붙음 (표시명 단일 비교)")
    void report_displayNameOnly_doesNotCrossMatchIngredientName() {
        // 화면에 "청양고추"로 표시되는 row인데, ingredient.name이 "고추"라는 이유만으로 "고추" 신고가
        // 이 row에 붙으면 사용자가 본 적도 없는 row를 신고한 것으로 처리됨 — 표시명과 신고 target을
        // 단일 비교해야 일관됨. 이 row의 표시명은 rawName="청양고추"이지 "고추"가 아니다.
        Ingredient pepper = master(1L, "고추");
        RecipeIngredient ri = row(101L, pepper, "청양고추", null);
        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(ri));

        devReportService.createReportByName(USER_ID, RECIPE_ID, request("고추"));

        ArgumentCaptor<RecipeIngredientReport> captor = ArgumentCaptor.forClass(RecipeIngredientReport.class);
        verify(reportRepository).save(captor.capture());
        assertThat(captor.getValue().getIngredientId())
                .as("**핵심**: 표시명='청양고추' row에 '고추' 신고 → 매칭 실패 → null id로 저장 (운영자 검수)")
                .isNull();
        assertThat(captor.getValue().getProposedName()).isEqualTo("고추");
    }

    @Test
    @DisplayName("[report] 매칭 실패 → ingredientId=null, proposedName 보존된 채 저장")
    void report_noMatch_savesWithNullIngredientId() {
        Ingredient garlic = master(1L, "마늘");
        RecipeIngredient ri = row(101L, garlic, "마늘", null);
        given(recipeIngredientRepository.findByRecipeId(RECIPE_ID)).willReturn(List.of(ri));

        devReportService.createReportByName(USER_ID, RECIPE_ID, request("황태머리"));

        ArgumentCaptor<RecipeIngredientReport> captor = ArgumentCaptor.forClass(RecipeIngredientReport.class);
        verify(reportRepository).save(captor.capture());
        RecipeIngredientReport saved = captor.getValue();
        assertThat(saved.getIngredientId())
                .as("매칭 실패해도 report는 생성 (운영자가 검수) — ingredientId만 null")
                .isNull();
        assertThat(saved.getProposedName()).isEqualTo("황태머리");
        assertThat(saved.getRecipeId()).isEqualTo(RECIPE_ID);
        assertThat(saved.getMemberId()).isEqualTo(USER_ID);
    }
}
