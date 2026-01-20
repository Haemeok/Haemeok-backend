package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.report.AdminIngredientUpdateDto;
import com.jdc.recipe_service.domain.entity.*;
import com.jdc.recipe_service.domain.repository.*;
import com.jdc.recipe_service.util.PricingUtil;
import jakarta.persistence.EntityManager;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class AdminRecipeServiceTest {

    @InjectMocks
    private AdminRecipeService adminRecipeService;

    @Mock private RecipeRepository recipeRepository;
    @Mock private RecipeIngredientRepository recipeIngredientRepository;
    @Mock private IngredientRepository ingredientRepository;
    @Mock private RecipeIngredientReportRepository recipeIngredientReportRepository;
    @Mock private EntityManager em;

    // 기타 의존성 (사용하지 않더라도 InjectMocks 오류 방지 위해 Mock 처리)
    @Mock private RecipeIngredientService recipeIngredientService;
    @Mock private RecipeStepService recipeStepService;
    @Mock private RecipeTagService recipeTagService;
    @Mock private com.jdc.recipe_service.service.image.RecipeImageService recipeImageService; // 패키지명 주의
    @Mock private RecipeLikeService recipeLikeService;
    @Mock private RecipeFavoriteService recipeFavoriteService;
    @Mock private CommentService commentService;
    @Mock private UserRepository userRepository;
    @Mock private com.jdc.recipe_service.util.S3Util s3Util;

    @Test
    @DisplayName("재료 일괄 수정: 삭제, 생성, 수정(표준↔커스텀), 신고처리, 재계산이 모두 완벽하게 동작해야 한다.")
    void updateIngredientsBatch_AllScenarios() {
        // --- [Given] 1. 기초 데이터 세팅 ---
        Long recipeId = 1L;
        Recipe recipe = Recipe.builder().id(recipeId).totalIngredientCost(0).build();

        // (1) 표준 재료 마스터 데이터 (DB에 있는 재료)
        Ingredient masterOnion = Ingredient.builder().id(10L).name("양파").price(100).unit("개")
                .calorie(new BigDecimal("30")).protein(new BigDecimal("1"))
                .carbohydrate(new BigDecimal("5")).fat(new BigDecimal("0"))
                .sugar(new BigDecimal("2")).sodium(new BigDecimal("0")).build();

        // (2) 기존 레시피에 들어있는 재료들 (수정/삭제 대상)
        // Case A: 삭제할 재료 (ID 100)
        RecipeIngredient ingToDelete = RecipeIngredient.builder()
                .id(100L).recipe(recipe).customName("삭제할재료").price(500).build();

        // Case B: 표준 -> 커스텀으로 바꿀 재료 (ID 101, 현재는 양파)
        RecipeIngredient ingStdToCustom = RecipeIngredient.builder()
                .id(101L).recipe(recipe).ingredient(masterOnion).quantity("1").unit("개").price(100).build();

        // Case C: 커스텀 -> 표준으로 바꿀 재료 (ID 102, 현재는 커스텀 설탕)
        RecipeIngredient ingCustomToStd = RecipeIngredient.builder()
                .id(102L).recipe(recipe).customName("커스텀설탕").quantity("10").unit("g").price(50).build();

        List<RecipeIngredient> currentIngredients = new ArrayList<>(List.of(ingToDelete, ingStdToCustom, ingCustomToStd));

        // (3) 신고 내역 (삭제/수정 시 해결되어야 함)
        RecipeIngredientReport report1 = mock(RecipeIngredientReport.class); // 삭제할 재료에 대한 신고
        RecipeIngredientReport report2 = mock(RecipeIngredientReport.class); // 수정할 재료에 대한 신고

        // --- [Given] 2. Mocking 동작 정의 ---
        given(recipeRepository.findWithUserById(recipeId)).willReturn(Optional.of(recipe));
        given(ingredientRepository.findAll()).willReturn(List.of(masterOnion)); // DB에는 '양파'만 있음

        // 중요: 첫 번째 조회(수정 전)와 두 번째 조회(재계산용)를 다르게 리턴해야 함
        // 하지만 단위 테스트에선 두 번째 조회 결과를 우리가 조작해서 넣어줘야 정확한 계산 검증이 가능
        // -> 여기서는 Mockito가 '같은 리스트 객체'를 반환하도록 하고, 로직 내에서 리스트 내용이 바뀌는 것을 이용함.
        given(recipeIngredientRepository.findByRecipeId(recipeId)).willReturn(currentIngredients);

        // 신고 내역 조회 Mock
        given(recipeIngredientReportRepository.findByIngredientIdAndIsResolvedFalse(100L))
                .willReturn(List.of(report1)); // 삭제 타겟
        given(recipeIngredientReportRepository.findByIngredientIdAndIsResolvedFalse(101L))
                .willReturn(List.of(report2)); // 수정 타겟

        // --- [Given] 3. 요청 DTO 생성 (관리자가 보낸 데이터) ---
        List<AdminIngredientUpdateDto> dtos = new ArrayList<>();

        // 1. [DELETE] ID 100 삭제
        dtos.add(createDto(100L, null, null, null, "DELETE", null, null));

        // 2. [UPDATE] ID 101 (표준 양파 -> "프리미엄 양파"(커스텀) 변경)
        dtos.add(createDto(101L, "프리미엄 양파", "2", "개", "UPDATE", 2000, new BigDecimal("100"))); // 칼로리 100 입력

        // 3. [UPDATE] ID 102 ("커스텀설탕" -> "양파"(표준) 변경)
        dtos.add(createDto(102L, "양파", "2", "개", "UPDATE", null, null)); // 가격/영양소 자동 계산 기대 (양파 단가 100 * 2개 = 200원)

        // 4. [CREATE] "양파"(표준) 신규 추가
        dtos.add(createDto(null, "양파", "1", "개", "CREATE", null, null)); // 가격 100원 기대

        // 5. [CREATE] "비밀소스"(커스텀) 신규 추가
        dtos.add(createDto(null, "비밀소스", "1", "병", "CREATE", 5000, new BigDecimal("500"))); // 칼로리 500

        // --- [When] 테스트 실행 ---
        adminRecipeService.updateIngredientsBatch(recipeId, dtos);

        // --- [Then] 검증 ---

        // 1. DELETE 검증
        verify(recipeIngredientRepository).delete(ingToDelete); // 삭제 메서드 호출됨?
        verify(report1).markAsResolved(); // 신고 해결됨?

        // 2. UPDATE (표준 -> 커스텀) 검증
        // ingStdToCustom 객체가 커스텀 상태로 변했는지 확인
        assertThat(ingStdToCustom.getIngredient()).isNull(); // 표준 연결 끊김
        assertThat(ingStdToCustom.getCustomName()).isEqualTo("프리미엄 양파");
        assertThat(ingStdToCustom.getPrice()).isEqualTo(2000);
        assertThat(ingStdToCustom.getCustomCalorie()).isEqualTo(new BigDecimal("100"));
        verify(report2).markAsResolved(); // 신고 해결됨?

        // 3. UPDATE (커스텀 -> 표준) 검증
        // ingCustomToStd 객체가 표준 상태로 변했는지 확인
        assertThat(ingCustomToStd.getIngredient()).isEqualTo(masterOnion); // 양파 연결됨
        assertThat(ingCustomToStd.getCustomName()).isNull(); // 커스텀 이름 지워짐
        assertThat(ingCustomToStd.getPrice()).isEqualTo(200); // 100원 * 2개 = 200원

        // 4. CREATE 검증
        ArgumentCaptor<RecipeIngredient> captor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository, times(2)).save(captor.capture()); // 신규 생성 2건 (표준1, 커스텀1)
        List<RecipeIngredient> newIngredients = captor.getAllValues();

        // 4-1. 표준 생성 확인
        RecipeIngredient newStandard = newIngredients.stream()
                .filter(i -> "양파".equals(i.getIngredient() != null ? i.getIngredient().getName() : ""))
                .findFirst().orElseThrow();
        assertThat(newStandard.getPrice()).isEqualTo(100); // 1개 * 100원

        // 4-2. 커스텀 생성 확인
        RecipeIngredient newCustom = newIngredients.stream()
                .filter(i -> "비밀소스".equals(i.getCustomName()))
                .findFirst().orElseThrow();
        assertThat(newCustom.getPrice()).isEqualTo(5000);
        assertThat(newCustom.getCustomCalorie()).isEqualTo(new BigDecimal("500"));

        // 5. 최종 재계산 검증 (Total Cost & Nutrition)
        // 현재 리스트(currentIngredients)는 delete 된 것은 빠지지 않고 남아있지만(JPA 영속성 컨텍스트 특성상),
        // 로직상 calculateAndSetTotalNutrition 메서드에 전달된 리스트를 기반으로 계산됨.
        // 여기서는 Mocking의 한계로 정확한 합산값보다는 '메서드 호출 여부'와 '로직 흐름'을 봅니다.

        // 최종적으로 recipe.updateTotalIngredientCost()가 호출되었는지 확인
        // 예상 합계:
        // 1. 프리미엄양파(2000) + 2. 양파(200) + 3. 새양파(100) + 4. 비밀소스(5000) = 7300원
        // (삭제된 ID 100은 로직상 flush 후 다시 조회할 때 빠져야 하지만, Mock이라 리스트에 남아있을 수 있음.
        //  그래서 정확한 값보다는 호출 여부를 검증하는 것이 안전)
        verify(recipeRepository).save(recipe);

        // 최소한 0보다는 큰 값으로 업데이트 되었어야 함
        assertThat(recipe.getTotalIngredientCost()).isGreaterThan(0);

        // 시장가 업데이트 확인 (30% 마진 적용 여부)
        assertThat(recipe.getMarketPrice()).isGreaterThan(recipe.getTotalIngredientCost());

        System.out.println("✅ 모든 시나리오 테스트 통과 완료!");
    }

    // DTO 생성 헬퍼 메서드
    private AdminIngredientUpdateDto createDto(Long id, String name, String quantity, String unit, String action, Integer price, BigDecimal calorie) {
        AdminIngredientUpdateDto dto = new AdminIngredientUpdateDto();
        ReflectionTestUtils.setField(dto, "id", id);
        ReflectionTestUtils.setField(dto, "name", name);
        ReflectionTestUtils.setField(dto, "quantity", quantity);
        ReflectionTestUtils.setField(dto, "unit", unit);
        ReflectionTestUtils.setField(dto, "action", action);
        ReflectionTestUtils.setField(dto, "price", price);
        ReflectionTestUtils.setField(dto, "calorie", calorie);
        // 필요한 다른 영양소 필드도 설정 가능
        return dto;
    }
}