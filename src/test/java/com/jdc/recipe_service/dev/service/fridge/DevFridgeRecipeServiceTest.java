package com.jdc.recipe_service.dev.service.fridge;

import com.jdc.recipe_service.dev.repository.fridge.DevFridgeRecipeQueryRepository;
import com.jdc.recipe_service.domain.dto.recipe.FridgeRecipeDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.entity.RecipeLike;
import com.jdc.recipe_service.domain.entity.RefrigeratorItem;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
import com.jdc.recipe_service.domain.repository.RefrigeratorItemRepository;
import com.jdc.recipe_service.domain.type.RecipeType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.data.domain.SliceImpl;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevFridgeRecipeService 단위 검증.
 *
 *  1. 빈 fridge → 빈 Slice (dev repository 호출 없음)
 *  2. dev repository 결과를 DTO로 변환 + likedByCurrentUser 매핑
 *  3. matchedIngredients vs missingIngredients 분리 (id 매칭 + name 정규화 매칭)
 *  4. pantry ingredient 변환에서 제외
 *  5. userId는 fridge/like 조회에 사용. dev repository에는 fridge ingredient ids/types/pageable 전달
 *     (userId는 dev repository에 직접 전달 안 됨 — 정책이 anonymous publicListedActive라 owner 분기 불필요)
 */
@ExtendWith(MockitoExtension.class)
class DevFridgeRecipeServiceTest {

    @Mock RefrigeratorItemRepository fridgeRepo;
    @Mock RecipeLikeRepository recipeLikeRepository;
    @Mock DevFridgeRecipeQueryRepository devFridgeRecipeQueryRepository;

    private DevFridgeRecipeService service;

    private static final Long USER_ID = 7L;
    private static final Pageable PAGE_10 = PageRequest.of(0, 10);
    private static final List<RecipeType> DEFAULT_TYPES = List.of(RecipeType.USER, RecipeType.YOUTUBE);

    @BeforeEach
    void setUp() {
        service = new DevFridgeRecipeService(fridgeRepo, recipeLikeRepository, devFridgeRecipeQueryRepository);
        ReflectionTestUtils.setField(service, "bucketName", "test-bucket");
        ReflectionTestUtils.setField(service, "region", "ap-northeast-2");
    }

    @Test
    @DisplayName("빈 fridge: dev repository 호출 없이 빈 Slice 반환 (early return)")
    void searchByFridgeDev_emptyFridge_noRepoCall_returnsEmpty() {
        given(fridgeRepo.findAllByUserId(USER_ID)).willReturn(List.of());

        Slice<FridgeRecipeDto> result = service.searchByFridgeDev(USER_ID, PAGE_10, DEFAULT_TYPES);

        assertThat(result.getContent()).isEmpty();
        verifyNoInteractions(devFridgeRecipeQueryRepository);
        verifyNoInteractions(recipeLikeRepository);
    }

    @Test
    @DisplayName("dev repo 결과가 비면 likeRepo도 호출 안 함, 빈 Slice")
    void searchByFridgeDev_emptyRepoResult_noLikeQuery_returnsEmpty() {
        Ingredient ingA = newIngredient(1L, "재료A", false);
        given(fridgeRepo.findAllByUserId(USER_ID)).willReturn(List.of(newFridgeItem(ingA)));
        given(devFridgeRecipeQueryRepository.searchRecipesByFridgeIngredientsDev(any(), eq(DEFAULT_TYPES), eq(PAGE_10)))
                .willReturn(new SliceImpl<>(List.of(), PAGE_10, false));

        Slice<FridgeRecipeDto> result = service.searchByFridgeDev(USER_ID, PAGE_10, DEFAULT_TYPES);

        assertThat(result.getContent()).isEmpty();
        verify(recipeLikeRepository, never()).findByUserIdAndRecipeIdIn(any(), anyList());
    }

    @Test
    @DisplayName("정상 흐름: matchedIngredients(id 매칭) + missingIngredients 분리 + likedByCurrentUser 매핑")
    void searchByFridgeDev_happyPath_buildsDto() {
        // user fridge: 재료A
        Ingredient ingA = newIngredient(1L, "재료A", false);
        given(fridgeRepo.findAllByUserId(USER_ID)).willReturn(List.of(newFridgeItem(ingA)));

        // recipe: 재료A (matched) + 재료B (missing)
        User owner = newUser(99L, "owner");
        Ingredient ingB = newIngredient(2L, "재료B", false);
        Recipe recipe = newRecipe(101L, "test", owner);
        recipe.getIngredients().add(newRecipeIngredient(recipe, ingA, null, null));
        recipe.getIngredients().add(newRecipeIngredient(recipe, ingB, null, "https://link.com/B"));

        given(devFridgeRecipeQueryRepository.searchRecipesByFridgeIngredientsDev(any(), eq(DEFAULT_TYPES), eq(PAGE_10)))
                .willReturn(new SliceImpl<>(List.of(recipe), PAGE_10, false));

        // user가 이 recipe를 like한 상태
        RecipeLike like = mockLikeFor(recipe);
        given(recipeLikeRepository.findByUserIdAndRecipeIdIn(USER_ID, List.of(101L)))
                .willReturn(List.of(like));

        Slice<FridgeRecipeDto> result = service.searchByFridgeDev(USER_ID, PAGE_10, DEFAULT_TYPES);

        assertThat(result.getContent()).hasSize(1);
        FridgeRecipeDto dto = result.getContent().get(0);

        // FridgeRecipeDto extends RecipeSimpleDto — super 필드는 dto.getId() 등으로 직접 접근
        assertThat(dto.getId()).isEqualTo(101L);
        assertThat(dto.getAuthorId()).isEqualTo(99L);
        assertThat(dto.getAuthorName()).isEqualTo("owner");
        assertThat(dto.isLikedByCurrentUser()).isTrue();

        assertThat(dto.getMatchedIngredients()).containsExactly("재료A");
        assertThat(dto.getMissingIngredients()).extracting(FridgeRecipeDto.MissingIngredientDto::getName)
                .containsExactly("재료B");
        assertThat(dto.getMissingIngredients().get(0).getCoupangLink()).isEqualTo("https://link.com/B");
    }

    @Test
    @DisplayName("pantry ingredient는 matched/missing 모두에서 제외 (소금 같은 기본 양념)")
    void searchByFridgeDev_pantryIngredient_excludedFromBoth() {
        Ingredient ingA = newIngredient(1L, "재료A", false);
        given(fridgeRepo.findAllByUserId(USER_ID)).willReturn(List.of(newFridgeItem(ingA)));

        User owner = newUser(99L, "owner");
        Ingredient pantry = newIngredient(50L, "소금", true); // pantry
        Recipe recipe = newRecipe(101L, "test", owner);
        recipe.getIngredients().add(newRecipeIngredient(recipe, ingA, null, null));
        recipe.getIngredients().add(newRecipeIngredient(recipe, pantry, null, null));

        given(devFridgeRecipeQueryRepository.searchRecipesByFridgeIngredientsDev(any(), eq(DEFAULT_TYPES), eq(PAGE_10)))
                .willReturn(new SliceImpl<>(List.of(recipe), PAGE_10, false));
        given(recipeLikeRepository.findByUserIdAndRecipeIdIn(USER_ID, List.of(101L)))
                .willReturn(List.of());

        Slice<FridgeRecipeDto> result = service.searchByFridgeDev(USER_ID, PAGE_10, DEFAULT_TYPES);

        FridgeRecipeDto dto = result.getContent().get(0);
        assertThat(dto.getMatchedIngredients()).containsExactly("재료A");
        // 소금은 pantry라 missing에도 안 들어감
        assertThat(dto.getMissingIngredients()).isEmpty();
    }

    @Test
    @DisplayName("name 정규화 매칭: 공백 차이는 같은 재료로 인식 (id가 달라도 trim+공백 제거 후 이름 같으면 matched)")
    void searchByFridgeDev_normalizedNameMatch() {
        // user fridge: 재료A (id=1, 공백 있는 이름)
        // 운영 normalizeName과 동일하게 trim + " "→"" 만 적용. 대소문자 처리는 없음 — 운영 V2와 정책 일치 유지.
        Ingredient userIng = newIngredient(1L, " 재료 A", false);
        given(fridgeRepo.findAllByUserId(USER_ID)).willReturn(List.of(newFridgeItem(userIng)));

        // recipe ingredient: 재료A (다른 id, 공백 없는 이름) — name 정규화로 같은 재료 인식
        User owner = newUser(99L, "owner");
        Ingredient recipeIng = newIngredient(2L, "재료A", false);
        Recipe recipe = newRecipe(101L, "test", owner);
        recipe.getIngredients().add(newRecipeIngredient(recipe, recipeIng, null, null));

        given(devFridgeRecipeQueryRepository.searchRecipesByFridgeIngredientsDev(any(), eq(DEFAULT_TYPES), eq(PAGE_10)))
                .willReturn(new SliceImpl<>(List.of(recipe), PAGE_10, false));
        given(recipeLikeRepository.findByUserIdAndRecipeIdIn(USER_ID, List.of(101L)))
                .willReturn(List.of());

        Slice<FridgeRecipeDto> result = service.searchByFridgeDev(USER_ID, PAGE_10, DEFAULT_TYPES);

        // 정규화 후 같은 재료 → matched
        assertThat(result.getContent().get(0).getMatchedIngredients()).containsExactly("재료A");
        assertThat(result.getContent().get(0).getMissingIngredients()).isEmpty();
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: rawName 우선 표시 — ingredient.name='고추'여도 rawName='청양고추'면 displayName='청양고추'")
    void searchByFridgeDev_displayNameUsesRawNameFirst() {
        // user fridge: "청양고추" (canonical)
        Ingredient userPepper = newIngredient(1L, "청양고추", false);
        given(fridgeRepo.findAllByUserId(USER_ID)).willReturn(List.of(newFridgeItem(userPepper)));

        // recipe ingredient: ingredient.name="고추" + rawName="청양고추" — AI fallback row 시뮬레이션
        User owner = newUser(99L, "owner");
        Ingredient pepperMaster = newIngredient(2L, "고추", false);
        Recipe recipe = newRecipe(101L, "test", owner);
        recipe.getIngredients().add(newRecipeIngredient(recipe, pepperMaster, "청양고추", null, null));

        given(devFridgeRecipeQueryRepository.searchRecipesByFridgeIngredientsDev(any(), eq(DEFAULT_TYPES), eq(PAGE_10)))
                .willReturn(new SliceImpl<>(List.of(recipe), PAGE_10, false));
        given(recipeLikeRepository.findByUserIdAndRecipeIdIn(USER_ID, List.of(101L)))
                .willReturn(List.of());

        Slice<FridgeRecipeDto> result = service.searchByFridgeDev(USER_ID, PAGE_10, DEFAULT_TYPES);

        FridgeRecipeDto dto = result.getContent().get(0);
        // matched: rawName "청양고추" 정규화 = user fridge "청양고추" 정규화 → match
        // displayName: rawName 우선 → "청양고추"
        assertThat(dto.getMatchedIngredients())
                .as("rawName이 ingredient.name보다 우선해서 표시됨 — 운영은 '고추'로 표시되었던 회귀")
                .containsExactly("청양고추");
        assertThat(dto.getMissingIngredients()).isEmpty();
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: 조회 후보 진입 후 — recipe에 mapped row + bypass row 섞여 있을 때 bypass row의 raw/customName도 매칭/표시 분류에 사용됨")
    void searchByFridgeDev_bypassRowAlongsideMappedRow_classifiedByRawAware() {
        // **중요**: dev fridge repository는 ingredient.id IN (...) 기반 join이라 ingredient=null bypass row
        // 단독으로는 후보 진입 불가. 이 테스트는 "이미 후보로 진입한 레시피의 bypass row가 raw-aware 분류에
        // 정확히 잡힌다"는 좁은 invariant만 검증.
        Ingredient userGarlic = newIngredient(1L, "마늘", false);
        given(fridgeRepo.findAllByUserId(USER_ID)).willReturn(List.of(newFridgeItem(userGarlic)));

        // recipe에 두 row: mapped(id 매칭으로 후보 진입의 근거) + bypass row(같은 레시피의 다른 라인 시뮬레이션)
        User owner = newUser(99L, "owner");
        Recipe recipe = newRecipe(101L, "test", owner);
        recipe.getIngredients().add(newRecipeIngredient(recipe, userGarlic, null, null));            // mapped → matched
        recipe.getIngredients().add(newRecipeIngredient(recipe, null, "마늘", "마늘", null));         // bypass, raw/custom="마늘"

        given(devFridgeRecipeQueryRepository.searchRecipesByFridgeIngredientsDev(any(), eq(DEFAULT_TYPES), eq(PAGE_10)))
                .willReturn(new SliceImpl<>(List.of(recipe), PAGE_10, false));
        given(recipeLikeRepository.findByUserIdAndRecipeIdIn(USER_ID, List.of(101L)))
                .willReturn(List.of());

        Slice<FridgeRecipeDto> result = service.searchByFridgeDev(USER_ID, PAGE_10, DEFAULT_TYPES);

        FridgeRecipeDto dto = result.getContent().get(0);
        // 두 row 모두 "마늘"로 표시 + 둘 다 matched (mapped는 id, bypass는 rawName 정규화)
        assertThat(dto.getMatchedIngredients()).containsExactly("마늘", "마늘");
        assertThat(dto.getMissingIngredients()).isEmpty();
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: 조회 후보 진입 후 — bypass row의 raw가 myNames에 없으면 missing으로 분류 (link은 customLink fallback)")
    void searchByFridgeDev_bypassRowDifferentName_classifiedAsMissing() {
        Ingredient userGarlic = newIngredient(1L, "마늘", false);
        given(fridgeRepo.findAllByUserId(USER_ID)).willReturn(List.of(newFridgeItem(userGarlic)));

        User owner = newUser(99L, "owner");
        Recipe recipe = newRecipe(101L, "test", owner);
        // mapped 마늘 (matched로 후보 진입 근거) + bypass "양파" (raw 기준 missing)
        recipe.getIngredients().add(newRecipeIngredient(recipe, userGarlic, null, null));
        recipe.getIngredients().add(newRecipeIngredient(recipe, null, "양파", "양파", "https://link.com/onion"));

        given(devFridgeRecipeQueryRepository.searchRecipesByFridgeIngredientsDev(any(), eq(DEFAULT_TYPES), eq(PAGE_10)))
                .willReturn(new SliceImpl<>(List.of(recipe), PAGE_10, false));
        given(recipeLikeRepository.findByUserIdAndRecipeIdIn(USER_ID, List.of(101L)))
                .willReturn(List.of());

        Slice<FridgeRecipeDto> result = service.searchByFridgeDev(USER_ID, PAGE_10, DEFAULT_TYPES);

        FridgeRecipeDto dto = result.getContent().get(0);
        assertThat(dto.getMatchedIngredients()).containsExactly("마늘");
        assertThat(dto.getMissingIngredients()).extracting(FridgeRecipeDto.MissingIngredientDto::getName)
                .containsExactly("양파");
        assertThat(dto.getMissingIngredients().get(0).getCoupangLink())
                .as("ingredient=null bypass missing은 customLink로 fallback")
                .isEqualTo("https://link.com/onion");
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: rawName/customName/ingredient.name 모두 다른 row — myNames에 일치하는 candidate가 있으면 matched")
    void searchByFridgeDev_threeWayNameCandidates_anyMatchWins() {
        // user fridge: "rawName으로만 등록한 재료"
        Ingredient userOnly = newIngredient(1L, "특수재료", false);
        given(fridgeRepo.findAllByUserId(USER_ID)).willReturn(List.of(newFridgeItem(userOnly)));

        // recipe row: rawName="특수재료" (사용자 입력), customName="다른이름" (AI가 다르게 적음),
        // ingredient.name="또다른이름" (master 매칭). rawName으로 매칭되어야 함.
        User owner = newUser(99L, "owner");
        Ingredient master = newIngredient(2L, "또다른이름", false);
        Recipe recipe = newRecipe(101L, "test", owner);
        recipe.getIngredients().add(newRecipeIngredient(recipe, master, "특수재료", "다른이름", null));

        given(devFridgeRecipeQueryRepository.searchRecipesByFridgeIngredientsDev(any(), eq(DEFAULT_TYPES), eq(PAGE_10)))
                .willReturn(new SliceImpl<>(List.of(recipe), PAGE_10, false));
        given(recipeLikeRepository.findByUserIdAndRecipeIdIn(USER_ID, List.of(101L)))
                .willReturn(List.of());

        Slice<FridgeRecipeDto> result = service.searchByFridgeDev(USER_ID, PAGE_10, DEFAULT_TYPES);

        FridgeRecipeDto dto = result.getContent().get(0);
        assertThat(dto.getMatchedIngredients())
                .as("rawName candidate가 myNames에 있으면 matched — 운영은 ingredient.name='또다른이름'으로만 봐서 miss")
                .containsExactly("특수재료");  // displayName도 rawName 우선
    }

    // ---------- mock entity helpers (실제 entity는 builder 안 보일 수도, 단순화) ----------

    private RefrigeratorItem newFridgeItem(Ingredient ingredient) {
        RefrigeratorItem item = RefrigeratorItem.builder().ingredient(ingredient).build();
        return item;
    }

    private Ingredient newIngredient(Long id, String name, boolean isPantry) {
        Ingredient ing = Ingredient.builder().name(name).isPantry(isPantry).build();
        ReflectionTestUtils.setField(ing, "id", id);
        return ing;
    }

    private User newUser(Long id, String nickname) {
        User user = User.builder().nickname(nickname).provider("test").oauthId("oid-" + id).build();
        ReflectionTestUtils.setField(user, "id", id);
        return user;
    }

    private Recipe newRecipe(Long id, String title, User owner) {
        Recipe recipe = Recipe.builder()
                .user(owner)
                .title(title)
                .ingredients(new ArrayList<>())
                .build();
        ReflectionTestUtils.setField(recipe, "id", id);
        return recipe;
    }

    private RecipeIngredient newRecipeIngredient(Recipe recipe, Ingredient ingredient,
                                                  String customName, String customLink) {
        return newRecipeIngredient(recipe, ingredient, null, customName, customLink);
    }

    /** rawName 명시 가능한 빌더 — raw-first 회귀 테스트용. */
    private RecipeIngredient newRecipeIngredient(Recipe recipe, Ingredient ingredient,
                                                  String rawName, String customName, String customLink) {
        RecipeIngredient ri = RecipeIngredient.builder()
                .recipe(recipe)
                .ingredient(ingredient)
                .rawName(rawName)
                .customName(customName)
                .customLink(customLink)
                .quantity("1")
                .unit("개")
                .build();
        return ri;
    }

    private RecipeLike mockLikeFor(Recipe recipe) {
        // RecipeLike의 정확한 빌더 시그니처 모르면 reflection으로 recipe만 setting
        RecipeLike like = org.mockito.Mockito.mock(RecipeLike.class);
        given(like.getRecipe()).willReturn(recipe);
        return like;
    }
}
