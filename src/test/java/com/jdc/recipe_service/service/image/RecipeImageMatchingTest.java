package com.jdc.recipe_service.service.image;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.DishType;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

@SpringBootTest
class RecipeImageMatchingTest {

    // 💡 핵심 1: 검증하려는 서비스는 '진짜' 객체로 주입받습니다.
    @Autowired
    private RecipeImageMatchingService imageMatchingService;

    // 💡 핵심 2: 서비스가 사용하는 DB(Repository)만 가짜(Mock)로 만듭니다.
    @MockBean
    private RecipeRepository recipeRepository;

    @Test
    @DisplayName("✅ 키워드 매칭 성공: DB에 조건에 맞는 레시피가 있으면 썸네일 키를 반환한다")
    void testImageMatchSuccess() {
        // Given
        List<String> keywords = List.of("김치찌개");
        DishType dishType = DishType.SOUP_STEW;

        // 1. 가짜 DB 설정: 해당 키워드의 레시피가 5개 있다고 가정
        when(recipeRepository.countCandidateRecipes(eq("김치찌개"), eq(dishType)))
                .thenReturn(5L);

        // 2. 가짜 DB 설정: 페이징으로 조회했을 때 가짜 레시피를 반환하도록 세팅
        Recipe mockRecipe = Recipe.builder().imageKey("s3/path/real_kimchi.jpg").build();
        when(recipeRepository.findCandidateRecipesByKeywordAndDishType(eq("김치찌개"), eq(dishType), any(PageRequest.class)))
                .thenReturn(new PageImpl<>(List.of(mockRecipe)));

        // When (진짜 로직 실행!)
        String resultImageKey = imageMatchingService.findMatchingImageKey(keywords, dishType);

        // Then
        assertThat(resultImageKey).isEqualTo("s3/path/real_kimchi.jpg");
        // 실제로 카운트 쿼리와 조회 쿼리가 각각 1번씩 실행되었는지 검증
        verify(recipeRepository, times(1)).countCandidateRecipes(anyString(), any(DishType.class));
        verify(recipeRepository, times(1)).findCandidateRecipesByKeywordAndDishType(anyString(), any(DishType.class), any(PageRequest.class));
    }

    @Test
    @DisplayName("✅ 키워드 매칭 실패: 조건에 맞는 레시피가 없으면 null을 반환하고, 기본 카테고리 이미지를 가져온다")
    void testImageMatchFail_Fallback() {
        // Given
        List<String> keywords = List.of("마라탕후루볶음");
        DishType dishType = DishType.FRIED_PAN;

        // 가짜 DB 설정: 해당 키워드의 레시피가 0개라고 가정
        when(recipeRepository.countCandidateRecipes(anyString(), any(DishType.class)))
                .thenReturn(0L);

        // When
        String resultImageKey = imageMatchingService.findMatchingImageKey(keywords, dishType);

        // Then: 매칭 실패 시 null이 나와야 정상
        assertThat(resultImageKey).isNull();

        // 번외 검증: 실패했을 때 기본 이미지를 제대로 가져오는지 확인
        String fallbackImage = imageMatchingService.getDefaultImageKeyForDishType(dishType.getDisplayName());
        assertThat(fallbackImage).isEqualTo("images/default-image/fried_pan.webp"); // DishType.FRIED_PAN 소문자 변환 로직 검증
    }
}