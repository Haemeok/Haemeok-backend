// RecipeIndexingService.java
package com.jdc.recipe_service.opensearch.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
import com.jdc.recipe_service.opensearch.dto.RecipeDocument;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.opensearch.action.delete.DeleteRequest;
import org.opensearch.action.index.IndexRequest;
import org.opensearch.action.update.UpdateRequest;
import org.opensearch.client.RequestOptions;
import org.opensearch.client.RestHighLevelClient;
import org.opensearch.common.xcontent.XContentType;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
@Slf4j
public class RecipeIndexingService {

    private final RestHighLevelClient client;
    private final ObjectMapper objectMapper;
    private final RecipeLikeRepository likeRepository;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    public void indexRecipe(Recipe recipe) {
        try {
            RecipeDocument doc = buildDocument(recipe);
            IndexRequest request = new IndexRequest("recipes")
                    .id(recipe.getId().toString())
                    .source(objectMapper.writeValueAsString(doc), XContentType.JSON);
            client.index(request, RequestOptions.DEFAULT);
            log.info("✅ 색인 생성 완료: {}", recipe.getId());
        } catch (IOException e) {
            log.error("OpenSearch 색인 실패: {}", e.getMessage(), e);
            throw new RuntimeException("색인 실패", e);
        }
    }

    public void updateRecipe(Recipe recipe) {
        try {
            RecipeDocument doc = buildDocument(recipe);
            UpdateRequest request = new UpdateRequest("recipes", recipe.getId().toString())
                    .doc(objectMapper.writeValueAsString(doc), XContentType.JSON);
            client.update(request, RequestOptions.DEFAULT);
            log.info("✅ 색인 수정 완료: {}", recipe.getId());
        } catch (IOException e) {
            log.error("OpenSearch 색인 업데이트 실패: {}", e.getMessage(), e);
            throw new RuntimeException("색인 업데이트 실패", e);
        }
    }

    public void deleteRecipe(Long recipeId) {
        try {
            DeleteRequest request = new DeleteRequest("recipes", recipeId.toString());
            client.delete(request, RequestOptions.DEFAULT);
            log.info("✅ 색인 삭제 완료");
        } catch (IOException e) {
            log.error("OpenSearch 색인 삭제 실패: {}", e.getMessage(), e);
        }
    }

    private RecipeDocument buildDocument(Recipe recipe) {
        // 1) 재료 이름 모으기
        List<String> ingredientNames = Optional.ofNullable(recipe.getIngredients())
                .orElse(Collections.emptyList())
                .stream()
                .map(i -> {
                    if (i.getIngredient() != null && i.getIngredient().getName() != null) {
                        return i.getIngredient().getName();
                    } else {
                        return i.getCustomName();
                    }
                })
                .filter(name -> name != null && !name.isBlank())
                .toList();

        // 2) 태그 이름 모으기
        List<String> tagNames = Optional.ofNullable(recipe.getTags())
                .orElse(Collections.emptyList())
                .stream()
                .map(t -> t.getTag().getDisplayName())
                .filter(name -> name != null && !name.isBlank())
                .toList();

        // 3) 좋아요 개수는 DB에서 직접 조회
        int likeCount = likeRepository.countByRecipeId(recipe.getId());

        return RecipeDocument.builder()
                .id(recipe.getId())
                .title(recipe.getTitle())
                .description(recipe.getDescription())
                .ingredients(String.join(", ", ingredientNames))
                .tags(tagNames)
                .dishType(recipe.getDishType().getDisplayName())
                .createdAt(recipe.getCreatedAt().toString())
                .likeCount(likeCount)
                .cookingTime(Optional.ofNullable(recipe.getCookingTime()).orElse(0))
                .imageUrl(generateImageUrl(recipe.getImageKey()))
                .build();
    }

    private String generateImageUrl(String key) {
        return key == null
                ? null
                : String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, key);
    }
}
