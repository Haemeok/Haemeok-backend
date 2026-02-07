package com.jdc.recipe_service.scheduler;

import com.jdc.recipe_service.client.CoupangApiClient;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeIngredientRepository;
import jakarta.persistence.EntityNotFoundException;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
public class IngredientLinkBatchScheduler {

    private final IngredientRepository ingredientRepository;
    private final RecipeIngredientRepository recipeIngredientRepository;
    private final CoupangApiClient coupangApiClient;

    //@Scheduled(cron = "0 0 6 * * *")
    public int updateAllIngredientLinks() {
        log.info("========== [배치 시작] 정규 재료 API 업데이트 ==========");

        List<Ingredient> ingredients = ingredientRepository.findAll();
        int successCount = 0;

        for (Ingredient ingredient : ingredients) {
            try {
                String link = coupangApiClient.searchProductUrl(ingredient.getName());

                if (link != null && !link.isEmpty()) {
                    ingredient.updateCoupangLink(link);
                    ingredientRepository.save(ingredient);
                    successCount++;
                }

                Thread.sleep(2000);

            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                break;
            } catch (Exception e) {
                log.error("정규 재료 처리 실패: {}", ingredient.getName(), e);
            }
        }
        log.info("========== [배치 종료] 정규 재료 성공: {} / 전체: {} ==========", successCount, ingredients.size());

        return successCount;
    }

    @Scheduled(cron = "0 * * * * *")
    public int updateCustomIngredientsContinuous() {

        Pageable limit = PageRequest.of(0, 50);
        List<String> targetNames = recipeIngredientRepository.findDistinctNamesNeedLink(limit);

        if (targetNames.isEmpty()) return 0;

        log.info("[커스텀재료] 고속 배치 시작 (대상: {}개)", targetNames.size());
        int successRows = 0;

        for (String name : targetNames) {
            try {
                String link = coupangApiClient.getSearchPageDeepLink(name);

                if (link != null && !link.isEmpty()) {
                    int count = recipeIngredientRepository.updateLinkByCustomName(name, link);
                    successRows += count;
                } else {
                    log.warn("링크 생성 실패: {}", name);
                }

                Thread.sleep(1000);

            } catch (Exception e) {
                log.error("배치 중단: {}", e.getMessage());
                break;
            }
        }
        return successRows;
    }

    @Scheduled(cron = "0 0/10 * * * *")
    public int updateMissingIngredientLinks() {

        List<Ingredient> ingredients = ingredientRepository.findTop20ByCoupangLinkIsNullOrderByIdAsc();

        if (ingredients.isEmpty()) return 0;

        log.info("========== [정규 재료] 링크 보충 시작 (대상: {}개) ==========", ingredients.size());
        int successCount = 0;

        for (Ingredient ingredient : ingredients) {
            try {
                String link = coupangApiClient.searchProductUrl(ingredient.getName());

                if (link != null && !link.isEmpty()) {
                    ingredient.updateCoupangLink(link);
                    successCount++;
                } else {
                    ingredient.updateCoupangLink("NONE");
                }

                ingredientRepository.save(ingredient);

                Thread.sleep(3000);

            } catch (Exception e) {
                log.error("정규 재료 API 에러: {}", e.getMessage());
                break;
            }
        }

        return successCount;
    }

    @Transactional
    public String updateLinkByIngredientId(Long ingredientId) {
        Ingredient ingredient = ingredientRepository.findById(ingredientId)
                .orElseThrow(() -> new EntityNotFoundException("해당 재료를 찾을 수 없습니다. ID: " + ingredientId));

        String link = coupangApiClient.searchProductUrl(ingredient.getName());

        if (link != null && !link.isEmpty()) {
            ingredient.updateCoupangLink(link);
            return link;
        } else {
            throw new RuntimeException("쿠팡 검색 결과가 없습니다.");
        }
    }
}
