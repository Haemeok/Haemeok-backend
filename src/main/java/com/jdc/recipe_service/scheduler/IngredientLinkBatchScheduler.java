package com.jdc.recipe_service.scheduler;

import com.jdc.recipe_service.client.CoupangApiClient;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeIngredientRepository;
import jakarta.persistence.EntityNotFoundException;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
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

    //@Scheduled(cron = "0 * * * * *")
    public int updateCustomIngredientsContinuous() {
        List<String> targetNames = recipeIngredientRepository.findDistinctNamesNeedLink();

        if (targetNames.isEmpty()) return 0;

        log.info("[커스텀재료] 고유 이름 {}개 처리 시작", targetNames.size());
        int successRows = 0;

        for (String name : targetNames) {
            try {
                String link = coupangApiClient.searchLandingUrl(name);

                if (link != null && !link.isEmpty()) {
                    int count = recipeIngredientRepository.updateLinkByCustomName(name, link);
                    successRows += count;
                    log.debug("업데이트 성공 [{}]: {}개 행 적용됨", name, count);
                } else {
                    recipeIngredientRepository.updateLinkByCustomName(name, "NONE");
                    log.info("검색 결과 없음 처리 [{}]: NONE 저장", name);
                }

                Thread.sleep(1500);

            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                break;
            } catch (Exception e) {
                log.error("커스텀 재료 처리 중 에러 [{}]: {}", name, e.getMessage());
            }
        }

        return successRows;
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
