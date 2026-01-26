package com.jdc.recipe_service.util;

import com.jdc.recipe_service.domain.dto.recipe.RecipeDetailDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.async.DeferredResult;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * recipeId별로, 아직 이미지 생성이 완료되지 않은 GET 요청(DeferredResult)을
 * 보관했다가, 이미지가 READY가 되면 한꺼번에 setResult(...) 호출해주는 Holder.
 */
@Component
@Slf4j
public class DeferredResultHolder {

    private final ConcurrentMap<Long, List<DeferredResult<ResponseEntity<?>>>> holder = new ConcurrentHashMap<>();
    private final Map<Long, DeferredResult<ResponseEntity<PresignedUrlResponse>>> resultMap = new ConcurrentHashMap<>();
    /**
     * recipeId에 해당하는 DeferredResult를 등록
     */
    public void add(Long recipeId, DeferredResult<ResponseEntity<?>> deferredResult) {
        holder.compute(recipeId, (key, list) -> {
            if (list == null) {
                list = new ArrayList<>();
            }
            list.add(deferredResult);
            return list;
        });
    }

    /**
     * [추가된 메서드] 대기 객체 생성 및 저장
     * Facade에서 호출합니다.
     */
    public DeferredResult<ResponseEntity<PresignedUrlResponse>> create(Long recipeId, Long timeout) {
        DeferredResult<ResponseEntity<PresignedUrlResponse>> result = new DeferredResult<>(timeout);

        resultMap.put(recipeId, result);

        result.onCompletion(() -> resultMap.remove(recipeId));
        result.onTimeout(() -> {
            log.warn("Recipe ID {} - Response Timeout", recipeId);
            resultMap.remove(recipeId);
        });
        result.onError((Throwable t) -> {
            log.error("Recipe ID {} - Async Error", recipeId, t);
            resultMap.remove(recipeId);
        });

        return result;
    }

    /**
     * 이미지 생성 완료 시 호출 (AsyncImageService에서 호출)
     * 저장된 대기 객체를 찾아 응답을 보냅니다.
     */
    public void completeAll(Long recipeId, ResponseEntity<PresignedUrlResponse> response) {
        DeferredResult<ResponseEntity<PresignedUrlResponse>> result = resultMap.remove(recipeId);

        if (result != null && !result.isSetOrExpired()) {
            result.setResult(response);
            log.info("Recipe ID {} - DeferredResult 응답 완료", recipeId);
        }
    }
    /**
     * (선택) 타임아웃 발생 시 보관된 DeferredResult만 제거할 때 사용
     */
    public void remove(Long recipeId, DeferredResult<ResponseEntity<?>> deferredResult) {
        List<DeferredResult<ResponseEntity<?>>> list = holder.get(recipeId);
        if (list != null) {
            list.remove(deferredResult);
        }
    }
}
