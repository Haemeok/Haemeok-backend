package com.jdc.recipe_service.util;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.async.DeferredResult;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * recipeId별로, 아직 이미지 생성이 완료되지 않은 GET 요청(DeferredResult)을
 * 보관했다가, 이미지가 READY가 되면 한꺼번에 setResult(...) 호출해주는 Holder.
 */
@Component
public class DeferredResultHolder {

    // key: recipeId, value: 해당 recipeId로 보류 중인 DeferredResult 목록
    private final ConcurrentMap<Long, List<DeferredResult<ResponseEntity<?>>>> holder = new ConcurrentHashMap<>();

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
     * 이미지 생성 완료 시 호출.
     * 해당 recipeId에 보관된 모든 DeferredResult에 setResult(...) 호출 → 응답을 내려준다.
     */
    public void completeAll(Long recipeId, ResponseEntity<?> response) {
        List<DeferredResult<ResponseEntity<?>>> list = holder.remove(recipeId);
        if (list == null) return;
        for (DeferredResult<ResponseEntity<?>> dr : list) {
            dr.setResult(response);
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
