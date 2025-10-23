package com.jdc.recipe_service.opensearch.indexingfailure;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class IndexingFailureLogService {

    private final IndexingFailureLogRepository repository;

    @Transactional
    public void createLog(Long recipeId) {
        if (repository.findByRecipeId(recipeId).isEmpty()) {
            try {
                repository.save(new IndexingFailureLog(recipeId));
                log.warn("Indexing Failure Log: 레시피 ID {} 기록됨.", recipeId);
            } catch (DataIntegrityViolationException e) {
                log.warn("Indexing Failure Log: 레시피 ID {} 기록 중 이미 존재하는 데이터 발견. 무시.", recipeId);
            }
        }
    }

    @Transactional(readOnly = true)
    public List<Long> getAllFailedRecipeIds() {
        return repository.findAll().stream()
                .map(IndexingFailureLog::getRecipeId)
                .collect(Collectors.toList());
    }


    @Transactional
    public void deleteByRecipeId(Long recipeId) {
        repository.deleteByRecipeId(recipeId);
        log.info("Indexing Failure Log: 레시피 ID {} 기록 삭제됨 (인덱싱 성공).", recipeId);
    }
}