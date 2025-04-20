package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.ingredient.*;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.mapper.IngredientMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.http.HttpStatus;

@Service
@RequiredArgsConstructor
@Transactional
public class IngredientService {

    private final IngredientRepository repo;

    /**
     * @param category 카테고리 필터 (optional) null 또는 blank일 경우 전체 조회
     * @param pageable 페이지/정렬 정보 (page, size, sort 등)
     */
    @Transactional(readOnly = true)
    public Page<IngredientResponseDto> getAll(String category, Pageable pageable) {
        Page<Ingredient> page;

        if (category != null && !category.isBlank()) {
            page = repo.findByCategoryIgnoreCase(category, pageable);
        } else {
            page = repo.findAll(pageable);
        }

        return page.map(IngredientMapper::toDto);
    }

    /** 생성 (관리자 전용) */
    public IngredientResponseDto create(IngredientRequestDto dto) {
        Ingredient entity = IngredientMapper.toEntity(dto);
        repo.save(entity);
        return IngredientMapper.toDto(entity);
    }

    /** 수정 (관리자 전용) */
    public IngredientResponseDto update(Long id, IngredientRequestDto dto) {
        Ingredient entity = repo.findById(id)
                .orElseThrow(() -> new ResponseStatusException(
                        HttpStatus.NOT_FOUND, "재료가 없습니다: " + id));
        // 간단히 모든 필드 덮어쓰기
        entity.setName(dto.getName());
        entity.setCategory(dto.getCategory());
        entity.setImageUrl(dto.getImageUrl());
        entity.setPrice(dto.getPrice());
        entity.setUnit(dto.getUnit());
        return IngredientMapper.toDto(entity);
    }

    /** 삭제 (관리자 전용) */
    public void delete(Long id) {
        if (!repo.existsById(id)) {
            throw new ResponseStatusException(
                    HttpStatus.NOT_FOUND, "재료가 없습니다: " + id);
        }
        repo.deleteById(id);
    }
}
