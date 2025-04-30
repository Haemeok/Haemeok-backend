package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.ingredient.*;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.QIngredient;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.repository.RefrigeratorItemRepository;
import com.jdc.recipe_service.mapper.IngredientMapper;
import com.querydsl.core.types.Projections;
import com.querydsl.core.types.dsl.BooleanExpression;
import com.querydsl.jpa.impl.JPAQuery;
import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.*;
import org.springframework.data.support.PageableExecutionUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.http.HttpStatus;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional
public class IngredientService {

    private final JPAQueryFactory queryFactory;
    private final IngredientRepository repo;
    private final RefrigeratorItemRepository fridgeRepo;

    private final QIngredient ing = QIngredient.ingredient;

    /**
     * @param keyword 검색어 (optional)
     * @param category 카테고리 필터 (optional) null 또는 blank일 경우 전체 조회
     * @param pageable 페이지/정렬 정보 (page, size, sort 등)
     */
    @Transactional(readOnly = true)
    public Page<IngredientSummaryDto> search(
            String keyword,
            String category,
            Boolean inFridge,
            Long userId,
            Pageable pageable) {

        // 1) 내 냉장고 재료 ID 집합
        Set<Long> fridgeIds = (userId != null)
                ? fridgeRepo.findByUserId(userId, Pageable.unpaged())
                .stream()
                .map(i -> i.getIngredient().getId())
                .collect(Collectors.toSet())
                : Set.of();

        // 2) 검색 조건 조립
        BooleanExpression nameCond =
                (keyword != null && !keyword.isBlank())
                        ? ing.name.containsIgnoreCase(keyword)
                        : null;
        BooleanExpression catCond =
                (category != null && !category.isBlank())
                        ? ing.category.equalsIgnoreCase(category)
                        : null;

        BooleanExpression fridgeCond = null;
        if (inFridge != null) {
            if (inFridge) {
                // 냉장고에 담긴 것만
                fridgeCond = ing.id.in(fridgeIds);
            } else {
                // 냉장고에 담기지 않은 것만
                fridgeCond = ing.id.notIn(fridgeIds);
            }
        }
        // 3) content 조회 쿼리
        JPAQuery<IngredientSummaryDto> contentQuery = queryFactory
                .select(Projections.constructor(
                        IngredientSummaryDto.class,
                        ing.id,
                        ing.name,
                        ing.category,
                        ing.imageUrl,
                        ing.unit,
                        ing.id.in(fridgeIds)         // inFridge 플래그
                ))
                .from(ing)
                .where(nameCond, catCond,fridgeCond)
                .orderBy(ing.name.asc())
                .offset(pageable.getOffset())
                .limit(pageable.getPageSize());

        List<IngredientSummaryDto> content = contentQuery.fetch();

        // 4) 전체 개수 조회 쿼리
        JPAQuery<Long> countQuery = queryFactory
                .select(ing.count())
                .from(ing)
                .where(nameCond, catCond,fridgeCond);

        // 5) Page 조립
        return PageableExecutionUtils.getPage(
                content,
                pageable,
                countQuery::fetchOne
        );
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
