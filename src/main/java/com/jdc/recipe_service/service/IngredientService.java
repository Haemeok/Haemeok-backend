package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.ingredient.*;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.QIngredient;
import com.jdc.recipe_service.domain.entity.QIngredientKeyword;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.repository.RefrigeratorItemRepository;
import com.jdc.recipe_service.mapper.IngredientMapper;
import com.querydsl.core.types.Expression;
import com.querydsl.core.types.Projections;
import com.querydsl.core.types.dsl.BooleanExpression;
import com.querydsl.core.types.dsl.Expressions;
import com.querydsl.jpa.JPAExpressions;
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
    private final QIngredientKeyword ingKeyword = QIngredientKeyword.ingredientKeyword;

    /**
     * @param keyword 검색어 (optional)
     * @param category 카테고리 필터 (optional) null 또는 blank일 경우 전체 조회
     * @param pageable 페이지/정렬 정보 (page, size, sort 등)
     */
    @Transactional(readOnly = true)
    public Page<IngredientSummaryDto> search(
            String keyword,
            String category,
            Long userId,
            boolean includeFridgeInfo,
            Pageable pageable) {

        Set<Long> fridgeIds;
        if (userId != null && includeFridgeInfo) {
            fridgeIds = fridgeRepo.findByUserId(userId, Pageable.unpaged())
                    .stream()
                    .map(i -> i.getIngredient().getId())
                    .collect(Collectors.toSet());
        } else {
            fridgeIds = Set.of();
        }

        BooleanExpression searchCond = null;
        if (keyword != null && !keyword.isBlank()) {
            searchCond = ing.name.containsIgnoreCase(keyword)
                    .or(ing.id.in(
                            JPAExpressions.select(ingKeyword.ingredient.id)
                                    .from(ingKeyword)
                                    .where(ingKeyword.keyword.containsIgnoreCase(keyword))
                    ));
        }

        BooleanExpression catCond = (category != null && !category.isBlank())
                ? ing.category.equalsIgnoreCase(category) : null;

        Expression<Boolean> inFridgeExpr = includeFridgeInfo
                ? ing.id.in(fridgeIds)
                : Expressions.nullExpression(Boolean.class);

        String s3Prefix = "https://haemeok-s3-bucket.s3.ap-northeast-2.amazonaws.com/images/ingredients/";
        Expression<String> imageUrlExpr = ing.name
                .prepend(s3Prefix)
                .append(".webp");

        List<IngredientSummaryDto> content = queryFactory
                .select(Projections.constructor(
                        IngredientSummaryDto.class,
                        ing.id,
                        ing.name,
                        ing.category,
                        imageUrlExpr,
                        ing.unit,
                        inFridgeExpr
                ))
                .from(ing)
                .where(searchCond, catCond)
                .distinct()
                .orderBy(ing.name.asc())
                .offset(pageable.getOffset())
                .limit(pageable.getPageSize())
                .fetch();

        Long total = queryFactory
                .select(ing.countDistinct())
                .from(ing)
                .where(searchCond, catCond)
                .fetchOne();
        if (total == null) total = 0L;

        return new PageImpl<>(content, pageable, total);
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
