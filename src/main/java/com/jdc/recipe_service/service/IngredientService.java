package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.ingredient.*;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.IngredientUnit;
import com.jdc.recipe_service.domain.entity.QIngredient;
import com.jdc.recipe_service.domain.entity.QIngredientKeyword;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.repository.IngredientUnitRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.RefrigeratorItemRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.mapper.IngredientMapper;
import com.querydsl.core.types.Expression;
import com.querydsl.core.types.OrderSpecifier;
import com.querydsl.core.types.Projections;
import com.querydsl.core.types.dsl.BooleanExpression;
import com.querydsl.core.types.dsl.CaseBuilder;
import com.querydsl.core.types.dsl.Expressions;
import com.querydsl.core.types.dsl.NumberExpression;
import com.querydsl.jpa.JPAExpressions;
import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.hashids.Hashids;
import org.springframework.data.domain.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.http.HttpStatus;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional
public class IngredientService {

    private final JPAQueryFactory queryFactory;
    private final IngredientRepository repo;
    private final IngredientUnitRepository ingredientUnitRepository;
    private final RefrigeratorItemRepository fridgeRepo;
    private final RecipeRepository recipeRepository;
    private final Hashids hashids;

    private final QIngredient ing = QIngredient.ingredient;
    private final QIngredientKeyword ingKeyword = QIngredientKeyword.ingredientKeyword;
    private static final String INGREDIENT_IMAGE_PREFIX =
            "https://haemeok-s3-bucket.s3.ap-northeast-2.amazonaws.com/images/ingredients/";
    private static final BigDecimal HUNDRED = BigDecimal.valueOf(100);
    private static final Comparator<IngredientUnit> UNIT_ORDER = Comparator
            .comparing((IngredientUnit u) -> !Boolean.TRUE.equals(u.getIsDefault()))
            .thenComparing(u -> nullToEmpty(u.getUnitLabelKo()))
            .thenComparing(IngredientUnit::getId, Comparator.nullsLast(Long::compareTo));

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

        OrderSpecifier<?>[] orderSpecifiers;

        if (keyword == null || keyword.isBlank()) {
            NumberExpression<Integer> categoryPriority = new CaseBuilder()
                    .when(ing.category.in("음료/주류", "조미료/양념")).then(2)
                    .otherwise(1);

            orderSpecifiers = new OrderSpecifier[]{
                    categoryPriority.asc(),
                    ing.usageCount.desc(),
                    ing.name.asc()
            };
        } else {
            orderSpecifiers = new OrderSpecifier[]{
                    ing.name.asc()
            };
        }

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
                .orderBy(orderSpecifiers)
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

    @Transactional(readOnly = true)
    public List<IngredientIdNameDto> findNamesByHashIds(List<String> hashIds) {
        List<Long> decodedIds = hashIds.stream()
                .map(h -> {
                    long[] decoded = hashids.decode(h);
                    return (decoded.length > 0) ? decoded[0] : null;
                })
                .filter(java.util.Objects::nonNull)
                .collect(Collectors.toList());

        List<Ingredient> ingredients = repo.findAllById(decodedIds);

        Map<Long, Ingredient> ingredientMap = ingredients.stream()
                .collect(Collectors.toMap(Ingredient::getId, i -> i));

        return decodedIds.stream()
                .filter(ingredientMap::containsKey)
                .map(id -> {
                    Ingredient i = ingredientMap.get(id);
                    return new IngredientIdNameDto(i.getId(), i.getName());
                })
                .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public IngredientUnitsResponse findUnitsByIngredientId(Long ingredientId) {
        if (ingredientId == null || !repo.existsById(ingredientId)) {
            throw new CustomException(ErrorCode.INGREDIENT_NOT_FOUND);
        }
        List<IngredientUnit> units = ingredientUnitRepository.findAllByIngredientIdIn(List.of(ingredientId));
        return toUnitsResponse(ingredientId, units);
    }

    @Transactional(readOnly = true)
    public IngredientUnitsBatchResponse findUnitsByIngredientIds(List<Long> ingredientIds) {
        if (ingredientIds == null || ingredientIds.isEmpty()) {
            throw new CustomException(ErrorCode.INVALID_INGREDIENT_REQUEST);
        }

        List<Long> orderedIds = ingredientIds.stream()
                .filter(Objects::nonNull)
                .distinct()
                .toList();
        if (orderedIds.isEmpty()) {
            throw new CustomException(ErrorCode.INVALID_INGREDIENT_REQUEST);
        }

        Set<Long> existingIds = repo.findAllById(orderedIds).stream()
                .map(Ingredient::getId)
                .collect(Collectors.toSet());
        if (!existingIds.containsAll(orderedIds)) {
            throw new CustomException(ErrorCode.INGREDIENT_NOT_FOUND);
        }

        Map<Long, List<IngredientUnit>> unitsByIngredientId = ingredientUnitRepository
                .findAllByIngredientIdIn(orderedIds)
                .stream()
                .filter(unit -> unit.getIngredient() != null && unit.getIngredient().getId() != null)
                .collect(Collectors.groupingBy(unit -> unit.getIngredient().getId()));

        List<IngredientUnitsResponse> items = orderedIds.stream()
                .map(id -> toUnitsResponse(id, unitsByIngredientId.getOrDefault(id, List.of())))
                .toList();
        return IngredientUnitsBatchResponse.builder()
                .items(items)
                .build();
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

    /** 재료 상세 (i버튼 팝업용): 보관법 + Top N 인기 레시피 */
    @Transactional(readOnly = true)
    public IngredientDetailDto findDetailById(Long ingredientId) {
        Ingredient entity = repo.findById(ingredientId)
                .orElseThrow(() -> new CustomException(ErrorCode.INGREDIENT_NOT_FOUND));

        List<RecipeSimpleDto> recipes = recipeRepository.findTopByIngredientId(ingredientId, 10);

        PairResolution pairResolution = resolvePairItems(entity.getGoodPairs(), entity.getBadPairs());

        return IngredientDetailDto.builder()
                .id(entity.getId())
                .name(entity.getName())
                .category(entity.getCategory())
                .imageUrl(imageUrl(entity.getName()))
                .coupangLink(entity.getCoupangLink())
                .nutritionPer100g(toNutritionPer100g(entity))
                .storageLocation(entity.getStorageLocation())
                .storageTemperature(entity.getStorageTemperature())
                .storageDuration(entity.getStorageDuration())
                .storageNotes(entity.getStorageNotes())
                .goodPairs(entity.getGoodPairs())
                .goodPairItems(pairResolution.goodPairItems())
                .badPairs(entity.getBadPairs())
                .badPairItems(pairResolution.badPairItems())
                .benefits(entity.getBenefits())
                .seasonMonths(entity.getSeasonMonths())
                .recommendedCookingMethods(entity.getRecommendedCookingMethods())
                .recipes(recipes)
                .build();
    }

    private IngredientNutritionPer100gDto toNutritionPer100g(Ingredient entity) {
        return IngredientNutritionPer100gDto.builder()
                .kcal(per100(entity.getKcalPerG()))
                .carbohydrateG(per100(entity.getCarbohydrateGPerG()))
                .proteinG(per100(entity.getProteinGPerG()))
                .fatG(per100(entity.getFatGPerG()))
                .sugarG(per100(entity.getSugarGPerG()))
                .sodiumMg(per100(entity.getSodiumMgPerG()))
                .build();
    }

    private BigDecimal per100(BigDecimal perGram) {
        return perGram == null ? null : perGram.multiply(HUNDRED).stripTrailingZeros();
    }

    private IngredientUnitsResponse toUnitsResponse(Long ingredientId, List<IngredientUnit> units) {
        return IngredientUnitsResponse.builder()
                .ingredientId(ingredientId)
                .units(toUnitDtos(units))
                .build();
    }

    private List<IngredientUnitDto> toUnitDtos(List<IngredientUnit> units) {
        if (units == null || units.isEmpty()) {
            return List.of();
        }
        return units.stream()
                .sorted(UNIT_ORDER)
                .map(unit -> IngredientUnitDto.builder()
                        .unit(unit.getUnitLabelKo())
                        .gramsPerUnit(unit.getGramsPerUnit())
                        .isDefault(Boolean.TRUE.equals(unit.getIsDefault()))
                        .build())
                .toList();
    }

    private static String nullToEmpty(String value) {
        return value == null ? "" : value;
    }

    private PairResolution resolvePairItems(String goodPairs, String badPairs) {
        List<String> goodNames = parsePairNames(goodPairs);
        List<String> badNames = parsePairNames(badPairs);
        if (goodNames.isEmpty() && badNames.isEmpty()) {
            return new PairResolution(List.of(), List.of());
        }

        List<String> lookupNames = new ArrayList<>();
        lookupNames.addAll(goodNames);
        lookupNames.addAll(badNames);
        Map<String, Ingredient> byName = repo.findAllByNameIn(lookupNames).stream()
                .filter(i -> i.getName() != null)
                .collect(Collectors.toMap(Ingredient::getName, i -> i, (left, right) -> left));

        return new PairResolution(toPairItems(goodNames, byName), toPairItems(badNames, byName));
    }

    private List<String> parsePairNames(String value) {
        if (value == null || value.isBlank()) {
            return List.of();
        }
        LinkedHashSet<String> names = new LinkedHashSet<>();
        for (String token : value.split("[/,\\n]+")) {
            String trimmed = token.trim();
            if (!trimmed.isBlank()) {
                names.add(trimmed);
            }
        }
        return List.copyOf(names);
    }

    private List<IngredientPairItemDto> toPairItems(List<String> names, Map<String, Ingredient> byName) {
        if (names.isEmpty()) {
            return List.of();
        }
        return names.stream()
                .map(name -> {
                    Ingredient ingredient = byName.get(name);
                    if (ingredient == null) {
                        return IngredientPairItemDto.builder()
                                .name(name)
                                .build();
                    }
                    return IngredientPairItemDto.builder()
                            .id(ingredient.getId())
                            .name(ingredient.getName())
                            .imageUrl(imageUrl(ingredient.getName()))
                            .build();
                })
                .toList();
    }

    private String imageUrl(String ingredientName) {
        return INGREDIENT_IMAGE_PREFIX + ingredientName + ".webp";
    }

    private record PairResolution(
            List<IngredientPairItemDto> goodPairItems,
            List<IngredientPairItemDto> badPairItems
    ) {}

    /** 삭제 (관리자 전용) */
    public void delete(Long id) {
        if (!repo.existsById(id)) {
            throw new ResponseStatusException(
                    HttpStatus.NOT_FOUND, "재료가 없습니다: " + id);
        }
        repo.deleteById(id);
    }
}
