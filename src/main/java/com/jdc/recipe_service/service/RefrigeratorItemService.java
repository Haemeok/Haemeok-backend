package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.fridge.RefrigeratorItemRequestDto;
import com.jdc.recipe_service.domain.dto.fridge.RefrigeratorItemResponseDto;
import com.jdc.recipe_service.domain.dto.fridge.RefrigeratorItemSummaryDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.RefrigeratorItem;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.repository.RefrigeratorItemRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.mapper.IngredientMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional
public class RefrigeratorItemService {

    private final RefrigeratorItemRepository repo;
    private final UserRepository       userRepo;
    private final IngredientRepository ingRepo;

    /**
     * 내 냉장고 아이템 조회(페이지 + 카테고리 필터)
     */
    @Transactional(readOnly = true)
    public Page<RefrigeratorItemSummaryDto> getMyItems(
            Long userId, String category, Pageable pageable) {

        Page<RefrigeratorItem> page;

        if (category != null && !category.isBlank()) {
            page = repo.findByUserIdAndIngredientCategoryIgnoreCase(
                    userId, category, pageable);
        } else {
            page = repo.findByUserId(userId, pageable);
        }

        return page.map(item ->
                new RefrigeratorItemSummaryDto(
                        item.getIngredient().getId(),
                        item.getIngredient().getName(),
                        item.getIngredient().getCategory(),
                        item.getIngredient().getImageUrl(),
                        item.getIngredient().getUnit()
                )
        );
    }

    /** 냉장고에 재료 단건 추가 */
    public RefrigeratorItemResponseDto addItem(Long userId, RefrigeratorItemRequestDto dto) {
        User user = userRepo.findById(userId)
                .orElseThrow(() -> new CustomException(ErrorCode.FRIDGE_UNAUTHORIZED));

        Ingredient ing = ingRepo.findById(dto.getIngredientId())
                .orElseThrow(() -> new CustomException(ErrorCode.INGREDIENT_NOT_FOUND));

        repo.findByUserIdAndIngredientId(userId, dto.getIngredientId())
                .ifPresent(i -> { throw new CustomException(ErrorCode.DUPLICATE_FRIDGE_ITEM); });

        RefrigeratorItem saved = repo.save(
                RefrigeratorItem.builder()
                        .user(user)
                        .ingredient(ing)
                        .build()
        );

        return RefrigeratorItemResponseDto.builder()
                .id(saved.getId())
                .ingredient(IngredientMapper.toDto(ing))
                .createdAt(saved.getCreatedAt())
                .build();
    }

    /** 냉장고에서 재료 단건 제거 */
    public void removeItem(Long userId, Long ingredientId) {
        RefrigeratorItem item = repo.findByUserIdAndIngredientId(userId, ingredientId)
                .orElseThrow(() -> new CustomException(ErrorCode.FRIDGE_ITEM_NOT_FOUND));
        repo.delete(item);
    }

    /** 냉장고에 재료 여러 개 Bulk 추가 */
    public void addItemsBulk(Long userId, List<Long> ingredientIds) {
        User user = userRepo.findById(userId)
                .orElseThrow(() -> new CustomException(ErrorCode.FRIDGE_UNAUTHORIZED));

        Set<Long> existingIds = repo.findAllByUserId(userId).stream()
                .map(item -> item.getIngredient().getId())
                .collect(Collectors.toSet());

        List<RefrigeratorItem> itemsToAdd = new ArrayList<>();

        for (Long id : ingredientIds) {
            if (existingIds.contains(id)) continue;

            Ingredient ing = ingRepo.findById(id)
                    .orElseThrow(() -> new CustomException(ErrorCode.INGREDIENT_NOT_FOUND));

            itemsToAdd.add(RefrigeratorItem.builder()
                    .user(user)
                    .ingredient(ing)
                    .build());
        }

        if (!itemsToAdd.isEmpty()) {
            repo.saveAll(itemsToAdd);
        }
    }

    /** 냉장고에서 재료 여러 개 Bulk 제거 */
    public void removeItemsBulk(Long userId, List<Long> ingredientIds) {
        List<RefrigeratorItem> toDelete = new ArrayList<>();
        for (Long id : ingredientIds) {
            RefrigeratorItem item = repo.findByUserIdAndIngredientId(userId, id)
                    .orElseThrow(() -> new CustomException(ErrorCode.FRIDGE_ITEM_NOT_FOUND));
            toDelete.add(item);
        }
        repo.deleteAll(toDelete);
    }
}
