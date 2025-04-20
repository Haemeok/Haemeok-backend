package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.fridge.RefrigeratorItemRequestDto;
import com.jdc.recipe_service.domain.dto.fridge.RefrigeratorItemBulkRequestDto;
import com.jdc.recipe_service.domain.dto.fridge.RefrigeratorItemResponseDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.RefrigeratorItem;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.repository.RefrigeratorItemRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.mapper.IngredientMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.server.ResponseStatusException;

import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
@Transactional
public class RefrigeratorItemService {

    private final RefrigeratorItemRepository repo;
    private final UserRepository       userRepo;
    private final IngredientRepository ingRepo;

    private final DateTimeFormatter fmt = DateTimeFormatter.ISO_LOCAL_DATE_TIME;

    /**
     * 내 냉장고 아이템 조회(페이지 + 카테고리 필터)
     */
    @Transactional(readOnly = true)
    public Page<RefrigeratorItemResponseDto> getMyItems(
            Long userId, String category, Pageable pageable) {

        Page<RefrigeratorItem> page;

        if (category != null && !category.isBlank()) {
            page = repo.findByUserIdAndIngredientCategoryIgnoreCase(
                    userId, category, pageable);
        } else {
            page = repo.findByUserId(userId, pageable);
        }

        return page.map(item -> RefrigeratorItemResponseDto.builder()
                .id(item.getId())
                .ingredient(IngredientMapper.toDto(item.getIngredient()))
                .createdAt(item.getCreatedAt().format(fmt))
                .updatedAt(item.getUpdatedAt().format(fmt))
                .build()
        );
    }

    /** 냉장고에 재료 단건 추가 */
    public RefrigeratorItemResponseDto addItem(Long userId, RefrigeratorItemRequestDto dto) {
        User user = userRepo.findById(userId)
                .orElseThrow(() -> new ResponseStatusException(
                        HttpStatus.NOT_FOUND, "사용자가 없습니다: " + userId));
        Ingredient ing = ingRepo.findById(dto.getIngredientId())
                .orElseThrow(() -> new ResponseStatusException(
                        HttpStatus.NOT_FOUND, "재료가 없습니다: " + dto.getIngredientId()));

        repo.findByUserIdAndIngredientId(userId, dto.getIngredientId())
                .ifPresent(i -> { throw new ResponseStatusException(
                        HttpStatus.CONFLICT, "이미 추가된 재료입니다."); });

        RefrigeratorItem saved = repo.save(
                RefrigeratorItem.builder()
                        .user(user)
                        .ingredient(ing)
                        .build()
        );

        return RefrigeratorItemResponseDto.builder()
                .id(saved.getId())
                .ingredient(IngredientMapper.toDto(ing))
                .createdAt(saved.getCreatedAt().format(fmt))
                .updatedAt(saved.getUpdatedAt().format(fmt))
                .build();
    }

    /** 냉장고에서 재료 단건 제거 */
    public void removeItem(Long userId, Long ingredientId) {
        RefrigeratorItem item = repo.findByUserIdAndIngredientId(userId, ingredientId)
                .orElseThrow(() -> new ResponseStatusException(
                        HttpStatus.NOT_FOUND, "냉장고에 없습니다."));
        repo.delete(item);
    }

    /** 냉장고에 재료 여러 개 Bulk 추가 */
    public void addItemsBulk(Long userId, List<Long> ingredientIds) {
        User user = userRepo.findById(userId)
                .orElseThrow(() -> new ResponseStatusException(
                        HttpStatus.NOT_FOUND, "사용자가 없습니다: " + userId));

        List<RefrigeratorItem> items = new ArrayList<>();
        for (Long id : ingredientIds) {
            Ingredient ing = ingRepo.findById(id)
                    .orElseThrow(() -> new ResponseStatusException(
                            HttpStatus.NOT_FOUND, "재료가 없습니다: " + id));
            Optional<RefrigeratorItem> exist = repo.findByUserIdAndIngredientId(userId, id);
            if (exist.isPresent()) {
                throw new ResponseStatusException(
                        HttpStatus.CONFLICT, "이미 추가된 재료입니다: " + id);
            }
            items.add(RefrigeratorItem.builder().user(user).ingredient(ing).build());
        }
        repo.saveAll(items);
    }

    /** 냉장고에서 재료 여러 개 Bulk 제거 */
    public void removeItemsBulk(Long userId, List<Long> ingredientIds) {
        List<RefrigeratorItem> toDelete = new ArrayList<>();
        for (Long id : ingredientIds) {
            RefrigeratorItem item = repo.findByUserIdAndIngredientId(userId, id)
                    .orElseThrow(() -> new ResponseStatusException(
                            HttpStatus.NOT_FOUND, "냉장고에 없습니다: " + id));
            toDelete.add(item);
        }
        repo.deleteAll(toDelete);
    }
}
