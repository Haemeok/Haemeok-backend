package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.dto.user.UserRequestDTO;
import com.jdc.recipe_service.domain.dto.user.UserResponseDTO;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeFavorite;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeFavoriteRepository;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.mapper.UserMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@Transactional
@RequiredArgsConstructor
public class UserService {

    private final UserRepository userRepository;
    private final RecipeLikeRepository recipeLikeRepository;
    private final RecipeFavoriteRepository recipeFavoriteRepository;

    // 유저 생성
    public UserResponseDTO createUser(UserRequestDTO request) {
        User user = UserMapper.toEntity(request);
        userRepository.save(user);
        return UserMapper.toDto(user);
    }

    // 유저 단일 조회
    public UserResponseDTO getUser(Long id) {
        User user = userRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("User not found with id: " + id));
        return UserMapper.toDto(user);
    }

    public User getGuestUser() {
        return userRepository.findById(3L) // ✅ 여기에 guest 계정 ID 하드코딩
                .orElseThrow(() -> new RuntimeException("테스트 유저가 없습니다."));
    }

    // 모든 유저 조회
    public List<UserResponseDTO> getAllUsers() {
        List<User> users = userRepository.findAll();
        return users.stream()
                .map(UserMapper::toDto)
                .toList();
    }

    // 유저 업데이트
    public UserResponseDTO updateUser(Long id, UserRequestDTO request) {
        User user = userRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("User not found with id: " + id));

        UserMapper.updateEntityFromDto(request, user);

        userRepository.save(user);
        return UserMapper.toDto(user);
    }

    // 유저 삭제
    public void deleteUser(Long id) {
        User user = userRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("User not found with id: " + id));
        userRepository.delete(user);
    }

    @Transactional(readOnly = true)
    public List<RecipeSimpleDto> getFavoriteRecipesByUser(Long targetUserId, Long currentUserId) {
        List<RecipeFavorite> favorites = recipeFavoriteRepository.findByUserId(targetUserId);
        List<Recipe> recipes = favorites.stream().map(RecipeFavorite::getRecipe).toList();

        List<Long> recipeIds = recipes.stream().map(Recipe::getId).toList();

        // 🔹 좋아요 수 bulk 조회
        Map<Long, Long> likeCountMap = recipeLikeRepository.countLikesForRecipeIds(recipeIds);

        // 🔹 로그인 유저의 좋아요 여부 bulk 조회
        Set<Long> likedIds = (currentUserId != null)
                ? recipeLikeRepository.findByUserIdAndRecipeIdIn(currentUserId, recipeIds)
                .stream()
                .map(like -> like.getRecipe().getId())
                .collect(Collectors.toSet())
                : Set.of();

        return recipes.stream()
                .map(recipe -> RecipeSimpleDto.builder()
                        .id(recipe.getId())
                        .title(recipe.getTitle())
                        .imageUrl(recipe.getImageUrl())
                        .authorName(recipe.getUser().getNickname())
                        .createdAt(recipe.getCreatedAt())
                        .likeCount(likeCountMap.getOrDefault(recipe.getId(), 0L)) // 🔥 여기 bulk 적용
                        .likedByCurrentUser(likedIds.contains(recipe.getId()))
                        .build())
                .toList();
    }



}
