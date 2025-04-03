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
    public List<RecipeSimpleDto> getFavoriteRecipesByUser(Long userId) {
        List<RecipeFavorite> favorites = recipeFavoriteRepository.findByUserId(userId);

        List<Recipe> recipes = favorites.stream()
                .map(RecipeFavorite::getRecipe)
                .toList();

        List<Long> recipeIds = recipes.stream().map(Recipe::getId).toList();
        Set<Long> likedRecipeIds = recipeLikeRepository.findByUserIdAndRecipeIdIn(userId, recipeIds)
                .stream().map(like -> like.getRecipe().getId())
                .collect(Collectors.toSet());

        return recipes.stream()
                .map(recipe -> RecipeSimpleDto.builder()
                        .id(recipe.getId())
                        .title(recipe.getTitle())
                        .imageUrl(recipe.getImageUrl())
                        .authorName(recipe.getUser().getNickname())
                        .createdAt(recipe.getCreatedAt())
                        .likeCount(recipeLikeRepository.countByRecipeId(recipe.getId()))
                        .likedByCurrentUser(likedRecipeIds.contains(recipe.getId()))
                        .build())
                .toList();
    }

}
