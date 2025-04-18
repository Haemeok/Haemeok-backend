package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.MyRecipeSummaryDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.dto.user.UserDto;
import com.jdc.recipe_service.domain.dto.user.UserRequestDTO;
import com.jdc.recipe_service.domain.dto.user.UserResponseDTO;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeFavorite;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeFavoriteRepository;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.mapper.UserMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@Transactional
@RequiredArgsConstructor
public class UserService {

    private final UserRepository userRepository;
    private final RecipeFavoriteRepository recipeFavoriteRepository;
    private final RecipeLikeRepository recipeLikeRepository;
    private final RecipeRepository recipeRepository;


    // 관리자 전용: 사용자 생성
    public UserResponseDTO createUser(UserRequestDTO dto) {
        User user = UserMapper.toEntity(dto);
        userRepository.save(user);
        return UserMapper.toDto(user);
    }

    // 관리자 전용: 전체 조회
    public List<UserResponseDTO> getAllUsers() {
        return userRepository.findAll()
                .stream()
                .map(UserMapper::toDto)
                .toList();
    }

    // 관리자 전용: 하드 삭제
    public void deleteUser(Long id) {
        User user = userRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("User not found with id: " + id));
        userRepository.delete(user);
    }

    // 유저 정보 조회(관리자 or 나)
    public UserResponseDTO getUser(Long id) {
        User user = userRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("User not found with id: " + id));
        return UserMapper.toDto(user);
    }


    // 프로필 조회(모든 사람)
    @Transactional(readOnly = true)
    public UserDto getPublicProfile(Long userId) {
        User user = userRepository.findById(userId)
                .orElseThrow(() -> new IllegalArgumentException("User not found: " + userId));
        return UserMapper.toSimpleDto(user);
    }


    //  프로필 수정 (관리자 or 나)
    public UserResponseDTO updateUser(Long id, UserRequestDTO dto) {
        User user = userRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("User not found with id: " + id));

        if (!user.getNickname().equals(dto.getNickname())
                && userRepository.findByNickname(dto.getNickname()).isPresent()) {
            throw new RuntimeException("이미 사용 중인 닉네임입니다.");
        }

        UserMapper.updateEntityFromDto(dto, user);
        return UserMapper.toDto(user);
    }



    // 내 즐겨찾기 조회
    @Transactional(readOnly = true)
    public List<RecipeSimpleDto> getFavoriteRecipesByUser(Long userId) {
        List<RecipeFavorite> favs = recipeFavoriteRepository.findByUserId(userId);
        List<Recipe> recipes = favs.stream()
                .map(RecipeFavorite::getRecipe)
                .toList();

        List<Long> ids = recipes.stream()
                .map(Recipe::getId)
                .toList();

        Map<Long, Long> likeCount = recipeLikeRepository.countLikesForRecipeIds(ids);

        return recipes.stream()
                .map(r -> RecipeSimpleDto.builder()
                        .id(r.getId())
                        .title(r.getTitle())
                        .imageUrl(r.getImageUrl())
                        .authorName(r.getUser().getNickname())
                        .createdAt(r.getCreatedAt())
                        .likeCount(likeCount.getOrDefault(r.getId(), 0L))
                        .likedByCurrentUser(true)
                        .build())
                .toList();
    }


    @Transactional(readOnly = true)
    public Page<MyRecipeSummaryDto> getMyRecipes(Long userId, Pageable pageable) {
        return recipeRepository.findByUserId(userId, pageable)
                .map(recipe -> MyRecipeSummaryDto.builder()
                        .id(recipe.getId())
                        .title(recipe.getTitle())
                        .imageUrl(recipe.getImageUrl())
                        .dishType(recipe.getDishType().getDisplayName())
                        .createdAt(recipe.getCreatedAt())
                        .isAiGenerated(recipe.isAiGenerated())
                        .build());
    }


}
