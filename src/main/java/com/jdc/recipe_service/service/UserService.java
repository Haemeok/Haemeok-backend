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
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.PageImpl;
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

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    public String generateImageUrl(String key) {
        return key == null ? null :
                String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, key);
    }

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
    public Page<RecipeSimpleDto> getFavoriteRecipesByUser(
            Long targetUserId,
            Long currentUserId,
            Pageable pageable) {

        // 1) 페이징된 즐겨찾기 엔티티 조회
        Page<RecipeFavorite> favPage =
                recipeFavoriteRepository.findByUserId(targetUserId, pageable);

        // 2) Recipe 객체 리스트 추출
        List<Recipe> recipes = favPage.getContent().stream()
                .map(RecipeFavorite::getRecipe)
                .toList();

        List<Long> recipeIds = recipes.stream()
                .map(Recipe::getId)
                .toList();

        // 3) bulk 좋아요 수 조회
        Map<Long, Long> likeCountMap =
                recipeLikeRepository.countLikesForRecipeIds(recipeIds);

        // 4) bulk 내 좋아요 여부 조회
        Set<Long> likedIds = (currentUserId != null)
                ? recipeLikeRepository.findByUserIdAndRecipeIdIn(currentUserId, recipeIds)
                .stream()
                .map(like -> like.getRecipe().getId())
                .collect(Collectors.toSet())
                : Set.of();

        // 5) DTO 매핑
        List<RecipeSimpleDto> dtos = recipes.stream()
                .map(recipe -> RecipeSimpleDto.builder()
                        .id(recipe.getId())
                        .title(recipe.getTitle())
                        .imageUrl(generateImageUrl(recipe.getImageKey()))
                        .authorName(recipe.getUser().getNickname())
                        .createdAt(recipe.getCreatedAt())
                        .likeCount(likeCountMap.getOrDefault(recipe.getId(), 0L)) // 🔥 여기 bulk 적용
                        .likedByCurrentUser(likedIds.contains(recipe.getId()))
                        .build())
                .toList();

        // 6) PageImpl로 감싸서 반환
        return new PageImpl<>(dtos, pageable, favPage.getTotalElements());
    }



    @Transactional(readOnly = true)
    public Page<MyRecipeSummaryDto> getMyRecipes(Long userId, Pageable pageable) {
        return recipeRepository.findByUserId(userId, pageable)
                .map(recipe -> MyRecipeSummaryDto.builder()
                        .id(recipe.getId())
                        .title(recipe.getTitle())
                        .imageUrl(generateImageUrl(recipe.getImageKey()))
                        .dishType(recipe.getDishType().getDisplayName())
                        .createdAt(recipe.getCreatedAt())
                        .isAiGenerated(recipe.isAiGenerated())
                        .build());
    }


}