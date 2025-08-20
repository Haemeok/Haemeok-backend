package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.MyRecipeSummaryDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponseItem;
import com.jdc.recipe_service.domain.dto.user.UserDto;
import com.jdc.recipe_service.domain.dto.user.UserPatchDTO;
import com.jdc.recipe_service.domain.dto.user.UserRequestDTO;
import com.jdc.recipe_service.domain.dto.user.UserResponseDTO;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeFavorite;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeFavoriteRepository;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.mapper.UserMapper;
import com.jdc.recipe_service.util.S3Util;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.PageImpl;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.*;
import java.util.stream.Collectors;

@Service
@Transactional
@RequiredArgsConstructor
public class UserService {

    private final UserRepository userRepository;
    private final RecipeFavoriteRepository recipeFavoriteRepository;
    private final RecipeLikeRepository recipeLikeRepository;
    private final RecipeRepository recipeRepository;
    private final S3Util s3Util;
    private final DailyQuotaService dailyQuotaService;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    public String generateImageUrl(String key) {
        return key == null ? null :
                String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, key);
    }

    // presigned URL 생성
    @Transactional(readOnly = true)
    public PresignedUrlResponseItem generateProfileImagePresign(Long userId) {
        User user = userRepository.findById(userId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));
        String key = String.format("images/profiles/%d/%s.jpg", userId, UUID.randomUUID());
        String presignedUrl = s3Util.createPresignedUrl(key);
        return PresignedUrlResponseItem.builder()
                .fileKey(key)
                .presignedUrl(presignedUrl)
                .build();
    }

    //  프로필 수정 (관리자 or 나)
    public UserResponseDTO updateUser(Long id, UserPatchDTO dto) {
        User user = userRepository.findById(id)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));

        if (!user.getNickname().equals(dto.getNickname())
                && userRepository.findByNickname(dto.getNickname()).isPresent()) {
            throw new CustomException(ErrorCode.DUPLICATE_NICKNAME);
        }

        user.updateProfile(dto.getNickname(), null, dto.getIntroduction());

        if (dto.getProfileImageKey() != null) {
            String key = dto.getProfileImageKey();
            if (key.isBlank()) {
                user.updateProfile(null, null, null);               // profileImage → null
                user.updateProfileImageKey(null);
            } else {
                user.updateProfileImageKey(key);
                user.updateProfile(null, generateImageUrl(key), null);
            }
        }

        return UserMapper.toDto(user);
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
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));
        userRepository.delete(user);
    }

    // 유저 정보 조회(관리자 or 나)
    public UserResponseDTO getUser(Long id) {
        User user = userRepository.findById(id)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));
        UserResponseDTO dto = UserMapper.toDto(user);

        int remainingQuota = dailyQuotaService.getRemainingQuota(id);
        dto.updateAiQuota(remainingQuota);
        return dto;
    }


    // 프로필 조회(모든 사람)
    @Transactional(readOnly = true)
    public UserDto getPublicProfile(Long userId) {
        User user = userRepository.findById(userId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));
        return UserMapper.toSimpleDto(user);
    }

    @Transactional(readOnly = true)
    public Page<RecipeSimpleDto> getFavoriteRecipesByUser(
            Long targetUserId,
            Long currentUserId,
            Pageable pageable) {

        Page<RecipeFavorite> favPage =
                recipeFavoriteRepository.findByUserId(targetUserId, pageable);

        List<Recipe> recipes = favPage.getContent().stream()
                .map(RecipeFavorite::getRecipe)
                .toList();

        List<Long> recipeIds = recipes.stream()
                .map(Recipe::getId)
                .toList();

        Map<Long, Long> likeCountMap = recipeLikeRepository.countLikesRaw(recipeIds)
                .stream()
                .collect(Collectors.toMap(
                        row -> (Long) row[0],
                        row -> (Long) row[1]
                ));

        Set<Long> likedIds = (currentUserId != null)
                ? recipeLikeRepository.findByUserIdAndRecipeIdIn(currentUserId, recipeIds)
                .stream()
                .map(like -> like.getRecipe().getId())
                .collect(Collectors.toSet())
                : Set.of();

        List<RecipeSimpleDto> dtos = recipes.stream()
                .map(recipe -> {
                    RecipeSimpleDto dto = new RecipeSimpleDto(
                            recipe.getId(),
                            recipe.getTitle(),
                            recipe.getImageKey(),
                            recipe.getUser().getId(),
                            recipe.getUser().getNickname(),
                            recipe.getUser().getProfileImage(),
                            recipe.getCreatedAt(),
                            likeCountMap.getOrDefault(recipe.getId(), 0L),
                            likedIds.contains(recipe.getId()),
                            recipe.getCookingTime(),
                            recipe.getAvgRating(),
                            recipe.getRatingCount()
                    );
                    dto.setImageUrl(generateImageUrl(recipe.getImageKey()));
                    return dto;
                })
                .toList();

        return new PageImpl<>(dtos, pageable, favPage.getTotalElements());
    }


    @Transactional(readOnly = true)
    public Page<MyRecipeSummaryDto> getUserRecipes(
            Long targetUserId,
            @Nullable Long viewerId,
            Pageable pageable) {

        userRepository.findById(targetUserId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));

        Page<Recipe> recipesPage = recipeRepository.findByUserId(targetUserId, pageable);

        Set<Long> likedIds;
        if (viewerId != null && recipesPage.hasContent()) {
            List<Long> ids = recipesPage.stream()
                    .map(Recipe::getId)
                    .toList();
            likedIds = recipeLikeRepository
                    .findByUserIdAndRecipeIdIn(viewerId, ids)
                    .stream()
                    .map(like -> like.getRecipe().getId())
                    .collect(Collectors.toSet());
        } else {
            likedIds = Collections.emptySet();
        }

        Page<MyRecipeSummaryDto> dtoPage = recipesPage.map(recipe ->
                MyRecipeSummaryDto.builder()
                        .id(recipe.getId())
                        .title(recipe.getTitle())
                        .imageUrl(generateImageUrl(recipe.getImageKey()))
                        .dishType(recipe.getDishType().getDisplayName())
                        .createdAt(recipe.getCreatedAt())
                        .isAiGenerated(recipe.isAiGenerated())
                        .isPrivate(recipe.getIsPrivate())
                        .likedByCurrentUser(likedIds.contains(recipe.getId()))
                        .build()
        );

        return dtoPage;
    }

}