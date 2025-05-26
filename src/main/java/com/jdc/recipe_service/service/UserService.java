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
import org.springframework.http.HttpStatus;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.web.server.ResponseStatusException;

import java.util.Collections;
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
            throw new ResponseStatusException(
                    HttpStatus.CONFLICT,
                    "이미 사용 중인 닉네임입니다."
            );
        }

        UserMapper.updateEntityFromDto(dto, user);
        return UserMapper.toDto(user);
    }

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

        // 3) 좋아요 개수 bulk 조회
        Map<Long, Long> likeCountMap = recipeLikeRepository.countLikesRaw(recipeIds)
                .stream()
                .collect(Collectors.toMap(
                        row -> (Long) row[0],
                        row -> (Long) row[1]
                ));

        // 4) 좋아요 여부
        Set<Long> likedIds = (currentUserId != null)
                ? recipeLikeRepository.findByUserIdAndRecipeIdIn(currentUserId, recipeIds)
                .stream()
                .map(like -> like.getRecipe().getId())
                .collect(Collectors.toSet())
                : Set.of();

        // 5) DTO 매핑 → ✅ 핵심 부분만 교체
        List<RecipeSimpleDto> dtos = recipes.stream()
                .map(recipe -> {
                    RecipeSimpleDto dto = new RecipeSimpleDto(
                            recipe.getId(),
                            recipe.getTitle(),
                            recipe.getImageKey(), // 나중에 setImageUrl()로 변환
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

        // 6) PageImpl로 감싸서 반환
        return new PageImpl<>(dtos, pageable, favPage.getTotalElements());
    }


    @Transactional(readOnly = true)
    public Page<MyRecipeSummaryDto> getUserRecipes(
            Long targetUserId,
            @Nullable Long viewerId,
            Pageable pageable) {

        // 1) 대상 유저 존재 체크 (404)
        userRepository.findById(targetUserId)
                .orElseThrow(() -> new RuntimeException("해당 사용자가 존재하지 않습니다."));

        // 2) 페이징된 Recipe 엔티티 페이지 조회
        Page<Recipe> recipesPage = recipeRepository.findByUserId(targetUserId, pageable);

        // 3) viewerId가 있으면 bulk로 좋아요 누른 recipeId 집합 조회
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

        // 4) DTO로 매핑 (빌더에 likedByCurrentUser 포함)
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