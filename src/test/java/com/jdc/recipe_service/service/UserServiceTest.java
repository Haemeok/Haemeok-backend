package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.entity.*;
import com.jdc.recipe_service.domain.repository.*;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.util.S3Util;
import jakarta.persistence.EntityManager;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.transaction.annotation.Transactional;


import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.verify;

@SpringBootTest
@Transactional
class UserServiceDeleteTest {

    @Autowired UserService userService;
    @Autowired UserRepository userRepository;
    @Autowired RecipeRepository recipeRepository;
    @Autowired RecipeCommentRepository recipeCommentRepository;
    @Autowired RecipeLikeRepository recipeLikeRepository;
    @Autowired EntityManager em;

    @MockitoBean
    S3Util s3Util;

    @Test
    @DisplayName("유저 하드 삭제 시 연관된 레시피, 댓글, 좋아요가 DB Cascade에 의해 모두 삭제되어야 한다")
    void deleteUser_Cascade_Test() {
        User targetUser = userRepository.save(User.builder()
                .nickname("삭제될유저")
                .provider("google")
                .oauthId("12345")
                .profileImageKey("test/profile.jpg")
                .build());

        User otherUser = userRepository.save(User.builder()
                .nickname("구경꾼")
                .provider("kakao")
                .oauthId("67890")
                .build());

        Recipe myRecipe = recipeRepository.save(Recipe.builder()
                .user(targetUser)
                .title("삭제될 레시피")
                .dishType(DishType.FRYING)
                .imageKey("test/recipe.jpg")
                .isPrivate(false)
                .isAiGenerated(false)
                .build());

        Recipe otherRecipe = recipeRepository.save(Recipe.builder()
                .user(otherUser).title("남의 레시피").build());

        recipeLikeRepository.save(RecipeLike.builder()
                .user(targetUser)
                .recipe(otherRecipe)
                .build());


        RecipeComment commentOnMyRecipe = recipeCommentRepository.save(RecipeComment.builder()
                .user(otherUser)
                .recipe(myRecipe)
                .comment("이 레시피 가지 마세요 ㅠㅠ")
                .build());

        em.flush();
        em.clear();

        userService.deleteUser(targetUser.getId());

        assertThat(userRepository.findById(targetUser.getId())).isEmpty();
        assertThat(recipeRepository.findById(myRecipe.getId())).isEmpty();
        assertThat(recipeCommentRepository.findById(commentOnMyRecipe.getId())).isEmpty();
        assertThat(recipeLikeRepository.existsByRecipeIdAndUserId(otherRecipe.getId(), targetUser.getId())).isFalse();

        verify(s3Util).deleteFiles(anyList());
    }
}