package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.entity.CommentLike;
import com.jdc.recipe_service.domain.entity.RecipeComment;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.CommentLikeRepository;
import com.jdc.recipe_service.domain.repository.RecipeCommentRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.*;

@ExtendWith(MockitoExtension.class)
class CommentLikeServiceTest {

    @Mock private CommentLikeRepository commentLikeRepository;
    @Mock private RecipeCommentRepository recipeCommentRepository;
    @Mock private UserRepository userRepository;

    @InjectMocks
    private CommentLikeService commentLikeService;

    private User user;
    private RecipeComment comment;

    @BeforeEach
    void setUp() {
        user = User.builder().id(10L).build();
        comment = RecipeComment.builder().build();
        ReflectionTestUtils.setField(comment, "id", 100L);
    }

    @Test
    @DisplayName("toggleLike: 이미 좋아요가 있으면 삭제 후 false 반환")
    void toggleLike_alreadyExists() {
        // Given
        CommentLike existing = CommentLike.builder()
                .comment(comment)
                .user(user)
                .build();
        ReflectionTestUtils.setField(existing, "id", 200L);

        given(commentLikeRepository.findByCommentIdAndUserId(100L, 10L))
                .willReturn(Optional.of(existing));

        // When
        boolean result = commentLikeService.toggleLike(100L, 10L);

        // Then
        assertThat(result).isFalse();
        verify(commentLikeRepository, times(1)).delete(existing);
        verifyNoMoreInteractions(recipeCommentRepository, userRepository);
    }

    @Test
    @DisplayName("toggleLike: 댓글이 없으면 COMMENT_NOT_FOUND 예외")
    void toggleLike_commentNotFound() {
        // Given
        given(commentLikeRepository.findByCommentIdAndUserId(999L, 10L))
                .willReturn(Optional.empty());
        given(recipeCommentRepository.findById(999L))
                .willReturn(Optional.empty());

        // When & Then
        assertThatThrownBy(() -> commentLikeService.toggleLike(999L, 10L))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> assertThat(((CustomException) e).getErrorCode()).isEqualTo(ErrorCode.COMMENT_NOT_FOUND));

        verify(commentLikeRepository, times(1))
                .findByCommentIdAndUserId(999L, 10L);
        verify(recipeCommentRepository, times(1)).findById(999L);
        verifyNoMoreInteractions(userRepository);
    }

    @Test
    @DisplayName("toggleLike: 사용자 없으면 USER_NOT_FOUND 예외")
    void toggleLike_userNotFound() {
        // Given
        given(commentLikeRepository.findByCommentIdAndUserId(100L, 10L))
                .willReturn(Optional.empty());
        given(recipeCommentRepository.findById(100L))
                .willReturn(Optional.of(comment));
        given(userRepository.findById(10L))
                .willReturn(Optional.empty());

        // When & Then
        assertThatThrownBy(() -> commentLikeService.toggleLike(100L, 10L))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> assertThat(((CustomException) e).getErrorCode()).isEqualTo(ErrorCode.USER_NOT_FOUND));

        verify(recipeCommentRepository, times(1)).findById(100L);
        verify(userRepository, times(1)).findById(10L);
    }

    @Test
    @DisplayName("toggleLike: 정상 호출 시 새로운 좋아요 저장 후 true 반환")
    void toggleLike_success() {
        // Given
        given(commentLikeRepository.findByCommentIdAndUserId(100L, 10L))
                .willReturn(Optional.empty());
        given(recipeCommentRepository.findById(100L))
                .willReturn(Optional.of(comment));
        given(userRepository.findById(10L))
                .willReturn(Optional.of(user));

        given(commentLikeRepository.save(any(CommentLike.class)))
                .willAnswer(invocation -> {
                    CommentLike cl = invocation.getArgument(0);
                    ReflectionTestUtils.setField(cl, "id", 300L);
                    return cl;
                });

        // When
        boolean result = commentLikeService.toggleLike(100L, 10L);

        // Then
        assertThat(result).isTrue();

        ArgumentCaptor<CommentLike> captor = ArgumentCaptor.forClass(CommentLike.class);
        verify(commentLikeRepository, times(1)).save(captor.capture());

        CommentLike saved = captor.getValue();
        assertThat(((RecipeComment) ReflectionTestUtils.getField(saved, "comment")).getId()).isEqualTo(100L);
        assertThat(((User) ReflectionTestUtils.getField(saved, "user")).getId()).isEqualTo(10L);
    }

    @Test
    @DisplayName("countLikes: 정상 호출 시 레포 countByCommentId 호출 결과 반환")
    void countLikes_success() {
        // Given
        given(commentLikeRepository.countByCommentId(100L)).willReturn(7);

        // When
        int cnt = commentLikeService.countLikes(100L);

        // Then
        assertThat(cnt).isEqualTo(7);
        verify(commentLikeRepository, times(1)).countByCommentId(100L);
    }
}
