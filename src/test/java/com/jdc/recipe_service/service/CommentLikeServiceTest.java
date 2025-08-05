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

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

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
        CommentLike existing = CommentLike.builder()
                .comment(comment)
                .user(user)
                .build();
        ReflectionTestUtils.setField(existing, "id", 200L);

        when(commentLikeRepository.findByCommentIdAndUserId(100L, 10L))
                .thenReturn(Optional.of(existing));

        boolean result = commentLikeService.toggleLike(100L, 10L);
        assertFalse(result);

        verify(commentLikeRepository, times(1)).delete(existing);
        verifyNoMoreInteractions(recipeCommentRepository, userRepository);
    }

    @Test
    @DisplayName("toggleLike: 댓글이 없으면 COMMENT_NOT_FOUND 예외")
    void toggleLike_commentNotFound() {
        when(commentLikeRepository.findByCommentIdAndUserId(999L, 10L))
                .thenReturn(Optional.empty());
        when(recipeCommentRepository.findById(999L))
                .thenReturn(Optional.empty());

        CustomException ex = assertThrows(CustomException.class, () ->
                commentLikeService.toggleLike(999L, 10L)
        );
        assertEquals(ErrorCode.COMMENT_NOT_FOUND, ex.getErrorCode());

        verify(commentLikeRepository, times(1))
                .findByCommentIdAndUserId(999L, 10L);
        verify(recipeCommentRepository, times(1)).findById(999L);
        verifyNoMoreInteractions(userRepository);
    }

    @Test
    @DisplayName("toggleLike: 사용자 없으면 USER_NOT_FOUND 예외")
    void toggleLike_userNotFound() {
        when(commentLikeRepository.findByCommentIdAndUserId(100L, 10L))
                .thenReturn(Optional.empty());
        when(recipeCommentRepository.findById(100L))
                .thenReturn(Optional.of(comment));
        when(userRepository.findById(10L))
                .thenReturn(Optional.empty());

        CustomException ex = assertThrows(CustomException.class, () ->
                commentLikeService.toggleLike(100L, 10L)
        );
        assertEquals(ErrorCode.USER_NOT_FOUND, ex.getErrorCode());

        verify(recipeCommentRepository, times(1)).findById(100L);
        verify(userRepository, times(1)).findById(10L);
    }

    @Test
    @DisplayName("toggleLike: 정상 호출 시 새로운 좋아요 저장 후 true 반환")
    void toggleLike_success() {
        when(commentLikeRepository.findByCommentIdAndUserId(100L, 10L))
                .thenReturn(Optional.empty());
        when(recipeCommentRepository.findById(100L))
                .thenReturn(Optional.of(comment));
        when(userRepository.findById(10L))
                .thenReturn(Optional.of(user));

        when(commentLikeRepository.save(any(CommentLike.class)))
                .thenAnswer(invocation -> {
                    CommentLike cl = invocation.getArgument(0);
                    ReflectionTestUtils.setField(cl, "id", 300L);
                    return cl;
                });

        boolean result = commentLikeService.toggleLike(100L, 10L);
        assertTrue(result);

        ArgumentCaptor<CommentLike> captor = ArgumentCaptor.forClass(CommentLike.class);
        verify(commentLikeRepository, times(1)).save(captor.capture());

        CommentLike saved = captor.getValue();
        assertEquals(100L, ((RecipeComment) ReflectionTestUtils.getField(saved, "comment")).getId());
        assertEquals(10L, ((User) ReflectionTestUtils.getField(saved, "user")).getId());
    }

    @Test
    @DisplayName("countLikes: 정상 호출 시 레포 countByCommentId 호출 결과 반환")
    void countLikes_success() {
        when(commentLikeRepository.countByCommentId(100L)).thenReturn(7);
        int cnt = commentLikeService.countLikes(100L);
        assertEquals(7, cnt);

        verify(commentLikeRepository, times(1)).countByCommentId(100L);
    }
}
