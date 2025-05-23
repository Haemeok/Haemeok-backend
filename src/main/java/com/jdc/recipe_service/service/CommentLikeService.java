package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.entity.CommentLike;
import com.jdc.recipe_service.domain.entity.RecipeComment;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.CommentLikeRepository;
import com.jdc.recipe_service.domain.repository.RecipeCommentRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class CommentLikeService {

    private final CommentLikeRepository commentLikeRepository;
    private final RecipeCommentRepository recipeCommentRepository;
    private final UserRepository userRepository;

    @Transactional
    public boolean toggleLike(Long commentId, Long userId) {
        CommentLike existing = commentLikeRepository.findByCommentIdAndUserId(commentId, userId);

        if (existing != null) {
            commentLikeRepository.delete(existing);
            return false;
        }

        RecipeComment comment = recipeCommentRepository.findById(commentId)
                .orElseThrow(() -> new CustomException(ErrorCode.COMMENT_NOT_FOUND));
        User user = userRepository.findById(userId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));

        CommentLike newLike = CommentLike.builder()
                .comment(comment)
                .user(user)
                .build();
        commentLikeRepository.save(newLike);
        return true;
    }

    @Transactional(readOnly = true)
    public int countLikes(Long commentId) {
        return commentLikeRepository.countByCommentId(commentId);
    }

}
