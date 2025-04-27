package com.jdc.recipe_service.mapper;

import com.jdc.recipe_service.domain.dto.comment.CommentDto;
import com.jdc.recipe_service.domain.entity.RecipeComment;


import java.util.List;

public class CommentMapper {

    public static CommentDto toDto(RecipeComment comment, boolean isLiked, int likeCount) {
        return CommentDto.builder()
                .id(comment.getId())
                .content(comment.getComment())
                .createdAt(comment.getCreatedAt())
                .author(UserMapper.toCommentUserDto(comment.getUser()))
                .likeCount(likeCount)
                .likedByCurrentUser(isLiked)
                .replies(List.of())
                .build();
    }

    public static CommentDto toDeletedDto(RecipeComment comment) {
        return CommentDto.builder()
                .id(comment.getId())
                .content("삭제된 댓글입니다.")
                .createdAt(comment.getCreatedAt())
                .author(null)
                .likeCount(0)
                .likedByCurrentUser(false)
                .replies(List.of())
                .isDeleted(true)
                .build();
    }

    public static CommentDto toReplyDto(RecipeComment comment, boolean isLiked, int likeCount) {
        return CommentDto.builder()
                .id(comment.getId())
                .content(comment.getComment())
                .createdAt(comment.getCreatedAt())
                .author(UserMapper.toCommentUserDto(comment.getUser()))
                .likeCount(likeCount)
                .likedByCurrentUser(isLiked)
                .build();
    }

    public static CommentDto toSimpleDto(RecipeComment comment) {
        return CommentDto.builder()
                .id(comment.getId())
                .content(comment.getComment())
                .createdAt(comment.getCreatedAt())
                .author(UserMapper.toCommentUserDto(comment.getUser()))
                .likeCount(0)
                .likedByCurrentUser(false)
                .replies(List.of())
                .build();
    }
}