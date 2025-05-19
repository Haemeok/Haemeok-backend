package com.jdc.recipe_service.mapper;

import com.jdc.recipe_service.domain.dto.comment.CommentDto;
import com.jdc.recipe_service.domain.dto.comment.ReplyDto;
import com.jdc.recipe_service.domain.entity.RecipeComment;

public class CommentMapper {

    public static CommentDto toDto(RecipeComment comment, boolean isLiked, int likeCount) {
        return CommentDto.builder()
                .id(comment.getId())
                .content(comment.getComment())
                .createdAt(comment.getCreatedAt())
                .author(UserMapper.toCommentUserDto(comment.getUser()))
                .likeCount(likeCount)
                .likedByCurrentUser(isLiked)
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
                .isDeleted(true)
                .build();
    }

    public static ReplyDto toReplyDto(RecipeComment c, boolean isLiked, int likeCount) {
        return ReplyDto.builder()
                .id(c.getId())
                .content(c.getComment())
                .createdAt(c.getCreatedAt())
                .author(UserMapper.toCommentUserDto(c.getUser()))
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
                .build();
    }
}