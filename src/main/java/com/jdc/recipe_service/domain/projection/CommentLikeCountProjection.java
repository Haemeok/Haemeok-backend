package com.jdc.recipe_service.domain.projection;

public interface CommentLikeCountProjection {
    Long getCommentId();
    int getLikeCount();
}
