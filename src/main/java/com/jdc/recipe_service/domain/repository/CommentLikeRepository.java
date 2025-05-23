package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.CommentLike;
import com.jdc.recipe_service.domain.projection.CommentLikeCountProjection;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface CommentLikeRepository extends JpaRepository<CommentLike, Long> {

    int countByCommentId(Long commentId);

    boolean existsByCommentIdAndUserId(Long commentId, Long userId);

    @Query("SELECT cl.comment.id FROM CommentLike cl WHERE cl.user.id = :userId AND cl.comment.id IN :commentIds")
    List<Long> findLikedCommentIdsByUser(@Param("userId") Long userId, @Param("commentIds") List<Long> commentIds);

    CommentLike findByCommentIdAndUserId(Long commentId, Long userId);

    @Query("SELECT cl.comment.id AS commentId, COUNT(cl) AS likeCount " +
            "FROM CommentLike cl " +
            "WHERE cl.comment.id IN :commentIds " +
            "GROUP BY cl.comment.id")
    List<CommentLikeCountProjection> countLikesByCommentIds(@Param("commentIds") List<Long> commentIds);

    void deleteByCommentIdIn(List<Long> commentIds);
}
