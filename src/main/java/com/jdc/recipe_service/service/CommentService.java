package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.comment.CommentDto;
import com.jdc.recipe_service.domain.dto.comment.CommentRequestDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeComment;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.projection.CommentLikeCountProjection;
import com.jdc.recipe_service.domain.repository.CommentLikeRepository;
import com.jdc.recipe_service.domain.repository.RecipeCommentRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.mapper.CommentMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Service
@RequiredArgsConstructor
@Transactional
public class CommentService {

    private final RecipeCommentRepository recipeCommentRepository;
    private final CommentLikeRepository commentLikeRepository;
    private final RecipeRepository recipeRepository;
    private final UserRepository userRepository;

    public List<CommentDto> getTop3CommentsWithLikes(Long recipeId, Long currentUserId) {
        List<RecipeComment> comments = recipeCommentRepository
                .findTop3ByRecipeIdAndParentCommentIsNull(recipeId, Pageable.ofSize(3))
                .stream()
                .filter(c -> !c.isDeleted() || !c.getReplies().isEmpty())
                .toList();

        List<Long> commentIds = comments.stream().map(RecipeComment::getId).toList();

        Map<Long, Integer> likeCountMap = commentLikeRepository.countLikesByCommentIds(commentIds).stream()
                .collect(Collectors.toMap(
                        CommentLikeCountProjection::getCommentId,
                        CommentLikeCountProjection::getLikeCount
                ));

        Set<Long> likedIds = new HashSet<>(commentLikeRepository.findLikedCommentIdsByUser(currentUserId, commentIds));

        return comments.stream().map(c -> {
            boolean isDeleted = c.isDeleted();
            boolean hasReplies = c.getReplies() != null && c.getReplies().stream().anyMatch(r -> !r.isDeleted());

            if (isDeleted && hasReplies) {
                return CommentMapper.toDeletedDto(c);
            } else if (isDeleted) {
                return null;
            } else {
                int likeCount = likeCountMap.getOrDefault(c.getId(), 0);
                boolean isLiked = likedIds.contains(c.getId());
                return CommentMapper.toDto(c, isLiked, likeCount);
            }
        }).filter(Objects::nonNull).toList();
    }

    public CommentDto createComment(Long recipeId, CommentRequestDto dto, User user) {
        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        RecipeComment comment = RecipeComment.builder()
                .recipe(recipe)
                .user(user)
                .comment(dto.getContent())
                .build();
        recipeCommentRepository.save(comment);

        return CommentMapper.toSimpleDto(comment);
    }


    public CommentDto createReply(Long recipeId, Long parentId, CommentRequestDto dto, Long userId) {
        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));

        RecipeComment parent = recipeCommentRepository.findById(parentId)
                .orElseThrow(() -> new CustomException(ErrorCode.COMMENT_NOT_FOUND));

        RecipeComment reply = RecipeComment.builder()
                .recipe(recipe)
                .user(user)
                .comment(dto.getContent())
                .parentComment(parent)
                .build();

        recipeCommentRepository.save(reply);

        return CommentMapper.toSimpleDto(reply);
    }


    public Page<CommentDto> getAllCommentsWithLikes(Long recipeId, Long currentUserId, Pageable pageable) {
        List<RecipeComment> comments = recipeCommentRepository.findAllTopLevelComments(recipeId);

        List<Long> allCommentIds = comments.stream()
                .flatMap(c -> Stream.concat(
                        Stream.of(c.getId()),
                        c.getReplies().stream()
                                .filter(reply -> !reply.isDeleted())
                                .map(RecipeComment::getId)
                ))
                .toList();

        Map<Long, Integer> likeCounts = commentLikeRepository.countLikesByCommentIds(allCommentIds).stream()
                .collect(Collectors.toMap(
                        CommentLikeCountProjection::getCommentId,
                        CommentLikeCountProjection::getLikeCount
                ));

        Set<Long> likedCommentIds = commentLikeRepository.findLikedCommentIdsByUser(currentUserId, allCommentIds)
                .stream()
                .collect(Collectors.toSet());

        List<CommentDto> commentDtos = comments.stream().map(comment -> {
            List<CommentDto> replies = comment.getReplies().stream()
                    .filter(reply -> !reply.isDeleted())
                    .map(reply -> CommentMapper.toReplyDto(
                            reply,
                            likedCommentIds.contains(reply.getId()),
                            likeCounts.getOrDefault(reply.getId(), 0)
                    ))
                    .toList();

            boolean isDeleted = comment.isDeleted();
            boolean hasReplies = !replies.isEmpty();

            if (isDeleted && hasReplies) {
                return CommentMapper.toDeletedDto(comment).toBuilder()
                        .replies(replies)
                        .replyCount(replies.size())
                        .build();
            } else if (isDeleted) {
                return null;
            } else {
                return CommentMapper.toDto(
                                comment,
                                likedCommentIds.contains(comment.getId()),
                                likeCounts.getOrDefault(comment.getId(), 0)
                        ).toBuilder()
                        .replies(replies)
                        .replyCount(replies.size())
                        .build();
            }
        }).filter(Objects::nonNull).toList();

        // 🔥 정렬 처리
        List<CommentDto> sorted = new ArrayList<>(commentDtos);
        if (!pageable.getSort().isEmpty()) {
            Sort.Order order = pageable.getSort().iterator().next();
            if ("likeCount".equals(order.getProperty())) {
                sorted.sort(Comparator.comparing(CommentDto::getLikeCount,
                        order.isAscending() ? Comparator.naturalOrder() : Comparator.reverseOrder()));
            } else { // 기본 createdAt
                sorted.sort(Comparator.comparing(CommentDto::getCreatedAt,
                        order.isAscending() ? Comparator.naturalOrder() : Comparator.reverseOrder()));
            }
        }

        // 🔥 페이지네이션 적용
        int start = (int) pageable.getOffset();
        int end = Math.min((start + pageable.getPageSize()), sorted.size());

        List<CommentDto> pageContent = start > sorted.size() ? Collections.emptyList() : sorted.subList(start, end);

        return new PageImpl<>(pageContent, pageable, sorted.size());
    }



    @Transactional
    public void deleteComment(Long commentId, Long userId) {
        RecipeComment comment = recipeCommentRepository.findById(commentId)
                .orElseThrow(() -> new CustomException(ErrorCode.COMMENT_NOT_FOUND));

        if (!comment.getUser().getId().equals(userId)) {
            throw new CustomException(ErrorCode.COMMENT_ACCESS_DENIED);
        }


        boolean hasReplies = !comment.getReplies().isEmpty();

        if (hasReplies) {
            comment.setDeleted(true);
        } else {
            commentLikeRepository.deleteAllByCommentId(commentId);
            recipeCommentRepository.delete(comment);
        }
    }

    public Page<CommentDto> getRepliesWithLikes(Long parentId, Long currentUserId, Pageable pageable) {
        List<RecipeComment> replies = recipeCommentRepository.findByParentCommentIdOrderByCreatedAtAsc(parentId)
                .stream()
                .filter(reply -> !reply.isDeleted())
                .toList();

        List<Long> replyIds = replies.stream().map(RecipeComment::getId).toList();

        Map<Long, Integer> likeCounts = commentLikeRepository.countLikesByCommentIds(replyIds).stream()
                .collect(Collectors.toMap(
                        CommentLikeCountProjection::getCommentId,
                        CommentLikeCountProjection::getLikeCount
                ));

        Set<Long> likedIds = new HashSet<>(commentLikeRepository.findLikedCommentIdsByUser(currentUserId, replyIds));

        List<CommentDto> replyDtos = replies.stream().map(reply ->
                CommentMapper.toReplyDto(
                        reply,
                        likedIds.contains(reply.getId()),
                        likeCounts.getOrDefault(reply.getId(), 0)
                )
        ).toList();

        // 🔥 정렬 처리
        List<CommentDto> sorted = new ArrayList<>(replyDtos);
        if (!pageable.getSort().isEmpty()) {
            Sort.Order order = pageable.getSort().iterator().next();
            if ("likeCount".equals(order.getProperty())) {
                sorted.sort(Comparator.comparing(CommentDto::getLikeCount,
                        order.isAscending() ? Comparator.naturalOrder() : Comparator.reverseOrder()));
            } else { // 기본 createdAt
                sorted.sort(Comparator.comparing(CommentDto::getCreatedAt,
                        order.isAscending() ? Comparator.naturalOrder() : Comparator.reverseOrder()));
            }
        }

        // 🔥 페이지네이션 적용
        int start = (int) pageable.getOffset();
        int end = Math.min((start + pageable.getPageSize()), sorted.size());
        List<CommentDto> pageContent = start > sorted.size() ? Collections.emptyList() : sorted.subList(start, end);

        return new PageImpl<>(pageContent, pageable, sorted.size());
    }

}
