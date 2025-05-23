package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.comment.CommentDto;
import com.jdc.recipe_service.domain.dto.comment.CommentRequestDto;
import com.jdc.recipe_service.domain.dto.comment.ReplyDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeComment;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.projection.CommentLikeCountProjection;
import com.jdc.recipe_service.domain.repository.*;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.mapper.CommentMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.*;
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
                .findTop3ByRecipeIdAndParentCommentIsNull(recipeId, Pageable.ofSize(3));

        List<Long> commentIds = comments.stream()
                .map(RecipeComment::getId)
                .toList();

        Map<Long, Integer> likeCountMap = commentLikeRepository
                .countLikesByCommentIds(commentIds).stream()
                .collect(Collectors.toMap(
                        CommentLikeCountProjection::getCommentId,
                        CommentLikeCountProjection::getLikeCount
                ));

        Set<Long> likedIds = new HashSet<>(
                commentLikeRepository.findLikedCommentIdsByUser(currentUserId, commentIds)
        );

        return comments.stream()
                .map(c -> {
                    int likeCount = likeCountMap.getOrDefault(c.getId(), 0);
                    boolean isLiked = likedIds.contains(c.getId());
                    int replyCount = c.getReplies().size();

                    return CommentMapper.toDto(c, isLiked, likeCount)
                            .toBuilder()
                            .replyCount(replyCount)
                            .build();
                })
                .toList();
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

        return CommentMapper.toDto(comment, false, 0);
    }

    public ReplyDto createReply(Long recipeId, Long parentId, CommentRequestDto dto, Long userId) {
        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));

        RecipeComment parent = recipeCommentRepository
                .findByIdAndRecipeId(parentId, recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.COMMENT_NOT_FOUND));

        RecipeComment reply = RecipeComment.builder()
                .recipe(recipe)
                .user(user)
                .comment(dto.getContent())
                .parentComment(parent)
                .build();

        recipeCommentRepository.save(reply);
        return CommentMapper.toReplyDto(reply, false, 0);
    }

    public Page<CommentDto> getAllCommentsWithLikes(Long recipeId, Long currentUserId, Pageable pageable) {
        Pageable repoPg = PageRequest.of(pageable.getPageNumber(), pageable.getPageSize());
        List<RecipeComment> comments = recipeCommentRepository
                .findAllWithRepliesAndUsers(recipeId, repoPg);

        if (comments.isEmpty()) {
            return new PageImpl<>(List.of(), pageable, 0);
        }

        List<Long> allIds = comments.stream()
                .flatMap(c -> Stream.concat(
                        Stream.of(c.getId()),
                        c.getReplies().stream().map(RecipeComment::getId)
                ))
                .toList();

        Map<Long, Integer> likeCountMap = commentLikeRepository
                .countLikesByCommentIds(allIds).stream()
                .collect(Collectors.toMap(
                        CommentLikeCountProjection::getCommentId,
                        CommentLikeCountProjection::getLikeCount
                ));

        Set<Long> likedByUser = new HashSet<>(
                commentLikeRepository.findLikedCommentIdsByUser(currentUserId, allIds)
        );

        List<CommentDto> dtos = comments.stream()
                .map(comment -> {
                    List<ReplyDto> replies = comment.getReplies().stream()
                            .map(r -> CommentMapper.toReplyDto(
                                    r,
                                    likedByUser.contains(r.getId()),
                                    likeCountMap.getOrDefault(r.getId(), 0)
                            ))
                            .toList();

                    return CommentMapper.toDto(
                                    comment,
                                    likedByUser.contains(comment.getId()),
                                    likeCountMap.getOrDefault(comment.getId(), 0)
                            ).toBuilder()
                            .replyCount(replies.size())
                            .build();
                })
                .toList();

        List<CommentDto> sorted = new ArrayList<>(dtos);
        if (pageable.getSort().isSorted()) {
            Sort.Order order = pageable.getSort().iterator().next();
            Comparator<CommentDto> comp = "likeCount".equals(order.getProperty())
                    ? Comparator.comparing(CommentDto::getLikeCount)
                    : Comparator.comparing(CommentDto::getCreatedAt);
            if (!order.isAscending()) {
                comp = comp.reversed();
            }
            sorted.sort(comp);
        }

        int start = (int) pageable.getOffset();
        int end = Math.min(start + pageable.getPageSize(), sorted.size());
        List<CommentDto> pageContent = start >= sorted.size()
                ? List.of()
                : sorted.subList(start, end);

        return new PageImpl<>(pageContent, pageable, sorted.size());
    }

    public CommentDto findByIdAndRecipeId(Long commentId, Long recipeId, Long currentUserId) {
        RecipeComment comment = recipeCommentRepository
                .findByIdAndRecipeId(commentId, recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.COMMENT_NOT_FOUND));

        int likeCount = commentLikeRepository.countByCommentId(commentId);
        boolean isLiked = currentUserId != null
                && commentLikeRepository.existsByCommentIdAndUserId(commentId, currentUserId);

        int replyCount = comment.getReplies().size();

        return CommentMapper.toDto(comment, isLiked, likeCount)
                .toBuilder()
                .replyCount(replyCount)
                .build();
    }

    @Transactional
    public void deleteComment(Long commentId, Long userId) {
        RecipeComment comment = recipeCommentRepository.findById(commentId)
                .orElseThrow(() -> new CustomException(ErrorCode.COMMENT_NOT_FOUND));
        if (!comment.getUser().getId().equals(userId)) {
            throw new CustomException(ErrorCode.COMMENT_ACCESS_DENIED);
        }

        List<Long> toDeleteIds = Stream.concat(
                Stream.of(commentId),
                comment.getReplies().stream().map(RecipeComment::getId)
        ).toList();
        commentLikeRepository.deleteByCommentIdIn(toDeleteIds);

        recipeCommentRepository.delete(comment);
    }

    @Transactional
    public void deleteAllByRecipeId(Long recipeId) {
        List<RecipeComment> comments = recipeCommentRepository.findByRecipeId(recipeId);
        List<Long> commentIds = comments.stream()
                .map(RecipeComment::getId)
                .toList();

        if (!commentIds.isEmpty()) {
            commentLikeRepository.deleteByCommentIdIn(commentIds);
        }
        recipeCommentRepository.deleteByRecipeId(recipeId);
    }

    public Page<ReplyDto> getRepliesWithLikes(Long parentId, Long currentUserId, Pageable pageable) {
        Sort originalSort = pageable.getSort();
        Sort.Order likeOrder = originalSort.getOrderFor("likeCount");

        List<Sort.Order> dbOrders = originalSort.stream()
                .filter(o -> !"likeCount".equals(o.getProperty()))
                .toList();
        Pageable dbPageable = PageRequest.of(pageable.getPageNumber(), pageable.getPageSize(),
                dbOrders.isEmpty() ? Sort.unsorted() : Sort.by(dbOrders));

        Page<RecipeComment> replyPage =
                recipeCommentRepository.findByParentCommentId(parentId, dbPageable);

        List<Long> replyIds = replyPage.getContent().stream()
                .map(RecipeComment::getId)
                .toList();

        Map<Long, Integer> likeCounts = commentLikeRepository
                .countLikesByCommentIds(replyIds).stream()
                .collect(Collectors.toMap(
                        CommentLikeCountProjection::getCommentId,
                        CommentLikeCountProjection::getLikeCount
                ));

        Set<Long> likedIds = currentUserId != null
                ? new HashSet<>(commentLikeRepository.findLikedCommentIdsByUser(currentUserId, replyIds))
                : Collections.emptySet();

        List<ReplyDto> dtos = replyPage.getContent().stream()
                .map(r -> CommentMapper.toReplyDto(
                        r,
                        likedIds.contains(r.getId()),
                        likeCounts.getOrDefault(r.getId(), 0)
                ))
                .toList();

        if (likeOrder != null) {
            Comparator<ReplyDto> comp = Comparator.comparing(ReplyDto::getLikeCount);
            if (!likeOrder.isAscending()) {
                comp = comp.reversed();
            }
            dtos.sort(comp);
        }

        return new PageImpl<>(dtos, pageable, replyPage.getTotalElements());
    }
}
