package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.comment.CommentDto;
import com.jdc.recipe_service.domain.dto.comment.CommentRequestDto;
import com.jdc.recipe_service.domain.dto.comment.ReplyDto;
import com.jdc.recipe_service.domain.dto.notification.NotificationCreateDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeComment;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.projection.CommentLikeCountProjection;
import com.jdc.recipe_service.domain.repository.CommentLikeRepository;
import com.jdc.recipe_service.domain.repository.RecipeCommentRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.domain.type.NotificationRelatedType;
import com.jdc.recipe_service.domain.type.NotificationType;
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
    private final NotificationService notificationService;


    public List<CommentDto> getTop3CommentsWithLikes(Long recipeId, Long currentUserId) {
        List<RecipeComment> comments = recipeCommentRepository
                .findTop3ByRecipeIdAndParentCommentIsNull(recipeId, Pageable.ofSize(3));

        List<Long> commentIds = comments.stream()
                .map(RecipeComment::getId)
                .collect(Collectors.toList());

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
                    int replyCount = Optional.ofNullable(c.getReplies()).orElse(List.of()).size();

                    return CommentMapper.toDto(c, isLiked, likeCount)
                            .toBuilder()
                            .replyCount(replyCount)
                            .build();
                })
                .collect(Collectors.toList());
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

        Long targetUserId = recipe.getUser().getId();
        if (!targetUserId.equals(user.getId())) {
            notificationService.createNotification(
                    NotificationCreateDto.builder()
                            .userId(targetUserId)
                            .actorId(user.getId())
                            .type(NotificationType.NEW_COMMENT)
                            .content(user.getNickname() + "님이 댓글을 남겼습니다.")
                            .relatedType(NotificationRelatedType.RECIPE)
                            .relatedId(recipeId)
                            .relatedUrl("/recipes/" + recipeId + "/comments")
                            .build()
            );
        }

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

        Long targetUserId = parent.getUser().getId();
        if (!targetUserId.equals(user.getId())) {
            notificationService.createNotification(
                    NotificationCreateDto.builder()
                            .userId(targetUserId)
                            .actorId(user.getId())
                            .type(NotificationType.NEW_REPLY)
                            .content(user.getNickname() + "님이 대댓글을 남겼습니다.")
                            .relatedType(NotificationRelatedType.COMMENT)
                            .relatedId(parentId)
                            .relatedUrl("/recipes/" + recipeId + "/comments/" +parentId)                            .build()
            );
        }

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
                .flatMap(c -> {
                    Long parentId = c.getId();
                    Stream<Long> replyIds = Optional.ofNullable(c.getReplies())
                            .orElse(List.of())
                            .stream().map(RecipeComment::getId);
                    return Stream.concat(Stream.of(parentId), replyIds);
                })
                .collect(Collectors.toList());

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
                    int parentLikeCount = likeCountMap.getOrDefault(comment.getId(), 0);
                    boolean parentLiked = likedByUser.contains(comment.getId());
                    int replyCount = Optional.ofNullable(comment.getReplies()).orElse(List.of()).size();

                    List<ReplyDto> replies = Optional.ofNullable(comment.getReplies())
                            .orElse(List.of())
                            .stream()
                            .map(r -> {
                                int rc = likeCountMap.getOrDefault(r.getId(), 0);
                                boolean rl = likedByUser.contains(r.getId());
                                return CommentMapper.toReplyDto(r, rl, rc);
                            })
                            .collect(Collectors.toList());

                    return CommentMapper.toDto(comment, parentLiked, parentLikeCount)
                            .toBuilder()
                            .replyCount(replyCount)
                            .build();
                })
                .collect(Collectors.toList());

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
        List<CommentDto> pageContent = start >= sorted.size() ? List.of() : sorted.subList(start, end);

        return new PageImpl<>(pageContent, pageable, sorted.size());
    }

    public CommentDto findByIdAndRecipeId(Long commentId, Long recipeId, Long currentUserId) {
        RecipeComment comment = recipeCommentRepository
                .findByIdAndRecipeId(commentId, recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.COMMENT_NOT_FOUND));

        int likeCount = commentLikeRepository.countByCommentId(commentId);
        boolean isLiked = currentUserId != null
                && commentLikeRepository.existsByCommentIdAndUserId(commentId, currentUserId);

        int replyCount = Optional.ofNullable(comment.getReplies()).orElse(List.of()).size();

        return CommentMapper.toDto(comment, isLiked, likeCount)
                .toBuilder()
                .replyCount(replyCount)
                .build();
    }

    public void deleteComment(Long commentId, Long userId) {
        RecipeComment comment = recipeCommentRepository.findById(commentId)
                .orElseThrow(() -> new CustomException(ErrorCode.COMMENT_NOT_FOUND));
        if (!comment.getUser().getId().equals(userId)) {
            throw new CustomException(ErrorCode.COMMENT_ACCESS_DENIED);
        }

        List<Long> toDeleteIds = Stream.concat(
                Stream.of(commentId),
                Optional.ofNullable(comment.getReplies()).orElse(List.of()).stream().map(RecipeComment::getId)
        ).collect(Collectors.toList());
        commentLikeRepository.deleteByCommentIdIn(toDeleteIds);

        recipeCommentRepository.delete(comment);
    }

    public void deleteAllByRecipeId(Long recipeId) {
        List<RecipeComment> comments = recipeCommentRepository.findByRecipeId(recipeId);
        List<Long> commentIds = comments.stream()
                .map(RecipeComment::getId)
                .collect(Collectors.toList());

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

        if (replyPage.isEmpty()) {
            return new PageImpl<>(List.of(), pageable, 0);
        }

        List<Long> replyIds = replyPage.getContent().stream()
                .map(RecipeComment::getId)
                .collect(Collectors.toList());

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
                .map(r -> {
                    int lc = likeCounts.getOrDefault(r.getId(), 0);
                    boolean il = likedIds.contains(r.getId());
                    return CommentMapper.toReplyDto(r, il, lc);
                })
                .collect(Collectors.toCollection(ArrayList::new));

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

