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

        RecipeComment parent = recipeCommentRepository.findByIdAndRecipeId(parentId, recipeId)
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
        // 1) 저장소 조회: page/size 만 복사하고 정렬 정보는 제거
        Pageable repoPg = PageRequest.of(
                pageable.getPageNumber(),
                pageable.getPageSize()
        );

        List<RecipeComment> comments = recipeCommentRepository
                .findAllWithRepliesAndUsers(recipeId, repoPg);

        if (comments.isEmpty()) {
            return new PageImpl<>(List.of(), pageable, 0);
        }

        // 2) 댓글 + 대댓글 전체 ID 수집
        List<Long> allIds = comments.stream()
                .flatMap(c -> Stream.concat(
                        Stream.of(c.getId()),
                        c.getReplies().stream()
                                .filter(r -> !r.isDeleted())
                                .map(RecipeComment::getId)
                ))
                .toList();

        // 3) 좋아요 카운트 조회
        Map<Long, Integer> likeCountMap = commentLikeRepository
                .countLikesByCommentIds(allIds).stream()
                .collect(Collectors.toMap(
                        CommentLikeCountProjection::getCommentId,
                        CommentLikeCountProjection::getLikeCount
                ));

        // 4) 현재 사용자가 좋아요 누른 댓글 ID 조회
        Set<Long> likedByUser = new HashSet<>(
                commentLikeRepository.findLikedCommentIdsByUser(currentUserId, allIds)
        );

        // 5) DTO 변환 (reply 포함)
        List<CommentDto> dtos = comments.stream()
                .map(comment -> {
                    // 대댓글 DTO
                    List<ReplyDto> replies = comment.getReplies().stream()
                            .filter(r -> !r.isDeleted())
                            .map(r -> CommentMapper.toReplyDto(
                                    r,
                                    likedByUser.contains(r.getId()),
                                    likeCountMap.getOrDefault(r.getId(), 0)
                            ))
                            .toList();

                    boolean isDeleted = comment.isDeleted();
                    boolean hasReplies = !replies.isEmpty();

                    if (isDeleted && hasReplies) {
                        return CommentMapper.toDeletedDto(comment)
                                .toBuilder()
                                .replyCount(replies.size())
                                .build();
                    } else if (isDeleted) {
                        return null; // 완전 삭제
                    } else {
                        return CommentMapper.toDto(
                                        comment,
                                        likedByUser.contains(comment.getId()),
                                        likeCountMap.getOrDefault(comment.getId(), 0)
                                ).toBuilder()
                                .replyCount(replies.size())
                                .build();
                    }
                })
                .filter(Objects::nonNull)
                .toList();

        // 6) 애플리케이션 레벨 정렬
        List<CommentDto> sorted = new ArrayList<>(dtos);  // mutable copy
        if (pageable.getSort().isSorted()) {
            Sort.Order order = pageable.getSort().iterator().next();
            Comparator<CommentDto> comp = "likeCount".equals(order.getProperty())
                    ? Comparator.comparing(CommentDto::getLikeCount)
                    : Comparator.comparing(CommentDto::getCreatedAt);
            if (!order.isAscending()) {
                comp = comp.reversed();
            }
            sorted.sort(comp);  // 이제 예외 없이 정렬됩니다
        }

        // 7) 페이징에도 `sorted` 를 사용
        int start = (int) pageable.getOffset();
        int end = Math.min(start + pageable.getPageSize(), sorted.size());
        List<CommentDto> pageContent = start >= sorted.size()
                ? List.of()
                : sorted.subList(start, end);

        return new PageImpl<>(pageContent, pageable, sorted.size());
    }

    public CommentDto findByIdAndRecipeId(Long commentId, Long recipeId, Long currentUserId) {
        RecipeComment comment = recipeCommentRepository.findByIdAndRecipeId(commentId, recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.COMMENT_NOT_FOUND));

        // 좋아요 카운트 & 여부
        int likeCount = commentLikeRepository.countByCommentId(commentId);
        boolean isLiked = currentUserId != null
                && commentLikeRepository.existsByCommentIdAndUserId(commentId, currentUserId);

        // 보여줄 대댓글 개수
        int replyCount = (int) comment.getReplies().stream()
                .filter(r -> !r.isDeleted())
                .count();

        // 매핑 & replyCount 주입
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


        boolean hasReplies = !comment.getReplies().isEmpty();

        if (hasReplies) {
            comment.setDeleted(true);
        } else {
            commentLikeRepository.deleteAllByCommentId(commentId);
            recipeCommentRepository.delete(comment);
        }
    }

    @Transactional
    public void deleteAllByRecipeId(Long recipeId) {
        // 1. 댓글 ID 목록 조회
        List<RecipeComment> comments = recipeCommentRepository.findByRecipeId(recipeId);
        List<Long> commentIds = comments.stream()
                .map(RecipeComment::getId)
                .toList();

        // 2. 댓글 좋아요 삭제
        if (!commentIds.isEmpty()) {
            commentLikeRepository.deleteByCommentIdIn(commentIds);
        }

        // 3. 댓글 삭제
        recipeCommentRepository.deleteByRecipeId(recipeId);
    }


    public Page<ReplyDto> getRepliesWithLikes(
            Long parentId,
            Long currentUserId,
            Pageable pageable
    ) {
        // 1) 원본 sort 에서 likeCount 정렬 정보 분리
        Sort originalSort = pageable.getSort();
        Sort.Order likeOrder = originalSort.getOrderFor("likeCount");

        // 2) DB 쿼리를 위한 sort: likeCount 제외한 나머지 (e.g. createdAt)
        List<Sort.Order> dbOrders = originalSort.stream()
                .filter(o -> !"likeCount".equals(o.getProperty()))
                .toList();
        Sort dbSort = dbOrders.isEmpty() ? Sort.unsorted() : Sort.by(dbOrders);
        Pageable dbPageable = PageRequest.of(
                pageable.getPageNumber(),
                pageable.getPageSize(),
                dbSort
        );

        // 3) DB에서 createdAt 기준 페이징 조회
        Page<RecipeComment> replyPage =
                recipeCommentRepository.findByParentCommentId(parentId, dbPageable);

        // 4) 좋아요 통계를 위한 댓글 ID 리스트
        List<Long> replyIds = replyPage.getContent().stream()
                .map(RecipeComment::getId)
                .toList();

        Map<Long, Integer> likeCounts = commentLikeRepository
                .countLikesByCommentIds(replyIds).stream()
                .collect(Collectors.toMap(
                        CommentLikeCountProjection::getCommentId,
                        CommentLikeCountProjection::getLikeCount
                ));

        Set<Long> likedIds = (currentUserId != null)
                ? new HashSet<>(commentLikeRepository.findLikedCommentIdsByUser(currentUserId, replyIds))
                : Collections.emptySet();

        // 5) 엔티티 → DTO 변환 (Stream.toList() 는 불변이므로, 가변 리스트로 복사)
        List<ReplyDto> immutableDtos = replyPage.getContent().stream()
                .map(r -> CommentMapper.toReplyDto(
                        r,
                        likedIds.contains(r.getId()),
                        likeCounts.getOrDefault(r.getId(), 0)
                ))
                .toList();
        List<ReplyDto> dtos = new ArrayList<>(immutableDtos);

        // 6) in-memory로 likeCount 정렬 적용 (요청에 likeCount 정렬이 있으면)
        if (likeOrder != null) {
            Comparator<ReplyDto> comp = Comparator.comparing(ReplyDto::getLikeCount);
            if (!likeOrder.isAscending()) {
                comp = comp.reversed();
            }
            dtos.sort(comp);
        }

        // 7) PageImpl 로 감싸서 원래 pageable 과 totalElements 유지
        return new PageImpl<>(
                dtos,
                pageable,
                replyPage.getTotalElements()
        );
    }



}

