package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.comment.CommentDto;
import com.jdc.recipe_service.domain.dto.comment.CommentRequestDto;
import com.jdc.recipe_service.domain.dto.comment.ReplyDto;
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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.*;

import org.springframework.test.util.ReflectionTestUtils;

import java.time.LocalDateTime;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class CommentServiceTest {

    @Mock private RecipeCommentRepository recipeCommentRepository;
    @Mock private CommentLikeRepository commentLikeRepository;
    @Mock private RecipeRepository recipeRepository;
    @Mock private UserRepository userRepository;

    @InjectMocks
    private CommentService commentService;

    private User user;
    private Recipe recipe;
    private RecipeComment comment1;
    private RecipeComment reply1;

    @BeforeEach
    void setUp() {
        user = User.builder().id(10L).nickname("tester").build();
        recipe = Recipe.builder().id(20L).build();

        // 최상위 댓글 세팅
        comment1 = RecipeComment.builder()
                .user(user)
                .recipe(recipe)
                .comment("첫 번째 댓글")
                .build();
        ReflectionTestUtils.setField(comment1, "id", 100L);
        ReflectionTestUtils.setField(comment1, "replies", new ArrayList<RecipeComment>());
        ReflectionTestUtils.setField(comment1, "createdAt", LocalDateTime.of(2025,1,1,10,0));

        // 대댓글 세팅
        reply1 = RecipeComment.builder()
                .user(user)
                .recipe(recipe)
                .comment("첫 번째 댓글의 답글")
                .parentComment(comment1)
                .build();
        ReflectionTestUtils.setField(reply1, "id", 101L);
        ReflectionTestUtils.setField(reply1, "createdAt", LocalDateTime.of(2025,1,1,11,0));

        // 부모 댓글의 replies 리스트에 대댓글 추가
        ((List<RecipeComment>) ReflectionTestUtils.getField(comment1, "replies")).add(reply1);
    }

    @Test
    @DisplayName("getTop3CommentsWithLikes: 정상 조회 시 매핑된 DTO 리스트 반환")
    void getTop3CommentsWithLikes_success() {
        // 1) 레포지토리에서 상위 3개 댓글 반환
        List<RecipeComment> topComments = List.of(comment1);
        when(recipeCommentRepository.findTop3ByRecipeIdAndParentCommentIsNull(eq(20L), any(Pageable.class)))
                .thenReturn(topComments);

        // 2) 좋아요 집계 프로젝션 결과 세팅
        CommentLikeCountProjection proj = new CommentLikeCountProjection() {
            public Long getCommentId() { return 100L; }
            public int getLikeCount() { return 5; }
        };
        when(commentLikeRepository.countLikesByCommentIds(List.of(100L)))
                .thenReturn(List.of(proj));

        // 3) 현재 사용자가 좋아요 누른 댓글 목록 (empty)
        when(commentLikeRepository.findLikedCommentIdsByUser(10L, List.of(100L)))
                .thenReturn(Collections.emptyList());

        CommentDto mappedDto = CommentDto.builder()
                .id(100L)
                .content("첫 번째 댓글")
                .createdAt(LocalDateTime.of(2025,1,1,10,0))
                .likeCount(5)
                .likedByCurrentUser(false)
                .replyCount(1)
                .build();

        try (MockedStatic<CommentMapper> mm = Mockito.mockStatic(CommentMapper.class)) {
            mm.when(() -> CommentMapper.toDto(eq(comment1), eq(false), eq(5)))
                    .thenReturn(mappedDto);

            List<CommentDto> result = commentService.getTop3CommentsWithLikes(20L, 10L);

            assertEquals(1, result.size());
            CommentDto dto = result.get(0);
            assertEquals(100L, dto.getId());
            assertEquals(5, dto.getLikeCount());
            assertFalse(dto.isLikedByCurrentUser());
            assertEquals(1, dto.getReplyCount());
        }

        verify(recipeCommentRepository, times(1))
                .findTop3ByRecipeIdAndParentCommentIsNull(eq(20L), any(Pageable.class));
        // 댓글이 비어 있지 않으므로, 빈 리스트가 아님 → 두 번 호출됨
        verify(commentLikeRepository, times(1))
                .countLikesByCommentIds(List.of(100L));
        verify(commentLikeRepository, times(1))
                .findLikedCommentIdsByUser(10L, List.of(100L));
    }

    @Test
    @DisplayName("getTop3CommentsWithLikes: 댓글이 없을 때 빈 리스트 반환하며, countLikesByCommentIds, findLikedCommentIdsByUser 는 빈 리스트로 호출됨")
    void getTop3CommentsWithLikes_empty() {
        when(recipeCommentRepository.findTop3ByRecipeIdAndParentCommentIsNull(20L, Pageable.ofSize(3)))
                .thenReturn(Collections.emptyList());

        List<CommentDto> result = commentService.getTop3CommentsWithLikes(20L, 10L);
        assertTrue(result.isEmpty());

        // 댓글이 없더라도 countLikesByCommentIds(emptyList) 호출됨
        verify(commentLikeRepository, times(1))
                .countLikesByCommentIds(Collections.emptyList());
        verify(commentLikeRepository, times(1))
                .findLikedCommentIdsByUser(10L, Collections.emptyList());
    }

    @Test
    @DisplayName("createComment: 레시피 없으면 RECIPE_NOT_FOUND 예외")
    void createComment_recipeNotFound() {
        when(recipeRepository.findById(20L)).thenReturn(Optional.empty());

        CommentRequestDto dto = new CommentRequestDto("내용");
        CustomException ex = assertThrows(CustomException.class, () ->
                commentService.createComment(20L, dto, user)
        );
        assertEquals(ErrorCode.RECIPE_NOT_FOUND, ex.getErrorCode());

        verify(recipeRepository, times(1)).findById(20L);
        verifyNoInteractions(recipeCommentRepository);
    }

    @Test
    @DisplayName("createComment: 정상 호출 시 매핑된 CommentDto 반환")
    void createComment_success() {
        when(recipeRepository.findById(20L)).thenReturn(Optional.of(recipe));
        when(recipeCommentRepository.save(any(RecipeComment.class)))
                .thenAnswer(invocation -> {
                    RecipeComment saved = invocation.getArgument(0);
                    ReflectionTestUtils.setField(saved, "id", 200L);
                    return saved;
                });

        CommentDto mappedDto = CommentDto.builder()
                .id(200L)
                .content("내용")
                .likeCount(0)
                .likedByCurrentUser(false)
                .replyCount(0)
                .createdAt(LocalDateTime.now())
                .build();

        try (MockedStatic<CommentMapper> mm = Mockito.mockStatic(CommentMapper.class)) {
            mm.when(() -> CommentMapper.toDto(any(RecipeComment.class), eq(false), eq(0)))
                    .thenReturn(mappedDto);

            CommentRequestDto dto = new CommentRequestDto("내용");
            CommentDto result = commentService.createComment(20L, dto, user);

            assertEquals(200L, result.getId());
            assertEquals("내용", result.getContent());
        }

        verify(recipeRepository, times(1)).findById(20L);
        verify(recipeCommentRepository, times(1)).save(any(RecipeComment.class));
    }

    @Test
    @DisplayName("createReply: 레시피 없으면 RECIPE_NOT_FOUND 예외")
    void createReply_recipeNotFound() {
        when(recipeRepository.findById(20L)).thenReturn(Optional.empty());

        CustomException ex = assertThrows(CustomException.class, () ->
                commentService.createReply(20L, 100L, new CommentRequestDto("답글"), 10L)
        );
        assertEquals(ErrorCode.RECIPE_NOT_FOUND, ex.getErrorCode());

        verify(recipeRepository, times(1)).findById(20L);
        verifyNoInteractions(userRepository, recipeCommentRepository);
    }

    @Test
    @DisplayName("createReply: 사용자 없으면 USER_NOT_FOUND 예외")
    void createReply_userNotFound() {
        when(recipeRepository.findById(20L)).thenReturn(Optional.of(recipe));
        when(userRepository.findById(10L)).thenReturn(Optional.empty());

        CustomException ex = assertThrows(CustomException.class, () ->
                commentService.createReply(20L, 100L, new CommentRequestDto("답글"), 10L)
        );
        assertEquals(ErrorCode.USER_NOT_FOUND, ex.getErrorCode());

        verify(recipeRepository, times(1)).findById(20L);
        verify(userRepository, times(1)).findById(10L);
        verifyNoMoreInteractions(recipeCommentRepository);
    }

    @Test
    @DisplayName("createReply: 부모 댓글 없으면 COMMENT_NOT_FOUND 예외")
    void createReply_parentNotFound() {
        when(recipeRepository.findById(20L)).thenReturn(Optional.of(recipe));
        when(userRepository.findById(10L)).thenReturn(Optional.of(user));
        when(recipeCommentRepository.findByIdAndRecipeId(999L, 20L)).thenReturn(Optional.empty());

        CustomException ex = assertThrows(CustomException.class, () ->
                commentService.createReply(20L, 999L, new CommentRequestDto("답글"), 10L)
        );
        assertEquals(ErrorCode.COMMENT_NOT_FOUND, ex.getErrorCode());

        verify(recipeRepository, times(1)).findById(20L);
        verify(userRepository, times(1)).findById(10L);
        verify(recipeCommentRepository, times(1))
                .findByIdAndRecipeId(999L, 20L);
    }

    @Test
    @DisplayName("createReply: 정상 호출 시 매핑된 ReplyDto 반환")
    void createReply_success() {
        when(recipeRepository.findById(20L)).thenReturn(Optional.of(recipe));
        when(userRepository.findById(10L)).thenReturn(Optional.of(user));
        when(recipeCommentRepository.findByIdAndRecipeId(100L, 20L))
                .thenReturn(Optional.of(comment1));

        when(recipeCommentRepository.save(any(RecipeComment.class)))
                .thenAnswer(invocation -> {
                    RecipeComment saved = invocation.getArgument(0);
                    ReflectionTestUtils.setField(saved, "id", 300L);
                    return saved;
                });

        ReplyDto mappedReply = ReplyDto.builder()
                .id(300L)
                .content("답글")
                .likeCount(0)
                .likedByCurrentUser(false)
                .createdAt(LocalDateTime.now())
                .build();

        try (MockedStatic<CommentMapper> mm = Mockito.mockStatic(CommentMapper.class)) {
            mm.when(() -> CommentMapper.toReplyDto(any(RecipeComment.class), eq(false), eq(0)))
                    .thenReturn(mappedReply);

            ReplyDto result = commentService.createReply(20L, 100L, new CommentRequestDto("답글"), 10L);
            assertEquals(300L, result.getId());
            assertEquals("답글", result.getContent());
        }

        verify(recipeCommentRepository, times(1)).save(any(RecipeComment.class));
    }

    @Test
    @DisplayName("getAllCommentsWithLikes: 페이지 정보에 따라 정렬된 CommentDto Page 반환")
    void getAllCommentsWithLikes_success() {
        // 1) 최상위 댓글 1개에 대댓글 1개 세팅
        List<RecipeComment> comments = List.of(comment1);
        Pageable pageable = PageRequest.of(0, 10, Sort.unsorted());
        when(recipeCommentRepository.findAllWithRepliesAndUsers(20L, pageable))
                .thenReturn(comments);

        // 좋아요 집계
        CommentLikeCountProjection p1 = new CommentLikeCountProjection() {
            public Long getCommentId() { return 100L; }
            public int getLikeCount() { return 2; }
        };
        CommentLikeCountProjection p2 = new CommentLikeCountProjection() {
            public Long getCommentId() { return 101L; }
            public int getLikeCount() { return 3; }
        };
        when(commentLikeRepository.countLikesByCommentIds(List.of(100L, 101L)))
                .thenReturn(List.of(p1, p2));
        when(commentLikeRepository.findLikedCommentIdsByUser(10L, List.of(100L, 101L)))
                .thenReturn(List.of(101L));

        CommentDto topMapped = CommentDto.builder()
                .id(100L).content("첫 번째 댓글")
                .likeCount(2).likedByCurrentUser(false).replyCount(1)
                .createdAt(LocalDateTime.of(2025,1,1,10,0))
                .build();
        ReplyDto replyMapped = ReplyDto.builder()
                .id(101L).content("첫 번째 댓글의 답글")
                .likeCount(3).likedByCurrentUser(true)
                .createdAt(LocalDateTime.of(2025,1,1,11,0))
                .build();

        try (MockedStatic<CommentMapper> mm = Mockito.mockStatic(CommentMapper.class)) {
            mm.when(() -> CommentMapper.toDto(eq(comment1), eq(false), eq(2)))
                    .thenReturn(topMapped);
            mm.when(() -> CommentMapper.toReplyDto(eq(reply1), eq(true), eq(3)))
                    .thenReturn(replyMapped);

            Page<CommentDto> page = commentService.getAllCommentsWithLikes(20L, 10L, pageable);
            assertEquals(1, page.getTotalElements());
            CommentDto dto = page.getContent().get(0);
            assertEquals(100L, dto.getId());
            assertEquals(2, dto.getLikeCount());
            assertFalse(dto.isLikedByCurrentUser());
            assertEquals(1, dto.getReplyCount());
        }

        verify(recipeCommentRepository, times(1))
                .findAllWithRepliesAndUsers(20L, pageable);
        verify(commentLikeRepository, times(1))
                .countLikesByCommentIds(List.of(100L, 101L));
        verify(commentLikeRepository, times(1))
                .findLikedCommentIdsByUser(10L, List.of(100L, 101L));
    }

    @Test
    @DisplayName("getAllCommentsWithLikes: 댓글 없으면 빈 Page 반환")
    void getAllCommentsWithLikes_empty() {
        Pageable pageable = PageRequest.of(0, 5);
        when(recipeCommentRepository.findAllWithRepliesAndUsers(20L, pageable))
                .thenReturn(Collections.emptyList());

        Page<CommentDto> page = commentService.getAllCommentsWithLikes(20L, 10L, pageable);
        assertEquals(0, page.getTotalElements());
        assertTrue(page.getContent().isEmpty());

        verify(recipeCommentRepository, times(1))
                .findAllWithRepliesAndUsers(20L, pageable);
        verifyNoMoreInteractions(commentLikeRepository);
    }

    @Test
    @DisplayName("findByIdAndRecipeId: 없는 댓글이면 COMMENT_NOT_FOUND 예외")
    void findByIdAndRecipeId_notFound() {
        when(recipeCommentRepository.findByIdAndRecipeId(999L, 20L))
                .thenReturn(Optional.empty());

        CustomException ex = assertThrows(CustomException.class, () ->
                commentService.findByIdAndRecipeId(999L, 20L, 10L)
        );
        assertEquals(ErrorCode.COMMENT_NOT_FOUND, ex.getErrorCode());

        verify(recipeCommentRepository, times(1))
                .findByIdAndRecipeId(999L, 20L);
    }

    @Test
    @DisplayName("findByIdAndRecipeId: 정상 호출 시 매핑된 CommentDto 반환")
    void findByIdAndRecipeId_success() {
        when(recipeCommentRepository.findByIdAndRecipeId(100L, 20L))
                .thenReturn(Optional.of(comment1));

        when(commentLikeRepository.countByCommentId(100L)).thenReturn(4);
        when(commentLikeRepository.existsByCommentIdAndUserId(100L, 10L)).thenReturn(true);

        CommentDto mapped = CommentDto.builder()
                .id(100L).content("첫 번째 댓글")
                .likeCount(4).likedByCurrentUser(true)
                .replyCount(1)
                .createdAt(LocalDateTime.of(2025,1,1,10,0))
                .build();

        try (MockedStatic<CommentMapper> mm = Mockito.mockStatic(CommentMapper.class)) {
            mm.when(() -> CommentMapper.toDto(comment1, true, 4)).thenReturn(mapped);

            CommentDto result = commentService.findByIdAndRecipeId(100L, 20L, 10L);
            assertEquals(100L, result.getId());
            assertEquals(4, result.getLikeCount());
            assertTrue(result.isLikedByCurrentUser());
            assertEquals(1, result.getReplyCount());
        }

        verify(recipeCommentRepository, times(1))
                .findByIdAndRecipeId(100L, 20L);
        verify(commentLikeRepository, times(1))
                .countByCommentId(100L);
        verify(commentLikeRepository, times(1))
                .existsByCommentIdAndUserId(100L, 10L);
    }

    @Test
    @DisplayName("deleteComment: 없는 댓글이면 COMMENT_NOT_FOUND 예외")
    void deleteComment_notFound() {
        when(recipeCommentRepository.findById(500L)).thenReturn(Optional.empty());

        CustomException ex = assertThrows(CustomException.class, () ->
                commentService.deleteComment(500L, 10L)
        );
        assertEquals(ErrorCode.COMMENT_NOT_FOUND, ex.getErrorCode());

        verify(recipeCommentRepository, times(1)).findById(500L);
        verifyNoMoreInteractions(commentLikeRepository, recipeCommentRepository);
    }

    @Test
    @DisplayName("deleteComment: 작성자 != 요청자 이면 COMMENT_ACCESS_DENIED 예외")
    void deleteComment_notOwner() {
        User other = User.builder().id(99L).build();
        RecipeComment otherComment = RecipeComment.builder()
                .user(other)
                .build();
        ReflectionTestUtils.setField(otherComment, "id", 600L);

        when(recipeCommentRepository.findById(600L)).thenReturn(Optional.of(otherComment));

        CustomException ex = assertThrows(CustomException.class, () ->
                commentService.deleteComment(600L, 10L)
        );
        assertEquals(ErrorCode.COMMENT_ACCESS_DENIED, ex.getErrorCode());

        verify(recipeCommentRepository, times(1)).findById(600L);
        verifyNoMoreInteractions(commentLikeRepository, recipeCommentRepository);
    }

    @Test
    @DisplayName("deleteComment: 정상 호출 시 댓글과 좋아요 레포 삭제 호출")
    void deleteComment_success() {
        when(recipeCommentRepository.findById(100L)).thenReturn(Optional.of(comment1));

        commentService.deleteComment(100L, 10L);

        // 100L(부모)과 101L(대댓글) → 두 개 모두 삭제
        verify(commentLikeRepository, times(1)).deleteByCommentIdIn(List.of(100L, 101L));
        verify(recipeCommentRepository, times(1)).delete(comment1);
    }

    @Test
    @DisplayName("deleteAllByRecipeId: 댓글이 없으면 댓글이 없더라도 deleteByRecipeId 호출")
    void deleteAllByRecipeId_empty() {
        when(recipeCommentRepository.findByRecipeId(20L)).thenReturn(Collections.emptyList());

        commentService.deleteAllByRecipeId(20L);

        verify(recipeCommentRepository, times(1)).findByRecipeId(20L);
        // 댓글 목록이 빈 리스트여도, deleteByRecipeId(recipeId) 는 호출돼야 함
        verify(recipeCommentRepository, times(1)).deleteByRecipeId(20L);
        verifyNoMoreInteractions(commentLikeRepository);
    }

    @Test
    @DisplayName("deleteAllByRecipeId: 댓글이 있으면 좋아요부터 삭제 후 댓글 일괄 삭제")
    void deleteAllByRecipeId_success() {
        List<RecipeComment> all = List.of(comment1, reply1);
        when(recipeCommentRepository.findByRecipeId(20L)).thenReturn(all);

        commentService.deleteAllByRecipeId(20L);

        verify(commentLikeRepository, times(1)).deleteByCommentIdIn(List.of(100L, 101L));
        verify(recipeCommentRepository, times(1)).deleteByRecipeId(20L);
    }

    @Test
    @DisplayName("getRepliesWithLikes: 정상 호출 시 정렬된 ReplyDto Page 반환")
    void getRepliesWithLikes_success() {
        Page<RecipeComment> pageEnt = new PageImpl<>(List.of(reply1), PageRequest.of(0, 5), 1);
        // 좋아요 기준 내림차순 정렬
        Pageable pageable = PageRequest.of(0, 5, Sort.by("likeCount").descending());

        // DB 조회 시에는 likeCount 정렬 제외
        when(recipeCommentRepository.findByParentCommentId(100L, PageRequest.of(0,5, Sort.unsorted())))
                .thenReturn(pageEnt);

        CommentLikeCountProjection p = new CommentLikeCountProjection() {
            public Long getCommentId() { return 101L; }
            public int getLikeCount() { return 4; }
        };
        when(commentLikeRepository.countLikesByCommentIds(List.of(101L)))
                .thenReturn(List.of(p));
        when(commentLikeRepository.findLikedCommentIdsByUser(10L, List.of(101L)))
                .thenReturn(List.of(101L));

        ReplyDto mapped = ReplyDto.builder()
                .id(101L)
                .content("첫 번째 댓글의 답글")
                .likeCount(4)
                .likedByCurrentUser(true)
                .createdAt(LocalDateTime.of(2025,1,1,11,0))
                .build();

        try (MockedStatic<CommentMapper> mm = Mockito.mockStatic(CommentMapper.class)) {
            mm.when(() -> CommentMapper.toReplyDto(reply1, true, 4))
                    .thenReturn(mapped);

            Page<ReplyDto> page = commentService.getRepliesWithLikes(100L, 10L, pageable);
            assertEquals(1, page.getTotalElements());
            ReplyDto dto = page.getContent().get(0);
            assertEquals(101L, dto.getId());
            assertEquals(4, dto.getLikeCount());
            assertTrue(dto.isLikedByCurrentUser());
        }

        verify(recipeCommentRepository, times(1))
                .findByParentCommentId(100L, PageRequest.of(0,5, Sort.unsorted()));
        verify(commentLikeRepository, times(1))
                .countLikesByCommentIds(List.of(101L));
        verify(commentLikeRepository, times(1))
                .findLikedCommentIdsByUser(10L, List.of(101L));
    }
}
