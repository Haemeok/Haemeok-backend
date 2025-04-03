package com.jdc.recipe_service.domain.dto.comment;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.jdc.recipe_service.domain.dto.user.CommentUserDto;
import com.jdc.recipe_service.domain.dto.user.UserDto;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Getter
@Builder(toBuilder = true)
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class CommentDto {
    private Long id;
    private String content;
    private LocalDateTime createdAt;
    private CommentUserDto author;

    private int likeCount;
    private boolean likedByCurrentUser;

    @Builder.Default
    private List<CommentDto> replies = new ArrayList<>();

    @JsonInclude(JsonInclude.Include.NON_NULL) // 👈 이 필드에만 적용!
    private Integer replyCount;

}
