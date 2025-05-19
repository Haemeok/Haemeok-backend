package com.jdc.recipe_service.domain.dto.comment;

import com.fasterxml.jackson.annotation.JsonUnwrapped;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.springframework.data.domain.Page;

@Getter
@AllArgsConstructor
@NoArgsConstructor
public class CommentWithRepliesDto {
    @JsonUnwrapped
    private CommentDto parentComment;
    private Page<ReplyDto> replies;
}