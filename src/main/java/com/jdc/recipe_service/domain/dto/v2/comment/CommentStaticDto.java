package com.jdc.recipe_service.domain.dto.v2.comment;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.jdc.recipe_service.domain.dto.user.CommentUserDto;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import java.time.LocalDateTime;

@Getter
@Builder(toBuilder = true)
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class CommentStaticDto {
    private Long id;
    private String content;

    @JsonFormat(
            shape = JsonFormat.Shape.STRING,
            pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'",
            timezone = "UTC"
    )
    private LocalDateTime createdAt;
    private CommentUserDto author;
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @Builder.Default
    private int replyCount = 0;
}