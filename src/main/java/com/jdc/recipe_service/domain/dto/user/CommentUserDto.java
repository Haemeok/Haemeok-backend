package com.jdc.recipe_service.domain.dto.user;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CommentUserDto {
    private Long id;
    private String nickname;
    private String profileImage;
}
