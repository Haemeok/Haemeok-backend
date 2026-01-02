package com.jdc.recipe_service.domain.dto.user;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CommentUserDto {
    @JsonSerialize(using = HashIdSerializer.class)
    private Long id;
    private String nickname;
    private String profileImage;
}
