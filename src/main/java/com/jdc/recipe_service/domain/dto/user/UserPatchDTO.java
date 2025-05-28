package com.jdc.recipe_service.domain.dto.user;

import jakarta.validation.constraints.Size;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserPatchDTO {
    @Size(max = 50)
    private String nickname;

    @Size(max = 255)
    private String introduction;

    @Size(max = 255)
    private String profileImageKey;
}