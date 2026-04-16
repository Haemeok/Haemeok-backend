package com.jdc.recipe_service.domain.dto.recipebook;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;

@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Schema(description = "레시피 저장 상태 응답")
public class RecipeSaveStatusResponse {

    @Schema(description = "저장 여부", example = "true")
    private boolean saved;

    @Schema(description = "저장된 폴더 수", example = "2")
    private int savedBookCount;

    @Schema(description = "저장된 폴더 목록")
    private List<SavedBookInfo> books;

    @Getter
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    @Schema(description = "저장된 폴더 정보")
    public static class SavedBookInfo {

        @JsonSerialize(using = HashIdSerializer.class)
        @Schema(description = "레시피북 ID")
        private Long id;

        @Schema(description = "레시피북 이름", example = "한식 모음")
        private String name;

        @Schema(description = "기본 레시피북 여부")
        private boolean isDefault;
    }
}
