package com.jdc.recipe_service.domain.dto.recipe.v2;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.querydsl.core.annotations.QueryProjection;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Getter
@Setter
@SuperBuilder
@NoArgsConstructor
@Schema(description = "레시피 간략 정적 정보 DTO")
public class RecipeSimpleStaticDto {

    @Schema(description = "레시피 ID")
    private Long id;

    @Schema(description = "레시피 제목")
    private String title;

    @Schema(description = "레시피 대표 이미지 URL")
    private String imageUrl;

    @Schema(description = "작성자 ID")
    private Long authorId;

    @Schema(description = "작성자 닉네임")
    private String authorName;

    @Schema(description = "작성자 프로필 이미지")
    private String profileImage;

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    @Schema(description = "생성일시 (UTC)")
    private LocalDateTime createdAt;

    @Schema(description = "예상 조리 시간 (분 단위)")
    private Integer cookingTime;

    @QueryProjection
    public RecipeSimpleStaticDto(Long id, String title, String imageUrl, Long authorId, String authorName, String profileImage, LocalDateTime createdAt, Integer cookingTime) {
        this.id = id;
        this.title = title;
        this.imageUrl = imageUrl;
        this.authorId = authorId;
        this.authorName = authorName;
        this.profileImage = profileImage;
        this.createdAt = createdAt;
        this.cookingTime = cookingTime;
    }
}