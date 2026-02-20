package com.jdc.recipe_service.domain.dto.url;

import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;

@Getter
@NoArgsConstructor
public class YoutubeExtractionResponse extends PresignedUrlResponse {
    private boolean created;
    private boolean refunded;

    @Builder(builderMethodName = "youtubeBuilder")
    public YoutubeExtractionResponse(Long recipeId, List<PresignedUrlResponseItem> uploads, boolean created, boolean refunded) {
        super(recipeId, uploads, created);
        this.created = created;
        this.refunded = refunded;
    }
}