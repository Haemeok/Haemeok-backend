package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.url.PresignedUrlRequest;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponseItem;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import software.amazon.awssdk.services.s3.model.PutObjectRequest;
import software.amazon.awssdk.services.s3.presigner.S3Presigner;
import software.amazon.awssdk.services.s3.presigner.model.PutObjectPresignRequest;

import java.time.Duration;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class RecipeUploadService {

    private final S3Presigner s3Presigner;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${app.s3.upload-base-path}")
    private String basePath;

    @Value("${app.s3.presigned-url-expiration-minutes}")
    private long expirationMinutes;

    public PresignedUrlResponse generatePresignedUrls(PresignedUrlRequest request, Long userId) {
        Long recipeId = request.getRecipeId();

        List<PresignedUrlResponseItem> uploads = request.getFiles().stream().map(file -> {
            String sanitizedFilename = file.getFilename().replaceAll("[^a-zA-Z0-9._-]", "");
            String fileKey;

            if ("main".equals(file.getType())) {
                fileKey = String.format("recipes/%d/%d/main.jpg", userId, recipeId);
            } else if ("step".equals(file.getType()) && file.getStepIndex() != null) {
                fileKey = String.format("recipes/%d/%d/steps/%d.jpg", userId, recipeId, file.getStepIndex());
            } else {
                throw new IllegalArgumentException("잘못된 파일 타입 또는 stepIndex 누락");
            }

            PutObjectRequest objectRequest = PutObjectRequest.builder()
                    .bucket(bucketName)
                    .key(fileKey)
                    .contentType(file.getContentType())
                    .build();

            PutObjectPresignRequest presignRequest = PutObjectPresignRequest.builder()
                    .signatureDuration(Duration.ofMinutes(expirationMinutes))
                    .putObjectRequest(objectRequest)
                    .build();

            String presignedUrl = s3Presigner.presignPutObject(presignRequest).url().toString();
            return new PresignedUrlResponseItem(presignedUrl, fileKey);
        }).toList();

        return new PresignedUrlResponse(uploads);
    }

}
