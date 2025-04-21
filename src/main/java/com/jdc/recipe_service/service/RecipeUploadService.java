package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.url.*;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import software.amazon.awssdk.services.s3.model.PutObjectRequest;
import software.amazon.awssdk.services.s3.presigner.S3Presigner;
import software.amazon.awssdk.services.s3.presigner.model.PutObjectPresignRequest;

import java.time.Duration;
import java.util.List;

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

    public PresignedUrlResponse generatePresignedUrlsForCreate(Long recipeId, Long userId, List<FileInfoRequest> files) {
        List<PresignedUrlResponseItem> uploads = files.stream().map(file -> {
            String fileKey = generateFileKey(userId, recipeId, file);

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
            return new PresignedUrlResponseItem(fileKey, presignedUrl);
        }).toList();

        return new PresignedUrlResponse(recipeId, uploads);
    }

    public UpdatePresignedUrlResponse generatePresignedUrlsForUpdate(Long recipeId, Long userId, List<FileInfoRequest> files) {
        List<PresignedUrlResponseItem> uploads = files.stream().map(file -> {
            String fileKey = generateFileKey(userId, recipeId, file);

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
            return new PresignedUrlResponseItem(fileKey, presignedUrl);
        }).toList();

        return new UpdatePresignedUrlResponse(uploads);
    }

    private String generateFileKey(Long userId, Long recipeId, FileInfoRequest file) {
        return "main".equals(file.getType()) ?
                String.format("recipes/%d/%d/main.jpg", userId, recipeId) :
                String.format("recipes/%d/%d/steps/%d.jpg", userId, recipeId, file.getStepIndex());
    }
}
