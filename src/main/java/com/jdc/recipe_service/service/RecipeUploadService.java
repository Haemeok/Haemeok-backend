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

    @Value("${app.s3.presigned-url-expiration-minutes}")
    private long expirationMinutes;

    public UpdatePresignedUrlResponse generatePresignedUrlsForUpdate(Long recipeId, Long userId, List<FileInfoRequest> files) {
        List<PresignedUrlResponseItem> uploads = files.stream().map(file -> {
            String contentType = file.getContentType();
            if (contentType == null || contentType.isEmpty()) {
                contentType = "image/jpeg";
            }

            String fileKey = generateFileKey(recipeId, file, contentType);

            PutObjectRequest objectRequest = PutObjectRequest.builder()
                    .bucket(bucketName)
                    .key(fileKey)
                    .contentType(contentType)
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

    private String generateFileKey(Long recipeId, FileInfoRequest file, String contentType) {
        String prefix = String.format("images/recipes/%d", recipeId);

        String extension = getFileExtension(contentType);

        if ("main".equals(file.getType())) {
            return String.format("%s/main%s", prefix, extension);
        } else {
            return String.format("%s/steps/%d%s", prefix, file.getStepIndex(), extension);
        }
    }

    private String getFileExtension(String contentType) {
        if (contentType == null) return ".jpg";
        return switch (contentType.toLowerCase()) {
            case "image/webp" -> ".webp";
            case "image/png" -> ".png";
            case "image/jpeg" -> ".jpg";
            default -> ".jpg";
        };
    }
}
