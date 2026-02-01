package com.jdc.recipe_service.service.image;

import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.url.FileInfoRequest;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponseItem;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeImage;
import com.jdc.recipe_service.domain.repository.RecipeImageRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.ImageStatus;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.mapper.RecipeMapper;
import com.jdc.recipe_service.util.S3Util;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
public class RecipeImageService {

    @Value("${spring.profiles.active:}")
    private String activeProfile;

    private final RecipeImageRepository recipeImageRepository;
    private final S3Util s3Util;
    private final AsyncImageService asyncImageService;
    private final RecipeRepository recipeRepository;

    @Transactional(readOnly = true)
    public List<RecipeImage> getImagesByRecipeId(Long recipeId) {
        return recipeImageRepository.findByRecipeId(recipeId);
    }

    @Transactional
    public void saveAll(List<RecipeImage> images) {
        recipeImageRepository.saveAll(images);
    }

    @Transactional
    public List<PresignedUrlResponseItem> generateAndSavePresignedUrls(Recipe recipe, List<FileInfoRequest> files) {
        List<PresignedUrlResponseItem> uploads = new ArrayList<>();
        List<RecipeImage> images = new ArrayList<>();

        for (FileInfoRequest fileInfo : files) {
            String slot = fileInfo.getType().equals("main")
                    ? "main"
                    : "step_" + fileInfo.getStepIndex();

            String originalExtension = getFileExtension(fileInfo.getContentType());

            String uploadKey = "original/images/recipes/" + recipe.getId() + "/" + slot + originalExtension;

            String dbKey = "images/recipes/" + recipe.getId() + "/" + slot + ".webp";

            String contentType = fileInfo.getContentType();
            if (contentType == null || contentType.isEmpty()) {
                contentType = "image/jpeg";
            }

            String presignedUrl = s3Util.createPresignedUrl(uploadKey, contentType);

            uploads.add(PresignedUrlResponseItem.builder()
                    .fileKey(uploadKey)
                    .presignedUrl(presignedUrl)
                    .build());

            images.add(RecipeImage.builder()
                    .recipe(recipe)
                    .slot(slot)
                    .fileKey(dbKey)
                    .status(ImageStatus.PENDING)
                    .build());
        }

        saveAll(images);
        return uploads;
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


    @Transactional
    public void deleteImagesByRecipeId(Long recipeId) {
        List<RecipeImage> images = recipeImageRepository.findByRecipeId(recipeId);
        List<String> fileKeys = images.stream()
                .map(RecipeImage::getFileKey)
                .toList();

        if (!"local".equals(activeProfile) && !fileKeys.isEmpty()) {
            List<String> emptyKeys = new ArrayList<>();
            for (String key : fileKeys) {
                if (s3Util.isZeroByteFile(key)) {
                    emptyKeys.add(key);
                }
            }

            try {
                s3Util.deleteFiles(fileKeys);
            } catch (Exception e) {
                log.error("❌ S3 파일 삭제 실패 - recipeId: {}, keys: {}, error: {}", recipeId, fileKeys, e.getMessage(), e);
            }

            if (!emptyKeys.isEmpty()) {
                log.warn("⚠️ 0바이트 이미지 파일 삭제됨: {}", emptyKeys);
            }
        }

        recipeImageRepository.deleteByRecipeId(recipeId);
    }

    @Transactional
    public void deleteByFileKey(String fileKey) {
        recipeImageRepository.deleteByFileKey(fileKey);
    }

    @Transactional
    public void regenerateAndApplyImage(Long recipeId) {
        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        RecipeCreateRequestDto recipeDto = RecipeMapper.toCreateDto(recipe);

        log.info("🎨 [Service] 레시피 ID {} 이미지 재생성 프로세스 시작", recipeId);

        asyncImageService.generateImageFromDto(recipeDto, recipe.getUser().getId())
                .thenAccept(generatedImageUrl -> {
                    if (generatedImageUrl != null && !generatedImageUrl.contains("no_image")) {
                        String s3Key = extractS3Key(generatedImageUrl);

                        recipe.updateImageKey(s3Key);
                        recipe.updateImageStatus(RecipeImageStatus.READY);
                        recipeRepository.save(recipe);
                        log.info("✅ 레시피 {} 이미지 업데이트 완료: {}", recipeId, s3Key);
                    }
                });
    }

    /**
     * URL에서 S3 Key 추출 (ExtractionService에서 이관)
     */
    private String extractS3Key(String fullUrl) {
        if (fullUrl == null || fullUrl.isBlank()) return null;
        try {
            java.net.URI uri = new java.net.URI(fullUrl);
            String path = uri.getPath();
            return (path != null && path.startsWith("/")) ? path.substring(1) : path;
        } catch (Exception e) {
            int imgIdx = fullUrl.indexOf("images/");
            return (imgIdx != -1) ? fullUrl.substring(imgIdx) : fullUrl;
        }
    }

}
