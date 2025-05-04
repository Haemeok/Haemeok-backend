package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.url.FileInfoRequest;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponseItem;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeImage;
import com.jdc.recipe_service.domain.repository.RecipeImageRepository;
import com.jdc.recipe_service.domain.type.ImageStatus;
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

            String fileKey = "recipes/" + recipe.getId() + "/" + slot + ".jpg";
            String presignedUrl = s3Util.createPresignedUrl(fileKey);

            uploads.add(PresignedUrlResponseItem.builder()
                    .fileKey(fileKey)
                    .presignedUrl(presignedUrl)
                    .build());

            images.add(RecipeImage.builder()
                    .recipe(recipe)
                    .slot(slot)
                    .fileKey(fileKey)
                    .status(ImageStatus.PENDING)
                    .build());
        }

        saveAll(images);
        return uploads;
    }


    @Transactional
    public void deleteImagesByRecipeId(Long recipeId) {
        List<RecipeImage> images = recipeImageRepository.findByRecipeId(recipeId);
        List<String> fileKeys = images.stream()
                .map(RecipeImage::getFileKey)
                .toList();

        if (!"local".equals(activeProfile) && !fileKeys.isEmpty()) {
            // 1. 0바이트 파일 필터링 (선택 사항)
            List<String> emptyKeys = new ArrayList<>();
            for (String key : fileKeys) {
                if (s3Util.isZeroByteFile(key)) {
                    emptyKeys.add(key);
                }
            }

            // 2. S3에서 삭제
            try {
                s3Util.deleteFiles(fileKeys);
            } catch (Exception e) {
                log.error("❌ S3 파일 삭제 실패 - recipeId: {}, keys: {}, error: {}", recipeId, fileKeys, e.getMessage(), e);
            }

            // 3. 0바이트 로그
            if (!emptyKeys.isEmpty()) {
                log.warn("⚠️ 0바이트 이미지 파일 삭제됨: {}", emptyKeys);
            }
        }

        // 4. DB 삭제
        recipeImageRepository.deleteAll(images);
    }

}
