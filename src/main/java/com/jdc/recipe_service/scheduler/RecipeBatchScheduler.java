package com.jdc.recipe_service.scheduler;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectReader;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.service.ai.RecipeTestService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import software.amazon.awssdk.core.ResponseBytes;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.model.*;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.time.LocalDate;
import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
public class RecipeBatchScheduler {

    private final S3Client s3Client;
    private final RecipeTestService recipeTestService;
    private final ObjectMapper objectMapper;

    @Value("${app.s3.bucket-name}")
    private String bucket;

    private static final String ROOT_PATH = "recipe-data/";
    private static final String SEEDS_PATH = ROOT_PATH + "seeds/";
    private static final String PROCESSED_PATH = ROOT_PATH + "processed/";

    private static final String PATH_ODD = SEEDS_PATH + "odd/";
    private static final String PATH_EVEN = SEEDS_PATH + "even/";

    /**
     * 매일 새벽 4시에 실행
     * odd 폴더와 even 폴더를 각각 확인하여 처리
     */
    @Scheduled(cron = "0 0 4 * * *")
    public void processDailyRecipeFiles() {
        log.info(">>>> [Scheduler] Daily Recipe Batch Started.");

        int dayOfMonth = LocalDate.now().getDayOfMonth();

        if (dayOfMonth % 2 != 0) {
            log.info(">>>> Today is Odd day ({}). Processing ODD folder.", dayOfMonth);
            processFolder(PATH_ODD, "odd");
        } else {
            log.info(">>>> Today is Even day ({}). Processing EVEN folder.", dayOfMonth);
            processFolder(PATH_EVEN, "even");
        }

        log.info(">>>> [Scheduler] Daily Recipe Batch Finished.");
    }

    private static final int DAILY_FILE_LIMIT = 5;

    /**
     * 특정 폴더의 파일들을 읽어서 지정된 type으로 처리하는 메서드
     * @param folderPrefix 조회할 S3 폴더 경로 (예: recipe-data/seeds/odd/)
     * @param type 적용할 타입 ("odd" or "even")
     */
    private void processFolder(String folderPrefix, String type) {
        try {
            log.info(">>>> [Scheduler] Checking folder: {} (Type: {})", folderPrefix, type);

            ListObjectsV2Request listRequest = ListObjectsV2Request.builder()
                    .bucket(bucket)
                    .prefix(folderPrefix)
                    .build();

            ListObjectsV2Response listResponse = s3Client.listObjectsV2(listRequest);
            List<S3Object> objects = listResponse.contents();

            if (objects.isEmpty()) {
                log.info(">>>> [Scheduler] No files found in {}", folderPrefix);
                return;
            }

            int processCount = 0;

            for (S3Object s3Object : objects) {
                if (processCount >= DAILY_FILE_LIMIT) {
                    log.info("Daily limit reached ({}) for folder {}", DAILY_FILE_LIMIT, folderPrefix);
                    break;
                }

                String key = s3Object.key();

                if (key.endsWith("/") || !key.toLowerCase().endsWith(".json")) {
                    continue;
                }

                try {
                    processSingleFile(key, type);
                    processCount++;
                } catch (Exception e) {
                    log.error(">>>> [Scheduler] Failed to process file: {}", key, e);
                }
            }

        } catch (Exception e) {
            log.error(">>>> [Scheduler] Error processing folder: {}", folderPrefix, e);
        }
    }

    /**
     * 개별 파일 처리 및 이동
     */
    private void processSingleFile(String key, String type) {
        log.info(">>>> [Scheduler] Processing file: {} with type='{}'", key, type);

        GetObjectRequest getRequest = GetObjectRequest.builder()
                .bucket(bucket)
                .key(key)
                .build();

        ResponseBytes<GetObjectResponse> objectBytes = s3Client.getObjectAsBytes(getRequest);
        String jsonContent = objectBytes.asUtf8String();

        try {
            ObjectReader reader = objectMapper.readerFor(new TypeReference<List<RecipeCreateRequestDto>>() {})
                    .with(DeserializationFeature.ACCEPT_SINGLE_VALUE_AS_ARRAY);

            List<RecipeCreateRequestDto> requests = reader.readValue(jsonContent);

            if (requests != null && !requests.isEmpty()) {
                List<RecipeCreateRequestDto> successList = recipeTestService.batchInsertRecipes(requests, type);

                log.info("Successfully inserted {}/{} recipes from {}", successList.size(), requests.size(), key);
            } else {
                log.warn("Empty recipe list in file: {}", key);
            }

            moveFileToProcessed(key, type);

        } catch (Exception e) {
            throw new RuntimeException("Error parsing or processing file: " + key, e);
        }
    }

    /**
     * 파일을 processed 폴더로 이동 (예: recipe-data/seeds/odd/a.json -> recipe-data/processed/odd/a.json)
     */
    private void moveFileToProcessed(String sourceKey, String type) {
        String fileName = sourceKey.substring(sourceKey.lastIndexOf("/") + 1);

        String destinationKey = PROCESSED_PATH + type + "/" + fileName;

        String encodedSource = bucket + "/" + sourceKey;
        try {
            encodedSource = URLEncoder.encode(bucket + "/" + sourceKey, StandardCharsets.UTF_8.toString())
                    .replaceAll("%2F", "/");
        } catch (Exception ignored) {}

        try {
            CopyObjectRequest copyRequest = CopyObjectRequest.builder()
                    .copySource(encodedSource)
                    .bucket(bucket)
                    .key(destinationKey)
                    .build();

            s3Client.copyObject(copyRequest);

            DeleteObjectRequest deleteRequest = DeleteObjectRequest.builder()
                    .bucket(bucket)
                    .key(sourceKey)
                    .build();

            s3Client.deleteObject(deleteRequest);

            log.info("Moved file to processed: {}", destinationKey);

        } catch (Exception e) {
            log.error("Failed to move file {} to {}", sourceKey, destinationKey, e);
            throw new RuntimeException("File move failed", e);
        }
    }
}