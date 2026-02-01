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
import org.springframework.scheduling.annotation.Async;
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

    private static final int DAILY_FILE_LIMIT = 3;

    /**
     * [스케줄러] 매일 새벽 4시 실행 (기본 5개)
     */
    @Scheduled(cron = "0 0 4 * * *")
    public void processDailyRecipeFiles() {
        log.info(">>>> [Scheduler] Daily Recipe Batch Started.");

        int dayOfMonth = LocalDate.now().getDayOfMonth();

        if (dayOfMonth % 2 != 0) {
            log.info(">>>> Today is Odd day ({}). Processing ODD folder.", dayOfMonth);
            processFolderWithLimit(PATH_ODD, "odd", DAILY_FILE_LIMIT);
        } else {
            log.info(">>>> Today is Even day ({}). Processing EVEN folder.", dayOfMonth);
            processFolderWithLimit(PATH_EVEN, "even", DAILY_FILE_LIMIT);
        }

        log.info(">>>> [Scheduler] Daily Recipe Batch Finished.");
    }

    /**
     * [수동 실행] 컨트롤러에서 호출용 (비동기)
     * limit과 type을 외부에서 받아서 실행함
     */
    @Async
    public void runManualBatch(String type, int limit) {
        log.info(">>>> [Manual Batch] Started. Type: {}, Limit: {}", type, limit);

        String folderPath;
        if ("odd".equalsIgnoreCase(type)) {
            folderPath = PATH_ODD;
        } else if ("even".equalsIgnoreCase(type)) {
            folderPath = PATH_EVEN;
        } else {
            log.error("Invalid type provided: {}", type);
            return;
        }

        processFolderWithLimit(folderPath, type, limit);

        log.info(">>>> [Manual Batch] Finished.");
    }

    /**
     * 내부 로직: 지정된 폴더에서 limit 개수만큼 처리
     */
    private void processFolderWithLimit(String folderPrefix, String type, int limit) {
        try {
            log.info(">>>> [Batch] Checking folder: {} (Type: {}, Limit: {})", folderPrefix, type, limit);

            ListObjectsV2Request listRequest = ListObjectsV2Request.builder()
                    .bucket(bucket)
                    .prefix(folderPrefix)
                    .build();

            ListObjectsV2Response listResponse = s3Client.listObjectsV2(listRequest);
            List<S3Object> objects = listResponse.contents();

            if (objects.isEmpty()) {
                log.info(">>>> [Batch] No files found in {}", folderPrefix);
                return;
            }

            int processCount = 0;

            for (S3Object s3Object : objects) {
                if (processCount >= limit) {
                    log.info("Batch limit reached ({}) for folder {}", limit, folderPrefix);
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
                    log.error(">>>> [Batch] Failed to process file: {}", key, e);
                }
            }

        } catch (Exception e) {
            log.error(">>>> [Batch] Error processing folder: {}", folderPrefix, e);
        }
    }

    /**
     * 개별 파일 처리 및 이동
     */
    private void processSingleFile(String key, String type) {
        log.info(">>>> [Batch] Processing file: {} with type='{}'", key, type);

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
     * 파일을 processed 폴더로 이동
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