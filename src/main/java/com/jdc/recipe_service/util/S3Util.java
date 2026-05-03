package com.jdc.recipe_service.util;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import software.amazon.awssdk.core.sync.RequestBody;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.model.*;
import software.amazon.awssdk.services.s3.presigner.S3Presigner;
import software.amazon.awssdk.services.s3.presigner.model.PutObjectPresignRequest;

import java.time.Duration;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Component
@RequiredArgsConstructor
@Slf4j
public class S3Util {

    private final S3Client s3Client;
    private final S3Presigner s3Presigner;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    public void ensureBucketExists() {
        try {
            s3Client.headBucket(HeadBucketRequest.builder()
                    .bucket(bucketName)
                    .build());
        } catch (NoSuchBucketException e) {
            log.info("버킷이 존재하지 않아 새로 생성합니다: {}", bucketName);
            s3Client.createBucket(CreateBucketRequest.builder()
                    .bucket(bucketName)
                    .build());
        }
    }

    public String createPresignedUrl(String fileKey, String contentType) {
        ensureBucketExists();
        PutObjectRequest objectRequest = PutObjectRequest.builder()
                .bucket(bucketName)
                .key(fileKey)
                .contentType(contentType)
                .build();

        PutObjectPresignRequest presignRequest = PutObjectPresignRequest.builder()
                .signatureDuration(Duration.ofMinutes(10))
                .putObjectRequest(objectRequest)
                .build();

        return s3Presigner.presignPutObject(presignRequest)
                .url()
                .toString();
    }

    public boolean doesObjectExist(String fileKey) {
        try {
            s3Client.headObject(HeadObjectRequest.builder()
                    .bucket(bucketName)
                    .key(fileKey)
                    .build());
            return true;
        } catch (S3Exception e) {
            return false;
        }
    }

    /**
     * doesObjectExist의 엄격 버전. <strong>404/NoSuchKey만 false</strong>를 반환하고, 그 외 S3Exception
     * (403 권한 문제, 5xx, 네트워크 등)은 그대로 propagate해서 catch-all 500으로 매핑되게 한다.
     *
     * <p>finalize 같은 "객체 존재 검증" 호출자는 권한/리전 오설정 같은 환경 문제가 "아직 변환 안 됨"으로
     * 잘못 보고되어 프론트가 무한 폴링하는 것을 막기 위해 이 메서드를 사용한다.
     */
    public boolean isObjectPresent(String fileKey) {
        try {
            s3Client.headObject(HeadObjectRequest.builder()
                    .bucket(bucketName)
                    .key(fileKey)
                    .build());
            return true;
        } catch (NoSuchKeyException e) {
            return false;
        } catch (S3Exception e) {
            if (e.statusCode() == 404) return false;
            throw e;
        }
    }

    public void deleteFiles(List<String> fileKeys) {
        if (fileKeys.isEmpty()) return;
        List<ObjectIdentifier> objects = fileKeys.stream()
                .map(key -> ObjectIdentifier.builder().key(key).build())
                .toList();
        s3Client.deleteObjects(DeleteObjectsRequest.builder()
                .bucket(bucketName)
                .delete(Delete.builder().objects(objects).build())
                .build());
    }

    public boolean isZeroByteFile(String fileKey) {
        try {
            HeadObjectResponse response = s3Client.headObject(HeadObjectRequest.builder()
                    .bucket(bucketName)
                    .key(fileKey)
                    .build());
            return response.contentLength() == 0;
        } catch (S3Exception e) {
            return false;
        }
    }

    /**
     * 바이트 배열을 S3에 업로드
     *
     * @param data  업로드할 바이트 배열
     * @param s3Key S3에 저장할 키
     */
    public void upload(byte[] data, String s3Key, String contentType) {
        ensureBucketExists();
        PutObjectRequest putReq = PutObjectRequest.builder()
                .bucket(bucketName)
                .key(s3Key)
                .contentType(contentType)
                .contentLength((long) data.length)
                .build();
        s3Client.putObject(putReq, RequestBody.fromBytes(data));
    }

    public void copyObject(String sourceKey, String destinationKey) {
        s3Client.copyObject(CopyObjectRequest.builder()
                .sourceBucket(bucketName)
                .sourceKey(sourceKey)
                .destinationBucket(bucketName)
                .destinationKey(destinationKey)
                .build());
    }

    public Set<String> listKeysInFolder(String folderPrefix) {
        if (folderPrefix == null || folderPrefix.isBlank()) {
            return Collections.emptySet();
        }

        String prefix = folderPrefix.endsWith("/") ? folderPrefix : folderPrefix + "/";

        ListObjectsV2Request request = ListObjectsV2Request.builder()
                .bucket(bucketName)
                .prefix(prefix)
                .build();

        return s3Client.listObjectsV2(request).contents().stream()
                .map(S3Object::key)
                .collect(Collectors.toSet());
    }
}
