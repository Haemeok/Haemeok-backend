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

import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.time.Duration;
import java.util.List;

@Component
@RequiredArgsConstructor
@Slf4j
public class S3Util {

    private final S3Client s3Client;
    private final S3Presigner s3Presigner;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    /**
     * 버킷이 존재하지 않으면 생성
     */
    public void ensureBucketExists() {
        try {
            HeadBucketRequest headBucket = HeadBucketRequest.builder()
                    .bucket(bucketName)
                    .build();
            s3Client.headBucket(headBucket);
        } catch (NoSuchBucketException e) {
            log.info("버킷이 존재하지 않아 새로 생성합니다: {}", bucketName);
            CreateBucketRequest createBucket = CreateBucketRequest.builder()
                    .bucket(bucketName)
                    .build();
            s3Client.createBucket(createBucket);
        }
    }

    /**
     * presigned PUT URL 생성 (유저 업로드용)
     */
    public String createPresignedUrl(String fileKey) {
        ensureBucketExists();

        log.info("📦 S3Presigner class: {}", s3Presigner.getClass().getName());

        PutObjectRequest objectRequest = PutObjectRequest.builder()
                .bucket(bucketName)
                .key(fileKey)
                .build();

        PutObjectPresignRequest presignRequest = PutObjectPresignRequest.builder()
                .signatureDuration(Duration.ofMinutes(10))
                .putObjectRequest(objectRequest)
                .build();

        return s3Presigner.presignPutObject(presignRequest).url().toString();
    }

    /**
     * S3 객체 존재 여부 확인
     */
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
     * S3 객체 삭제 (복수)
     */
    public void deleteFiles(List<String> fileKeys) {
        if (fileKeys.isEmpty()) return;

        List<ObjectIdentifier> objects = fileKeys.stream()
                .map(key -> ObjectIdentifier.builder().key(key).build())
                .toList();

        DeleteObjectsRequest deleteRequest = DeleteObjectsRequest.builder()
                .bucket(bucketName)
                .delete(Delete.builder().objects(objects).build())
                .build();

        s3Client.deleteObjects(deleteRequest);
    }

    /**
     * S3 객체가 0바이트인지 확인 (존재하지 않으면 false)
     */
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
     * 외부 URL(externalUrl)에 있는 이미지를 스트리밍 받아,
     * S3에 "s3Key" 위치로 업로드한 뒤, S3 key를 반환한다.
     *
     * @param externalUrl 외부(예: DALL·E)에서 생성된 이미지 URL
     * @param s3Key       S3에 저장할 key (예: "recipes/42/123/ai-generated-main.jpg")
     * @return 실제 업로드된 S3 key
     * @throws Exception 다운로드 혹은 S3 업로드 중 에러 발생 시
     */
    public String uploadFromUrl(String externalUrl, String s3Key) throws Exception {
        ensureBucketExists();

        URL url = new URL(externalUrl);
        URLConnection conn = url.openConnection();
        String contentType = conn.getContentType();
        long contentLength = conn.getContentLengthLong();
        InputStream inputStream = conn.getInputStream();

        PutObjectRequest putReq = PutObjectRequest.builder()
                .bucket(bucketName)
                .key(s3Key)
                .contentType(contentType)
                .contentLength(contentLength)
                .build();

        s3Client.putObject(putReq, RequestBody.fromInputStream(inputStream, contentLength));
        inputStream.close();

        return s3Key;
    }
}
