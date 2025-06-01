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
     * presigned PUT URL 생성 (유저 업로드용)
     */
    public String createPresignedUrl(String fileKey) {
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
     * 외부 URL(externalUrl)에 있는 이미지를 스트리밍 받아,
     * S3에 "s3Key" 위치로 업로드한 뒤, S3 key를 반환한다.
     *
     * @param externalUrl 외부(예: DALL·E)에서 생성된 이미지 URL
     * @param s3Key       S3에 저장할 key (예: "recipes/42/123/ai-generated-main.jpg")
     * @return 실제 업로드된 S3 key
     * @throws Exception 다운로드 혹은 S3 업로드 중 에러 발생 시
     */
    public String uploadFromUrl(String externalUrl, String s3Key) throws Exception {
        // 1) 외부 URL에서 InputStream 얻기
        URL url = new URL(externalUrl);
        URLConnection conn = url.openConnection();
        // 필요하다면 conn.setRequestProperty("User-Agent", ""); 로 헤더 추가 가능
        String contentType = conn.getContentType();
        long contentLength = conn.getContentLengthLong();
        InputStream inputStream = conn.getInputStream();

        // 2) S3에 업로드
        PutObjectRequest putReq = PutObjectRequest.builder()
                .bucket(bucketName)
                .key(s3Key)
                // ContentType과 ContentLength를 지정하면 브라우저가 이미지 표시에 용이합니다.
                .contentType(contentType)
                .contentLength(contentLength)
                .build();

        s3Client.putObject(putReq, RequestBody.fromInputStream(inputStream, contentLength));
        inputStream.close();

        // 3) S3 key 반환 (필요 시 getFullUrl(s3Key)로 URL을 생성해도 됨)
        return s3Key;
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
     * 주어진 S3 key로 presigned GET URL 반환 (Public Access가 막혀있는 경우 사용)
     *
     * @param fileKey       S3 key
     * @param expireSeconds presigned URL 유효기간(초)
     * @return presigned GET URL 문자열
     */
    public String generatePresignedGetUrl(String fileKey, long expireSeconds) {
        // AWS SDK v2의 Presigner를 사용하여 presigned GET URL 생성 예시
        // (실제 사용 시 SDK 버전에 따라 import 경로나 메서드명이 약간 다를 수 있습니다)
        GetObjectRequest getReq = GetObjectRequest.builder()
                .bucket(bucketName)
                .key(fileKey)
                .build();

        software.amazon.awssdk.services.s3.presigner.model.GetObjectPresignRequest presignReq =
                software.amazon.awssdk.services.s3.presigner.model.GetObjectPresignRequest.builder()
                        .signatureDuration(Duration.ofSeconds(expireSeconds))
                        .getObjectRequest(getReq)
                        .build();

        return s3Presigner.presignGetObject(presignReq).url().toString();
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
     * S3의 full URL 생성 (퍼블릭 버킷 혹은 Public Access 허용 시 사용)
     */
    public String getFullUrl(String fileKey) {
        return "https://" + bucketName + ".s3.amazonaws.com/" + fileKey;
    }
}
