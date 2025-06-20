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

    public String createPresignedUrl(String fileKey) {
        ensureBucketExists();
        PutObjectRequest objectRequest = PutObjectRequest.builder()
                .bucket(bucketName)
                .key(fileKey)
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

    /**
     * 바이트 배열을 S3에 업로드
     *
     * @param data  업로드할 바이트 배열
     * @param s3Key S3에 저장할 키
     */
    public void upload(byte[] data, String s3Key) {
        ensureBucketExists();
        PutObjectRequest putReq = PutObjectRequest.builder()
                .bucket(bucketName)
                .key(s3Key)
                .contentLength((long) data.length)
                .build();
        s3Client.putObject(putReq, RequestBody.fromBytes(data));
    }
}
