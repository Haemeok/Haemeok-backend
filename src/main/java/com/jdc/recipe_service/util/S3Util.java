package com.jdc.recipe_service.util;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.presigner.S3Presigner;
import software.amazon.awssdk.services.s3.model.*;
import software.amazon.awssdk.services.s3.presigner.model.PutObjectPresignRequest;

import java.net.URL;
import java.time.Duration;
import java.util.List;

@Component
@RequiredArgsConstructor
public class S3Util {

    private final S3Client s3Client;
    private final S3Presigner s3Presigner;

    @Value("${cloud.aws.s3.bucket}")
    private String bucketName;

    /**
     * presigned URL 생성
     */
    public String createPresignedUrl(String fileKey) {
        PutObjectRequest objectRequest = PutObjectRequest.builder()
                .bucket(bucketName)
                .key(fileKey)
                .build();

        PutObjectPresignRequest presignRequest = PutObjectPresignRequest.builder()
                .signatureDuration(Duration.ofMinutes(10))
                .putObjectRequest(objectRequest)
                .build();

        URL presignedUrl = s3Presigner.presignPutObject(presignRequest).url();
        return presignedUrl.toString();
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
     * S3의 full URL 생성
     */
    public String getFullUrl(String fileKey) {
        return "https://" + bucketName + ".s3.amazonaws.com/" + fileKey;
    }
}
