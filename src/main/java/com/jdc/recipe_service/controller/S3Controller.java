package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.service.S3Uploader;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/s3")
@Tag(name = "S3 업로드 API", description = "S3에 파일을 업로드하는 기능을 제공합니다.")
public class S3Controller {

    private final S3Uploader s3Uploader;

    @PostMapping("/upload")
    @Operation(summary = "파일 업로드", description = "S3에 MultipartFile을 업로드하고 저장된 파일명을 반환합니다.")
    public String upload(
            @Parameter(description = "업로드할 파일") @RequestParam("file") MultipartFile file) throws Exception {
        String fileName = file.getOriginalFilename();
        return s3Uploader.uploadFile(file, fileName);
    }
}
