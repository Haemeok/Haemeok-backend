package com.jdc.recipe_service.service.article;

import com.jdc.recipe_service.domain.dto.article.ArticleImageFinalizeResponse;
import com.jdc.recipe_service.domain.dto.article.ArticleImagePresignedUrlRequest;
import com.jdc.recipe_service.domain.dto.article.ArticleImagePresignedUrlResponse;
import com.jdc.recipe_service.domain.repository.article.CurationArticleRepository;
import com.jdc.recipe_service.exception.ArticleImagesNotReadyException;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.util.S3Util;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

/**
 * 어드민 큐레이션 아티클 이미지 업로드.
 *
 * <p>레시피 이미지와 동일한 원본→webp 변환 구조 + articleId 기반 key 정책:
 * <pre>
 *   uploadKey  : original/images/articles/{articleId}/{uuid}.{ext}   (프론트 PUT 업로드 대상)
 *   imageKey   : images/articles/{articleId}/{uuid}.webp             (변환 후 최종, DB/MDX에 저장)
 * </pre>
 *
 * <p>articleId를 path에 박아두는 이유는 운영 추적성 — S3 key만 봐도 어느 글에 속한 이미지인지 알 수 있고,
 * 글 삭제 시 prefix 단위 정리(`original/images/articles/{id}/`, `images/articles/{id}/`)가 가능해진다.
 *
 * <p>fileKey는 서버에서만 생성해 프론트가 S3 path를 결정하지 못하게 한다. DRAFT/PUBLISHED/ARCHIVED 어떤 상태에서도
 * 어드민은 이미지 업로드 URL을 받을 수 있다 — 발행 후 본문 수정 중 이미지 추가 시나리오를 막지 않기 위함.
 *
 * <p><b>fileSize 검증의 한계 (V1)</b>: 요청 DTO의 fileSize ≤ 10MB는 <em>요청값에 대한 사전 검증</em>일 뿐
 * presigned PUT URL 자체가 실제 업로드 크기를 강제하지 않는다. 클라이언트가 fileSize 245678로 신청하고 실제로는
 * 50MB를 PUT해도 S3는 받아준다. 어드민 전용 + 운영자 신뢰 가정하에 V1에서는 사전 검증만 둔다.
 *
 * <p>참고: 본 서비스의 {@link #finalizeImages} 는 <strong>변환 결과 .webp 객체의 존재 여부만 확인</strong>하며
 * ContentLength(실제 업로드 크기) 검사는 하지 않는다. 따라서 fileSize 강제는 finalize로 해결되지 않는다.
 * 강제가 필요해지면 별도 작업이 필요하다:
 * <ul>
 *   <li>presigned <em>POST</em> + content-length-range 정책으로 전환, 또는</li>
 *   <li>finalize에서 HeadObjectResponse.contentLength()까지 검사하도록 확장 (단, 변환 후 webp의 크기는 원본과 다름)</li>
 * </ul>
 *
 * <p>TODO(image-pipeline): Lambda recipe-image-resizer가 articles 분기에서 다음 입출력을 처리해야 한다.
 * <pre>
 *   입력: original/images/articles/{articleId}/{uuid}.{ext}
 *   출력: images/articles/{articleId}/{uuid}.webp
 *   변환: width 1024, withoutEnlargement true, webp quality 85
 * </pre>
 * recipes 라우팅({@code original/images/recipes/} → {@code images/recipes/})은 이미 잡혀 있고, 여기에
 * articles 분기가 추가되어야 한다. 인프라/Lambda 갱신은 별도 배포 항목.
 */
@Service
@RequiredArgsConstructor
public class CurationArticleImageUploadService {

    private static final Set<String> ALLOWED_CONTENT_TYPES =
            Set.of("image/jpeg", "image/png", "image/webp");

    private static final Map<String, String> EXTENSION_BY_CONTENT_TYPE = Map.of(
            "image/jpeg", "jpg",
            "image/png", "png",
            "image/webp", "webp"
    );

    private static final long MAX_FILE_SIZE_BYTES = 10L * 1024 * 1024;

    private static final String IMAGE_KEY_PREFIX = "images/articles";
    private static final String UPLOAD_KEY_PREFIX = "original/" + IMAGE_KEY_PREFIX;
    private static final String CONVERTED_EXTENSION = "webp";

    private final S3Util s3Util;
    private final CurationArticleRepository articleRepo;

    public ArticleImagePresignedUrlResponse issuePresignedUrl(Long articleId,
                                                              ArticleImagePresignedUrlRequest req) {
        if (!articleRepo.existsById(articleId)) {
            throw new CustomException(ErrorCode.ARTICLE_NOT_FOUND);
        }

        String contentType = req.getContentType();
        if (!ALLOWED_CONTENT_TYPES.contains(contentType)) {
            throw new CustomException(ErrorCode.ARTICLE_IMAGE_INVALID_CONTENT_TYPE);
        }
        if (req.getFileSize() > MAX_FILE_SIZE_BYTES) {
            throw new CustomException(ErrorCode.ARTICLE_IMAGE_TOO_LARGE);
        }

        String uuid = UUID.randomUUID().toString();
        String uploadKey = String.format("%s/%d/%s.%s",
                UPLOAD_KEY_PREFIX, articleId, uuid, EXTENSION_BY_CONTENT_TYPE.get(contentType));
        String imageKey = String.format("%s/%d/%s.%s",
                IMAGE_KEY_PREFIX, articleId, uuid, CONVERTED_EXTENSION);

        String presignedUrl = s3Util.createPresignedUrl(uploadKey, contentType);

        return ArticleImagePresignedUrlResponse.builder()
                .uploadKey(uploadKey)
                .imageKey(imageKey)
                .presignedUrl(presignedUrl)
                .build();
    }

    /**
     * 프론트가 PUT을 끝낸 imageKey들이 S3에 실제로 존재하는지(=Lambda 변환 완료) 확인한다.
     *
     * <p>검증 순서:
     * <ol>
     *   <li>articleId 존재</li>
     *   <li>각 key가 {@code images/articles/{articleId}/}로 시작하고 {@code .webp}로 끝나는지 (다른 글의 키 사칭 방지)</li>
     *   <li>S3 HEAD로 객체 존재 여부 확인</li>
     * </ol>
     *
     * <p>모든 키가 존재하면 ready=true 응답. 하나라도 누락되면 {@link ArticleImagesNotReadyException}을 던져
     * 409 + missingKeys 응답으로 매핑된다.
     */
    public ArticleImageFinalizeResponse finalizeImages(Long articleId, List<String> imageKeys) {
        if (!articleRepo.existsById(articleId)) {
            throw new CustomException(ErrorCode.ARTICLE_NOT_FOUND);
        }

        String requiredPrefix = String.format("%s/%d/", IMAGE_KEY_PREFIX, articleId);
        List<String> distinct = imageKeys.stream().distinct().toList();

        for (String key : distinct) {
            if (key == null || !key.startsWith(requiredPrefix) || !key.endsWith("." + CONVERTED_EXTENSION)) {
                // 다른 article의 키 사칭 방지 + 변환 결과(.webp)만 허용
                throw new CustomException(ErrorCode.INVALID_INPUT_VALUE,
                        "imageKey 형식이 잘못되었습니다: " + key);
            }
        }

        List<String> missing = new ArrayList<>();
        List<String> present = new ArrayList<>();
        for (String key : distinct) {
            // isObjectPresent는 404만 false. 권한/네트워크 문제는 예외로 propagate되어 catch-all 500이 되며,
            // 프론트가 "아직 변환 안 됨"으로 오해하고 무한 폴링하는 것을 막는다.
            if (s3Util.isObjectPresent(key)) {
                present.add(key);
            } else {
                missing.add(key);
            }
        }

        if (!missing.isEmpty()) {
            throw new ArticleImagesNotReadyException(missing, present);
        }

        return ArticleImageFinalizeResponse.builder()
                .ready(true)
                .imageKeys(present)
                .build();
    }
}
