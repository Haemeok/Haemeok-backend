package com.jdc.recipe_service.exception;

import lombok.Getter;
import org.springframework.http.HttpStatus;

@Getter
public enum ErrorCode {

    // --- User (100) ---
    USER_NOT_FOUND(HttpStatus.NOT_FOUND, "101", "요청한 사용자가 존재하지 않습니다."),
    DUPLICATE_NICKNAME(HttpStatus.CONFLICT, "102", "이미 사용 중인 닉네임입니다."),
    UNAUTHORIZED(HttpStatus.UNAUTHORIZED, "103", "인증이 필요합니다."),
    USER_ACCESS_DENIED(HttpStatus.FORBIDDEN, "104", "다른 사용자의 리소스에 접근할 수 없습니다."),

    // --- Recipe (200) ---
    RECIPE_NOT_FOUND(HttpStatus.NOT_FOUND, "201", "요청한 레시피가 존재하지 않습니다."),
    RECIPE_ACCESS_DENIED(HttpStatus.FORBIDDEN, "202", "레시피에 대한 접근 권한이 없습니다."),
    ALREADY_LIKED_RECIPE(HttpStatus.CONFLICT, "203", "이미 좋아요한 레시피입니다."),
    ALREADY_FAVORITED_RECIPE(HttpStatus.CONFLICT, "204", "이미 즐겨찾기한 레시피입니다."),
    INVALID_DISH_TYPE(HttpStatus.BAD_REQUEST, "205", "유효하지 않은 요리 유형(dishType)입니다."),
    INVALID_TAG_NAME(HttpStatus.BAD_REQUEST, "206", "유효하지 않은 태그입니다."),
    RATING_NOT_FOUND(HttpStatus.NOT_FOUND, "207", "요청한 레시피 평가가 존재하지 않습니다."),
    CANNOT_MAKE_PUBLIC_WITHOUT_IMAGE(HttpStatus.BAD_REQUEST, "208", "이미지 없이 AI 생성 레시피를 공개로 설정할 수 없습니다."),
    USER_RECIPE_IMAGE_REQUIRED(HttpStatus.BAD_REQUEST, "209", "유저가 작성한 레시피에는 메인 이미지가 반드시 포함되어야 합니다."),
    RECIPE_PRIVATE_ACCESS_DENIED(HttpStatus.FORBIDDEN, "210", "비공개 레시피에 대한 접근 권한이 없습니다."),

    // --- Comment (300) ---
    COMMENT_NOT_FOUND(HttpStatus.NOT_FOUND, "301", "요청한 댓글이 존재하지 않습니다."),
    COMMENT_ACCESS_DENIED(HttpStatus.FORBIDDEN, "302", "댓글에 대한 접근 권한이 없습니다."),
    ALREADY_LIKED_COMMENT(HttpStatus.CONFLICT, "303", "이미 좋아요한 댓글입니다."),
    INVALID_COMMENT_CONTENT(HttpStatus.BAD_REQUEST, "304", "댓글 내용이 유효하지 않습니다."),

    // --- Ingredient (400) ---
    INGREDIENT_NOT_FOUND(HttpStatus.NOT_FOUND, "401", "요청한 재료가 존재하지 않습니다."),
    INVALID_INGREDIENT_QUANTITY(HttpStatus.BAD_REQUEST, "402", "재료 수량이 유효하지 않습니다."),
    DUPLICATE_INGREDIENT(HttpStatus.CONFLICT, "403", "이미 존재하는 재료입니다."),
    INVALID_INGREDIENT_REQUEST(HttpStatus.BAD_REQUEST, "404", "잘못된 재료 요청입니다."),
    CUSTOM_INGREDIENT_INFO_MISSING(HttpStatus.BAD_REQUEST, "405", "새로운 재료는 가격과 단위를 함께 입력해야 합니다."),
    MISSING_INGREDIENT_NAME(HttpStatus.BAD_REQUEST, "506", "재료명(name 또는 customName)이 비어 있을 수 없습니다."),

    // --- Fridge (500) ---
    INVALID_FRIDGE_REQUEST(HttpStatus.BAD_REQUEST, "501", "잘못된 냉장고 요청입니다."),
    FRIDGE_UNAUTHORIZED(HttpStatus.UNAUTHORIZED, "502", "로그인이 필요합니다."),
    FRIDGE_ACCESS_DENIED(HttpStatus.FORBIDDEN, "503", "냉장고 권한이 없습니다."),
    FRIDGE_ITEM_NOT_FOUND(HttpStatus.NOT_FOUND, "504", "냉장고에 재료가 없습니다."),
    DUPLICATE_FRIDGE_ITEM(HttpStatus.CONFLICT, "505", "이미 냉장고에 존재하는 재료입니다."),

    // --- Auth (600) ---
    INVALID_REFRESH_TOKEN(HttpStatus.BAD_REQUEST, "601", "유효하지 않은 리프레시 토큰입니다."),
    REFRESH_TOKEN_EXPIRED(HttpStatus.BAD_REQUEST, "602", "리프레시 토큰이 만료되었습니다."),
    AUTH_UNAUTHORIZED(HttpStatus.UNAUTHORIZED, "603", "인증이 필요합니다."),
    LOGOUT_FAILED(HttpStatus.INTERNAL_SERVER_ERROR, "604", "로그아웃 처리에 실패했습니다."),

    // --- AI (700) ---
    AI_RECIPE_GENERATION_FAILED(HttpStatus.INTERNAL_SERVER_ERROR, "701", "AI 레시피 생성에 실패했습니다."),
    INVALID_AI_RECIPE_REQUEST(HttpStatus.BAD_REQUEST, "702", "AI 레시피 요청 형식이 잘못되었습니다."),

    // --- Common (900) ---
    INVALID_INPUT_VALUE(HttpStatus.BAD_REQUEST, "901", "잘못된 입력값입니다."),
    METHOD_NOT_ALLOWED(HttpStatus.METHOD_NOT_ALLOWED, "902", "허용되지 않은 메소드입니다."),
    INTERNAL_SERVER_ERROR(HttpStatus.INTERNAL_SERVER_ERROR, "903", "서버 내부 오류입니다."),
    NULL_POINTER(HttpStatus.BAD_REQUEST, "904", "필수 데이터가 누락되었습니다."),

    // --- Search (950) ---
    SEARCH_FAILURE(HttpStatus.INTERNAL_SERVER_ERROR, "950", "검색 처리 중 오류가 발생했습니다."),
    INGREDIENT_SEARCH_ERROR(HttpStatus.INTERNAL_SERVER_ERROR, "951", "재료 검색 처리 중 오류가 발생했습니다."),
    INGREDIENT_FALLBACK_SEARCH_ERROR(HttpStatus.INTERNAL_SERVER_ERROR, "952", "재료 대체 검색 처리 중 오류가 발생했습니다."),
    FRIDGE_RECIPE_SEARCH_ERROR(HttpStatus.INTERNAL_SERVER_ERROR, "953", "냉장고 기반 레시피 조회 중 오류가 발생했습니다."),
    ;

    private final HttpStatus status;
    private final String code;
    private final String message;

    ErrorCode(HttpStatus status, String code, String message) {
        this.status = status;
        this.code = code;
        this.message = message;
    }
}