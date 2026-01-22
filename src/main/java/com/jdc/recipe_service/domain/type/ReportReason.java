package com.jdc.recipe_service.domain.type;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum ReportReason {
    WRONG_QUANTITY("양이 틀림"),
    WRONG_NAME("재료 이름이 틀림"),
    NOT_EXIST("없는 재료임 (삭제 필요)"),
    MISSING("빠진 재료 (추가 필요)"),
    ETC("기타");

    private final String description;
}