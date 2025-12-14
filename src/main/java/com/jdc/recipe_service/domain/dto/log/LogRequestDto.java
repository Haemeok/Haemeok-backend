package com.jdc.recipe_service.domain.dto.log;

import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class LogRequestDto {

    private String action;

    private String uuid;
}