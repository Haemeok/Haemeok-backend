package com.jdc.recipe_service.domain.dto;


import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class TokenRefreshRequestDTO {
    private String refreshToken;
}