package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.auth.AuthTokens;
import com.jdc.recipe_service.domain.dto.auth.CodeDto;
import com.jdc.recipe_service.service.AuthService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseCookie;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;


@RestController
@RequiredArgsConstructor
public class OAuthController {

    private final AuthService authService;

    @PostMapping("/login/oauth2/code/{provider}")
    public ResponseEntity<Void> oauthCallback(
            @PathVariable String provider,
            @RequestBody CodeDto codeDto,
            @RequestHeader(value = "X-Env", defaultValue = "prod") String env) {

        AuthTokens tokens = authService.handleLogin(provider,codeDto.getCode());

        boolean isLocal = "local".equalsIgnoreCase(env);

        var accessB = ResponseCookie.from("accessToken", tokens.getAccessToken())
                .path("/")
                .httpOnly(true)
                .sameSite("Lax")
                .maxAge(15 * 60);

        var refreshB = ResponseCookie.from("refreshToken", tokens.getRefreshToken())
                .path("/")
                .httpOnly(true)
                .sameSite("Lax")
                .maxAge(7 * 24 * 60 * 60);

        if (!isLocal) {
            accessB.secure(true).domain(".haemeok.com");
            refreshB.secure(true).domain(".haemeok.com");
        }

        return ResponseEntity.ok()
                .header(HttpHeaders.SET_COOKIE, accessB.build().toString())
                .header(HttpHeaders.SET_COOKIE, refreshB.build().toString())
                .build();
    }
}