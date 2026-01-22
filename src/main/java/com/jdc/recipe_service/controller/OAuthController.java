package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.auth.AuthTokens;
import com.jdc.recipe_service.domain.dto.auth.CodeDto;
import com.jdc.recipe_service.service.AuthService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseCookie;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@Slf4j
@RestController
@RequiredArgsConstructor
public class OAuthController {

    private final AuthService authService;

    @PostMapping("/login/oauth2/code/{provider}")
    public ResponseEntity<Void> oauthCallback(
            @PathVariable String provider,
            @RequestBody CodeDto codeDto,
            @RequestHeader(value = "X-Env", defaultValue = "prod") String env) {

        log.info("[Controller] callback for provider={}, codeDto={}, env={}",
                provider, codeDto, env);

        AuthTokens tokens = authService.handleLogin(provider,codeDto.getCode(),env);

        log.info("[Controller] generated tokens for provider={}: accessToken(length)={}, refreshToken(length)={}",
                provider, tokens.getAccessToken().length(), tokens.getRefreshToken().length());

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

        if ("prod".equalsIgnoreCase(env)) {
            accessB.secure(true).domain(".recipio.kr");
            refreshB.secure(true).domain(".recipio.kr");
        } else if ("local".equalsIgnoreCase(env)) {
        } else {
            accessB.secure(true).sameSite("None");
            refreshB.secure(true).sameSite("None");
        }

        return ResponseEntity.ok()
                .header(HttpHeaders.SET_COOKIE, accessB.build().toString())
                .header(HttpHeaders.SET_COOKIE, refreshB.build().toString())
                .build();
    }
}