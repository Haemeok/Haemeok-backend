package com.jdc.recipe_service.config;

import com.jdc.recipe_service.jwt.JwtAuthenticationFilter;
import com.jdc.recipe_service.security.CustomAuthenticationEntryPoint;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.oauth2.client.*;
import org.springframework.security.oauth2.client.endpoint.DefaultAuthorizationCodeTokenResponseClient;
import org.springframework.security.oauth2.client.endpoint.OAuth2AccessTokenResponseClient;
import org.springframework.security.oauth2.client.endpoint.OAuth2AuthorizationCodeGrantRequest;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;
import org.springframework.http.client.SimpleClientHttpRequestFactory;
import org.springframework.web.client.RestTemplate;
import org.springframework.http.converter.FormHttpMessageConverter;
import org.springframework.security.oauth2.core.http.converter.OAuth2AccessTokenResponseHttpMessageConverter;

import java.util.Arrays;

@Configuration
@EnableWebSecurity
@EnableMethodSecurity
@RequiredArgsConstructor
public class SecurityConfig {

    private final JwtAuthenticationFilter jwtFilter;
    private final CustomAuthenticationEntryPoint entryPoint;
    private final Environment env;

    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
        http
                .csrf(AbstractHttpConfigurer::disable)
                .cors(cors -> cors.configurationSource(corsConfig()))
                .sessionManagement(sm -> sm.sessionCreationPolicy(SessionCreationPolicy.STATELESS));

        // --- 로컬 프로필: H2 콘솔, local-token만 풀고, 나머지는 운영과 동일하게 보안 처리 ---
        if (Arrays.asList(env.getActiveProfiles()).contains("local")) {
            http
                    .authorizeHttpRequests(auth -> auth
                            .requestMatchers(HttpMethod.OPTIONS, "/**").permitAll()
                            .requestMatchers("/ws/notifications/**").permitAll()

                            .requestMatchers("/api/webhooks/**").permitAll()

                            .requestMatchers("/api/notifications/**").authenticated()
                            .requestMatchers("/api/notification-preferences/**").authenticated()

                            // 1) 로컬 전용: H2 콘솔, JWT 발급용 엔드포인트
                            .requestMatchers("/h2-console/**", "/local-token").permitAll()
                            .requestMatchers("/actuator/**").permitAll()
                            .requestMatchers(HttpMethod.POST, "/login/oauth2/code/**").permitAll()
                            .requestMatchers(HttpMethod.POST, "/api/token/test-login").permitAll()
                            .requestMatchers("/api/test/ai-recipe/**").permitAll()
                            .requestMatchers(HttpMethod.POST, "/api/logs/**").permitAll()
                            .requestMatchers(HttpMethod.GET, "/api/logs/stats").permitAll()
                            .requestMatchers(HttpMethod.POST, "/api/ingredients/units/batch").permitAll()
                            .requestMatchers(HttpMethod.POST, "/api/dev/recipes/status").permitAll()  // dev V3 batch 동적 상태 — anonymous 허용 (gate는 service)
                            .requestMatchers(HttpMethod.GET, "/api/recipes/reports").hasRole("ADMIN")
                            // 2-a) 레시피 하위 중 인증 필요 GET
                            .requestMatchers(HttpMethod.GET, "/api/recipes/*/saved-books").authenticated()

                            // 2) 공개 GET (인증 없이 모두 허용)
                            .requestMatchers(HttpMethod.GET,
                                    "/api/ingredients",
                                    "/api/ingredients/names",
                                    "/api/ingredients/*/units",
                                    "/api/ingredients/*",
                                    "/api/recipes/*/comments",
                                    "/api/recipes/*/comments/*/replies",
                                    "/api/recipes/*",
                                    "/api/recipes/*/remixes",
                                    "/api/recipes/*/recommendations",
                                    "/api/recipes/simple",
                                    "/api/recipes/search",
                                    "/api/recipes/by-tag",
                                    "/api/recipes/by-dish-type",
                                    "/api/dish-types",
                                    "/api/tags",
                                    "/api/users/*",
                                    "/api/users/*/recipes",
                                    "/api/v2/recipes/**",
                                    "/api/recipes/popular",
                                    "/api/recipes/budget",
                                    "/api/recipes/youtube/recommend",
                                    // TODO(dev V3): 아래 /api/dev/* 경로는 V3 dev 컨트롤러
                                    // (com.jdc.recipe_service.dev.*) 도착 시 재활성화. 그 전까진 404.
                                    "/api/dev/recipes/ai/status/**",
                                    "/api/dev/recipes/youtube/status/**",
                                    "/api/dev/recipes/youtube/check",
                                    "/api/dev/recipes/*",  // dev V3 recipe 상세 (GET only — POST는 위 ai/youtube와 다른 segment)
                                    "/api/dev/recipes/*/comments",  // dev V3 댓글 목록 — anonymous 허용 (gate는 service)
                                    "/api/dev/recipes/*/comments/*/replies",  // dev V3 대댓글 목록 — anonymous 허용
                                    "/api/dev/recipes/*/status",  // dev V3 단건 동적 상태 — anonymous 허용 (gate는 service)
                                    "/api/dev/recipes/*/recommendations",  // dev V3 추천 — anonymous 허용 (base recipe gate는 service)
                                    "/api/dev/recipes/*/remixes",  // dev V3 리믹스 목록 — anonymous 허용 (base recipe gate는 service)
                                    "/api/dev/ingredients/*",  // dev V3 ingredient 상세 (운영 /api/ingredients/* 미러)
                                    "/api/dev/users/*/recipes",  // dev V3 사용자 레시피 목록 (운영 /api/users/*/recipes 미러)
                                    "/api/dev/search/**",  // dev V3 search mirrors
                                    "/api/curation-articles",        // public 큐레이션 아티클 목록 (PUBLISHED만 service가 강제)
                                    "/api/curation-articles/*",      // public 큐레이션 아티클 상세 (slug; PUBLISHED만 service가 강제)
                                    "/api/recipes/sitemap"
                            ).permitAll()

                            // 3) 보호된 GET (JWT 필요)
                            .requestMatchers("/api/me/recipe-books/**").authenticated()

                            .requestMatchers(HttpMethod.GET,
                                    "/api/me",
                                    "/api/me/favorites",
                                    "/api/me/recipes",
                                    "/api/products",
                                    "/api/credits/history",
                                    "/api/me/fridge/items",
                                    "/api/me/fridge/items/ids",
                                    "/api/me/calendar/**",
                                    "/api/me/records/**",
                                    "/api/me/streak",
                                    "/api/me/survey",
                                    "/api/me/fridge/recipes/**",
                                    "/api/dev/me/fridge/recipes/**",  // dev V3 fridge 추천 (운영 /api/me/fridge/recipes 미러)
                                    "/api/dev/me/recipes",  // dev V3 내 레시피 목록 (운영 /api/me/recipes 미러)
                                    "/api/dev/me/favorites",  // dev V3 즐겨찾기 (운영 /api/me/favorites 미러)
                                    "/api/dev/me/recipe-books/**",  // dev V3 레시피북 read (운영 /api/me/recipe-books/** 미러, write는 후속)
                                    "/api/dev/recipes/*/saved-books",  // dev V3 본인 폴더 저장 상태 조회 (인증 필수, 게이트 없음)
                                    "/api/dev/me/calendar",            // dev V3 캘린더 day records (silent filter)
                                    "/api/dev/me/records/**",          // dev V3 요리 기록 timeline + detail (silent filter)
                                    "/api/ratings/recipe/*/me",
                                    "/api/dev/ratings/recipe/*/me", // dev V3 내 평점 조회 (운영 미러)
                                    "/api/users/*/profile-image/presign"
                            ).authenticated()

                            // 4) 보호된 POST
                            .requestMatchers(HttpMethod.POST,
                                    "/api/me/fridge/items",
                                    "/api/recipes/*/reports",
                                    "/api/me/records",
                                    "/api/me/survey",
                                    "/api/recipes/*/comments",
                                    "/api/recipes/*/comments/*/replies",
                                    "/api/comments/*/like",
                                    "/api/recipes",
                                    "/api/recipes/ai",
                                    "/api/recipes/*/presigned-urls",
                                    "/api/recipes/*/like",
                                    "/api/recipes/*/favorite",
                                    "/api/dev/recipes/*/like",     // dev V3 like (운영 미러 + 권한 게이트)
                                    "/api/dev/recipes/*/favorite", // dev V3 favorite (운영 미러 + 권한 게이트)
                                    "/api/dev/recipes/*/comments", // dev V3 댓글 작성 (운영 미러 + 권한 게이트)
                                    "/api/dev/recipes/*/comments/*/replies", // dev V3 대댓글 작성
                                    "/api/dev/comments/*/like",   // dev V3 댓글 좋아요 (운영 미러 + recipe 가시성 게이트)
                                    "/api/ratings/recipe/*",
                                    "/api/dev/ratings/recipe/*",   // dev V3 평점 등록/수정 (운영 미러 + 분기 게이트)
                                    "/api/token/logout",
                                    "/api/token/logout/all",
                                    "/api/me/fridge/items/bulk",
                                    "/api/recipes/*/private",
                                    "/api/recipes/*/finalize",
                                    "/api/ws-ticket",
                                    "/api/dev/recipes",            // dev V3 레시피 직접 등록 (운영 미러 + remix origin 게이트)
                                    "/api/dev/recipes/*/presigned-urls", // dev V3 presigned URL (owner + ACTIVE, 운영 leak 차단)
                                    "/api/dev/recipes/*/finalize",       // dev V3 finalize (owner + ACTIVE, admin escape 없음)
                                    "/api/dev/recipes/ai",
                                    "/api/dev/recipes/youtube/extract",
                                    "/api/dev/me/recipe-books",          // dev V3 레시피북 생성 (운영 미러)
                                    "/api/dev/me/recipe-books/*/recipes", // dev V3 레시피북 레시피 추가 (RESTRICTED 차단)
                                    "/api/dev/me/records",              // dev V3 요리 기록 생성 (recipe 가시성 게이트)
                                    "/api/dev/recipes/*/reports"        // dev V3 재료 신고 (recipe 가시성 게이트)
                            ).authenticated()

                            // 5) 보호된 PUT
                            .requestMatchers(HttpMethod.PUT,
                                    "/api/me",
                                    "/api/recipes/*",
                                    "/api/dev/recipes/*",          // dev V3 레시피 수정 (owner + ACTIVE only)
                                    "/api/ingredients",
                                    "/api/recipes/*/images",
                                    "/api/dev/recipes/*/images",   // dev V3 이미지 키 업데이트 (owner + ACTIVE)
                                    "/api/dev/me/recipe-books/order", // dev V3 레시피북 순서 변경 (운영 미러)
                                    "/api/users/*"
                            ).authenticated()

                            // 6) 보호된 DELETE
                            .requestMatchers(HttpMethod.DELETE,
                                    "/api/me",
                                    "/api/me/fridge/items/*",
                                    "/api/me/fridge/items/bulk",
                                    "/api/recipes/*/comments/*",
                                    "/api/dev/recipes/*/comments/*", // dev V3 댓글 삭제 (운영 미러)
                                    "/api/recipes/*",
                                    "/api/dev/recipes/*",          // dev V3 레시피 삭제 (owner cleanup right)
                                    "/api/ratings/recipe/*",
                                    "/api/dev/ratings/recipe/*",   // dev V3 평점 삭제 (운영 미러)
                                    "/api/dev/me/recipe-books/*",          // dev V3 레시피북 삭제 (운영 미러)
                                    "/api/dev/me/recipe-books/*/recipes",  // dev V3 레시피북 레시피 제거 (cleanup right)
                                    "/api/me/records/*",
                                    "/api/dev/me/records/*",              // dev V3 요리 기록 삭제 (cleanup right)
                                    "/api/ingredients"
                            ).authenticated()

                            .requestMatchers(HttpMethod.PATCH,
                                    "/api/users/*",
                                    "/api/me",
                                    "/api/dev/recipes/*/visibility",     // dev V3 가시성 변경
                                    "/api/dev/me/recipe-books/*"         // dev V3 레시피북 이름 변경 (운영 미러)
                            ).authenticated()

                            // 7) 관리자용 API
                            .requestMatchers("/api/admin/**").hasRole("ADMIN")
                            .requestMatchers("/api/opensearch/**").hasRole("ADMIN")
                            .requestMatchers("/api/test/recipes/**").hasRole("ADMIN")

                            // 8) 나머지 요청은 모두 공개
                            .anyRequest().permitAll()

                    )
                    .headers(h -> h.frameOptions(frame -> frame.disable()));
            http.addFilterBefore(jwtFilter, UsernamePasswordAuthenticationFilter.class);
            http.exceptionHandling(e -> e.authenticationEntryPoint(entryPoint));

            return http.build();
        }

        // --- 운영/스테이징 ---
        http
                // HTTPS 강제
                .requiresChannel(ch -> ch
                        .requestMatchers("/api/**").requiresSecure()
                        .requestMatchers("/ws/notifications/**").requiresSecure()
                        .requestMatchers("/test-ws.html").requiresSecure()
                )
                .cors(cors -> cors.configurationSource(corsConfig()))
                .csrf(AbstractHttpConfigurer::disable)

                .authorizeHttpRequests(auth -> auth
                        .requestMatchers(HttpMethod.OPTIONS, "/**").permitAll()
                        .requestMatchers("/test-ws.html").permitAll()
                        .requestMatchers(HttpMethod.POST, "/login/oauth2/code/**").permitAll()

                        .requestMatchers("/api/webhooks/**").permitAll()

                        // WebSocket 핸드쉐이크 & SockJS 엔드포인트
                        .requestMatchers("/ws/notifications/**").permitAll()

                        .requestMatchers("/api/notifications/**").authenticated()
                        .requestMatchers("/api/notification-preferences/**").authenticated()
                        // 챗봇 (recipe sub-resource) — POST/GET /api/recipes/{id}/chat[/history]
                        // /api/recipes/**의 다른 광범위 매처보다 specific하게 먼저 catch.
                        .requestMatchers("/api/recipes/*/chat", "/api/recipes/*/chat/**").authenticated()
                        // 챗봇 user-scoped — GET /api/chat/quota
                        .requestMatchers("/api/chat/quota").authenticated()

                        // 1) 공개 엔드포인트
                        .requestMatchers(
                                "/v3/api-docs/**",
                                "/swagger-ui/**",
                                "/swagger-ui.html",
                                "/swagger-resources/**",
                                "/webjars/**",
                                "/api/token/refresh",
                                "/api/token/test-login",
                                "/api/tags/**",
                                "/", "/oauth2/**", "/login/**", "/error",
                                "/h2-console/**",
                                "/actuator/**"
                        ).permitAll()

                        .requestMatchers("/api/opensearch/**").hasRole("ADMIN")

                        // 레시피북: 모든 메서드 인증 필요
                        .requestMatchers("/api/me/recipe-books/**").authenticated()

                        // 2) GET 중 인증 필요
                        .requestMatchers(HttpMethod.GET,
                                "/api/me",
                                "/api/me/favorites",
                                "/api/me/recipes",
                                "/api/products",
                                "/api/credits/history",
                                "/api/me/fridge/items",
                                "/api/me/fridge/items/ids",
                                "/api/me/calendar/**",
                                "/api/me/records/**",
                                "/api/me/streak",
                                "/api/me/survey",
                                "/api/me/fridge/recipes/**",
                                "/api/dev/me/fridge/recipes/**",  // dev V3 fridge 추천 — local 분기와 정합
                                "/api/dev/me/recipes",  // dev V3 내 레시피 목록 — local 분기와 정합
                                "/api/dev/me/favorites",  // dev V3 즐겨찾기 — local 분기와 정합
                                "/api/dev/me/recipe-books/**",  // dev V3 레시피북 read — local 분기와 정합
                                "/api/dev/recipes/*/saved-books",  // dev V3 본인 폴더 저장 상태 — local 분기와 정합
                                "/api/dev/me/calendar",            // dev V3 캘린더 day records — local 분기와 정합
                                "/api/dev/me/records/**",          // dev V3 요리 기록 — local 분기와 정합
                                "/api/ratings/recipe/*/me",
                                "/api/dev/ratings/recipe/*/me", // dev V3 내 평점 조회 — local 분기와 정합
                                "/api/users/*/profile-image/presign"
                        ).authenticated()

                        .requestMatchers(HttpMethod.GET, "/api/recipes/reports").hasRole("ADMIN")

                        // 3-a) 레시피 하위 중 인증 필요 GET
                        .requestMatchers(HttpMethod.GET, "/api/recipes/*/saved-books").authenticated()

                        // 3) 읽기 전용 GET (모두 허용)
                        .requestMatchers(HttpMethod.GET,
                                "/api/ingredients",
                                "/api/ingredients/names",
                                "/api/ingredients/*/units",
                                "/api/ingredients/*",
                                "/api/recipes/*/comments",
                                "/api/recipes/*/comments/*/replies",
                                "/api/recipes/*",
                                "/api/recipes/*/remixes",
                                "/api/recipes/*/recommendations",
                                "/api/recipes/simple",
                                "/api/recipes/search",
                                "/api/recipes/by-tag",
                                "/api/recipes/by-dish-type",
                                "/api/dish-types",
                                "/api/tags",
                                "/api/users/*",
                                "/api/users/*/recipes",
                                "/api/search/**",
                                "/api/v2/recipes/**",
                                "/api/recipes/popular",
                                "/api/recipes/budget",
                                "/api/recipes/youtube/check",
                                "/api/recipes/youtube/recommend",
                                "/api/recipes/youtube/status/**",
                                "/api/recipes/ai/status/**",
                                "/api/dev/recipes/ai/status/**",
                                "/api/dev/recipes/youtube/status/**",
                                "/api/dev/recipes/youtube/check",
                                "/api/dev/recipes/*",  // dev V3 recipe 상세 (GET) — local 분기와 정합
                                "/api/dev/recipes/*/comments",  // dev V3 댓글 목록 — anonymous 허용 (gate는 service)
                                "/api/dev/recipes/*/comments/*/replies",  // dev V3 대댓글 목록 — anonymous 허용
                                "/api/dev/recipes/*/status",  // dev V3 단건 동적 상태 — local 분기와 정합
                                "/api/dev/recipes/*/recommendations",  // dev V3 추천 — local 분기와 정합
                                "/api/dev/recipes/*/remixes",  // dev V3 리믹스 목록 — local 분기와 정합
                                "/api/dev/ingredients/*",  // dev V3 ingredient 상세 — local 분기와 정합
                                "/api/dev/users/*/recipes",  // dev V3 사용자 레시피 목록 — local 분기와 정합
                                "/api/dev/search/**",  // dev V3 search mirrors
                                "/api/curation-articles",        // public 큐레이션 아티클 목록 (PUBLISHED만 service가 강제)
                                "/api/curation-articles/*",      // public 큐레이션 아티클 상세 (slug; PUBLISHED만 service가 강제)
                                "/api/recipes/sitemap"
                        ).permitAll()

                        // [추가된 부분] POST
                        .requestMatchers(HttpMethod.POST, "/api/v2/recipes/status").permitAll()
                        .requestMatchers(HttpMethod.POST, "/api/dev/recipes/status").permitAll()  // dev V3 batch 동적 상태 — local 분기와 정합
                        .requestMatchers(HttpMethod.POST, "/api/logs/**").permitAll()
                        .requestMatchers(HttpMethod.GET, "/api/logs/stats").permitAll()
                        .requestMatchers(HttpMethod.POST, "/api/ingredients/units/batch").permitAll()

                        // 4) 인증 필요 POST
                        .requestMatchers(HttpMethod.POST,
                                "/api/ingredients",
                                "/api/recipes/*/reports",
                                "/api/recipes/*/comments",
                                "/api/recipes/*/comments/*/replies",
                                "/api/comments/*/like",
                                "/api/recipes",
                                "/api/recipes/ai",
                                "/api/recipes/ai/v2",
                                "/api/recipes/extract",
                                "/api/recipes/extract/v2",
                                "/api/recipes/*/presigned-urls",
                                "/api/recipes/*/like",
                                "/api/recipes/*/favorite",
                                "/api/dev/recipes/*/like",     // dev V3 like — local 분기와 정합
                                "/api/dev/recipes/*/favorite", // dev V3 favorite — local 분기와 정합
                                "/api/dev/recipes/*/comments", // dev V3 댓글 작성 — local 분기와 정합
                                "/api/dev/recipes/*/comments/*/replies", // dev V3 대댓글 작성 — local 분기와 정합
                                "/api/dev/comments/*/like",   // dev V3 댓글 좋아요 — local 분기와 정합
                                "/api/me/fridge/items",
                                "/api/me/fridge/items/bulk",
                                "/api/me/survey",
                                "/api/me/records",
                                "/api/ratings/recipe/*",
                                "/api/dev/ratings/recipe/*",   // dev V3 평점 등록/수정 — local 분기와 정합
                                "/api/token/logout",
                                "/api/token/logout/all",
                                "/api/recipes/*/private",
                                "/api/recipes/*/finalize",
                                "/api/ws-ticket",
                                "/api/dev/recipes",            // dev V3 레시피 직접 등록 — local 분기와 정합
                                "/api/dev/recipes/*/presigned-urls", // dev V3 presigned URL — local 분기와 정합
                                "/api/dev/recipes/*/finalize", // dev V3 finalize — local 분기와 정합
                                "/api/dev/recipes/ai",
                                "/api/dev/recipes/youtube/extract",
                                "/api/dev/me/recipe-books",          // dev V3 레시피북 생성 — local 분기와 정합
                                "/api/dev/me/recipe-books/*/recipes", // dev V3 레시피북 레시피 추가 — local 분기와 정합
                                "/api/dev/me/records",              // dev V3 요리 기록 생성 — local 분기와 정합
                                "/api/dev/recipes/*/reports"        // dev V3 재료 신고 — local 분기와 정합
                        ).authenticated()

                        // 5) 인증 필요 PUT
                        .requestMatchers(HttpMethod.PUT,
                                "/api/ingredients",
                                "/api/recipes/*",
                                "/api/dev/recipes/*",          // dev V3 레시피 수정 — local 분기와 정합
                                "/api/me",
                                "/api/recipes/*/images",
                                "/api/dev/recipes/*/images",   // dev V3 이미지 키 업데이트 — local 분기와 정합
                                "/api/dev/me/recipe-books/order", // dev V3 레시피북 순서 변경 — local 분기와 정합
                                "/api/users/*"
                        ).authenticated()

                        // 6) 인증 필요 DELETE
                        .requestMatchers(HttpMethod.DELETE,
                                "/api/ingredients",
                                "/api/recipes/*/comments/*",
                                "/api/dev/recipes/*/comments/*", // dev V3 댓글 삭제 — local 분기와 정합
                                "/api/recipes/*",
                                "/api/dev/recipes/*",          // dev V3 레시피 삭제 — local 분기와 정합
                                "/api/ratings/recipe/*",
                                "/api/dev/ratings/recipe/*",   // dev V3 평점 삭제 — local 분기와 정합
                                "/api/dev/me/recipe-books/*",          // dev V3 레시피북 삭제 — local 분기와 정합
                                "/api/dev/me/recipe-books/*/recipes",  // dev V3 레시피북 레시피 제거 — local 분기와 정합
                                "/api/me",
                                "/api/me/records/*",
                                "/api/dev/me/records/*",              // dev V3 요리 기록 삭제 — local 분기와 정합
                                "/api/me/fridge/items/*",
                                "/api/me/fridge/items/bulk"
                        ).authenticated()
                        // 7) 인증 필요 PATCH
                        .requestMatchers(HttpMethod.PATCH,
                                "/api/users/*",
                                "/api/me",
                                "/api/dev/recipes/*/visibility", // dev V3 가시성 변경 (prod 분기 정합)
                                "/api/dev/me/recipe-books/*"     // dev V3 레시피북 이름 변경 — local 분기와 정합
                        ).authenticated()

                        // 8) 관리자용 API
                        .requestMatchers("/api/admin/**").hasRole("ADMIN")
                        .requestMatchers(HttpMethod.POST, "/api/recipes/*/analyze").hasRole("ADMIN")
                        .requestMatchers(HttpMethod.POST, "/api/recipes/*/nutrition").hasRole("ADMIN")
                        .requestMatchers("/api/test/recipes/**").hasRole("ADMIN")
                        .requestMatchers("/api/test/ai-recipe/**").hasRole("ADMIN")

                        // 9) 나머지 전부 차단
                        .anyRequest().denyAll()
                )
                .exceptionHandling(e -> e.authenticationEntryPoint(entryPoint))
                .formLogin(AbstractHttpConfigurer::disable)
                .addFilterBefore(jwtFilter, UsernamePasswordAuthenticationFilter.class)
                .headers(headers -> headers.frameOptions(frame -> frame.disable()));
        return http.build();
    }

    @Bean
    public CorsConfigurationSource corsConfig() {
        CorsConfiguration cfg = new CorsConfiguration();
        cfg.setAllowedOriginPatterns(Arrays.asList(
                "http://localhost:3000",
                "http://localhost:5173",
                "https://www.recipio.kr",
                "https://recipio.kr",
                "https://*.vercel.app",
                "https://haemeok.vercel.app"
        ));
        cfg.setAllowedMethods(Arrays.asList("GET","POST","PUT","PATCH","DELETE","OPTIONS"));
        cfg.setAllowedHeaders(Arrays.asList("*"));
        cfg.setAllowCredentials(true);

        UrlBasedCorsConfigurationSource src = new UrlBasedCorsConfigurationSource();
        src.registerCorsConfiguration("/**", cfg);
        return src;
    }
    @Bean
    public OAuth2AccessTokenResponseClient<OAuth2AuthorizationCodeGrantRequest> accessTokenResponseClient() {
        SimpleClientHttpRequestFactory factory = new SimpleClientHttpRequestFactory();
        factory.setConnectTimeout(5000);
        factory.setReadTimeout(10000);

        RestTemplate restTemplate = new RestTemplate(factory);
        restTemplate.setMessageConverters(Arrays.asList(
                new FormHttpMessageConverter(),
                new OAuth2AccessTokenResponseHttpMessageConverter()
        ));

        DefaultAuthorizationCodeTokenResponseClient client = new DefaultAuthorizationCodeTokenResponseClient();
        client.setRestOperations(restTemplate);
        return client;
    }
    /*
    @Bean
    public OAuth2AuthorizedClientService authorizedClientService(
            ClientRegistrationRepository clientRegistrations) {
        return new InMemoryOAuth2AuthorizedClientService(clientRegistrations);
    }
    @Bean
    public OAuth2AuthorizedClientManager authorizedClientManager(
            ClientRegistrationRepository clientRegistrations,
            OAuth2AuthorizedClientService clientService) {

        var provider = OAuth2AuthorizedClientProviderBuilder.builder()
                .authorizationCode()
                .refreshToken()
                .build();

        var manager = new AuthorizedClientServiceOAuth2AuthorizedClientManager(
                clientRegistrations, clientService
        );
        manager.setAuthorizedClientProvider(provider);
        return manager;
    }

     */
}

