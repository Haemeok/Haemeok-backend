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
import org.springframework.security.oauth2.client.registration.ClientRegistrationRepository;
import org.springframework.security.oauth2.client.web.DefaultOAuth2AuthorizedClientManager;
import org.springframework.security.oauth2.client.web.HttpSessionOAuth2AuthorizedClientRepository;
import org.springframework.security.oauth2.client.web.OAuth2AuthorizedClientRepository;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;

import java.util.Arrays;
import java.util.List;

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

                            .requestMatchers("/api/notifications/**").authenticated()
                            .requestMatchers("/api/notification-preferences/**").authenticated()

                            // 1) 로컬 전용: H2 콘솔, JWT 발급용 엔드포인트
                            .requestMatchers("/h2-console/**", "/local-token").permitAll()
                            .requestMatchers("/actuator/**").permitAll()
                            .requestMatchers(HttpMethod.POST, "/login/oauth2/code/**").permitAll()
                            .requestMatchers("/api/test/ai-recipe/**").permitAll()
                            // 2) 공개 GET (인증 없이 모두 허용)
                            .requestMatchers(HttpMethod.GET,
                                    "/api/ingredients",
                                    "/api/recipes/*/comments",
                                    "/api/recipes/*/comments/*/replies",
                                    "/api/recipes/*",
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
                                    "/api/recipes/budget"
                            ).permitAll()

                            // 3) 보호된 GET (JWT 필요)
                            .requestMatchers(HttpMethod.GET,
                                    "/api/me",
                                    "/api/me/favorites",
                                    "/api/me/fridge/items",
                                    "/api/me/fridge/items/ids",
                                    "/api/me/calendar/**",
                                    "/api/me/records/**",
                                    "/api/me/streak",
                                    "/api/me/survey",
                                    "/api/me/fridge/recipes/**",
                                    "/api/ratings/recipe/*/me",
                                    "/api/users/*/profile-image/presign"
                            ).authenticated()

                            // 4) 보호된 POST
                            .requestMatchers(HttpMethod.POST,
                                    "/api/me/fridge/items",
                                    "/api/me/records",
                                    "/api/me/survey",
                                    "/api/recipes/*/comments",
                                    "/api/recipes/*/comments/*/replies",
                                    "/api/comments/*/like",
                                    "/api/recipes",
                                    "/api/recipes/*/presigned-urls",
                                    "/api/recipes/*/like",
                                    "/api/recipes/*/favorite",
                                    "/api/ratings/recipe/*",
                                    "/api/token/logout",
                                    "/api/token/logout/all",
                                    "/api/me/fridge/items/bulk",
                                    "/api/recipes/*/private",
                                    "/api/recipes/*/finalize",
                                    "/api/ws-ticket"
                            ).authenticated()

                            // 5) 보호된 PUT
                            .requestMatchers(HttpMethod.PUT,
                                    "/api/me",
                                    "/api/recipes/*",
                                    "/api/ingredients",
                                    "/api/recipes/*/images",
                                    "/api/users/*"
                            ).authenticated()

                            // 6) 보호된 DELETE
                            .requestMatchers(HttpMethod.DELETE,
                                    "/api/me",
                                    "/api/me/fridge/items/*",
                                    "/api/me/fridge/items/bulk",
                                    "/api/recipes/*/comments/*",
                                    "/api/recipes/*",
                                    "/api/ratings/recipe/*",
                                    "/api/me/records/*",
                                    "/api/ingredients"
                            ).authenticated()

                            .requestMatchers(HttpMethod.PATCH,
                                    "/api/users/*",
                                    "/api/me"
                            ).authenticated()

                            // 7) 관리자용 API
                            .requestMatchers("/api/admin/**").hasRole("ADMIN")
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

                        // WebSocket 핸드쉐이크 & SockJS 엔드포인트
                        .requestMatchers("/ws/notifications/**").permitAll()

                        .requestMatchers("/api/notifications/**").authenticated()
                        .requestMatchers("/api/notification-preferences/**").authenticated()

                        // 1) 공개 엔드포인트
                        .requestMatchers(
                                "/v3/api-docs/**",
                                "/swagger-ui/**",
                                "/swagger-ui.html",
                                "/swagger-resources/**",
                                "/webjars/**",
                                "/api/token/refresh",
                                "/api/tags/**",
                                "/", "/oauth2/**", "/login/**", "/error",
                                "/h2-console/**",
                                "/api/opensearch/**",
                                "/api/test/ai-recipe/**"
                        ).permitAll()


                        // 2) GET 중 인증 필요
                        .requestMatchers(HttpMethod.GET,
                                "/api/me",
                                "/api/me/favorites",
                                "/api/me/fridge/items",
                                "/api/me/fridge/items/ids",
                                "/api/me/calendar/**",
                                "/api/me/records/**",
                                "/api/me/streak",
                                "/api/me/survey",
                                "/api/me/fridge/recipes/**",
                                "/api/ratings/recipe/*/me",
                                "/api/users/*/profile-image/presign"
                        ).authenticated()

                        // 3) 읽기 전용 GET (모두 허용)
                        .requestMatchers(HttpMethod.GET,
                                "/api/ingredients",
                                "/api/recipes/*/comments",
                                "/api/recipes/*/comments/*/replies",
                                "/api/recipes/*",
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
                                "/api/recipes/budget"
                        ).permitAll()

                        // [추가된 부분] POST /v2/recipes/status를 공개 허용
                        .requestMatchers(HttpMethod.POST,
                                "/api/v2/recipes/status"
                        ).permitAll()

                        // 4) 인증 필요 POST
                        .requestMatchers(HttpMethod.POST,
                                "/api/ingredients",
                                "/api/recipes/*/comments",
                                "/api/recipes/*/comments/*/replies",
                                "/api/comments/*/like",
                                "/api/recipes",
                                "/api/recipes/*/presigned-urls",
                                "/api/recipes/*/like",
                                "/api/recipes/*/favorite",
                                "/api/me/fridge/items",
                                "/api/me/fridge/items/bulk",
                                "/api/me/survey",
                                "/api/me/records",
                                "/api/ratings/recipe/*",
                                "/api/token/logout",
                                "/api/token/logout/all",
                                "/api/recipes/*/private",
                                "/api/recipes/*/finalize",
                                "/api/ws-ticket"
                        ).authenticated()

                        // 5) 인증 필요 PUT
                        .requestMatchers(HttpMethod.PUT,
                                "/api/ingredients",
                                "/api/recipes/*",
                                "/api/me",
                                "/api/recipes/*/images",
                                "/api/users/*"
                        ).authenticated()

                        // 6) 인증 필요 DELETE
                        .requestMatchers(HttpMethod.DELETE,
                                "/api/ingredients",
                                "/api/recipes/*/comments/*",
                                "/api/recipes/*",
                                "/api/ratings/recipe/*",
                                "/api/me",
                                "/api/me/records/*",
                                "/api/me/fridge/items/*",
                                "/api/me/fridge/items/bulk"
                        ).authenticated()
                        // 7) 인증 필요 PATCH
                        .requestMatchers(HttpMethod.PATCH,
                                "/api/users/*",
                                "/api/me"
                        ).authenticated()

                        // 8) 관리자용 API
                        .requestMatchers("/api/admin/**").hasRole("ADMIN")
                        .requestMatchers(HttpMethod.POST, "/api/recipes/*/analyze").hasRole("ADMIN")
                        .requestMatchers(HttpMethod.POST, "/api/recipes/*/nutrition").hasRole("ADMIN")
                        .requestMatchers("/api/test/recipes/**").hasRole("ADMIN")

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
                "https://recipio.kr"
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
        return new DefaultAuthorizationCodeTokenResponseClient();
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

