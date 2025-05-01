package com.jdc.recipe_service.config;

import com.jdc.recipe_service.jwt.JwtAuthenticationFilter;
import com.jdc.recipe_service.security.CustomAuthenticationEntryPoint;
import com.jdc.recipe_service.security.oauth.CustomOAuth2UserService;
import com.jdc.recipe_service.security.oauth.OAuth2AuthenticationSuccessHandler;
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
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;

import java.util.Arrays;

@Configuration
@EnableWebSecurity
@EnableMethodSecurity
@RequiredArgsConstructor
public class SecurityConfig {

    private final CustomOAuth2UserService oauth2UserService;
    private final OAuth2AuthenticationSuccessHandler successHandler;
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
                            // 1) 로컬 전용: H2 콘솔, JWT 발급용 엔드포인트
                            .requestMatchers("/h2-console/**", "/local-token").permitAll()

                            // 2) 공개 GET (인증 없이 모두 허용)
                            .requestMatchers(HttpMethod.GET,
                                    "/api/ingredients",
                                    "/api/recipes/*/comments",
                                    "/api/recipes/*/comments/*/replies",
                                    "/api/recipes/*",
                                    "/api/recipes/simple",
                                    "/api/recipes/search",
                                    "/api/recipes/by-tag",
                                    "/api/recipes/by-dish-type",
                                    "/api/dish-types",
                                    "/api/tags",
                                    "/api/users/*",
                                    "/api/users/*/recipes"
                            ).permitAll()

                            // 3) 보호된 GET (JWT 필요)
                            .requestMatchers(HttpMethod.GET,
                                    "/api/me",
                                    "/api/me/favorites",
                                    "/api/me/fridge/items"
                            ).authenticated()

                            // 4) 보호된 POST
                            .requestMatchers(HttpMethod.POST,
                                    "/api/me/fridge/items",
                                    "/api/recipes/*/comments",
                                    "/api/recipes/*/comments/*/replies",
                                    "/api/comments/*/like",
                                    "/api/recipes",
                                    "/api/recipes/*/presigned-urls",
                                    "/api/recipes/*/like",
                                    "/api/recipes/*/favorite",
                                    "/api/recipes/with-images",
                                    "/api/token/logout",
                                    "/api/me/fridge/items/bulk",
                                    "/api/recipes/user",
                                    "/api/recipes/user/with-images"
                            ).authenticated()

                            // 5) 보호된 PUT
                            .requestMatchers(HttpMethod.PUT,
                                    "/api/me",
                                    "/api/recipes/*",
                                    "/api/ratings/recipe/*",
                                    "/api/ingredients",
                                    "/api/recipes/*/images",
                                    "/api/recipes/user/*"
                            ).authenticated()

                            // 6) 보호된 DELETE
                            .requestMatchers(HttpMethod.DELETE,
                                    "/api/me",
                                    "/api/me/fridge/items/*",
                                    "/api/me/fridge/items/bulk",
                                    "/api/recipes/*/comments/*",
                                    "/api/recipes/*",
                                    "/api/ratings/recipe/*",
                                    "/api/ingredients"
                            ).authenticated()

                            // 7) 나머지 요청은 모두 공개
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
                .requiresChannel(ch -> ch.anyRequest().requiresSecure())

                .cors(cors -> cors.configurationSource(corsConfig()))
                .csrf(AbstractHttpConfigurer::disable)

                .authorizeHttpRequests(auth -> auth

                        .requestMatchers(HttpMethod.OPTIONS, "/**").permitAll()

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
                                "/api/opensearch/**"
                        ).permitAll()


                        // 2) GET 중 인증 필요
                        .requestMatchers(HttpMethod.GET,
                                "/api/me",
                                "/api/me/favorites",
                                "/api/me/fridge/items"
                        ).authenticated()

                        // 3) 읽기 전용 GET (모두 허용)
                        .requestMatchers(HttpMethod.GET,
                                "/api/ingredients",
                                "/api/recipes/*/comments",
                                "/api/recipes/*/comments/*/replies",
                                "/api/recipes/*",
                                "/api/recipes/simple",
                                "/api/recipes/search",
                                "/api/recipes/by-tag",
                                "/api/recipes/by-dish-type",
                                "/api/recipes/dish-types",
                                "/api/tags",
                                "/api/users/*",
                                "/api/users/*/recipes",
                                "/api/search/**"
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
                                "/api/recipes/with-images",
                                "/api/me/fridge/items/bulk",
                                "/api/recipes/user",
                                "/api/recipes/user/with-images",
                                "/api/token/logout"
                        ).authenticated()

                        // 5) 인증 필요 PUT
                        .requestMatchers(HttpMethod.PUT,
                                "/api/ingredients",
                                "/api/recipes/*",
                                "/api/ratings/recipe/*",
                                "/api/me",
                                "/api/recipes/*/images",
                                "/api/recipes/user/*"
                        ).authenticated()

                        // 6) 인증 필요 DELETE
                        .requestMatchers(HttpMethod.DELETE,
                                "/api/ingredients",
                                "/api/recipes/*/comments/*",
                                "/api/recipes/*",
                                "/api/ratings/recipe/*",
                                "/api/me",
                                "/api/me/fridge/items/*",
                                "/api/me/fridge/items/bulk"
                        ).authenticated()

                        // 7) 나머지 전부 차단
                        .anyRequest().denyAll()
                )
                .exceptionHandling(e -> e.authenticationEntryPoint(entryPoint))
                .formLogin(AbstractHttpConfigurer::disable)
                .oauth2Login(oauth -> oauth
                        .userInfoEndpoint(u -> u.userService(oauth2UserService))
                        .successHandler(successHandler)
                )
                .addFilterBefore(jwtFilter, UsernamePasswordAuthenticationFilter.class)
                .headers(headers -> headers.frameOptions(frame -> frame.disable()));
        return http.build();
    }

    @Bean
    public CorsConfigurationSource corsConfig() {
        CorsConfiguration cfg = new CorsConfiguration();
        cfg.setAllowedOrigins(Arrays.asList(
                "http://localhost:5173",
                "https://www.haemeok.com"
        ));
        cfg.setAllowedMethods(Arrays.asList("GET","POST","PUT","DELETE","OPTIONS"));
        cfg.setAllowedHeaders(Arrays.asList("*"));
        cfg.setAllowCredentials(true);

        UrlBasedCorsConfigurationSource src = new UrlBasedCorsConfigurationSource();
        src.registerCorsConfiguration("/**", cfg);
        return src;
    }
}
