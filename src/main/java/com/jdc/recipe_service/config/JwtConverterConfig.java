package com.jdc.recipe_service.config;

import com.jdc.recipe_service.domain.repository.UserRepository;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.convert.converter.Converter;
import org.springframework.security.authentication.AbstractAuthenticationToken;
import org.springframework.security.oauth2.jwt.Jwt;

@Configuration
public class JwtConverterConfig {

    private final UserRepository userRepository;

    public JwtConverterConfig(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    @Bean
    public Converter<Jwt, AbstractAuthenticationToken> jwtAuthenticationConverter() {
        return new CustomJwtAuthenticationConverter(userRepository);
    }
}