package com.jdc.recipe_service.config;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import org.hashids.Hashids;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.MethodParameter;
import org.springframework.web.bind.support.WebDataBinderFactory;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.method.support.ModelAndViewContainer;
import org.springframework.web.servlet.HandlerMapping;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import java.io.IOException;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.List;
import java.util.Map;

@Configuration
public class HashIdConfig implements WebMvcConfigurer {

    @Value("${app.hashids.salt}")
    private String salt;

    @Value("${app.hashids.min-length:8}")
    private int minLength;

    private static Hashids staticHashids;

    @Bean
    public Hashids hashids() {
        Hashids instance = new Hashids(salt, minLength);
        staticHashids = instance;
        return instance;
    }

    @Override
    public void addArgumentResolvers(List<HandlerMethodArgumentResolver> resolvers) {
        resolvers.add(new HandlerMethodArgumentResolver() {
            @Override
            public boolean supportsParameter(MethodParameter parameter) {
                return parameter.hasParameterAnnotation(DecodeId.class) && parameter.getParameterType().equals(Long.class);
            }

            @Override
            public Object resolveArgument(MethodParameter parameter, ModelAndViewContainer mavContainer,
                                          NativeWebRequest webRequest, WebDataBinderFactory binderFactory) {

                DecodeId annotation = parameter.getParameterAnnotation(DecodeId.class);
                String paramName = parameter.getParameterName();

                if (annotation != null && !annotation.value().isEmpty()) {
                    paramName = annotation.value();
                }

                String value = null;

                Map<String, String> pathVars = (Map<String, String>) webRequest.getAttribute(
                        HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE, 0);

                if (pathVars != null && pathVars.containsKey(paramName)) {
                    value = pathVars.get(paramName);
                }
                else {
                    value = webRequest.getParameter(paramName);
                }

                if (value == null) return null;

                if (value.matches("^[0-9]+$")) return Long.valueOf(value);

                long[] decoded = staticHashids.decode(value);
                if (decoded.length == 0) throw new IllegalArgumentException("Invalid Hash ID: " + value);
                return decoded[0];
            }
        });
    }

    @Target(ElementType.PARAMETER)
    @Retention(RetentionPolicy.RUNTIME)
    public @interface DecodeId {
        String value() default "";
    }

    public static class HashIdSerializer extends JsonSerializer<Long> {
        @Override
        public void serialize(Long value, JsonGenerator gen, SerializerProvider serializers) throws IOException {
            if (value != null) gen.writeString(staticHashids.encode(value));
            else gen.writeNull();
        }
    }

    public static class HashIdDeserializer extends JsonDeserializer<Long> {
        @Override
        public Long deserialize(JsonParser p, DeserializationContext ctxt) throws IOException {
            String value = p.getText();
            if (value == null || value.isBlank()) return null;
            if (value.matches("^[0-9]+$")) return Long.valueOf(value);

            long[] decoded = staticHashids.decode(value);
            if (decoded.length == 0) throw new IllegalArgumentException("Invalid Hash ID: " + value);
            return decoded[0];
        }
    }

    /**
     * 엄격(strict) HashID 디코더. 숫자 문자열 fallback을 허용하지 않는다.
     *
     * <p>{@link HashIdDeserializer}는 마이그레이션 호환을 위해 raw 숫자 문자열도 받아주지만, 이건
     * 신규 API에서 의도치 않은 raw Long 입력을 통과시켜 정책을 우회하게 만든다.
     *
     * <p>큐레이션 아티클처럼 "처음부터 HashID만 받겠다"고 정한 신규 도메인의 request DTO에 사용한다.
     * 기존 도메인(레시피, 유저 등)은 lenient {@link HashIdDeserializer}를 그대로 쓴다.
     */
    public static class StrictHashIdDeserializer extends JsonDeserializer<Long> {
        @Override
        public Long deserialize(JsonParser p, DeserializationContext ctxt) throws IOException {
            String value = p.getText();
            if (value == null || value.isBlank()) return null;
            // 숫자만으로 된 입력은 거부 — raw Long을 그대로 박는 호출을 차단한다.
            if (value.matches("^[0-9]+$")) {
                throw new IllegalArgumentException("HashID는 문자열로만 입력해야 합니다 (숫자 입력 거부): " + value);
            }
            long[] decoded = staticHashids.decode(value);
            if (decoded.length == 0) {
                throw new IllegalArgumentException("Invalid Hash ID: " + value);
            }
            return decoded[0];
        }
    }
}
