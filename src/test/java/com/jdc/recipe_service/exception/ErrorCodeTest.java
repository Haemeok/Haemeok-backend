package com.jdc.recipe_service.exception;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Map;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;

class ErrorCodeTest {

    @Test
    @DisplayName("ErrorCode code 값은 중복되지 않는다")
    void errorCodeValuesAreUnique() {
        Map<String, Long> countsByCode = Arrays.stream(ErrorCode.values())
                .collect(Collectors.groupingBy(ErrorCode::getCode, Collectors.counting()));

        assertThat(countsByCode)
                .allSatisfy((code, count) -> assertThat(count)
                        .as("duplicate error code: %s", code)
                        .isEqualTo(1));
    }
}
