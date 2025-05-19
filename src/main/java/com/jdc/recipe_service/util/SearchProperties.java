package com.jdc.recipe_service.util;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@ConfigurationProperties(prefix = "search")
@Component
@Getter
@Setter
public class SearchProperties {
    private String engine;
}
