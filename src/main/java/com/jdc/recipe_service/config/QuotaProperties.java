package com.jdc.recipe_service.config;

import lombok.Getter; import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.time.ZoneId;

@Configuration
@ConfigurationProperties(prefix = "ai.quota")
@Getter @Setter
public class QuotaProperties {
    private int perDay = 2;
    private int youtubePerDay = 3;
    // dev V3 YouTube extraction quota (variable-cost: 2 또는 5). 기본 20.
    private int devYoutubePerDay = 20;
    private String timezone = "Asia/Seoul";
    public ZoneId zoneId() { return ZoneId.of(timezone); }
}
