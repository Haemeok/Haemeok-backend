package com.jdc.recipe_service.service.chat;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import io.github.bucket4j.Bandwidth;
import io.github.bucket4j.Bucket;
import io.github.bucket4j.Refill;
import org.springframework.stereotype.Service;

import java.time.Duration;

@Service
public class RateLimitService {

    private static final int CAPACITY_PER_MINUTE = 10;
    private static final int MAX_TRACKED_USERS = 100_000;

    private final Cache<Long, Bucket> userBuckets;

    public RateLimitService() {
        this.userBuckets = Caffeine.newBuilder()
                .expireAfterAccess(Duration.ofHours(1))
                .maximumSize(MAX_TRACKED_USERS)
                .build();
    }

    public void checkUserRate(Long userId) {
        Bucket bucket = userBuckets.get(userId, this::createBucket);
        if (bucket == null || !bucket.tryConsume(1)) {
            throw new CustomException(ErrorCode.CHAT_RATE_LIMITED);
        }
    }

    private Bucket createBucket(Long userId) {
        Bandwidth limit = Bandwidth.classic(
                CAPACITY_PER_MINUTE,
                Refill.intervally(CAPACITY_PER_MINUTE, Duration.ofMinutes(1))
        );
        return Bucket.builder().addLimit(limit).build();
    }
}
