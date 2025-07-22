package com.jdc.recipe_service.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

@RestController
@RequiredArgsConstructor
public class WebSocketTicketController {

    private final RedisTemplate<String, String> redisTemplate;

    @PostMapping("/api/ws-ticket")
    public ResponseEntity<Map<String, String>> issueWebSocketTicket(
            @AuthenticationPrincipal UserDetails user
    ) {
        String ticket = UUID.randomUUID().toString();
        redisTemplate.opsForValue()
                .set(ticket, user.getUsername(), 30, TimeUnit.SECONDS);
        return ResponseEntity.ok(Map.of("ticket", ticket));
    }
}
