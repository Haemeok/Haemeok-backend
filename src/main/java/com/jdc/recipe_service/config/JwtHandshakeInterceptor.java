package com.jdc.recipe_service.config;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.lang.NonNull;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketHandler;
import org.springframework.web.socket.server.HandshakeInterceptor;
import org.springframework.http.server.ServerHttpRequest;
import org.springframework.http.server.ServerHttpResponse;
import org.springframework.http.server.ServletServerHttpRequest;

import jakarta.servlet.http.HttpServletRequest;
import java.util.Map;

@Slf4j
@Component
@RequiredArgsConstructor
public class JwtHandshakeInterceptor implements HandshakeInterceptor {

    private final RedisTemplate<String, String> redisTemplate;
    private final UserDetailsService userDetailsService;

    @Override
    public boolean beforeHandshake(@NonNull ServerHttpRequest request,
                                   @NonNull ServerHttpResponse response,
                                   @NonNull WebSocketHandler wsHandler,
                                   @NonNull Map<String, Object> attributes) {
        if (!(request instanceof ServletServerHttpRequest servlet)) {
            log.warn("Handshake failed: Not a servlet request.");
            return false;
        }

        HttpServletRequest httpReq = servlet.getServletRequest();
        String ticket = httpReq.getParameter("token");
        if (ticket == null) {
            log.warn("Handshake failed: Missing ticket parameter.");
            return false;
        }

        String username = redisTemplate.opsForValue().get(ticket);
        if (username != null && redisTemplate.delete(ticket)) {
            UserDetails user = userDetailsService.loadUserByUsername(username);
            Authentication auth = new UsernamePasswordAuthenticationToken(
                    user, null, user.getAuthorities());
            attributes.put("user", auth);
            log.info("Handshake successful for user: {}", username);
            return true;
        }

        log.warn("Handshake failed: Invalid or expired ticket {}", ticket);
        return false;
    }

    @Override
    public void afterHandshake(@NonNull ServerHttpRequest request,
                               @NonNull ServerHttpResponse response,
                               @NonNull WebSocketHandler wsHandler,
                               Exception exception) {
    }
}
