package com.jdc.recipe_service.config;

import org.springframework.http.server.ServerHttpRequest;
import org.springframework.lang.NonNull;
import org.springframework.security.core.Authentication;
import org.springframework.web.socket.WebSocketHandler;
import org.springframework.web.socket.server.support.DefaultHandshakeHandler;

import java.security.Principal;
import java.util.Map;

public class PrincipalHandshakeHandler extends DefaultHandshakeHandler {
    @Override
    protected Principal determineUser(
            @NonNull ServerHttpRequest request,
            @NonNull WebSocketHandler wsHandler,
            @NonNull Map<String, Object> attributes
    ) {
        Object user = attributes.get("user");
        if (user instanceof Authentication) {
            return (Authentication) user;
        }
        return super.determineUser(request, wsHandler, attributes);
    }
}