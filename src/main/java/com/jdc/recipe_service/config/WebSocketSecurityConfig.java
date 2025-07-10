package com.jdc.recipe_service.config;

import org.springframework.context.annotation.Configuration;
import org.springframework.messaging.simp.SimpMessageType;
import org.springframework.security.config.annotation.web.messaging.MessageSecurityMetadataSourceRegistry;
import org.springframework.security.config.annotation.web.socket.AbstractSecurityWebSocketMessageBrokerConfigurer;
import org.springframework.security.config.annotation.web.socket.EnableWebSocketSecurity;

@Configuration
@EnableWebSocketSecurity
public class WebSocketSecurityConfig extends AbstractSecurityWebSocketMessageBrokerConfigurer {
    @Override
    protected void configureInbound(MessageSecurityMetadataSourceRegistry messages) {
        messages
                .simpTypeMatchers(
                        SimpMessageType.CONNECT,
                        SimpMessageType.HEARTBEAT,
                        SimpMessageType.UNSUBSCRIBE,
                        SimpMessageType.DISCONNECT
                ).permitAll()
                .simpTypeMatchers(SimpMessageType.SUBSCRIBE, SimpMessageType.MESSAGE)
                .authenticated()
                .simpSubscribeDestMatchers("/user/queue/**").authenticated()
                .simpDestMatchers("/app/**").authenticated()
                .anyMessage().denyAll();
    }
    @Override protected boolean sameOriginDisabled() { return true; }
}
