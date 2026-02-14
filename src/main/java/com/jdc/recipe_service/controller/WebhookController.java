package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.service.credit.LemonSqueezyWebhookService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/webhooks")
@RequiredArgsConstructor
public class WebhookController {

    private final LemonSqueezyWebhookService webhookService;

    @PostMapping("/lemonsqueezy")
    public ResponseEntity<Void> handleLemonSqueezyWebhook(
            @RequestHeader("X-Signature") String signature,
            @RequestHeader("X-Event-Name") String eventName,
            @RequestBody String payload
    ) {
        webhookService.validateWebhookSignature(signature, payload);

        webhookService.handleWebhookEvent(eventName, payload);

        return ResponseEntity.ok().build();
    }
}