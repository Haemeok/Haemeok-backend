package com.jdc.recipe_service.controller.product;

import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.product.SubscriptionService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;

@RestController
@RequestMapping("/api/subscriptions")
@RequiredArgsConstructor
public class SubscriptionController {

    private final SubscriptionService subscriptionService;

    @PostMapping("/upgrade")
    public ResponseEntity<Void> upgradeSubscription(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @RequestBody Map<String, String> request
    ) {
        Long userId = userDetails.getUser().getId();

        String newVariantId = request.get("variantId");
        subscriptionService.upgradeSubscription(userId, newVariantId);
        return ResponseEntity.ok().build();
    }
}