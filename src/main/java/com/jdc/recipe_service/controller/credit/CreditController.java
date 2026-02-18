package com.jdc.recipe_service.controller.credit;

import com.jdc.recipe_service.domain.dto.credit.CreditHistoryResponseDto;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.user.UserCreditService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/credits")
@RequiredArgsConstructor
public class CreditController {

    private final UserCreditService userCreditService;

    @GetMapping("/history")
    public ResponseEntity<Page<CreditHistoryResponseDto>> getMyCreditHistory(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @PageableDefault(size = 20) Pageable pageable
    ) {
        Long userId = userDetails.getUser().getId();
        return ResponseEntity.ok(userCreditService.getCreditHistories(userId, pageable));
    }
}