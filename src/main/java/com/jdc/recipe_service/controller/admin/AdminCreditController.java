package com.jdc.recipe_service.controller.admin;

import com.jdc.recipe_service.service.credit.CreditBatchService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/admin")
@RequiredArgsConstructor
public class AdminCreditController {

    private final CreditBatchService creditBatchService;

    @PostMapping("/grant-all-welcome")
    public String grantWelcomeCreditToAllUsers() {
        return creditBatchService.grantWelcomeCreditToAllUsers();
    }
}