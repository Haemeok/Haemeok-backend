package com.jdc.recipe_service.controller.product;

import com.jdc.recipe_service.domain.dto.product.ProductResponseDto;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.product.ProductService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequestMapping("/api/products")
@RequiredArgsConstructor
public class ProductController {

    private final ProductService productService;

    @GetMapping
    public ResponseEntity<List<ProductResponseDto>> getProducts(
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long userId = userDetails.getUser().getId();

        return ResponseEntity.ok(productService.getActiveProducts(userId));
    }
}