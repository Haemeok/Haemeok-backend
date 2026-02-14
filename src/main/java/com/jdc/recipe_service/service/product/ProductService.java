package com.jdc.recipe_service.service.product;

import com.jdc.recipe_service.domain.dto.product.ProductResponseDto;
import com.jdc.recipe_service.domain.entity.credit.CreditProduct;
import com.jdc.recipe_service.domain.repository.credit.CreditProductRepository;
import com.jdc.recipe_service.domain.type.credit.CreditType;
import lombok.RequiredArgsConstructor;
import org.hashids.Hashids;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class ProductService {

    private final CreditProductRepository creditProductRepository;
    private final Hashids hashids;

    private static final String STORE_URL = "https://recipio.lemonsqueezy.com";

    public List<ProductResponseDto> getActiveProducts(Long userId) {
        List<CreditProduct> products = creditProductRepository.findAllByTypeAndIsActiveTrueOrderByCreditAmountAsc(CreditType.SUBSCRIPTION);

        String encodedUserId = hashids.encode(userId);

        return products.stream()
                .map(product -> {
                    String dynamicUrl = STORE_URL + "/checkout"
                            + "?cart=" + product.getLemonSqueezyVariantId() + ":1"
                            + "&checkout[custom][user_id]=" + encodedUserId;

                    return ProductResponseDto.from(product, dynamicUrl);
                })
                .toList();
    }
}