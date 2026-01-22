package com.jdc.recipe_service.security.oauth;

import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.security.PrivateKey;
import java.util.Date;

@Slf4j
@Component
@RequiredArgsConstructor
public class AppleClientSecretGenerator {

    @Value("${apple.team-id}")
    private String teamId;

    @Value("${apple.key-id}")
    private String keyId;

    @Value("${apple.private-key}")
    private String privateKeyPem;

    @Value("${spring.security.oauth2.client.registration.apple.client-id}")
    private String clientId;

    public String createClientSecret() {
        Date now = new Date();
        Date expiration = new Date(now.getTime() + 3600000);

        return Jwts.builder()
                .setHeaderParam("kid", keyId)
                .setHeaderParam("alg", "ES256")
                .setIssuer(teamId)
                .setIssuedAt(now)
                .setExpiration(expiration)
                .setAudience("https://appleid.apple.com")
                .setSubject(clientId)
                .signWith(getPrivateKey(), SignatureAlgorithm.ES256)
                .compact();
    }

    private PrivateKey getPrivateKey() {
        try {
            String cleanKey = privateKeyPem
                    .replace("-----BEGIN PRIVATE KEY-----", "")
                    .replace("-----END PRIVATE KEY-----", "")
                    .replace("\\n", "")
                    .replaceAll("\\s+", "");

            byte[] decodedKey = java.util.Base64.getDecoder().decode(cleanKey);

            java.security.KeyFactory keyFactory = java.security.KeyFactory.getInstance("EC");
            PrivateKey privateKey = keyFactory.generatePrivate(new java.security.spec.PKCS8EncodedKeySpec(decodedKey));

            return privateKey;

        } catch (Exception e) {
            log.error("Failed to parse Apple private key", e);
            throw new RuntimeException("Apple Private Key Parsing Error", e);
        }
    }
}