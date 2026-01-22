package com.jdc.recipe_service.security.oauth;

import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.bouncycastle.asn1.pkcs.PrivateKeyInfo;
import org.bouncycastle.openssl.PEMParser;
import org.bouncycastle.openssl.jcajce.JcaPEMKeyConverter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.io.StringReader;
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
        log.info("🍎 [애플 설정 확인] TeamID=[{}], KeyID=[{}], ClientID=[{}]", teamId, keyId, clientId);
        if (privateKeyPem == null || privateKeyPem.isBlank()) {
            log.error("🍎 [치명적 오류] Private Key가 비어있습니다!");
        } else {
            log.info("🍎 [키 로딩 성공] Private Key 길이: {}", privateKeyPem.length());
        }
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
            log.info("🍎 [키 정제 완료] 헤더 제거 후 길이: {}", cleanKey.length());

            byte[] decodedKey = java.util.Base64.getDecoder().decode(cleanKey);
            log.info("🍎 [Base64 디코딩 완료] 바이트 길이: {}", decodedKey.length);

            java.security.KeyFactory keyFactory = java.security.KeyFactory.getInstance("EC");
            PrivateKey privateKey = keyFactory.generatePrivate(new java.security.spec.PKCS8EncodedKeySpec(decodedKey));

            log.info("🍎 [PrivateKey 객체 생성 성공] 알고리즘: {}, 포맷: {}", privateKey.getAlgorithm(), privateKey.getFormat());

            return privateKey;

        } catch (Exception e) {
            log.error("🍎 [키 파싱 대실패] 이유: {}", e.getMessage());
            throw new RuntimeException("Apple Private Key Parsing Error", e);
        }
    }
}