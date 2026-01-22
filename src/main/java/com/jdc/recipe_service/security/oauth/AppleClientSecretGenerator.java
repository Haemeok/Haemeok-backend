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
        try (StringReader stringReader = new StringReader(privateKeyPem.replace("\\n", "\n"));
             PEMParser pemParser = new PEMParser(stringReader)) {

            Object object = pemParser.readObject();
            JcaPEMKeyConverter converter = new JcaPEMKeyConverter();

            if (object instanceof PrivateKeyInfo) {
                return converter.getPrivateKey((PrivateKeyInfo) object);
            }
            throw new IllegalArgumentException("Invalid Private Key format");
        } catch (Exception e) {
            log.error("Failed to parse Apple private key", e);
            throw new RuntimeException("Apple Private Key Parsing Error", e);
        }
    }
}