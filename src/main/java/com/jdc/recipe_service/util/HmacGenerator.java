package com.jdc.recipe_service.util;

import org.apache.commons.codec.binary.Hex;
import org.springframework.stereotype.Component;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

@Component
public class HmacGenerator {

    private static final String ALGORITHM = "HmacSHA256";

    public String generate(String method, String uri, String secretKey, String accessKey) {
        String[] parts = uri.split("\\?");
        String path = parts[0];
        String query = parts.length > 1 ? parts[1] : "";

        SimpleDateFormat dateFormatGmt = new SimpleDateFormat("yyMMdd'T'HHmmss'Z'");
        dateFormatGmt.setTimeZone(TimeZone.getTimeZone("GMT"));
        String datetime = dateFormatGmt.format(new Date());

        String message = datetime + method + path + query;

        try {
            SecretKeySpec signingKey = new SecretKeySpec(secretKey.getBytes(StandardCharsets.UTF_8), ALGORITHM);
            Mac mac = Mac.getInstance(ALGORITHM);
            mac.init(signingKey);
            byte[] rawHmac = mac.doFinal(message.getBytes(StandardCharsets.UTF_8));
            String signature = Hex.encodeHexString(rawHmac);

            return String.format("CEA algorithm=%s, access-key=%s, signed-date=%s, signature=%s",
                    "HmacSHA256", accessKey, datetime, signature);
        } catch (Exception e) {
            throw new RuntimeException("HMAC 서명 생성 실패", e);
        }
    }
}