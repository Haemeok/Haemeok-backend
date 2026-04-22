package com.jdc.recipe_service.util;

import java.util.regex.Pattern;

public final class LogSanitizer {

    private static final Pattern URL_API_KEY = Pattern.compile("([?&])key=[^&\\s\"]+");
    private static final Pattern BEARER_TOKEN = Pattern.compile("(?i)Bearer\\s+[A-Za-z0-9._\\-]+");
    private static final Pattern GOOG_API_KEY_HEADER = Pattern.compile("(?i)x-goog-api-key\\s*[:=]\\s*[^\\s,\"]+");

    private LogSanitizer() {}

    public static String mask(String raw) {
        if (raw == null) return "null";
        String s = URL_API_KEY.matcher(raw).replaceAll("$1key=***");
        s = BEARER_TOKEN.matcher(s).replaceAll("Bearer ***");
        s = GOOG_API_KEY_HEADER.matcher(s).replaceAll("x-goog-api-key: ***");
        return s;
    }
}
