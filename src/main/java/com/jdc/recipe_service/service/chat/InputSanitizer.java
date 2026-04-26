package com.jdc.recipe_service.service.chat;

import java.util.regex.Pattern;

public final class InputSanitizer {

    private static final Pattern HTML_TAG = Pattern.compile("<[^>]+>");
    private static final Pattern WHITESPACE = Pattern.compile("\\s+");

    private InputSanitizer() {}

    public static String sanitize(String input) {
        if (input == null) return null;
        String cleaned = HTML_TAG.matcher(input).replaceAll("");
        cleaned = WHITESPACE.matcher(cleaned).replaceAll(" ").trim();
        return cleaned;
    }
}
