package com.jdc.recipe_service.testsupport;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;

public record ChatTestCase(
        int seq,
        String recipe,
        String category,
        String expected,
        String question,
        String miniLabel,
        String answer,
        boolean classifiedCorrectly
) {

    private static final String CSV_PATH = "/test-data/test_results_pipeline_200.csv";

    public static List<ChatTestCase> loadAll() {
        InputStream is = ChatTestCase.class.getResourceAsStream(CSV_PATH);
        Objects.requireNonNull(is, "CSV not found on classpath: " + CSV_PATH);

        try (Reader reader = new InputStreamReader(is, StandardCharsets.UTF_8);
             BufferedReader br = new BufferedReader(reader)) {

            br.mark(1);
            int first = br.read();
            if (first != 0xFEFF) {
                br.reset();
            }

            CSVFormat format = CSVFormat.DEFAULT.builder()
                    .setHeader()
                    .setSkipHeaderRecord(true)
                    .setIgnoreEmptyLines(true)
                    .build();

            try (CSVParser parser = new CSVParser(br, format)) {
                return parser.getRecords().stream()
                        .map(ChatTestCase::fromRecord)
                        .toList();
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    private static ChatTestCase fromRecord(CSVRecord r) {
        return new ChatTestCase(
                Integer.parseInt(r.get("번호").trim()),
                r.get("레시피"),
                r.get("카테고리"),
                r.get("예상"),
                r.get("질문"),
                r.get("Mini분류"),
                r.get("답변"),
                "True".equalsIgnoreCase(r.get("분류정확").trim())
        );
    }
}
