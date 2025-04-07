package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.type.TagType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@RestController
public class TagController {

    @GetMapping("/api/tags")
    public List<Map<String, String>> getAllTags() {
        return Arrays.stream(TagType.values())
                .map(tag -> Map.of(
                        "code", tag.name(),
                        "name", tag.getDisplayName()
                ))
                .collect(Collectors.toList());
    }
}
