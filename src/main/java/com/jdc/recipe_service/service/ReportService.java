package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.report.IngredientReportRequest;
import com.jdc.recipe_service.domain.dto.report.ReportResponse;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.entity.RecipeIngredientReport;
import com.jdc.recipe_service.domain.repository.RecipeIngredientReportRepository;
import com.jdc.recipe_service.domain.repository.RecipeIngredientRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class ReportService {

    private final RecipeIngredientReportRepository reportRepository;
    private final RecipeIngredientRepository recipeIngredientRepository;

    @Transactional
    public void createReportByName(Long recipeId, Long memberId, IngredientReportRequest request) {

        List<RecipeIngredient> ingredients = recipeIngredientRepository.findByRecipeId(recipeId);

        String targetName = request.getIngredientName().trim();

        Optional<RecipeIngredient> match = ingredients.stream()
                .filter(ri -> {
                    String name = (ri.getIngredient() != null)
                            ? ri.getIngredient().getName()
                            : ri.getCustomName();
                    return name != null && name.trim().equalsIgnoreCase(targetName);
                })
                .findFirst();

        Long matchedId = null;
        if (match.isPresent()) {
            RecipeIngredient target = match.get();
            matchedId = (target.getIngredient() != null)
                    ? target.getIngredient().getId()
                    : target.getId();
        }

        RecipeIngredientReport report = RecipeIngredientReport.builder()
                .recipeId(recipeId)
                .ingredientId(matchedId)
                .memberId(memberId)
                .reason(request.getReason())
                .userMemo(request.getMemo())
                .proposedName(request.getIngredientName())
                .build();

        reportRepository.save(report);
    }

    @Transactional(readOnly = true)
    public List<ReportResponse> getAllReports(boolean onlyUnresolved) {
        List<RecipeIngredientReport> reports;

        if (onlyUnresolved) {
            reports = reportRepository.findByIsResolvedFalseOrderByCreatedAtDesc();
        } else {
            reports = reportRepository.findAll(Sort.by(Sort.Direction.DESC, "createdAt"));
        }

        return reports.stream()
                .map(ReportResponse::from)
                .toList();
    }
}