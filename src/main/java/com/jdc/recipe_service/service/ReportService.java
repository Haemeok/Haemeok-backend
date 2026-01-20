package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.report.IngredientReportRequest;
import com.jdc.recipe_service.domain.dto.report.ReportResponse;
import com.jdc.recipe_service.domain.entity.RecipeIngredientReport;
import com.jdc.recipe_service.domain.repository.RecipeIngredientReportRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@RequiredArgsConstructor
public class ReportService {

    private final RecipeIngredientReportRepository reportRepository;

    @Transactional
    public void createReport(Long recipeId, Long ingredientId,Long memberId, IngredientReportRequest request) {

        RecipeIngredientReport report = RecipeIngredientReport.builder()
                .recipeId(recipeId)
                .ingredientId(ingredientId)
                .memberId(memberId)
                .reason(request.getReason())
                .userMemo(request.getMemo())
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