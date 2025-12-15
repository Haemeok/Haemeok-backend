package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.log.LogRequestDto;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.ActionLogService;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/logs")
public class TrackingController {

    private final ActionLogService actionLogService;

    @PostMapping("/click")
    public ResponseEntity<String> trackClick(
            @RequestBody LogRequestDto requestDto,
            HttpServletRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        String ip = getClientIp(request);

        String userAgent = request.getHeader("User-Agent");

        Long loginUserId = null;
        if (userDetails != null) {
            loginUserId = userDetails.getId();
        }

        actionLogService.saveLog(
                requestDto.getAction(),
                requestDto.getUuid(),
                ip,
                userAgent,
                loginUserId
        );

        return ResponseEntity.ok("ok");
    }

    @GetMapping("/stats")
    @PreAuthorize("hasRole('ADMIN')")
    public String viewStats() {
        Map<String, Object> data = actionLogService.getDashboardData();

        long todayVisitors = (long) data.get("todayVisitors");
        long todayClicks = (long) data.get("todayClicks");
        List<Object[]> allStats = (List<Object[]>) data.get("allStats");

        StringBuilder html = new StringBuilder();
        html.append("<html><head><title>서비스 통계</title>");
        html.append("<style>");
        html.append("body { font-family: 'Pretendard', sans-serif; padding: 30px; background-color: #f8f9fa; }");
        html.append(".container { max-width: 800px; margin: 0 auto; }");
        html.append(".summary-card { background: white; padding: 20px; border-radius: 12px; box-shadow: 0 2px 10px rgba(0,0,0,0.05); margin-bottom: 30px; display: flex; justify-content: space-around; text-align: center; }");
        html.append(".stat-box { flex: 1; border-right: 1px solid #eee; }");
        html.append(".stat-box:last-child { border-right: none; }");
        html.append(".label { font-size: 14px; color: #666; margin-bottom: 5px; }");
        html.append(".number { font-size: 32px; font-weight: bold; color: #333; }");
        html.append(".highlight { color: #007bff; }");
        html.append("table { width: 100%; border-collapse: collapse; background: white; border-radius: 8px; overflow: hidden; box-shadow: 0 2px 10px rgba(0,0,0,0.05); }");
        html.append("th, td { padding: 15px; border-bottom: 1px solid #eee; text-align: center; }");
        html.append("th { background-color: #f1f3f5; font-weight: 600; }");
        html.append("h2 { color: #343a40; margin-bottom: 15px; }");
        html.append("</style></head><body>");

        html.append("<div class='container'>");
        html.append("<h1>📊 대시보드</h1>");

        html.append("<h2>📅 오늘의 현황</h2>");
        html.append("<div class='summary-card'>");
        html.append(String.format("<div class='stat-box'><div class='label'>오늘 방문자</div><div class='number highlight'>%d명</div></div>", todayVisitors));
        html.append(String.format("<div class='stat-box'><div class='label'>오늘 클릭수</div><div class='number'>%d회</div></div>", todayClicks));
        html.append("</div>");

        html.append("<h2>📈 전체 누적 통계</h2>");
        html.append("<table>");
        html.append("<tr><th>Action 이름</th><th>누적 순방문자</th><th>누적 클릭수</th></tr>");

        for (Object[] row : allStats) {
            html.append(String.format("<tr><td>%s</td><td style='color:#007bff; font-weight:bold;'>%d</td><td>%d</td></tr>",
                    row[0], row[1], row[2]));
        }

        html.append("</table>");
        html.append("<br><button onclick='location.reload()' style='padding:12px 24px; cursor:pointer; background:#333; color:white; border:none; border-radius:6px;'>새로고침 🔄</button>");
        html.append("</div></body></html>");

        return html.toString();
    }

    private String getClientIp(HttpServletRequest request) {
        String ip = request.getHeader("X-Forwarded-For");
        if (ip == null || ip.length() == 0 || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getHeader("Proxy-Client-IP");
        }
        if (ip == null || ip.length() == 0 || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getHeader("WL-Proxy-Client-IP");
        }
        if (ip == null || ip.length() == 0 || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getHeader("HTTP_CLIENT_IP");
        }
        if (ip == null || ip.length() == 0 || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getHeader("HTTP_X_FORWARDED_FOR");
        }
        if (ip == null || ip.length() == 0 || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getRemoteAddr();
        }
        return ip;
    }
}