package com.jdc.recipe_service.service.credit;

import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Slf4j
public class CreditBatchService {

    private final UserRepository userRepository;
    private final UserCreditService userCreditService;

    /**
     * 전 회원 웰컴 크레딧 일괄 지급 (배치 처리)
     * - 트랜잭션 분리를 위해 별도 서비스로 구성함
     */
    public String grantWelcomeCreditToAllUsers() {
        int pageNumber = 0;
        int pageSize = 100;
        int successCount = 0;
        int failCount = 0;

        long totalUsers = userRepository.count();
        log.info("🚀 [Batch] 웰컴 크레딧 일괄 지급 시작 (총 대상: {}명)", totalUsers);

        while (true) {
            Page<User> userPage = userRepository.findAll(
                    PageRequest.of(pageNumber, pageSize, Sort.by("id").ascending())
            );

            if (!userPage.hasContent()) break;

            for (User user : userPage.getContent()) {
                try {
                    userCreditService.grantWelcomeCredit(user);
                    successCount++;
                } catch (Exception e) {
                    failCount++;
                    log.error("❌ [Batch] 지급 실패 UserID={}: {}", user.getId(), e.getMessage());
                }
            }

            log.info("⏳ [Batch] 진행 중... {}페이지 완료 (성공: {}, 실패: {})", pageNumber + 1, successCount, failCount);
            pageNumber++;
        }

        log.info("✅ [Batch] 작업 완료! (성공: {}, 실패: {})", successCount, failCount);
        return String.format("총 %d명 중 성공 %d명, 실패 %d명", totalUsers, successCount, failCount);
    }
}