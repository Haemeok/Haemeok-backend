package com.jdc.recipe_service.service.credit;

import com.jdc.recipe_service.domain.dto.credit.CreditHistoryResponseDto;
import com.jdc.recipe_service.domain.entity.credit.CreditHistory;
import com.jdc.recipe_service.domain.entity.credit.CreditProduct;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.entity.UserCredit;
import com.jdc.recipe_service.domain.repository.credit.CreditHistoryRepository;
import com.jdc.recipe_service.domain.repository.credit.CreditProductRepository;
import com.jdc.recipe_service.domain.repository.user.UserCreditRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.domain.type.credit.CreditTransactionType;
import com.jdc.recipe_service.domain.type.credit.CreditType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

@Service
@RequiredArgsConstructor
@Slf4j
public class UserCreditService {

    private final UserCreditRepository userCreditRepository;
    private final UserRepository userRepository;
    private final CreditHistoryRepository creditHistoryRepository;
    private final CreditProductRepository creditProductRepository;

    /**
     * [사용] 크레딧 차감 (대량 차감 지원)
     * usage: 차감할 크레딧 양 (예: 1, 3, 5)
     * 우선순위(구독->보너스->무료->유료)대로 여러 티켓에서 순차 차감합니다.
     */
    @Transactional
    public void useCredit(Long userId, int usage) {
        if (usage <= 0) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "차감할 크레딧은 0보다 커야 합니다.");
        }

        List<UserCredit> credits = userCreditRepository.findUseableCredits(userId, PageRequest.of(0, 100));

        if (credits.isEmpty()) {
            throw new CustomException(ErrorCode.PAYMENT_REQUIRED, "사용 가능한 크레딧이 없습니다.");
        }

        int remainingUsage = usage;

        for (UserCredit credit : credits) {
            if (remainingUsage <= 0) break;

            int available = credit.getAmount();

            if (available >= remainingUsage) {
                credit.use(remainingUsage);
                remainingUsage = 0;
            } else {
                credit.use(available);
                remainingUsage -= available;
            }
        }

        if (remainingUsage > 0) {
            throw new CustomException(ErrorCode.PAYMENT_REQUIRED, "크레딧 잔액이 부족합니다. (부족분: " + remainingUsage + ")");
        }

        saveHistory(
                credits.get(0).getUser(),
                -usage,
                CreditTransactionType.USE,
                usage + " 크레딧 사용",
                null
        );
    }

    @Transactional
    public void refundCredit(Long userId, int amount) {
        User user = userRepository.findById(userId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));

        grantCredit(user, CreditType.BONUS, amount, 30, "SYSTEM_REFUND_AI_FAILURE");

        log.info("↺ 크레딧 환불 완료: UserID={}, Amount={}", userId, amount);
    }

    /**
     * [지급 V1] 만료일 직접 지정 (Webhook용)
     * - 레몬스퀴즈가 계산해준 정확한 '다음 갱신일(renews_at)'을 사용하기 위해 필요함.
     * - 30일/31일/윤달 여부를 백엔드에서 계산하지 않고 PG사 기준을 따름.
     */
    @Transactional
    public void grantCredit(User user, CreditType type, int amount, LocalDateTime expiresAt, String transactionId) {
        UserCredit credit = UserCredit.builder()
                .user(user)
                .creditType(type)
                .amount(amount)
                .originalAmount(amount)
                .transactionId(transactionId)
                .expiresAt(expiresAt)
                .build();

        userCreditRepository.save(credit);

        CreditTransactionType txType = switch (type) {
            case PAID -> CreditTransactionType.CHARGE;
            case SUBSCRIPTION -> CreditTransactionType.SUBSCRIPTION;
            case BONUS, BASIC -> CreditTransactionType.BONUS;
        };

        saveHistory(user, amount, txType, type.getDescription() + " 지급", transactionId);
        log.info("💰 크레딧 지급: UserID={}, Type={}, Amount={}, Expires={}", user.getId(), type, amount, expiresAt);
    }

    /**
     * [지급 V2] 일수 지정 (이벤트/수동지급용)
     * - 기존 코드를 유지하기 위한 오버로딩 메서드
     */
    @Transactional
    public void grantCredit(User user, CreditType type, int amount, int validDays, String transactionId) {
        LocalDateTime expiresAt = (type == CreditType.PAID)
                ? LocalDateTime.now().plusYears(5)
                : LocalDateTime.now().plusDays(validDays);

        grantCredit(user, type, amount, expiresAt, transactionId);
    }

    /**
     * [신규] 회원가입 축하 무료 크레딧 지급 (BASIC)
     * - 가입 직후 Controller에서 호출
     */
    @Transactional
    public void grantWelcomeCredit(User user) {
        CreditProduct welcomeProduct = creditProductRepository.findByName("WELCOME_GIFT")
                .orElse(null);

        int amount = (welcomeProduct != null) ? welcomeProduct.getCreditAmount() : 10;
        int days   = (welcomeProduct != null) ? welcomeProduct.getValidDays() : 365;

        grantCredit(user, CreditType.BASIC, amount, days, "WELCOME_GIFT");

        log.info("🎁 신규 가입 축하금 지급: User={}, Amount={}", user.getId(), amount);
    }

    @Transactional(readOnly = true)
    public Page<CreditHistoryResponseDto> getCreditHistories(Long userId, Pageable pageable) {
        return creditHistoryRepository.findAllByUserIdOrderByCreatedAtDesc(userId, pageable)
                .map(CreditHistoryResponseDto::from);
    }

    /**
     * [마케팅] 친구 초대 보상 처리
     */
    @Transactional
    public void processReferralReward(Long inviterId, Long newUserId) {
        User inviter = userRepository.findById(inviterId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));
        User newUser = userRepository.findById(newUserId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));

        grantCredit(newUser, CreditType.BONUS, 3, 30, "WELCOME_BONUS");

        if (inviter.getMonthlyInviteCount() < 5) {
            grantCredit(inviter, CreditType.BONUS, 3, 30, "REFERRAL_REWARD_" + newUserId);
            inviter.increaseInviteCount();
        } else {
            log.info("🚫 초대 보상 한도 초과로 지급 건너뜀: UserID={}", inviterId);
        }
    }

    /**
     * [챌린지] 요리 인증 시 페이백
     */
    @Transactional
    public void processChallengePayback(Long userId, Long recipeId) {
        User user = userRepository.findById(userId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));

        grantCredit(user, CreditType.BONUS, 1, 30, "CHALLENGE_PAYBACK_" + recipeId);
    }

    /**
     * [조회] 유저의 총 보유 크레딧 (화면 표시용)
     */
    @Transactional(readOnly = true)
    public int getUserCreditBalance(Long userId) {
        return userCreditRepository.calculateTotalBalance(userId);
    }

    /**
     * [관리자용] 결제 환불 시 크레딧 회수
     * - Order ID로 정확하게 찾아서 회수 (최적화 적용됨)
     */
    @Transactional
    public void revokeCredit(Long userId, String transactionId) {
        // DB에서 바로 조회 (Stream 필터링 제거로 성능 향상)
        UserCredit targetCredit = userCreditRepository.findByTransactionId(transactionId)
                .orElseThrow(() -> new CustomException(ErrorCode.RESOURCE_NOT_FOUND, "해당 결제 건으로 지급된 크레딧이 없습니다."));

        if (!targetCredit.getUser().getId().equals(userId)) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "해당 유저의 결제 건이 아닙니다.");
        }

        int revokeAmount = targetCredit.getAmount();

        if (revokeAmount <= 0) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "이미 모두 사용한 크레딧이라 회수할 수 없습니다.");
        }

        targetCredit.use(revokeAmount);

        saveHistory(
                targetCredit.getUser(),
                -revokeAmount,
                CreditTransactionType.REFUND,
                "관리자 직권 환불/회수 (결제ID: " + transactionId + ")",
                transactionId
        );

        log.info("👮‍♂️ 관리자 환불 처리 완료: UserID={}, TransactionID={}, 회수량={}", userId, transactionId, revokeAmount);
    }

    @Transactional(readOnly = true)
    public Map<String, Integer> getCreditSummary(Long userId) {
        List<UserCredit> credits = userCreditRepository.findUseableCredits(userId, PageRequest.of(0, 1000));

        int subAmount = credits.stream()
                .filter(c -> c.getCreditType() == CreditType.SUBSCRIPTION)
                .mapToInt(UserCredit::getAmount)
                .sum();

        int cashAmount = credits.stream()
                .filter(c -> c.getCreditType() != CreditType.SUBSCRIPTION)
                .mapToInt(UserCredit::getAmount)
                .sum();

        return Map.of("subscription", subAmount, "cash", cashAmount);
    }

    private void saveHistory(User user, int amount, CreditTransactionType type, String desc, String txId) {
        int currentBalance = getUserCreditBalance(user.getId());

        CreditHistory history = CreditHistory.builder()
                .user(user)
                .amount(amount)
                .balanceAfter(currentBalance)
                .transactionType(type)
                .description(desc)
                .transactionId(txId)
                .build();

        creditHistoryRepository.save(history);
    }
}