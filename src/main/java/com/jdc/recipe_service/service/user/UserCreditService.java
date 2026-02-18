package com.jdc.recipe_service.service.user;

import com.jdc.recipe_service.domain.dto.credit.CreditHistoryResponseDto;
import com.jdc.recipe_service.domain.entity.credit.CreditHistory;
import com.jdc.recipe_service.domain.entity.credit.CreditProduct;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.entity.user.UserCredit;
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
     * [사용] 크레딧 차감 (대량 차감 지원 + 중복 방지 + 추적성)
     * usage: 차감할 크레딧 양
     * refType: 사용처 구분 (예: "RECIPE_GEN")
     * refId: 사용처 ID (예: 1023)
     * transactionId: 중복 방지 키
     */
    @Transactional
    public void useCredit(Long userId, int usage, String refType, Long refId, String transactionId) {
        if (transactionId != null && creditHistoryRepository.existsByTransactionId(transactionId)) {
            log.warn("♻️ 이미 처리된 크레딧 차감 요청입니다. (TxId={})", transactionId);
            return;
        }

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));

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
                user,
                -usage,
                CreditTransactionType.USE,
                usage + " 크레딧 사용 (" + refType + ":" + refId + ")",
                transactionId,
                refType,
                refId
        );
    }

    @Transactional
    public void refundCredit(Long userId, int amount, String customDesc, String refType, Long refId) {
        User user = userRepository.findById(userId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));

        String txId = "REFUND_" + refType + "_" + refId + "_" + System.currentTimeMillis();
        UserCredit credit = UserCredit.builder()
                .user(user)
                .creditType(CreditType.BONUS)
                .amount(amount)
                .originalAmount(amount)
                .transactionId(txId)
                .expiresAt(LocalDateTime.now().plusDays(30))
                .build();
        userCreditRepository.save(credit);

        saveHistory(
                user,
                amount,
                CreditTransactionType.REFUND,
                customDesc,
                txId,
                refType,
                refId
        );

        log.info("↺ 크레딧 환불 처리 완료: UserID={}, Amount={}, Reason={}", userId, amount, customDesc);
    }
    /**
     * [지급 V1] 만료일 직접 지정
     */
    @Transactional
    public void grantCredit(User user, CreditType type, int amount, LocalDateTime expiresAt, String transactionId) {
        if (transactionId != null && userCreditRepository.findByTransactionId(transactionId).isPresent()) {
            log.warn("♻️ 이미 지급된 트랜잭션입니다. (TxId={})", transactionId);
            return;
        }

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

        saveHistory(user, amount, txType, type.getDescription() + " 지급", transactionId, null, null);

        log.info("💰 크레딧 지급: UserID={}, Type={}, Amount={}, Expires={}", user.getId(), type, amount, expiresAt);
    }

    /**
     * [지급 V2] 일수 지정 (이벤트/수동지급용)
     */
    @Transactional
    public void grantCredit(User user, CreditType type, int amount, int validDays, String transactionId) {
        LocalDateTime expiresAt = (type == CreditType.PAID)
                ? LocalDateTime.now().plusYears(5)
                : LocalDateTime.now().plusDays(validDays);

        grantCredit(user, type, amount, expiresAt, transactionId);
    }

    /**
     * [신규] 회원가입 축하 무료 크레딧 지급
     */
    @Transactional
    public void grantWelcomeCredit(User user) {
        CreditProduct welcomeProduct = creditProductRepository.findByName("WELCOME_GIFT")
                .orElse(null);

        int amount = (welcomeProduct != null) ? welcomeProduct.getCreditAmount() : 10;
        int days   = (welcomeProduct != null) ? welcomeProduct.getValidDays() : 365;

        grantCredit(user, CreditType.BASIC, amount, days, "WELCOME_GIFT_" + user.getId());

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

        grantCredit(newUser, CreditType.BONUS, 3, 30, "WELCOME_BONUS_" + newUserId);

        if (inviter.getMonthlyInviteCount() < 5) {
            grantCredit(inviter, CreditType.BONUS, 3, 30, "REFERRAL_REWARD_" + newUserId);
            inviter.increaseInviteCount();
        } else {
            log.info("🚫 초대 보상 한도 초과로 지급 건너뜀: UserID={}", inviterId);
        }
    }

    @Transactional
    public void processChallengePayback(Long userId, Long recipeId) {
        User user = userRepository.findById(userId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));

        grantCredit(user, CreditType.BONUS, 1, 30, "CHALLENGE_PAYBACK_" + recipeId);
    }

    @Transactional(readOnly = true)
    public int getUserCreditBalance(Long userId) {
        return userCreditRepository.calculateTotalBalance(userId);
    }

    /**
     * [관리자용] 결제 환불 시 크레딧 회수
     */
    @Transactional
    public void revokeCredit(Long userId, String transactionId) {
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
                transactionId,
                "ADMIN_REVOKE",
                null
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

    private void saveHistory(User user, int amount, CreditTransactionType type, String desc,
                             String txId, String refType, Long refId) {

        int currentBalance = getUserCreditBalance(user.getId());

        CreditHistory history = CreditHistory.builder()
                .user(user)
                .amount(amount)
                .balanceAfter(currentBalance)
                .transactionType(type)
                .description(desc)
                .transactionId(txId)
                .referenceType(refType)
                .referenceId(refId)
                .build();

        creditHistoryRepository.save(history);
    }
}