package com.jdc.recipe_service.service.credit;

import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.entity.user.UserCredit;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.domain.repository.credit.CreditHistoryRepository;
import com.jdc.recipe_service.domain.repository.credit.CreditProductRepository;
import com.jdc.recipe_service.domain.repository.user.UserCreditRepository;
import com.jdc.recipe_service.domain.type.credit.CreditType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.user.UserCreditService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageRequest;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class UserCreditServiceTest {

    @InjectMocks
    private UserCreditService userCreditService;

    @Mock
    private UserCreditRepository userCreditRepository;
    @Mock
    private UserRepository userRepository;
    @Mock
    private CreditHistoryRepository creditHistoryRepository;
    @Mock
    private CreditProductRepository creditProductRepository;

    @Test
    @DisplayName("성공: 구독(기간제) 크레딧이 유료 크레딧보다 먼저 차감되어야 한다.")
    void useCredit_Priority_Success() {
        // given
        Long userId = 1L;
        User user = User.builder().id(userId).build();

        // [New] 유저 조회 Mocking 필요
        when(userRepository.findById(userId)).thenReturn(Optional.of(user));
        // [New] 중복 거래 아님 설정
        when(creditHistoryRepository.existsByTransactionId(any())).thenReturn(false);

        // 상황: 구독 크레딧 3개, 유료 크레딧 5개 보유 중
        UserCredit subCredit = UserCredit.builder()
                .user(user).creditType(CreditType.SUBSCRIPTION).amount(3).originalAmount(3)
                .expiresAt(LocalDateTime.now().plusDays(5)).build();

        UserCredit paidCredit = UserCredit.builder()
                .user(user).creditType(CreditType.PAID).amount(5).originalAmount(5)
                .expiresAt(LocalDateTime.now().plusYears(1)).build();

        // Repository는 정렬된 순서대로 반환한다고 가정
        when(userCreditRepository.findUseableCredits(eq(userId), any(PageRequest.class)))
                .thenReturn(List.of(subCredit, paidCredit));

        when(userCreditRepository.calculateTotalBalance(userId)).thenReturn(4); // 사용 후 잔액 가정

        // when: 4개를 사용함 (파라미터 추가됨)
        userCreditService.useCredit(userId, 4, "TEST_REF", 100L, "TX_12345");

        // then
        // 1. 구독 크레딧(3개)은 다 써서 0이 되어야 함
        assertThat(subCredit.getAmount()).isEqualTo(0);

        // 2. 유료 크레딧(5개)은 1개가 차감되어 4개가 남아야 함
        assertThat(paidCredit.getAmount()).isEqualTo(4);

        // 3. 히스토리가 저장되었는지 확인
        verify(creditHistoryRepository, times(1)).save(any());
    }

    @Test
    @DisplayName("실패: 잔액이 부족하면 예외가 발생해야 한다.")
    void useCredit_Insufficient_Balance() {
        // given
        Long userId = 1L;
        User user = User.builder().id(userId).build();

        when(userRepository.findById(userId)).thenReturn(Optional.of(user));
        when(creditHistoryRepository.existsByTransactionId(any())).thenReturn(false);

        UserCredit bonusCredit = UserCredit.builder()
                .user(user).creditType(CreditType.BONUS).amount(2).build();

        when(userCreditRepository.findUseableCredits(eq(userId), any(PageRequest.class)))
                .thenReturn(List.of(bonusCredit));

        // when & then: 5개를 쓰려고 하면 에러 발생
        assertThatThrownBy(() -> userCreditService.useCredit(userId, 5, "TEST", 1L, "TX_FAIL"))
                .isInstanceOf(CustomException.class)
                .hasFieldOrPropertyWithValue("errorCode", ErrorCode.PAYMENT_REQUIRED);
    }

    @Test
    @DisplayName("실패: 가진 크레딧이 아예 없으면 예외 발생")
    void useCredit_No_Credit() {
        Long userId = 1L;
        User user = User.builder().id(userId).build();

        when(userRepository.findById(userId)).thenReturn(Optional.of(user));
        when(creditHistoryRepository.existsByTransactionId(any())).thenReturn(false);

        when(userCreditRepository.findUseableCredits(eq(userId), any(PageRequest.class)))
                .thenReturn(Collections.emptyList());

        assertThatThrownBy(() -> userCreditService.useCredit(userId, 1, "TEST", 1L, "TX_EMPTY"))
                .isInstanceOf(CustomException.class);
    }

    @Test
    @DisplayName("성공: 이미 처리된 트랜잭션ID라면 아무것도 하지 않고 종료해야 한다(Idempotency).")
    void useCredit_Duplicate_Transaction_Ignored() {
        // given
        Long userId = 1L;
        String duplicateTxId = "TX_DUP_123";

        // 이미 존재하는 트랜잭션이라고 설정
        when(creditHistoryRepository.existsByTransactionId(duplicateTxId)).thenReturn(true);

        // when
        userCreditService.useCredit(userId, 1, "TEST", 1L, duplicateTxId);

        // then
        // 1. 유저 조회나 크레딧 조회 로직이 실행되지 않아야 함
        verify(userRepository, never()).findById(any());
        verify(userCreditRepository, never()).findUseableCredits(any(), any());

        // 2. 히스토리 저장도 안 되어야 함
        verify(creditHistoryRepository, never()).save(any());
    }
}