package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.type.QuotaType;

public interface DailyQuotaDao {
    boolean tryConsume(Long userId, QuotaType type, int limit);
    void refundOnce(Long userId, QuotaType type);
    int remainingToday(Long userId, QuotaType type, int limit);

    // 가변 차감용. 시작 가능 조건만 used_count < limit으로 검사하고 amount만큼 더한다.
    // used_count + amount > limit이어도 허용 (마지막 한 번의 기회 보장).
    // 다음 호출은 used_count >= limit이라 자동 거부된다.
    // amount <= 0이면 false 반환 (방어 가드: 음수 amount로 used_count를 줄이는 것 차단).
    boolean tryConsume(Long userId, QuotaType type, int limit, int amount);

    // 명시적 날짜 기준 가변 차감 — cross-midnight 정확성용.
    // dev V3 chargeGeminiUpgrade가 job.quotaUsedOn(시작 차감 날짜)에 대해 추가 차감할 때 사용.
    // 시작 row가 그 날짜에 이미 만들어져 있으면 += amount, 없으면 INSERT.
    // amount <= 0 또는 usedOn null이면 false (방어 가드).
    boolean tryConsume(Long userId, QuotaType type, int limit, int amount, java.time.LocalDate usedOn);

    // 가변 차감 환불. amount만큼 used_count에서 빼되 0 미만으로 내려가진 않는다.
    // amount <= 0이면 no-op (방어 가드: 음수 amount로 used_count를 늘리는 것 차단).
    void refund(Long userId, QuotaType type, int amount);

    // 명시적 날짜 기준 환불 — cross-midnight 환불 정확성용.
    // dev V3가 차감 시점 날짜를 job에 저장하고 그 날짜로 환불할 때 사용.
    // amount <= 0 또는 usedOn null이면 no-op.
    void refund(Long userId, QuotaType type, int amount, java.time.LocalDate usedOn);
}