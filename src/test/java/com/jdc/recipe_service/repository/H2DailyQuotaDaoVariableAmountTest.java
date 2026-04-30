package com.jdc.recipe_service.repository;

import com.jdc.recipe_service.config.QuotaProperties;
import com.jdc.recipe_service.domain.repository.H2DailyQuotaDao;
import com.jdc.recipe_service.domain.type.QuotaType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.JdbcTest;
import org.springframework.jdbc.core.JdbcTemplate;

import java.time.LocalDate;
import java.time.ZoneId;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;

/**
 * H2DailyQuotaDao의 가변 차감 오버로드 시맨틱을 고정한다.
 *  - 시작 가능 조건은 used_count < limit만. 합산 후 limit 초과 허용 (마지막 기회 보장).
 *  - amount <= 0은 방어 가드로 거부 (음수로 counter를 줄이거나 늘리는 버그 차단).
 *  - refund는 0 미만으로 떨어지지 않음.
 *
 * H2 임베디드 DB로 실제 SQL을 검증한다 (H2DailyQuotaDao는 H2 MERGE 문법 사용).
 */
@JdbcTest
class H2DailyQuotaDaoVariableAmountTest {

    @Autowired
    JdbcTemplate jdbc;

    QuotaProperties props;
    H2DailyQuotaDao dao;

    private static final ZoneId TEST_ZONE = ZoneId.of("UTC");
    private static final Long USER_ID = 1L;
    private static final QuotaType TYPE = QuotaType.YOUTUBE_EXTRACTION;
    private static final int LIMIT = 20;

    @BeforeEach
    void setUp() {
        props = mock(QuotaProperties.class);
        given(props.zoneId()).willReturn(TEST_ZONE);
        dao = new H2DailyQuotaDao(jdbc, props);

        jdbc.execute("DROP TABLE IF EXISTS user_daily_ai_usage_counter");
        jdbc.execute("""
            CREATE TABLE user_daily_ai_usage_counter (
                user_id    BIGINT      NOT NULL,
                used_on    DATE        NOT NULL,
                quota_type VARCHAR(50) NOT NULL,
                used_count INT         NOT NULL,
                PRIMARY KEY (user_id, used_on, quota_type)
            )
            """);
    }

    private void seedUsedCount(int count) {
        jdbc.update(
                "INSERT INTO user_daily_ai_usage_counter (user_id, used_on, quota_type, used_count) VALUES (?, ?, ?, ?)",
                USER_ID, LocalDate.now(TEST_ZONE), TYPE.name(), count
        );
    }

    private int currentUsed() {
        return jdbc.queryForObject(
                "SELECT used_count FROM user_daily_ai_usage_counter WHERE user_id=? AND used_on=? AND quota_type=?",
                Integer.class,
                USER_ID, LocalDate.now(TEST_ZONE), TYPE.name()
        );
    }

    // --- happy paths ---

    @Test
    @DisplayName("18 + 5 = 23: 시작 조건만 used_count < limit이면 합이 limit을 초과해도 허용 (마지막 기회)")
    void tryConsume_eighteenPlusFive_succeedsAndExceedsLimit() {
        // given
        seedUsedCount(18);

        // when
        boolean ok = dao.tryConsume(USER_ID, TYPE, LIMIT, 5);

        // then
        assertThat(ok).isTrue();
        assertThat(currentUsed()).isEqualTo(23);
    }

    @Test
    @DisplayName("row가 없을 때 첫 호출은 used_count = amount로 INSERT 된다")
    void tryConsume_noRow_insertsWithAmount() {
        // when
        boolean ok = dao.tryConsume(USER_ID, TYPE, LIMIT, 5);

        // then
        assertThat(ok).isTrue();
        assertThat(currentUsed()).isEqualTo(5);
    }

    @Test
    @DisplayName("refund: amount만큼 정확히 빠진다")
    void refund_subtractsAmount() {
        // given
        seedUsedCount(23);

        // when
        dao.refund(USER_ID, TYPE, 5);

        // then
        assertThat(currentUsed()).isEqualTo(18);
    }

    @Test
    @DisplayName("refund: 0 미만으로 내려가지 않는다")
    void refund_clampsToZero() {
        // given
        seedUsedCount(2);

        // when
        dao.refund(USER_ID, TYPE, 5);

        // then
        assertThat(currentUsed()).isEqualTo(0);
    }

    // --- failure / boundary ---

    @Test
    @DisplayName("limit을 이미 넘긴 상태(used_count=23)에서는 tryConsume이 거부되고 used_count 변화 없음")
    void tryConsume_alreadyOverLimit_rejected() {
        // given
        seedUsedCount(23);

        // when
        boolean ok = dao.tryConsume(USER_ID, TYPE, LIMIT, 5);

        // then
        assertThat(ok).isFalse();
        assertThat(currentUsed()).isEqualTo(23);
    }

    @Test
    @DisplayName("used_count == limit 정확히 같으면 amount=1이라도 거부 (시작 조건 미충족)")
    void tryConsume_exactlyAtLimit_rejected() {
        // given
        seedUsedCount(LIMIT);

        // when
        boolean ok = dao.tryConsume(USER_ID, TYPE, LIMIT, 1);

        // then
        assertThat(ok).isFalse();
        assertThat(currentUsed()).isEqualTo(LIMIT);
    }

    // --- guard: amount <= 0 ---

    @Test
    @DisplayName("amount = 0이면 가드로 거부, used_count 변화 없음")
    void tryConsume_zeroAmount_rejectedByGuard() {
        // given
        seedUsedCount(5);

        // when
        boolean ok = dao.tryConsume(USER_ID, TYPE, LIMIT, 0);

        // then
        assertThat(ok).isFalse();
        assertThat(currentUsed()).isEqualTo(5);
    }

    @Test
    @DisplayName("amount < 0이면 가드로 거부, used_count 변화 없음 (음수 차감 차단)")
    void tryConsume_negativeAmount_rejectedByGuard() {
        // given
        seedUsedCount(5);

        // when
        boolean ok = dao.tryConsume(USER_ID, TYPE, LIMIT, -3);

        // then
        assertThat(ok).isFalse();
        assertThat(currentUsed()).isEqualTo(5);
    }

    @Test
    @DisplayName("refund: amount = 0이면 no-op")
    void refund_zeroAmount_noOp() {
        // given
        seedUsedCount(10);

        // when
        dao.refund(USER_ID, TYPE, 0);

        // then
        assertThat(currentUsed()).isEqualTo(10);
    }

    @Test
    @DisplayName("refund: amount < 0이면 no-op (음수로 used_count 증가 차단)")
    void refund_negativeAmount_noOp() {
        // given
        seedUsedCount(10);

        // when
        dao.refund(USER_ID, TYPE, -3);

        // then
        assertThat(currentUsed()).isEqualTo(10);
    }

    // --- date-aware tryConsume (cross-midnight Gemini upgrade 정확성) ---

    @Test
    @DisplayName("tryConsume(amount, usedOn): 명시 날짜 row가 정확히 +amount (today 영향 없음)")
    void tryConsumeWithDate_targetsExplicitDate_onlyThatRowChanges() {
        // given: 어제와 오늘 둘 다 row가 있음. 어제=2 (BASIC만), 오늘=다른 활동
        LocalDate today = LocalDate.now(TEST_ZONE);
        LocalDate yesterday = today.minusDays(1);
        seedUsedCount(7); // today=7
        jdbc.update(
                "INSERT INTO user_daily_ai_usage_counter (user_id, used_on, quota_type, used_count) VALUES (?, ?, ?, ?)",
                USER_ID, yesterday, TYPE.name(), 2
        );

        // when: 23:55 BASIC 차감(어제) → 00:05 Gemini upgrade는 어제 row에 +3 차감되어야 함
        boolean ok = dao.tryConsume(USER_ID, TYPE, Integer.MAX_VALUE, 3, yesterday);

        // then: 어제만 2→5, 오늘은 그대로
        assertThat(ok).isTrue();
        Integer yesterdayUsed = jdbc.queryForObject(
                "SELECT used_count FROM user_daily_ai_usage_counter WHERE user_id=? AND used_on=? AND quota_type=?",
                Integer.class, USER_ID, yesterday, TYPE.name());
        assertThat(yesterdayUsed).isEqualTo(5);
        assertThat(currentUsed()).isEqualTo(7);
    }

    @Test
    @DisplayName("tryConsume(amount, usedOn): 해당 날짜 row가 없으면 INSERT (used_count=amount)")
    void tryConsumeWithDate_noRow_inserts() {
        LocalDate yesterday = LocalDate.now(TEST_ZONE).minusDays(1);

        boolean ok = dao.tryConsume(USER_ID, TYPE, Integer.MAX_VALUE, 3, yesterday);

        assertThat(ok).isTrue();
        Integer yesterdayUsed = jdbc.queryForObject(
                "SELECT used_count FROM user_daily_ai_usage_counter WHERE user_id=? AND used_on=? AND quota_type=?",
                Integer.class, USER_ID, yesterday, TYPE.name());
        assertThat(yesterdayUsed).isEqualTo(3);
    }

    @Test
    @DisplayName("tryConsume(amount<=0, usedOn): false (방어 가드)")
    void tryConsumeWithDate_zeroAmount_rejected() {
        LocalDate yesterday = LocalDate.now(TEST_ZONE).minusDays(1);
        boolean ok = dao.tryConsume(USER_ID, TYPE, Integer.MAX_VALUE, 0, yesterday);
        assertThat(ok).isFalse();
    }

    @Test
    @DisplayName("tryConsume(amount, usedOn=null): false (방어 가드)")
    void tryConsumeWithDate_nullDate_rejected() {
        boolean ok = dao.tryConsume(USER_ID, TYPE, Integer.MAX_VALUE, 3, null);
        assertThat(ok).isFalse();
    }

    // --- date-aware refund (cross-midnight 정확성) ---

    @Test
    @DisplayName("refund(amount, usedOn): today와 다른 명시 날짜 row가 정확히 빠진다 (today 영향 없음)")
    void refundWithDate_targetsExplicitDate_onlyThatRowChanges() {
        // given: 어제와 오늘 둘 다 row가 있음
        LocalDate today = LocalDate.now(TEST_ZONE);
        LocalDate yesterday = today.minusDays(1);
        seedUsedCount(10); // today=10
        jdbc.update(
                "INSERT INTO user_daily_ai_usage_counter (user_id, used_on, quota_type, used_count) VALUES (?, ?, ?, ?)",
                USER_ID, yesterday, TYPE.name(), 7
        );

        // when: 어제 날짜로 환불 (자정 넘어 실패한 시작 quota를 시작일 기준으로 환불하는 시나리오)
        dao.refund(USER_ID, TYPE, 3, yesterday);

        // then: 어제만 7→4, 오늘은 그대로
        Integer yesterdayUsed = jdbc.queryForObject(
                "SELECT used_count FROM user_daily_ai_usage_counter WHERE user_id=? AND used_on=? AND quota_type=?",
                Integer.class, USER_ID, yesterday, TYPE.name());
        assertThat(yesterdayUsed).isEqualTo(4);
        assertThat(currentUsed()).isEqualTo(10);
    }

    @Test
    @DisplayName("refund(amount, usedOn): 0 미만으로 내려가지 않는다")
    void refundWithDate_clampsToZero() {
        // given: 어제 row=2
        LocalDate yesterday = LocalDate.now(TEST_ZONE).minusDays(1);
        jdbc.update(
                "INSERT INTO user_daily_ai_usage_counter (user_id, used_on, quota_type, used_count) VALUES (?, ?, ?, ?)",
                USER_ID, yesterday, TYPE.name(), 2
        );

        // when
        dao.refund(USER_ID, TYPE, 5, yesterday);

        // then
        Integer yesterdayUsed = jdbc.queryForObject(
                "SELECT used_count FROM user_daily_ai_usage_counter WHERE user_id=? AND used_on=? AND quota_type=?",
                Integer.class, USER_ID, yesterday, TYPE.name());
        assertThat(yesterdayUsed).isZero();
    }

    @Test
    @DisplayName("refund(amount, usedOn=null): no-op (방어 가드)")
    void refundWithDate_nullDate_noOp() {
        seedUsedCount(10);

        dao.refund(USER_ID, TYPE, 3, null);

        assertThat(currentUsed()).isEqualTo(10);
    }

    @Test
    @DisplayName("refund(amount=0, usedOn): no-op (방어 가드)")
    void refundWithDate_zeroAmount_noOp() {
        LocalDate yesterday = LocalDate.now(TEST_ZONE).minusDays(1);
        jdbc.update(
                "INSERT INTO user_daily_ai_usage_counter (user_id, used_on, quota_type, used_count) VALUES (?, ?, ?, ?)",
                USER_ID, yesterday, TYPE.name(), 7
        );

        dao.refund(USER_ID, TYPE, 0, yesterday);

        Integer yesterdayUsed = jdbc.queryForObject(
                "SELECT used_count FROM user_daily_ai_usage_counter WHERE user_id=? AND used_on=? AND quota_type=?",
                Integer.class, USER_ID, yesterday, TYPE.name());
        assertThat(yesterdayUsed).isEqualTo(7);
    }
}
