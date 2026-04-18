-- refresh_tokens.token 컬럼에 UNIQUE 인덱스를 추가한다.
--
-- 배경: refresh 회전은 `WHERE token = :t`로 단일 row를 특정하는 게 전제다.
-- 현재 엔티티(RefreshToken.java)도 Optional<RefreshToken> 반환을 전제로 설계되어 있어,
-- 논리적으로 중복 token은 비정상 상태다. 그런데 테이블 생성이 Flyway 이전(ddl-auto=update 시절)에
-- 이뤄져 실DB에 index가 없는 상태로 방치되어 있었다.
--
-- 회전 경로(AuthService.refresh의 findByTokenForUpdate)가 PESSIMISTIC_WRITE로 동작하려면
-- 먼저 토큰 컬럼의 unique 인덱스가 있어야 한다. 인덱스 없이 테이블 스캔하며 row-level lock을
-- 잡으면 next-key lock 범위가 커져 refresh 동시성이 묶인다.
--
-- 운영 전제(반드시 배포 전에 확인):
--   SELECT token, COUNT(*) c FROM refresh_tokens GROUP BY token HAVING c > 1;
-- 위 쿼리가 0 rows여야 이 마이그레이션이 안전하게 통과한다. dup가 발견되면 이 파일을
-- 적용하기 전에 운영 SQL로 정리하고(각 dup 그룹에서 expired_at이 가장 큰 row 1개만 유지),
-- 다시 precheck로 0 rows를 확인한 뒤에 배포한다. dup 정리는 본 migration에 포함시키지 않는다 ―
-- 세션 일부가 끊기는 side effect를 schema change 히스토리에 묻으면 안 되기 때문이다.
--
-- 호환성: 읽기/쓰기 경로는 변함없이 `WHERE token=:t`다. 인덱스 추가는 additive이고
-- 기존 쿼리 플랜을 스캔에서 seek으로 바꿔 성능이 개선되는 쪽이라 회귀 위험은 없다.

ALTER TABLE refresh_tokens
    ADD CONSTRAINT uk_rt_token UNIQUE (token);
