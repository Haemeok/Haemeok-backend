-- refresh_tokens 테이블에 grace 회전을 위한 두 컬럼(previous_token, previous_token_grace_until)과
-- previous_token 조회용 인덱스를 추가한다.
--
-- 배경: Android WebView의 CookieManager가 쿠키를 디스크에 flush하기 전에 앱이 백그라운드로
-- 가거나 프로세스가 죽으면, 서버는 이미 회전된 "새 refresh token"을 Set-Cookie로 내렸음에도
-- 클라이언트는 "옛 refresh token"을 다시 보내오는 현상이 간헐적으로 재현된다. 이 때 서버가
-- 옛 토큰을 즉시 invalid로 끊으면 프론트는 force-logout을 발화하고 사용자는 로그아웃된다.
--
-- 해결: "직전 회전에서 발급했던 값"을 같은 row에 previous_token으로 들고, 짧은 유예기간
-- (previous_token_grace_until) 안에 들어온 옛 토큰을 "이미 회전된 현재 토큰"으로 동등 처리한다.
-- Redis에 grace 레코드를 따로 두는 안은 DB commit과 Redis write 사이의 miss window가
-- 프론트 force-logout과 겹치면 똑같이 로그아웃을 만든다 ― 그래서 한 row에 묶었다.
--
-- 인덱스 결정: previous_token은 UNIQUE가 아닌 평문 INDEX로 둔다.
--   - previous_token은 소스-오브-트루스 key가 아니라 "한 tick 전" 이력이다. 사용자 여러 기기에서
--     동시에 회전이 일어나면 동일 previous_token을 공유하는 row가 잠깐 존재할 수 있다(드물지만).
--   - UNIQUE를 걸면 이 drop-in 이력 처리가 schema violation으로 바뀌어 refresh가 먼저 500을 낸다.
--   - 우리가 실제로 필요한 건 equality seek 성능이지 글로벌 유일성이 아니다.
--
-- 호환성: 두 컬럼 모두 NULL 허용이라 기존 row는 영향 없음(전체 row = NULL로 존재). 회전 경로
-- (AuthService.refresh)가 이 컬럼을 읽고 쓰지만, 옛 코드/옛 row 조합에서도 정상 동작하도록
-- previous_token IS NULL / grace_until 만료는 "기존 단일 lookup"과 동일하게 fallthrough된다.
--
-- 운영 주의: ALTER ADD COLUMN은 MySQL 8에서 INSTANT(또는 INPLACE)로 내려가지만, 같은 문에
-- CREATE INDEX가 섞이면 INPLACE가 강제되어 metadata lock이 길어질 수 있다. 트래픽이 낮은
-- 시간대에 적용하고, 적용 직전 long-running tx 유무를 확인하는 것을 권장한다
-- (information_schema.innodb_trx, SHOW PROCESSLIST).

ALTER TABLE refresh_tokens
    ADD COLUMN previous_token VARCHAR(255) NULL,
    ADD COLUMN previous_token_grace_until DATETIME NULL,
    ADD INDEX idx_rt_prev_token (previous_token);
