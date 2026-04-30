package com.jdc.recipe_service.service.ingredient.backfill;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * 정규화 백필 설정 — recipe_ingredients의 amount_value/ingredient_unit_id/normalized_grams/
 * resolution_status 4개 필드를 채우는 작업.
 *
 * <p><b>이름 주의</b>: API V2와 혼동을 피하기 위해 "v2" 명칭을 쓰지 않는다. 이 백필은 정규화
 * 필드를 채우는 1회성 데이터 작업이지 API 버전이 아니다.
 *
 * <p><b>운영 안전 기본값</b>: 모두 보수적으로 설정. 기본 enabled=false로 ApplicationRunner가
 * 자동 실행되지 않도록 한다. dryRun=true로 실수로 enabled=true 켜도 DB write가 일어나지 않게.
 *
 * <p>실제 실행 시 환경변수로 명시 override 필요:
 * <pre>
 * INGREDIENT_NORMALIZATION_BACKFILL_ENABLED=true
 * INGREDIENT_NORMALIZATION_BACKFILL_DRY_RUN=false
 * INGREDIENT_NORMALIZATION_BACKFILL_BATCH_SIZE=1000
 * INGREDIENT_NORMALIZATION_BACKFILL_MAX_ROWS=5000  # 0이면 무제한
 * INGREDIENT_NORMALIZATION_BACKFILL_START_ID=0     # 중단 후 재개 시 마지막 처리 id
 * INGREDIENT_NORMALIZATION_BACKFILL_SLEEP_MS=100   # chunk 간 sleep
 * INGREDIENT_NORMALIZATION_BACKFILL_STOP_ON_ERROR=false
 * </pre>
 */
@Component
@ConfigurationProperties(prefix = "ingredient.normalization-backfill")
@Getter
@Setter
public class RecipeIngredientBackfillProperties {

    /** 백필 자동 실행 여부. 기본 false — ApplicationRunner가 즉시 return. */
    private boolean enabled = false;

    /** dry-run 모드. true면 DB write 없이 통계 로그만 남김. */
    private boolean dryRun = true;

    /** 한 chunk에서 처리할 row 수. 트랜잭션 단위. */
    private int batchSize = 1000;

    /** 시작 id (id > startId). 중단 후 재개 시 마지막 처리 id 지정. */
    private long startId = 0;

    /** 한 실행에서 처리할 최대 row 수. 0이면 무제한. */
    private long maxRows = 0;

    /** chunk 간 sleep (ms) — DB 부하 조절. */
    private long sleepMs = 100;

    /** chunk 처리 중 예외 발생 시 즉시 중단 여부. false면 다음 chunk로 진행. */
    private boolean stopOnError = false;
}
