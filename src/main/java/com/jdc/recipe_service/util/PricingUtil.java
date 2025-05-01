package com.jdc.recipe_service.util;

import java.util.concurrent.ThreadLocalRandom;

public class PricingUtil {

    /**
     * 1) 기본 마진율을 기준으로 ±5% 범위 내에서 랜덤한 마진율 뽑기
     *    최하 0% 미만으로 떨어지지 않게(min>=0) 처리
     */
    public static int randomizeMarginPercent(int basePercent) {
        int min = Math.max(basePercent - 5, 0);
        int max = basePercent + 5;
        return ThreadLocalRandom.current().nextInt(min, max + 1);
    }

    /**
     * 2) 마진율(percent)을 재료비(cost)에 적용해서 최종 시중가를 계산
     *    반올림 오차를 줄이기 위해 (cost * (100 + percent) + 50) / 100 사용
     */
    public static int applyMargin(int cost, int marginPercent) {
        return (cost * (100 + marginPercent) + 50) / 100;
    }
}