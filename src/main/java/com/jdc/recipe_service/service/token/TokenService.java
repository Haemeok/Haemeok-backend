package com.jdc.recipe_service.service.token;

import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.domain.type.QuotaType;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@RequiredArgsConstructor
public class TokenService {

    private final UserRepository userRepository;

    /**
     * ID 목록을 받아 토큰을 일괄 지급합니다.
     * @return 업데이트된 행(User)의 개수
     */
    @Transactional
    public int giveTokenToUsersBulk(List<Long> userIds, QuotaType type, int amount) {
        if (userIds == null || userIds.isEmpty()) {
            return 0;
        }

        if (type == QuotaType.YOUTUBE_EXTRACTION) {
            return userRepository.bulkAddYoutubeToken(userIds, amount);
        } else if (type == QuotaType.AI_GENERATION) {
            return userRepository.bulkAddAiToken(userIds, amount);
        } else {
            throw new IllegalArgumentException("지원하지 않는 토큰 타입입니다: " + type);
        }
    }
}