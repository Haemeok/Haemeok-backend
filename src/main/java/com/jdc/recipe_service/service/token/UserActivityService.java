package com.jdc.recipe_service.service.token;

import com.jdc.recipe_service.domain.entity.UserDailyAccess;
import com.jdc.recipe_service.domain.repository.UserDailyAccessRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;

@Slf4j
@Service
@RequiredArgsConstructor
public class UserActivityService {

    private final UserDailyAccessRepository userDailyAccessRepository;

    @Transactional
    public void saveUserVisit(String userIdString, String osType) {
        try {
            Long userId = Long.parseLong(userIdString);

            UserDailyAccess access = UserDailyAccess.builder()
                    .userId(userId)
                    .accessDate(LocalDate.now())
                    .osType(osType)
                    .build();

            userDailyAccessRepository.save(access);

        } catch (NumberFormatException e) {
            log.error("유저 ID 파싱 실패: {}", userIdString);
        } catch (Exception e) {
            log.debug("일일 방문자 집계 저장 스킵: {}", e.getMessage());
        }
    }
}