package com.jdc.recipe_service.service.token;

import com.jdc.recipe_service.domain.entity.UserDailyAccess;
import com.jdc.recipe_service.domain.repository.UserDailyAccessRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;

@Slf4j
@Service
@RequiredArgsConstructor
public class UserActivityService {

    private final UserDailyAccessRepository userDailyAccessRepository;

    @Async
    public void saveUserVisit(String userIdString, String osType) {
        Long userId = Long.parseLong(userIdString);
        LocalDate todayKst = LocalDate.now(ZoneId.of("Asia/Seoul"));
        LocalDateTime nowKst = LocalDateTime.now(ZoneId.of("Asia/Seoul"));
        userDailyAccessRepository.insertIgnoreVisit(userId, todayKst, osType, nowKst);
    }
}