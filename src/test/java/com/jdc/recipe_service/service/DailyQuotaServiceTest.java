package com.jdc.recipe_service.service;

import com.jdc.recipe_service.config.QuotaProperties;
import com.jdc.recipe_service.domain.repository.DailyQuotaDao;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.MediaType;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.test.context.support.WithMockUser;

import java.time.ZoneId;
import java.util.List;

import static org.apache.http.client.methods.RequestBuilder.post;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.*;
import static org.springframework.security.config.http.MatcherType.mvc;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.client.match.MockRestRequestMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;


@ExtendWith(MockitoExtension.class)
class DailyQuotaServiceTest {

    @Mock
    DailyQuotaDao dao;
    @Mock QuotaProperties props;
    DailyQuotaService svc;

    @BeforeEach
    void setup() {
        svc = new DailyQuotaService(dao, props);
        SecurityContextHolder.clearContext();
    }

    private void setAuth(String role) {
        var auth = new UsernamePasswordAuthenticationToken(
                "u", "pw", List.of(new SimpleGrantedAuthority(role)));
        SecurityContextHolder.getContext().setAuthentication(auth);
    }

    @Test
    void first_ok_second_throws_429() {
        // ROLE_USER로 제한 적용
        setAuth("ROLE_USER");

        // 이 테스트에서만 필요한 stubbing만!
        when(props.zoneId()).thenReturn(ZoneId.of("Asia/Seoul"));
        when(dao.tryConsume(1L)).thenReturn(true).thenReturn(false);

        // 1번째 OK
        svc.consumeForUserOrThrow(1L);

        // 2번째 429
        assertThrows(
                DailyQuotaService.DailyQuotaExceededException.class,
                () -> svc.consumeForUserOrThrow(1L)
        );
    }

    @Test
    void admin_bypasses_limit() {
        setAuth("ROLE_ADMIN");

        // admin은 우회 → 어떤 stubbing도 필요 없음
        svc.consumeForUserOrThrow(99L);

        // dao/props에 상호작용 없어야 더 정확
        verifyNoInteractions(dao, props);
    }
}
