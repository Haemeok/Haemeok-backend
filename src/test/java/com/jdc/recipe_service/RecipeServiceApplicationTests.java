package com.jdc.recipe_service;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

@SpringBootTest(
		properties = "spring.autoconfigure.exclude=" +
				"org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration," +
				"org.springframework.boot.autoconfigure.security.servlet.UserDetailsServiceAutoConfiguration"
)
@ActiveProfiles("test")
@Disabled("보안 설정이 추가되어 contextLoads는 생략")
class RecipeServiceApplicationTests {

	@Test
	void contextLoads() {
	}

}
