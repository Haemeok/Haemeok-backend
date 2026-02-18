package com.jdc.recipe_service.domain.repository.user;

import com.jdc.recipe_service.domain.entity.user.UserSubscription;
import org.springframework.data.jpa.repository.JpaRepository;
import java.util.Optional;

public interface UserSubscriptionRepository extends JpaRepository<UserSubscription, Long> {
    Optional<UserSubscription> findByUserId(Long userId);
}