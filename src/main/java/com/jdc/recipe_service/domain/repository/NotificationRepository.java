package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.Notification;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface NotificationRepository extends JpaRepository<Notification, Long> {
    Page<Notification> findByUserIdOrderByCreatedAtDesc(Long userId, Pageable pageable);
    List<Notification> findByUserIdAndIsReadFalse(Long userId);
    Page<Notification> findByUserIdAndIsReadFalseOrderByCreatedAtDesc(Long userId, Pageable pageable);
    Page<Notification> findByUserIdAndIsReadTrueOrderByCreatedAtDesc(Long userId, Pageable pageable);
    void deleteByUserId(Long userId);
    long countByUserIdAndIsReadFalse(Long userId);
}