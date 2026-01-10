package com.jdc.recipe_service.domain.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Entity
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "youtube_target_channels")
public class YoutubeTargetChannel {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private String channelName;

    @Column(nullable = false)
    private String channelUrl;

    @Column(nullable = false)
    private boolean isActive;

    public YoutubeTargetChannel(String channelName, String channelUrl) {
        this.channelName = channelName;
        this.channelUrl = channelUrl;
        this.isActive = true;
    }
}