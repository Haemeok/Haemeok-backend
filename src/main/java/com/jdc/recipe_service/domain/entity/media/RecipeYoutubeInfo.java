package com.jdc.recipe_service.domain.entity.media;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.common.BaseTimeEntity;
import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(
        name = "recipe_youtube_info",
        uniqueConstraints = {
                @UniqueConstraint(name = "uq_youtube_video_id", columnNames = {"video_id"}),
                @UniqueConstraint(name = "uq_youtube_recipe_id", columnNames = {"recipe_id"})
        },
        indexes = {
                @Index(name = "idx_youtube_video_id", columnList = "video_id"),
                @Index(name = "idx_youtube_channel_id", columnList = "channel_id")
        }
)
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class RecipeYoutubeInfo extends BaseTimeEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @OneToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "recipe_id", nullable = false)
    private Recipe recipe;

    @Column(name = "video_id", nullable = false, length = 32)
    private String videoId;

    @Column(name = "youtube_url", length = 255)
    private String youtubeUrl;

    @Column(name = "channel_name", length = 100)
    private String channelName;

    @Column(name = "channel_id", length = 100)
    private String channelId;

    @Column(name = "video_title", length = 255)
    private String videoTitle;

    @Column(name = "thumbnail_url", length = 500)
    private String thumbnailUrl;

    @Column(name = "channel_profile_url", length = 500)
    private String channelProfileUrl;

    @Column(name = "subscriber_count")
    private Long subscriberCount;

    @Column(name = "video_view_count")
    private Long videoViewCount;

    @Column(name = "extractor_id")
    private Long extractorId;

    public void updateYoutubeInfo(String channelName, String channelId, String videoTitle,
                                  String thumbnailUrl, String channelProfileUrl,
                                  Long subscriberCount, Long videoViewCount) {
        this.channelName = channelName;
        this.channelId = channelId;
        this.videoTitle = videoTitle;
        this.thumbnailUrl = thumbnailUrl;
        this.channelProfileUrl = channelProfileUrl;
        this.subscriberCount = subscriberCount;
        this.videoViewCount = videoViewCount;
    }

    public void updateExtractorId(Long extractorId) {
        this.extractorId = extractorId;
    }
}