package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.user.UserSurveyDto;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.entity.UserSurvey;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.domain.repository.UserSurveyRepository;
import com.jdc.recipe_service.domain.type.TagType;
import com.jdc.recipe_service.mapper.SurveyMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class SurveyService {

    private final UserSurveyRepository surveyRepository;
    private final UserRepository userRepository;

    @Transactional(readOnly = true)
    public UserSurveyDto getSurvey(Long userId) {
        return surveyRepository.findByUserId(userId)
                .map(SurveyMapper::toDto)
                .orElse(null);
    }

    @Transactional
    public UserSurveyDto saveOrUpdate(Long userId, UserSurveyDto dto) {
        User user = userRepository.getReferenceById(userId);

        boolean isNew = surveyRepository.findByUserId(userId).isEmpty();

        UserSurvey survey = surveyRepository.findByUserId(userId)
                .orElseGet(() -> SurveyMapper.toEntity(user, dto));

        survey.updateFromDto(dto);
        surveyRepository.save(survey);

        if (isNew) {
            user.markSurveyCompleted();
            userRepository.save(user);
        }

        return SurveyMapper.toDto(survey);
    }

}
