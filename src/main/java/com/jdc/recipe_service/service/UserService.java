package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.UserRequestDTO;
import com.jdc.recipe_service.domain.dto.UserResponseDTO;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.mapper.UserMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@Transactional
@RequiredArgsConstructor
public class UserService {

    private final UserRepository userRepository;
    private final UserMapper userMapper;

    // 유저 생성
    public UserResponseDTO createUser(UserRequestDTO request) {
        User user = userMapper.toEntity(request);

        userRepository.save(user);
        return userMapper.toDto(user);
    }

    // 유저 단일 조회
    public UserResponseDTO getUser(Long id) {
        User user = userRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("User not found with id: " + id));
        return userMapper.toDto(user);
    }

    // 모든 유저 조회
    public List<UserResponseDTO> getAllUsers() {
        List<User> users = userRepository.findAll();
        return users.stream()
                .map(userMapper::toDto)
                .toList();
    }

    // 유저 업데이트
    public UserResponseDTO updateUser(Long id, UserRequestDTO request) {
        User user = userRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("User not found with id: " + id));

        userMapper.updateEntityFromDto(request, user);

        userRepository.save(user);
        return userMapper.toDto(user);
    }

    // 유저 삭제
    public void deleteUser(Long id) {
        User user = userRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("User not found with id: " + id));
        userRepository.delete(user);
    }
}
