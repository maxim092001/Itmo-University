#pragma once

#include <cstdint>
#include <string>
#include <array>

inline unsigned char *encode(unsigned char *start, const uint8_t value) {
    return &(*start = value) + 1;
}


inline unsigned char *encode(unsigned char *start, const uint32_t value) {
    *start++ = static_cast<unsigned char>((value >> 24) & 0xFF);
    *start++ = static_cast<unsigned char>((value >> 16) & 0xFF);
    *start++ = static_cast<unsigned char>((value >> 8) & 0xFF);
    *start++ = static_cast<unsigned char>(value & 0xFF);
    return start;
}

inline unsigned char *encode(unsigned char *start, const std::string &str, const size_t field_size) {
    size_t i = 0;

    for (; i < str.size() && i < field_size; ++i) {
        *start++ = str[i];
    }

    for (; i < field_size; ++i) {
        *start++ = ' ';
    }

    return start;
}