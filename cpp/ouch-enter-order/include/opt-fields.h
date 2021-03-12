#pragma once

#include "codec.h"

#define FIELD(name, _, __, protocol_type, ctype) \
inline unsigned char * encode_field_##name(unsigned char * start, const ctype value) { \
    return encode_##protocol_type(start, value); \
}

#include "new-enter-order.inl"