# Build types for various sanitizer modes
set(CMAKE_CONFIGURATION_TYPES "ASAN;MSAN;USAN" CACHE STRING "" FORCE)

# General compile and link options
set(COMPILE_OPTS -Wall -Wextra -Werror -pedantic -pedantic-errors)
set(LINK_OPTS "")

# Sanitizers options
if (CMAKE_BUILD_TYPE MATCHES ASAN)
    list(APPEND COMPILE_OPTS -fsanitize=address -fno-omit-frame-pointer
        -fno-inline -fno-sanitize-recover=all)
    list(APPEND LINK_OPTS -fsanitize=address)
endif()
if (CMAKE_BUILD_TYPE MATCHES MSAN)
    list(APPEND COMPILE_OPTS -fsanitize=memory
        -fno-omit-frame-pointer -fsanitize-memory-track-origins=2
        -fno-sanitize-recover=all)
    list(APPEND LINK_OPTS -fsanitize=memory
        -fsanitize-memory-track-origins=2)
endif()
if (CMAKE_BUILD_TYPE MATCHES USAN)
    list(APPEND COMPILE_OPTS
        -fsanitize=undefined,float-cast-overflow,float-divide-by-zero
        -fno-omit-frame-pointer -fno-sanitize-recover=all
        -fsanitize-recover=alignment)
    list(APPEND LINK_OPTS
        -fsanitize=undefined,float-cast-overflow,float-divide-by-zero)
endif()
