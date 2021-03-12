cmake_minimum_required(VERSION 3.0)

set(CMAKE_CONFIGURATION_TYPES "ASAN;MSAN;USAN" CACHE STRING "" FORCE)

set(PROJECT_NAME ouch_enter_order)
project(${PROJECT_NAME})

# Set up the compiler flags
set(CMAKE_CXX_FLAGS "-g -Wall -Wextra -Werror")
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Inlcude directories
set(COMMON_INCLUDES ${PROJECT_SOURCE_DIR}/include)
include_directories(${COMMON_INCLUDES})

# Source files
file(GLOB SRC_FILES ${PROJECT_SOURCE_DIR}/src/*.cpp)

# Separate executable: main
list(REMOVE_ITEM SRC_FILES ${PROJECT_SOURCE_DIR}/src/main.cpp)

# Compile source files into a library
add_library(ouch_enter_order_lib ${SRC_FILES})

# Main is separate
add_executable(ouch_enter_order ${PROJECT_SOURCE_DIR}/src/main.cpp)

# linking Main against the library
target_link_libraries(ouch_enter_order ouch_enter_order_lib)

# google test is a git submodule
add_subdirectory(./googletest)

enable_testing()

# Include the gtest library
include_directories(${gtest_SOURCE_DIR}/include ${gtest_SOURCE_DIR})

# Test files
file(GLOB TEST_SRC_FILES ${PROJECT_SOURCE_DIR}/test/*.cpp)

# Unit tests
add_executable(runUnitTests ${TEST_SRC_FILES})
target_compile_options(runUnitTests PRIVATE -pedantic -pedantic-errors)

# Standard linking to gtest stuff
target_link_libraries(runUnitTests gtest gtest_main)

# Extra linking for the project
target_link_libraries(runUnitTests ouch_enter_order_lib)

# Sanitizers modes
if (CMAKE_BUILD_TYPE MATCHES ASAN)
    target_compile_options(runUnitTests PRIVATE -fsanitize=address
        -fno-omit-frame-pointer -fno-sanitize-recover=all)
    target_link_options(runUnitTests PRIVATE -fsanitize=address)
endif()
if (CMAKE_BUILD_TYPE MATCHES MSAN)
    target_compile_options(runUnitTests PRIVATE -fsanitize=memory
        -fno-omit-frame-pointer -fsanitize-memory-track-origins=2
        -fno-sanitize-recover=all)
    target_link_options(runUnitTests PRIVATE -fsanitize=memory
        -fsanitize-memory-track-origins=2)
endif()
if (CMAKE_BUILD_TYPE MATCHES USAN)
    target_compile_options(runUnitTests PRIVATE
        -fsanitize=undefined,float-cast-overflow,float-divide-by-zero
        -fno-omit-frame-pointer -fno-sanitize-recover=all
        -fsanitize-recover=alignment)
    target_link_options(runUnitTests PRIVATE
        -fsanitize=undefined,float-cast-overflow,float-divide-by-zero)
endif()
