cmake_minimum_required(VERSION 3.13)

include(test/Strict.cmake)

set(PROJECT_NAME genome_assembly)
project(${PROJECT_NAME})

# Set up the compiler flags
set(CMAKE_CXX_FLAGS "-g")
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
add_library(genome_assembly_lib ${SRC_FILES})
target_compile_options(genome_assembly_lib PUBLIC ${COMPILE_OPTS})
target_link_options(genome_assembly_lib PUBLIC ${LINK_OPTS})

# Main is separate
add_executable(genome-assembly ${PROJECT_SOURCE_DIR}/src/main.cpp)
target_compile_options(genome-assembly PRIVATE ${COMPILE_OPTS})
target_link_options(genome-assembly PRIVATE ${LINK_OPTS})

# linking Main against the library
target_link_libraries(genome-assembly genome_assembly_lib)

# google test is a git submodule
add_subdirectory(googletest)

enable_testing()

# test is a git submodule
add_subdirectory(test)

add_test(NAME tests COMMAND runUnitTests)
