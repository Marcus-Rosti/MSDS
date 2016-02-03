cmake_minimum_required(VERSION 2.6)
project(code)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -O3 -Wall")

add_executable(original original.c)
add_executable(generate_input generate_input.c)
