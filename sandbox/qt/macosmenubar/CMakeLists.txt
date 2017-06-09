cmake_minimum_required(VERSION 2.8.12)
project(testproject)

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_AUTOMOC ON)

find_package(Qt5Widgets)
add_executable(testapp main.cpp)
target_link_libraries(testapp Qt5::Widgets)