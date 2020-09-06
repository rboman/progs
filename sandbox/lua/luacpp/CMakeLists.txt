
CMAKE_MINIMUM_REQUIRED(VERSION 3.1)
PROJECT(LUA)

SET(SRCS main.cpp)

find_package(LUA REQUIRED)  # lua <=5.3 with cmake 3.12
message("LUA_INCLUDE_DIR = ${LUA_INCLUDE_DIR}")
message("LUA_LIBRARIES = ${LUA_LIBRARIES}")

ADD_EXECUTABLE(mylua ${SRCS})
TARGET_INCLUDE_DIRECTORIES(mylua PUBLIC ${LUA_INCLUDE_DIR})
target_link_libraries(mylua ${LUA_LIBRARIES})

configure_file(config.h.in config.h)
include_directories(${PROJECT_BINARY_DIR})
