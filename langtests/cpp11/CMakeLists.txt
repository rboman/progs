cmake_minimum_required(VERSION 2.6)

project(Cpp11 CXX)

ADD_DEFINITIONS(
    /Qstd=c++11
)


set(SRCS main.cpp)

add_executable(cpp11 ${SRCS})

#set_target_properties(cpp11 PROPERTIES PLATFORM_TOOLSET "Intel C++ Compiler XE 14.0") # marche pas