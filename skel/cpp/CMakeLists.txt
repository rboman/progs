cmake_minimum_required(VERSION 2.6)
project(Skel CXX)
file(GLOB SRCS *.h *.cpp *.inl *.hpp)
add_executable(skelcpp ${SRCS})
enable_testing()
add_test(NAME skelcpp COMMAND ${EXECUTABLE_OUTPUT_PATH}/skelcpp
         WORKING_DIRECTORY ${PROJECT_BINARY_DIR})