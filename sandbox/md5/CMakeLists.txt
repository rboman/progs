CMAKE_MINIMUM_REQUIRED(VERSION 3.1)
PROJECT(Md5Test CXX)

SET(SRCS main.cpp md5.cpp md5.h)
ADD_EXECUTABLE(md5test ${SRCS})

ENABLE_TESTING()
ADD_TEST(NAME md5test COMMAND ${EXECUTABLE_OUTPUT_PATH}/md5test
         WORKING_DIRECTORY ${PROJECT_BINARY_DIR})