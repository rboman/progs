# CMake input file of sky.so

FILE(GLOB SRCS *.h *.cpp *.inl *.hpp)

ADD_LIBRARY(sky SHARED ${SRCS})
MACRO_DebugPostfix(sky)
TARGET_INCLUDE_DIRECTORIES(sky PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

SOURCE_GROUP(base       REGULAR_EXPRESSION ".*\\.(cpp|inl|hpp|h)")
