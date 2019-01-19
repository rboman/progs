# CMake input file of gauss.so

FILE(GLOB SRCS *.h *.cpp *.inl *.hpp)

ADD_LIBRARY(gauss SHARED ${SRCS})
MACRO_DebugPostfix(gauss)
TARGET_INCLUDE_DIRECTORIES(gauss PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

SOURCE_GROUP(base       REGULAR_EXPRESSION ".*\\.(cpp|inl|hpp|h)")
