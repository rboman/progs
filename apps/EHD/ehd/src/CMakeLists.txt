# CMake input file of ehd.so

FILE(GLOB SRCS *.h *.cpp *.inl *.hpp)

ADD_LIBRARY(ehd SHARED ${SRCS})
MACRO_DebugPostfix(ehd)

TARGET_LINK_LIBRARIES(ehd gauss sky)
TARGET_INCLUDE_DIRECTORIES(ehd PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

SOURCE_GROUP(base       REGULAR_EXPRESSION ".*\\.(cpp|inl|hpp|h)")
