# CMake input file of ndh.so

FILE(GLOB SRCS *.h *.cpp *.inl *.hpp)

ADD_LIBRARY(ndh SHARED ${SRCS})
MACRO_DebugPostfix(ndh)

SOURCE_GROUP(base       REGULAR_EXPRESSION ".*\\.(cpp|inl|hpp|h)")
