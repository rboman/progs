# CMake input file of ehd.so

FILE(GLOB SRCS *.h *.cpp *.inl *.hpp)

ADD_LIBRARY(ehd SHARED ${SRCS})
MACRO_DebugPostfix(ehd)

# ---
INCLUDE_DIRECTORIES( ${PROJECT_SOURCE_DIR}/gauss/src ${PROJECT_SOURCE_DIR}/sky/src 
 )
TARGET_LINK_LIBRARIES(ehd gauss sky)

SOURCE_GROUP(base       REGULAR_EXPRESSION ".*\\.(cpp|inl|hpp|h)")
