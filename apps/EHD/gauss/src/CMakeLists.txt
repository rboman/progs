# CMake input file of gauss.so

FILE(GLOB SRCS *.h *.cpp *.inl *.hpp)

ADD_LIBRARY(gauss SHARED ${SRCS})
MACRO_DebugPostfix(gauss)

# ---
#INCLUDE_DIRECTORIES( ${PROJECT_SOURCE_DIR}/... )
#TARGET_LINK_LIBRARIES(gauss ...)

SOURCE_GROUP(base       REGULAR_EXPRESSION ".*\\.(cpp|inl|hpp|h)")
