# CMake input file of sky.so

FILE(GLOB SRCS *.h *.cpp *.inl *.hpp)

ADD_LIBRARY(sky SHARED ${SRCS})
MACRO_DebugPostfix(sky)

# ---
#INCLUDE_DIRECTORIES( ${PROJECT_SOURCE_DIR}/... )
#TARGET_LINK_LIBRARIES(sky ...)

SOURCE_GROUP(base       REGULAR_EXPRESSION ".*\\.(cpp|inl|hpp|h)")
