# CMake input file of dcm.so

FILE(GLOB SRCS *.h *.cpp *.inl *.hpp)

ADD_LIBRARY(dcm SHARED ${SRCS})
MACRO_DebugPostfix(dcm)

# ---
#INCLUDE_DIRECTORIES( ${PROJECT_SOURCE_DIR}/... )
#TARGET_LINK_LIBRARIES(dcm ...)

SOURCE_GROUP(base       REGULAR_EXPRESSION ".*\\.(cpp|inl|hpp|h)")
