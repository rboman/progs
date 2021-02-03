
# find Eigen
find_path(EIGEN_INCLUDE_DIRS "Eigen/Dense" PATHS "${PROJECT_SOURCE_DIR}/eigen" "/usr/include/eigen3")
MESSAGE(STATUS "EIGEN_INCLUDE_DIRS=" ${EIGEN_INCLUDE_DIRS})


SET(SRCS main.cpp linalg.cpp linalg.h)
ADD_EXECUTABLE(invert_matrix ${SRCS})
TARGET_INCLUDE_DIRECTORIES(invert_matrix PUBLIC ${EIGEN_INCLUDE_DIRS})
TARGET_LINK_LIBRARIES(invert_matrix ${GMSH_LIBRARIES})

# Eigen requires a patch to compile SSE instructions with PGI
#  => we could either use the latest Eigen (master branch from https://gitlab.com/libeigen/eigen)
#  => or we disable SSE instructions

IF(${CMAKE_CXX_COMPILER_ID} STREQUAL "PGI")
    TARGET_COMPILE_DEFINITIONS(invert_matrix PUBLIC EIGEN_DONT_VECTORIZE)
ENDIF()

