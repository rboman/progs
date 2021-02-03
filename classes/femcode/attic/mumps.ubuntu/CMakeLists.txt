# GMM / MUMPS sequentiel mais code MPI

cmake_minimum_required(VERSION 2.7)

project(TestMUMPS CXX)

if(WIN32)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS -D_SCL_SECURE_NO_DEPRECATE)
endif(WIN32)

# mumps/sequentiel
add_definitions(-DGMM_USES_MUMPS)  
include_directories(${PROJECT_SOURCE_DIR}/gmm)

# mumps/mpi
find_package(MPI REQUIRED)
add_definitions(-DGMM_USES_MPI) # MUMPS en MPI 
include_directories(${MPI_INCLUDE_PATH})

add_executable(testMUMPS testMUMPS.cpp)

TARGET_LINK_LIBRARIES(testMUMPS dmumps)  # mumps/sequentiel

TARGET_LINK_LIBRARIES(testMUMPS ${MPI_LIBRARIES})
