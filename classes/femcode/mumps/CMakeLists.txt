# Solve a system using MUMPSseq in a sequential and an mpi code
#===============================================================================

cmake_minimum_required(VERSION 2.7)

#SET(CMAKE_C_COMPILER "icc" CACHE FILEPATH "" FORCE)
#SET(CMAKE_CXX_COMPILER "icpc" CACHE FILEPATH "" FORCE)

project(TestMUMPS)

if(WIN32)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS -D_SCL_SECURE_NO_DEPRECATE)
endif(WIN32)

# for FindMUMPS and FindGMM
LIST(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}")

# -- MUMPS --
FIND_PACKAGE(MUMPS REQUIRED)
include_directories(${MUMPS_INCLUDE_DIRS})

# -- LAPACK --
FIND_PACKAGE(LAPACK REQUIRED)
message(STATUS "LAPACK_LIBRARIES=${LAPACK_LIBRARIES}")

# -- GMM --
FIND_PACKAGE(GMM REQUIRED)
include_directories(${GMM_INCLUDE_DIRS})
add_definitions(-DGMM_USES_LAPACK)  
add_definitions(-DGMM_USES_MUMPS)

# -- CTest
enable_testing()

#===============================================================================
# codeseq: MUMPS seq dans un code seq

add_executable(codeseq codeseq.cpp)
TARGET_LINK_LIBRARIES(codeseq ${MUMPS_LIBRARIES} ${LAPACK_LIBRARIES})
add_test(NAME codeseq COMMAND codeseq)
#===============================================================================
# codempi: MUMPS seq dans un code MPI

find_package(MPI REQUIRED)
#message(STATUS "MPI_CXX_COMPILE_FLAGS=${MPI_CXX_COMPILE_FLAGS}")
message(STATUS "MPIEXEC=${MPIEXEC}")
message(STATUS "MPIEXEC_NUMPROC_FLAG=${MPIEXEC_NUMPROC_FLAG}")
message(STATUS "MPIEXEC_PREFLAGS=${MPIEXEC_PREFLAGS}")
message(STATUS "MPIEXEC_POSTFLAGS=${MPIEXEC_POSTFLAGS}")

message(STATUS "MPI_CXX_COMPILE_FLAGS=${MPI_CXX_COMPILE_FLAGS}")
message(STATUS "MPI_CXX_LINK_FLAGS=${MPI_CXX_LINK_FLAGS}")
message(STATUS "MPI_CXX_INCLUDE_PATH=${MPI_CXX_INCLUDE_PATH}")
message(STATUS "MPI_CXX_LIBRARIES=${MPI_CXX_LIBRARIES}")

add_executable(codempi codempi.cpp)
target_include_directories(codempi PUBLIC ${MPI_INCLUDE_PATH})
TARGET_LINK_LIBRARIES(codempi ${MUMPS_LIBRARIES}
${LAPACK_LIBRARIES} ${MPI_CXX_LIBRARIES})
# link: MPI a la fin sinon ca merde avec icpc (fabulous)!!
#target_compile_definitions(codempi PUBLIC "-DGMM_USES_MPI") # non! (ok uniqt si mumps/mpi)

#add_test(NAME codempi COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} 6 codempi)
# 1. mpiexec marche pas sur fabulous/intel => mpirun
# 2. ne pas oublier "./" (merde avec mpirun de intel)
add_test(NAME codempi COMMAND mpirun ${MPIEXEC_NUMPROC_FLAG} 6 ./codempi)
#===============================================================================



