# GMM / MUMPS sequentiel mais code MPI

cmake_minimum_required(VERSION 2.7)

SET(CMAKE_C_COMPILER "icc" CACHE FILEPATH "" FORCE)
SET(CMAKE_CXX_COMPILER "icpc" CACHE FILEPATH "" FORCE)

project(TestMUMPS CXX)

if(WIN32)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS -D_SCL_SECURE_NO_DEPRECATE)
endif(WIN32)

# mumps/sequentiel
add_definitions(-DGMM_USES_MUMPS)  
include_directories($ENV{HOME}/local/include)

# mumps/mpi
find_package(MPI REQUIRED)
#add_definitions(-DGMM_USES_MPI) # GMM sequentiel mais code MPI 
include_directories(${MPI_INCLUDE_PATH})

add_executable(testMUMPS testMUMPS.cpp)


find_library( MUMPS_LIBRARY1 
              dmumps
              PATHS "$ENV{HOME}/local/lib/"
                    "/usr/lib" )

find_library( MUMPS_LIBRARY2 
              mumps_common
              PATHS "$ENV{HOME}/local/lib/"
                    "/usr/lib" )

find_library( MUMPS_LIBRARY3 
              pord 
              PATHS "$ENV{HOME}/local/lib/"
                    "/usr/lib" )

find_library( MUMPS_LIBRARY4 
              mpiseq 
              PATHS "$ENV{HOME}/local/lib/"
                    "/usr/lib" )







#TARGET_LINK_LIBRARIES(testMUMPS dmumps_seq)  # mumps/sequentiel


TARGET_LINK_LIBRARIES(testMUMPS ${MUMPS_LIBRARY1} ${MUMPS_LIBRARY2}
                      ${MUMPS_LIBRARY3} blas pthread
/cm/shared/apps/intel/composer_xe/2013_sp1.1.106/compiler/lib/intel64/libifcore.so)  # mumps nic4

TARGET_LINK_LIBRARIES(testMUMPS ${MPI_LIBRARIES} ${MUMPS_LIBRARY4})
