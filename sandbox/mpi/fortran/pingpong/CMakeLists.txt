PROJECT(PINGPONG Fortran)
cmake_minimum_required(VERSION 3.1)



add_executable(pingpong ping-pong-simple.f90 )

find_package(MPI REQUIRED)
add_definitions(${MPI_Fortran_COMPILE_FLAGS})
include_directories(${MPI_Fortran_INCLUDE_PATH})
link_directories(${MPI_Fortran_LIBRARIES})
target_link_libraries(pingpong ${MPI_Fortran_LIBRARIES})

