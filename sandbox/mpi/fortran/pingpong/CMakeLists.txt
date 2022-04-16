PROJECT(PINGPONG Fortran)
cmake_minimum_required(VERSION 3.9)


# the "MPI target" (MPI::) requires cmake >=3.9
# see https://cliutils.gitlab.io/modern-cmake/chapters/packages/MPI.html
find_package(MPI REQUIRED)
message(STATUS "Run: ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_PREFLAGS} EXECUTABLE ${MPIEXEC_POSTFLAGS} ARGS")

add_executable(ping-pong-simple ping-pong-simple.f90 )
target_link_libraries(ping-pong-simple PUBLIC MPI::MPI_Fortran)

add_executable(ping-pong ping-pong.f90 )
target_link_libraries(ping-pong PUBLIC MPI::MPI_Fortran)

