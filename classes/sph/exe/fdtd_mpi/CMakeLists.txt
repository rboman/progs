FILE(GLOB SRCS *.h *.cpp)
ADD_EXECUTABLE(fdtd_mpi ${SRCS})

# MPI --
# --------------------------------------------------------------
# Finds MPI (including MSMPI)
# --------------------------------------------------------------
if(WIN32)  
    FIND_PATH(MPI_INCLUDE_PATH NAMES  mpi.h HINTS "$ENV{MSMPI_INC}")
    if("${CMAKE_SIZEOF_VOID_P}" EQUAL 8)
        set(MS_MPI_ARCH_DIR x64)
        find_library(MPI_LIBRARIES msmpi PATHS "$ENV{MSMPI_LIB64}")
    else()
        set(MS_MPI_ARCH_DIR x86)
        find_library(MPI_LIBRARIES msmpi PATHS "$ENV{MSMPI_LIB32}")
    endif()
else()
    find_package(MPI REQUIRED)
endif()
# --------------------------------------------------------------

include_directories(${MPI_INCLUDE_PATH})
target_link_libraries(fdtd_mpi ${MPI_LIBRARIES})
IF( MINGW )
    add_definitions(-DMSMPI_NO_SAL)
ENDIF()


# OpenMP --
find_package(OpenMP REQUIRED)
if(OPENMP_FOUND)
    #MESSAGE ("OpenMP_CXX_FLAGS=${OpenMP_CXX_FLAGS}")
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
endif()



INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR}/vtl)
TARGET_LINK_LIBRARIES(fdtd_mpi vtl)


# Tests
add_test(NAME fdtd_mpi
         COMMAND mpirun -np 2 ${CMAKE_BINARY_DIR}/bin/fdtd_mpi)