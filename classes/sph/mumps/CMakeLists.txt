FILE(GLOB SRCS *.h *.cpp)
ADD_EXECUTABLE(test_mumps ${SRCS})
INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR}/vtl ${PROJECT_SOURCE_DIR}/json)
TARGET_LINK_LIBRARIES(test_mumps vtl json)



# MPI --
# --------------------------------------------------------------
# Finds MPI (including MSMPI)
# --------------------------------------------------------------
if(WIN32)  
    FIND_PATH(MPI_INCLUDE_PATH NAMES  mpi.h HINTS "$ENV{MSMPI_INC}")
    if("${CMAKE_SIZEOF_VOID_P}" EQUAL 8)
        set(MS_MPI_ARCH_DIR x64)
        find_library(MPI_LIBRARIES         msmpi    PATHS "$ENV{MSMPI_LIB64}")
        find_library(MPI_Fortran_LIBRARIES msmpifec PATHS "$ENV{MSMPI_LIB64}")
    else()
        set(MS_MPI_ARCH_DIR x86)
        find_library(MPI_LIBRARIES         msmpi    PATHS "$ENV{MSMPI_LIB32}")
        find_library(MPI_Fortran_LIBRARIES msmpifec PATHS "$ENV{MSMPI_LIB32}")
    endif()
else()
    find_package(MPI REQUIRED)
endif()
# --------------------------------------------------------------

include_directories(${MPI_INCLUDE_PATH})
#target_link_libraries(test_mumps ${MPI_LIBRARIES})
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

# MUMPS ----------------------------------------------------------- 
MESSAGE("Looking for MUMPS...")
# mumps include dir
find_path(MUMPS_INCLUDE_DIRS NAMES "dmumps_c.h")
message(STATUS "MUMPS_INCLUDE_DIRS=" ${MUMPS_INCLUDE_DIRS})
if(NOT MUMPS_INCLUDE_DIRS)
    message(FATAL_ERROR "MUMPS include dir not found!")
endif()
include_directories(${MUMPS_INCLUDE_DIRS})

# mumps libs
set(MUMPS_LIBRARIES "")

# MUMPS libs to be found
SET(_MUMPS_LIBSET dmumps mumps_common pord)

foreach(LIB ${_MUMPS_LIBSET})
    find_library(FOUND_LIB ${LIB})
    MESSAGE(STATUS "${LIB}=${FOUND_LIB}")
    if(FOUND_LIB)
        list(APPEND MUMPS_LIBRARIES ${FOUND_LIB})
    endif()
    unset(FOUND_LIB CACHE)
endforeach()

target_link_libraries(test_mumps ${MUMPS_LIBRARIES})

# METIS/ParMETIS ----------------------------------------------------------- 
MESSAGE("Looking for METIS/ParMETIS...")
find_path(METIS_INCLUDE_DIRS NAMES "metis.h")
message(STATUS "METIS_INCLUDE_DIRS=" ${METIS_INCLUDE_DIRS})
if(NOT METIS_INCLUDE_DIRS)
    message(FATAL_ERROR "METIS include dir not found!")
endif()
include_directories(${METIS_INCLUDE_DIRS})

# metis libs
set(METIS_LIBRARIES "")

# METIS libs to be found
SET(_METIS_LIBSET parmetis metis)

foreach(LIB ${_METIS_LIBSET})
    find_library(FOUND_LIB ${LIB})
    MESSAGE(STATUS "${LIB}=${FOUND_LIB}")
    if(FOUND_LIB)
        list(APPEND METIS_LIBRARIES ${FOUND_LIB})
    endif()
    unset(FOUND_LIB CACHE)
endforeach()

target_link_libraries(test_mumps ${METIS_LIBRARIES})

# MKL libs ------------------
MESSAGE("Looking for MKL...")
set(MKL_LIBRARIES "")

IF(MINGW)
    # static link, otherwise mkl_scalapack_intelmpi_lp64 called at runtime when metis is used
    SET(_MKL_LIBSET mkl_scalapack_lp64 mkl_intel_lp64 mkl_intel_thread mkl_core mkl_blacs_msmpi_lp64  libiomp5md)
ELSE()
    SET(_MKL_LIBSET mkl_scalapack_lp64 mkl_gf_lp64 mkl_gnu_thread mkl_core mkl_blacs_openmpi_lp64)
ENDIF()

foreach(LIB ${_MKL_LIBSET})
    find_library(FOUND_LIB ${LIB} PATHS $ENV{MKLROOT}/lib/intel64)
    MESSAGE(STATUS "${LIB}=${FOUND_LIB}")
    if(FOUND_LIB)
        list(APPEND MKL_LIBRARIES ${FOUND_LIB})
    endif()
    unset(FOUND_LIB CACHE)
endforeach()

# start-group/end-group permet de resoudreles dependances circulaires lors d'un link statique (mingw)
target_link_libraries(test_mumps -Wl,--start-group ${MKL_LIBRARIES} -Wl,--end-group )

# MPI / Fortran ------------------
target_link_libraries(test_mumps ${MPI_Fortran_LIBRARIES} ${MPI_LIBRARIES} ${CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES})

# RapidJSON include dir
find_path(RAPIDJSON_INCLUDE_DIRS NAMES "rapidjson/rapidjson.h")
message(STATUS "RAPIDJSON_INCLUDE_DIRS=" ${RAPIDJSON_INCLUDE_DIRS})
if(NOT RAPIDJSON_INCLUDE_DIRS)
    message(FATAL_ERROR "RapidJSON include dir not found!")
endif()
include_directories(${RAPIDJSON_INCLUDE_DIRS})


# Tests
add_test(NAME test_mumps
         COMMAND ${CMAKE_BINARY_DIR}/bin/test_mumps)