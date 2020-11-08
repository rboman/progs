PROJECT(VTKLITE CXX Fortran)
# "Fortran" is required here because of MUMPS static library
cmake_minimum_required(VERSION 3.0)

# put all generated files in a single folder (required in windows for exe linked to dlls)
SET(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin CACHE PATH "")
SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin CACHE PATH "")
MARK_AS_ADVANCED(LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)

# build type is "" by default in Linux
IF(NOT CMAKE_BUILD_TYPE)
    SET( CMAKE_BUILD_TYPE "Release" CACHE STRING "" FORCE)
ENDIF(NOT CMAKE_BUILD_TYPE)

# enable C++11
include(CheckCXXCompilerFlag)
check_cxx_compiler_flag("-std=c++11" STD11CHECK)
IF(STD11CHECK)
    ADD_DEFINITIONS(-std=c++11 )
ENDIF()
IF("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
    ADD_DEFINITIONS(-Wno-literal-suffix)
ENDIF()


# add doxygen ------------------------------------------------------------
# from https://vicrucann.github.io/tutorials/quick-cmake-doxygen/
option(BUILD_DOC "Build documentation" ON)

# check if Doxygen is installed
find_package(Doxygen)
if (DOXYGEN_FOUND)
    # set input and output files
    set(DOXYGEN_IN ${PROJECT_SOURCE_DIR}/Doxyfile.in)
    set(DOXYGEN_OUT ${PROJECT_BINARY_DIR}/Doxyfile)

    # request to configure the file
    configure_file(${DOXYGEN_IN} ${DOXYGEN_OUT} @ONLY)
    #message("Doxygen build started")

    # note the option ALL which allows to build the docs together with the application
    # "make dox" rebuilds the doc
    add_custom_target( dox #ALL
        COMMAND ${DOXYGEN_EXECUTABLE} ${DOXYGEN_OUT}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Generating API documentation with Doxygen"
        VERBATIM )
else (DOXYGEN_FOUND)
  message("Doxygen need to be installed to generate the doxygen documentation")
endif (DOXYGEN_FOUND)
# ------------------------------------------------------------------------

enable_testing()

# activation des modules
OPTION(MATH0471_USE_MUMPS    "Compile exe depending on MUMPS"     ON)
OPTION(MATH0471_USE_MPI      "Compile exe depending on MPI"       ON)


ADD_SUBDIRECTORY( tools )
ADD_SUBDIRECTORY( vtl )
ADD_SUBDIRECTORY( json )
ADD_SUBDIRECTORY( exe/sph )
ADD_SUBDIRECTORY( exe/fdtd )
IF(MATH0471_USE_MPI)
    ADD_SUBDIRECTORY( exe/fdtd_mpi )
ENDIF()
IF(MATH0471_USE_MUMPS)
    ADD_SUBDIRECTORY( mumps )
ENDIF()

ADD_SUBDIRECTORY( tests )
