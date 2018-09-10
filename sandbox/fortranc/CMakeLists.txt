PROJECT(FPROGS CXX C Fortran)
CMAKE_MINIMUM_REQUIRED(VERSION 2.6)

# CMake management of fortran name mangling
include(FortranCInterface)
FortranCInterface_HEADER(FC.h MACRO_NAMESPACE "FC_")
FortranCInterface_VERIFY(CXX) # test facultatif

#SET(CMAKE_VERBOSE_MAKEFILE TRUE) # affiche les lignes de commande

# configure Fortran compiler
GET_FILENAME_COMPONENT(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

IF (Fortran_COMPILER_NAME MATCHES "f95")
    SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall") 
ENDIF()


MESSAGE(STATUS "Fortran_COMPILER_NAME        : " ${Fortran_COMPILER_NAME})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS          : " ${CMAKE_Fortran_FLAGS})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS_RELEASE  : " ${CMAKE_Fortran_FLAGS_RELEASE})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS_DEBUG    : " ${CMAKE_Fortran_FLAGS_DEBUG})



ADD_EXECUTABLE(fortranc main.f90 c_routines.cpp f_routines.f90)
INCLUDE_DIRECTORIES(${PROJECT_BINARY_DIR})  # for FC.h

#enable_testing()
#add_test(NAME prog01 COMMAND ${EXECUTABLE_OUTPUT_PATH}/prog01
#         WORKING_DIRECTORY ${PROJECT_BINARY_DIR})