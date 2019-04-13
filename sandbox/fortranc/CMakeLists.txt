CMAKE_MINIMUM_REQUIRED(VERSION 3.1)
PROJECT(FPROGS CXX C Fortran)

# CMake management of fortran name mangling
INCLUDE(FortranCInterface)
FortranCInterface_HEADER(FC.h MACRO_NAMESPACE "FC_")
FortranCInterface_VERIFY(CXX) # test facultatif

#SET(CMAKE_VERBOSE_MAKEFILE TRUE) # affiche les lignes de commande

# configure Fortran compiler
GET_FILENAME_COMPONENT(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

IF(Fortran_COMPILER_NAME MATCHES "f95")
    SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall") 
ENDIF()

MESSAGE(STATUS "Fortran_COMPILER_NAME        : " ${Fortran_COMPILER_NAME})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS          : " ${CMAKE_Fortran_FLAGS})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS_RELEASE  : " ${CMAKE_Fortran_FLAGS_RELEASE})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS_DEBUG    : " ${CMAKE_Fortran_FLAGS_DEBUG})

# la ligne suivante ne marche pas avec Visual Studio + Intel fortran 
# (on peut pas mixer fortran & c dans un target)
#ADD_EXECUTABLE(fortranc main.f90 c_routines.cpp f_routines.f90) 

# version Visual Studio + Intel fortran (ok aussi pour gcc)
ADD_LIBRARY(codef f_routines.f90)
ADD_LIBRARY(codec c_routines.cpp)
ADD_EXECUTABLE(fortranc main.f90) 
TARGET_LINK_LIBRARIES(fortranc codec codef) # avec VS, on peut intervertir l'ordre des libs! (pas avec gcc)

INCLUDE_DIRECTORIES(${PROJECT_BINARY_DIR})  # for FC.h

ENABLE_TESTING()
ADD_TEST(NAME fortranc COMMAND ${EXECUTABLE_OUTPUT_PATH}/fortranc
         WORKING_DIRECTORY ${PROJECT_BINARY_DIR})