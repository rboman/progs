CMAKE_MINIMUM_REQUIRED(VERSION 3.1)
PROJECT(FORTRAN_OOP Fortran)

#SET(CMAKE_VERBOSE_MAKEFILE TRUE) # affiche les lignes de commande

# configure Fortran compiler
GET_FILENAME_COMPONENT(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

IF(Fortran_COMPILER_NAME MATCHES "f95" OR Fortran_COMPILER_NAME MATCHES "gfortran")
    # SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall") 
    SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -Wextra -fPIC -fmax-errors=1 -fcheck=all -fbacktrace")
    SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wimplicit-interface")
    SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wpedantic")
ELSEIF(Fortran_COMPILER_NAME MATCHES "ifort")
    IF(MSVC)
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /MP")  # parallel build with MSVC
        string(REPLACE "/W1" "/warn:all" CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}")
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /check:all")
    ELSE()
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -warn all -check all")
    ENDIF()
ENDIF()

MESSAGE(STATUS "Fortran_COMPILER_NAME        : " ${Fortran_COMPILER_NAME})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS          : " ${CMAKE_Fortran_FLAGS})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS_RELEASE  : " ${CMAKE_Fortran_FLAGS_RELEASE})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS_DEBUG    : " ${CMAKE_Fortran_FLAGS_DEBUG})



# executables
#   .f90/.f95 => free format | .f/.for => fixed

FILE(GLOB SRCS *.f90)
ADD_EXECUTABLE(oop ${SRCS})


