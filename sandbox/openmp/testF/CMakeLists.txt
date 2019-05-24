CMAKE_MINIMUM_REQUIRED(VERSION 3.1)
PROJECT(OMP_TEST Fortran)

#SET(CMAKE_VERBOSE_MAKEFILE TRUE) # affiche les lignes de commande

# configure Fortran compiler
GET_FILENAME_COMPONENT(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

IF(Fortran_COMPILER_NAME MATCHES "f95")
    SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -fopenmp") 
ENDIF()

IF(Fortran_COMPILER_NAME MATCHES "ifort")
    IF(WIN32)
        SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /Qopenmp" )
        SET(CMAKE_Fortran_FLAGS_RELEASE "/O3 /D NDEBUG")
        #SET(CMAKE_Fortran_FLAGS_DEBUG   "/Od /debug:full /dbglibs")
    ENDIF()
ENDIF()
	
MESSAGE(STATUS "Fortran_COMPILER_NAME        : " ${Fortran_COMPILER_NAME})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS          : " ${CMAKE_Fortran_FLAGS})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS_RELEASE  : " ${CMAKE_Fortran_FLAGS_RELEASE})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS_DEBUG    : " ${CMAKE_Fortran_FLAGS_DEBUG})

ADD_EXECUTABLE(omp_test omp_test.f)
