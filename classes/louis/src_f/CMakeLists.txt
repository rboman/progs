CMAKE_MINIMUM_REQUIRED(VERSION 3.18...3.22)
PROJECT(LOUIS_F 
        VERSION 1.0
        DESCRIPTION "Original SPH code from Louis GOFFIN (Fortran)"
        HOMEPAGE_URL "https://github.com/rboman/progs"
        LANGUAGES Fortran)

#SET(CMAKE_VERBOSE_MAKEFILE TRUE) # affiche les lignes de commande

# configure Fortran compiler
GET_FILENAME_COMPONENT(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

IF (Fortran_COMPILER_NAME MATCHES "f95" OR Fortran_COMPILER_NAME MATCHES "gfortran")
    SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall") 
ENDIF()

IF(Fortran_COMPILER_NAME MATCHES "ifort")
    IF(WIN32)
        SET(CMAKE_Fortran_FLAGS_RELEASE "/O3 /D NDEBUG")
        #SET(CMAKE_Fortran_FLAGS_DEBUG   "/Od /debug:full /dbglibs")
    ENDIF()
ENDIF()
	
MESSAGE(STATUS "Fortran_COMPILER_NAME        : " ${Fortran_COMPILER_NAME})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS          : " ${CMAKE_Fortran_FLAGS})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS_RELEASE  : " ${CMAKE_Fortran_FLAGS_RELEASE})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS_DEBUG    : " ${CMAKE_Fortran_FLAGS_DEBUG})

FILE(GLOB SRCS *.f90)
ADD_EXECUTABLE(louis)
TARGET_SOURCES(louis PRIVATE ${SRCS})

# find OpenMP
FIND_PACKAGE(OpenMP)
IF(OPENMP_FOUND)
    MESSAGE (STATUS "OpenMP enabled (OpenMP_Fortran_FLAGS=${OpenMP_Fortran_FLAGS})")
ELSE()
    MESSAGE (STATUS "OpenMP disabled.")
ENDIF()
IF(OpenMP_Fortran_FOUND)
    TARGET_LINK_LIBRARIES(louis PUBLIC OpenMP::OpenMP_Fortran)
ENDIF()


