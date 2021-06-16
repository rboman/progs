CMAKE_MINIMUM_REQUIRED(VERSION 3.1)
PROJECT(FPROGS Fortran)

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

ADD_EXECUTABLE(prog01 prog01.f90 progmod.f90)
ADD_EXECUTABLE(test_entry test_entry.f90) 
ADD_EXECUTABLE(test_formats test_formats.f90)
ADD_EXECUTABLE(test_debug test_debug.f90)
ADD_EXECUTABLE(test_sort test_sort.f90)
ADD_EXECUTABLE(test_quicksort test_quicksort.f90)
ADD_EXECUTABLE(test_print test_print.f90)
ADD_EXECUTABLE(test_strings test_strings.f90)
ADD_EXECUTABLE(test_getarg test_getarg.f90)
ADD_EXECUTABLE(test_stdin test_stdin.f90)


# ctest
ENABLE_TESTING()
ADD_TEST(NAME prog01 COMMAND ${EXECUTABLE_OUTPUT_PATH}/prog01
         WORKING_DIRECTORY ${PROJECT_BINARY_DIR})
