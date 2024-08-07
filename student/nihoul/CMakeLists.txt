CMAKE_MINIMUM_REQUIRED(VERSION 3.1)
PROJECT(Nihoul Fortran)

# configure Fortran compiler
GET_FILENAME_COMPONENT(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

IF(Fortran_COMPILER_NAME MATCHES "f95")
    SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall") 
ENDIF()

MESSAGE(STATUS "Fortran_COMPILER_NAME        : " ${Fortran_COMPILER_NAME})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS          : " ${CMAKE_Fortran_FLAGS})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS_RELEASE  : " ${CMAKE_Fortran_FLAGS_RELEASE})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS_DEBUG    : " ${CMAKE_Fortran_FLAGS_DEBUG})

# reference executable (unmodified)
ADD_EXECUTABLE(nihoul_orig orig/nihoul.for) 

FILE(GLOB SRCS nihoul.f90)      # .f90/.f95 => free format | .f/.for => fixed
ADD_EXECUTABLE(nihoul ${SRCS}) 

ENABLE_TESTING()
ADD_TEST(NAME nihoul COMMAND ${EXECUTABLE_OUTPUT_PATH}/nihoul
         WORKING_DIRECTORY ${PROJECT_BINARY_DIR})