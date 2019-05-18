CMAKE_MINIMUM_REQUIRED(VERSION 3.1)
PROJECT(FPROGS Fortran)

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

FILE(GLOB SRCS *.for)
ADD_LIBRARY(tfe STATIC ${SRCS})

ADD_EXECUTABLE(test_bicg test_bicg.for) # .f90/.f95 => free format | .f/.for => fixed
TARGET_LINK_LIBRARIES(test_bicg tfe)

ADD_EXECUTABLE(test_bicgst test_bicgst.for) # .f90/.f95 => free format | .f/.for => fixed
TARGET_LINK_LIBRARIES(test_bicgst tfe)

ADD_EXECUTABLE(test_cgs test_cgs.for) # .f90/.f95 => free format | .f/.for => fixed
TARGET_LINK_LIBRARIES(test_cgs tfe)

# ENABLE_TESTING()
# ADD_TEST(NAME theoric COMMAND ${EXECUTABLE_OUTPUT_PATH}/theoric
#         WORKING_DIRECTORY ${PROJECT_BINARY_DIR})