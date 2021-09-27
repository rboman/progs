# Copyright 2020 University of Liège
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

PROJECT(LOUIS Fortran)
CMAKE_MINIMUM_REQUIRED(VERSION 2.6)

#SET(CMAKE_VERBOSE_MAKEFILE TRUE) # affiche les lignes de commande

# configure Fortran compiler
GET_FILENAME_COMPONENT(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

IF (Fortran_COMPILER_NAME MATCHES "f95" OR Fortran_COMPILER_NAME MATCHES "gfortran")
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

ADD_EXECUTABLE(louis src/SPH_module.f90 src/SPH_simulation.f90)

