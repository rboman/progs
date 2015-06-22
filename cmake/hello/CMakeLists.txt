# tests bidons de de cmake
# http://www.cmake.org/cmake/help/v2.8.10/cmake.html
#

cmake_minimum_required(VERSION 2.8)
message(STATUS "hello")

project(Hello)

set(HELLO_SRCS main.cpp)

add_executable(Hello ${HELLO_SRCS})

get_source_file_property(MYPRP main.cpp LOCATION)
message(STATUS "MYPRP=" ${MYPRP})




SET(MKL_LIB_PATH_DESC "directory containing mkl*.lib")
SET(MKL_LIB_PATH_MESG "Set the MKL_LIB_PATH cmake cache entry to the ${MKL_LIB_PATH_DESC}")
FIND_PATH(MKL_LIB_PATH 
             NAMES mkl_dll.lib mkl_core
             DOC "The ${MKL_LIB_PATH_MESG}"
            )
IF(NOT MKL_LIB_PATH)
    MESSAGE(FATAL_ERROR ${MKL_LIB_PATH_MESG})
ENDIF(NOT MKL_LIB_PATH)





# set(MKL_LIB_PATH "C:\\Program Files (x86)\\Intel\\ComposerXE-2011\\mkl\\lib\\intel64")
# 
# find_library(MKL_SOLVER_LIBRARY
# NAMES mkl_solver_lp64
# PATHS ${MKL_LIB_PATH}
# )
# message(STATUS "MKL_SOLVER_LIBRARY=" ${MKL_SOLVER_LIBRARY})
# 
# find_library(MKL_CORE_LIBRARY
# NAMES mkl_core
# PATHS ${MKL_LIB_PATH}
# )
# message(STATUS "MKL_CORE_LIBRARY=" ${MKL_CORE_LIBRARY})
# 
# if(MKL_CORE_LIBRARY)
#    target_link_libraries (Hello ${MKL_SOLVER_LIBRARY} ${MKL_CORE_LIBRARY})
# endif()


