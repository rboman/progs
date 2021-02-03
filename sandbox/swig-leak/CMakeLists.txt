CMAKE_MINIMUM_REQUIRED(VERSION 3.1)
# ----------------------------------------------------------------------------
PROJECT(SWIG_LEAK CXX)
# ----------------------------------------------------------------------------

OPTION(USE_PY3        "Use Python 3"     OFF)

# send all binaries to ./bin
SET(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin CACHE PATH "")
SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin CACHE PATH  "")

# on Linux, set optimisation by default
IF(NOT CMAKE_BUILD_TYPE)
    SET(CMAKE_BUILD_TYPE "Release" CACHE STRING "" FORCE)
ENDIF()

# compiler settings
IF(CMAKE_CXX_COMPILER_ID MATCHES "MSVC")
    ADD_DEFINITIONS(-D_CRT_SECURE_NO_WARNINGS -D_SCL_SECURE_NO_DEPRECATE)
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /MP")  # parallel build with MSVC
ENDIF()

# LIST(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/CMake")
# INCLUDE(fwkMacros)

# -- GENERAL OPTIONS --

# print some info
# MESSAGE(STATUS "CMAKE_SYSTEM_NAME=\"${CMAKE_SYSTEM_NAME}\"")

# -- C++11
SET(CMAKE_CXX_STANDARD 11)
SET(CMAKE_CXX_STANDARD_REQUIRED ON)



# -- Search for Python 2 or 3 depending on "USE_PY3"
IF (CMAKE_VERSION VERSION_LESS 3.12.0)
    # "old" way with old cmake 
    #   - ok on linux with python2 and 3 installed
    #   - ok on windows with a single python in the PATH
    #     otherwise finds python3 even if python2 is required and python3 in the PATH
    IF(USE_PY3)
        FIND_PACKAGE(PythonInterp 3 EXACT REQUIRED)
        FIND_PACKAGE(PythonLibs 3 EXACT REQUIRED)
    ELSE()
        FIND_PACKAGE(PythonInterp 2.7 EXACT REQUIRED)
        FIND_PACKAGE(PythonLibs 2.7 EXACT REQUIRED)
    ENDIF()
    # IF(NOT PYTHON_DEBUG_LIBRARY AND PYTHON_LIBRARY) # [RB] is it still needed?
    #     SET(PYTHON_DEBUG_LIBRARY ${PYTHON_LIBRARY})
    # ENDIF()
ELSE()
    # new way since 3.12
    IF(USE_PY3)
        find_package (Python3 COMPONENTS Interpreter Development)
        # use Python3_ROOT_DIR is wrong python found (e.g. anaconda)
        SET(PYTHON_EXECUTABLE ${Python3_EXECUTABLE})
        SET(PYTHON_LIBRARIES ${Python3_LIBRARIES})
        SET(PYTHON_INCLUDE_PATH ${Python3_INCLUDE_DIRS}) 
        SET(PYTHONLIBS_VERSION_STRING ${Python3_VERSION})       
    ELSE()
        find_package (Python2 COMPONENTS Interpreter Development)
        # use Python2_ROOT_DIR is wrong python found (e.g. anaconda)
        SET(PYTHON_EXECUTABLE ${Python2_EXECUTABLE})
        SET(PYTHON_LIBRARIES ${Python2_LIBRARIES})
        SET(PYTHON_INCLUDE_PATH ${Python2_INCLUDE_DIRS})  
        SET(PYTHONLIBS_VERSION_STRING ${Python2_VERSION})  
    ENDIF()
ENDIF()
MESSAGE(STATUS "PYTHON_EXECUTABLE:FILEPATH=${PYTHON_EXECUTABLE}")
MESSAGE(STATUS "PYTHON_LIBRARIES:FILEPATH=${PYTHON_LIBRARIES}")
MESSAGE(STATUS "PYTHON_INCLUDE_DIR:FILEPATH=${PYTHON_INCLUDE_DIR}")
MESSAGE(STATUS "PYTHONLIBS_VERSION_STRING=${PYTHONLIBS_VERSION_STRING}")

# -- Search for SWIG
FIND_PACKAGE(SWIG REQUIRED)
IF(CMAKE_GENERATOR MATCHES "Visual Studio") # not MSVC because of nmake & jom
    SET(CMAKE_SWIG_OUTDIR "${EXECUTABLE_OUTPUT_PATH}/$(Configuration)/")
ELSE()
    SET(CMAKE_SWIG_OUTDIR "${EXECUTABLE_OUTPUT_PATH}")
ENDIF()

if (${PYTHONLIBS_VERSION_STRING} MATCHES "^3.")
    message(STATUS "SWIG: Using Python 3 mode.")
    set(CMAKE_SWIG_FLAGS -py3 -O)
    #set(CMAKE_SWIG_FLAGS -O)
else()
    message(STATUS "SWIG: Using Python 2 mode.")
    set(CMAKE_SWIG_FLAGS -O)
endif()

# ----------- project


INCLUDE(${SWIG_USE_FILE})

FILE(GLOB SRCS *.h *.cpp *.inl *.swg)
FILE(GLOB ISRCS *.i)

SET_SOURCE_FILES_PROPERTIES(${ISRCS} PROPERTIES CPLUSPLUS ON)


IF(CMAKE_VERSION VERSION_LESS 3.8.0)
    SWIG_ADD_MODULE(leak python ${ISRCS} ${SRCS})
ELSE()
    SWIG_ADD_LIBRARY(leak LANGUAGE python SOURCES ${ISRCS} ${SRCS})
ENDIF()

TARGET_INCLUDE_DIRECTORIES(_leak PRIVATE ${PYTHON_INCLUDE_PATH} ${PROJECT_SOURCE_DIR})

SWIG_LINK_LIBRARIES(leak ${PYTHON_LIBRARIES})
