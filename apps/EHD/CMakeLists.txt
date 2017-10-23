# ----------------------------------------------------------------------------
PROJECT(EHD CXX)
# ----------------------------------------------------------------------------
CMAKE_MINIMUM_REQUIRED(VERSION 2.6)


SET(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin CACHE PATH
                        "Single output directory for building all libraries.")
SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin CACHE PATH
                        "Single output directory for building all executables.")
MARK_AS_ADVANCED(LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)

IF(NOT CMAKE_BUILD_TYPE)
    SET( CMAKE_BUILD_TYPE "Release" CACHE STRING 
         "Choose the type of build, options are: None Debug Release RelWithDebInfo MinSizeRel."
         FORCE)
ENDIF(NOT CMAKE_BUILD_TYPE)

IF(MSVC)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS -D_SCL_SECURE_NO_DEPRECATE)
ENDIF(MSVC)

IF(MSVC_VERSION)
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /MP")  # parallel build with MSVC
ENDIF(MSVC_VERSION)

LIST(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/CMake")
INCLUDE(fwkMacros)

# -- GENERAL OPTIONS --

# print some info
MESSAGE(STATUS "CMAKE_SYSTEM_NAME=\"${CMAKE_SYSTEM_NAME}\"")

# -- C++11
MESSAGE(STATUS "CMAKE_CXX_COMPILER_ID=${CMAKE_CXX_COMPILER_ID}")
IF( (CMAKE_CXX_COMPILER_ID MATCHES "GNU") OR (CMAKE_CXX_COMPILER_ID MATCHES "Intel"))
#IF(UNIX)
	ADD_DEFINITIONS(
	    -std=gnu++0x #c++0x # =c++11 # Or -std=c++0x
	)
	SET(CMAKE_SHARED_LINKER_FLAGS "-Wl,--no-as-needed")
ENDIF()
IF( CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    ADD_DEFINITIONS(-std=c++11 -Wno-deprecated-register)
ENDIF()

# -- Search for Python
FIND_PACKAGE(PythonInterp REQUIRED)
FIND_PACKAGE(PythonLibs REQUIRED)
message(STATUS "PYTHON_EXECUTABLE:FILEPATH=${PYTHON_EXECUTABLE}")
message(STATUS "PYTHON_LIBRARY:FILEPATH=${PYTHON_LIBRARY}")
message(STATUS "PYTHON_INCLUDE_DIR:FILEPATH=${PYTHON_INCLUDE_DIR}")
message(STATUS "PYTHON_FRAMEWORK_INCLUDES=${PYTHON_FRAMEWORK_INCLUDES}")
message(STATUS "PYTHONLIBS_VERSION_STRING=${PYTHONLIBS_VERSION_STRING}")
message(STATUS "Python_FRAMEWORKS=${Python_FRAMEWORKS}")


# -- Search for SWIG
FIND_PACKAGE(SWIG REQUIRED)
IF(CMAKE_GENERATOR MATCHES "Visual Studio") # not MSVC because of nmake & jom
    SET(CMAKE_SWIG_OUTDIR "${EXECUTABLE_OUTPUT_PATH}/$(Configuration)/")
ELSE()
    SET(CMAKE_SWIG_OUTDIR "${EXECUTABLE_OUTPUT_PATH}")
ENDIF()

# -- CTest
#enable_testing()

# -- Sub directories

ADD_SUBDIRECTORY( sky )
ADD_SUBDIRECTORY( gauss )
ADD_SUBDIRECTORY( ehd )
