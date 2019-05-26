CMAKE_MINIMUM_REQUIRED(VERSION 3.1)
PROJECT(FPROGS Fortran C )

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


# -- Search for Python
FIND_PACKAGE(PythonInterp 2.7 REQUIRED)
FIND_PACKAGE(PythonLibs 2.7 REQUIRED)
MESSAGE(STATUS "PYTHON_EXECUTABLE:FILEPATH=${PYTHON_EXECUTABLE}")
MESSAGE(STATUS "PYTHON_LIBRARY:FILEPATH=${PYTHON_LIBRARY}")
MESSAGE(STATUS "PYTHON_INCLUDE_DIR:FILEPATH=${PYTHON_INCLUDE_DIR}")
MESSAGE(STATUS "PYTHON_FRAMEWORK_INCLUDES=${PYTHON_FRAMEWORK_INCLUDES}")
MESSAGE(STATUS "PYTHONLIBS_VERSION_STRING=${PYTHONLIBS_VERSION_STRING}")
MESSAGE(STATUS "Python_FRAMEWORKS=${Python_FRAMEWORKS}")

# serach for f2py
find_program(F2PY_EXECUTABLE NAMES f2py f2py${PYTHON_VERSION_MAJOR})
message("F2PY_EXECUTABLE=${F2PY_EXECUTABLE}")

# Find out the include path of numpy
execute_process(
COMMAND "${PYTHON_EXECUTABLE}" -c
        "from __future__ import print_function\ntry: import numpy; print(numpy.get_include(), end='')\nexcept:pass\n"
        OUTPUT_VARIABLE __numpy_path)
message("__numpy_path=${__numpy_path}")

# And the version of numpy
execute_process(
COMMAND "${PYTHON_EXECUTABLE}" -c
        "from __future__ import print_function\ntry: import numpy; print(numpy.__version__, end='')\nexcept:pass\n"
OUTPUT_VARIABLE __numpy_version)
message("__numpy_version=${__numpy_version}")

# Find out the include path of numpy/f2py/src
execute_process(
COMMAND "${PYTHON_EXECUTABLE}" -c
        "from __future__ import print_function; import os; import numpy.f2py; print(os.path.dirname(numpy.f2py.__file__).replace(os.sep,'/'), end='')"
        OUTPUT_VARIABLE __f2py_path)
message("__f2py_path=${__f2py_path}")

# fortranobject.h
FIND_PATH(F2PY_SRC_DIR NAMES "fortranobject.h" HINTS ${__f2py_path}/src) # "C:/msys64/mingw64/lib/python2.7/site-packages/numpy/f2py/src")
message("F2PY_SRC_DIR=${F2PY_SRC_DIR}")


set(f2py_module_name vect)
set(generated_module_file ${CMAKE_CURRENT_BINARY_DIR}/${f2py_module_name}${PYTHON_EXTENSION_MODULE_SUFFIX})
set(fortran_src_file ${CMAKE_CURRENT_SOURCE_DIR}/norme.f90 ${CMAKE_CURRENT_SOURCE_DIR}/fib.f)

add_custom_command(
    OUTPUT vectmodule.c
    COMMAND ${F2PY_EXECUTABLE}
    -m ${f2py_module_name}
    --lower   # gcc cree du lowercase quel que soit le nom dans le code
    ${fortran_src_file}
    DEPENDS ${fortran_src_file}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )


ADD_LIBRARY(vectfor ${fortran_src_file})

ADD_LIBRARY(vect SHARED vectmodule.c ${F2PY_SRC_DIR}/fortranobject.c ) 
TARGET_INCLUDE_DIRECTORIES(vect PRIVATE ${PYTHON_INCLUDE_PATH})
TARGET_INCLUDE_DIRECTORIES(vect PRIVATE ${F2PY_SRC_DIR})
TARGET_LINK_LIBRARIES(vect vectfor ${PYTHON_LIBRARY})
SET_TARGET_PROPERTIES(vect PROPERTIES SUFFIX .pyd)
SET_TARGET_PROPERTIES(vect PROPERTIES PREFIX "")
