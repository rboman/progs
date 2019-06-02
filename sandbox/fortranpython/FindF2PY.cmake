# FindF2PY
#
# output variables:
#   F2PY_EXECUTABLE
#   NUMPY_PATH
#   NUMPY_VERSION
#   F2PY_PATH
#   F2PY_SRC_DIR
#

# -- Search for Python
FIND_PACKAGE(PythonInterp 2.7 REQUIRED)
FIND_PACKAGE(PythonLibs 2.7 REQUIRED)
MESSAGE(STATUS "PYTHON_EXECUTABLE:FILEPATH=${PYTHON_EXECUTABLE}")
MESSAGE(STATUS "PYTHON_LIBRARY:FILEPATH=${PYTHON_LIBRARY}")
MESSAGE(STATUS "PYTHON_INCLUDE_DIR:FILEPATH=${PYTHON_INCLUDE_DIR}")
MESSAGE(STATUS "PYTHON_FRAMEWORK_INCLUDES=${PYTHON_FRAMEWORK_INCLUDES}")
MESSAGE(STATUS "PYTHONLIBS_VERSION_STRING=${PYTHONLIBS_VERSION_STRING}")
MESSAGE(STATUS "Python_FRAMEWORKS=${Python_FRAMEWORKS}")

# search for f2py   => F2PY_EXECUTABLE
FIND_PROGRAM(F2PY_EXECUTABLE NAMES f2py f2py${PYTHON_VERSION_MAJOR})
MESSAGE("F2PY_EXECUTABLE=${F2PY_EXECUTABLE}")

# Find out the include path of numpy => NUMPY_PATH
EXECUTE_PROCESS(
COMMAND "${PYTHON_EXECUTABLE}" -c
        "from __future__ import print_function\ntry: import numpy; print(numpy.get_include(), end='')\nexcept:pass\n"
        OUTPUT_VARIABLE NUMPY_PATH)
MESSAGE("NUMPY_PATH=${NUMPY_PATH}")

# And the version of numpy => NUMPY_VERSION
EXECUTE_PROCESS(
COMMAND "${PYTHON_EXECUTABLE}" -c
        "from __future__ import print_function\ntry: import numpy; print(numpy.__version__, end='')\nexcept:pass\n"
OUTPUT_VARIABLE NUMPY_VERSION)
MESSAGE("NUMPY_VERSION=${NUMPY_VERSION}")

# Find out the include path of numpy/f2py/src => F2PY_PATH
EXECUTE_PROCESS(
COMMAND "${PYTHON_EXECUTABLE}" -c
        "from __future__ import print_function; import os; import numpy.f2py; print(os.path.dirname(numpy.f2py.__file__).replace(os.sep,'/'), end='')"
        OUTPUT_VARIABLE F2PY_PATH)
MESSAGE("F2PY_PATH=${F2PY_PATH}")

# fortranobject.h => F2PY_SRC_DIR
FIND_PATH(F2PY_SRC_DIR NAMES "fortranobject.h" HINTS ${F2PY_PATH}/src) # "C:/msys64/mingw64/lib/python2.7/site-packages/numpy/f2py/src")
MESSAGE("F2PY_SRC_DIR=${F2PY_SRC_DIR}")

# ----------------------------------------------------------------------------   

MACRO(F2PY_MACRO F2PYMODULE FSRCS_LIST USEMODULES USEFUNCTIONS)
    MESSAGE(STATUS "Setting up f2py Fortran/Python interface \"${F2PYMODULE}\"")
    MESSAGE("USEMODULES=${USEMODULES}")
    MESSAGE("USEFUNCTIONS=${USEFUNCTIONS}")

    SET(C_OUTPUTS ${F2PYMODULE}module.c)
    SET(F90_OUTPUTS ${F2PYMODULE}-f2pywrappers2.f90)  # if modules are presents
    SET(F77_OUTPUTS ${F2PYMODULE}-f2pywrappers.f)     # if f77 functions (outside modules)

    SET(F_OUTPUTS "")
    IF(${USEMODULES})
        SET(F_OUTPUTS ${F_OUTPUTS} ${F90_OUTPUTS})
    ENDIF()
    IF(${USEFUNCTIONS})
        SET(F_OUTPUTS ${F_OUTPUTS} ${F77_OUTPUTS})
    ENDIF()
    SET(OUTPUTS ${C_OUTPUTS} ${F_OUTPUTS})
    MESSAGE("OUTPUTS=${OUTPUTS}")

    ADD_CUSTOM_COMMAND(
        OUTPUT ${OUTPUTS}
        COMMAND ${F2PY_EXECUTABLE}
        -m ${F2PYMODULE}
        --lower   # gcc cree du lowercase quel que soit le nom dans le code
        ${${FSRCS_LIST}}
        DEPENDS ${${FSRCS_LIST}}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )

    # static library "XXXfor" with the fortran source and fortran wrappers
    ADD_LIBRARY(${F2PYMODULE}for ${${FSRCS_LIST}} ${F_OUTPUTS}) 

    # shared python module XXX.pyd  
    ADD_LIBRARY(${F2PYMODULE} SHARED ${F2PYMODULE}module.c ${F2PY_SRC_DIR}/fortranobject.c ) 
    SET_TARGET_PROPERTIES(${F2PYMODULE} PROPERTIES SUFFIX .pyd)
    SET_TARGET_PROPERTIES(${F2PYMODULE} PROPERTIES PREFIX "")
    TARGET_INCLUDE_DIRECTORIES(${F2PYMODULE} PRIVATE ${PYTHON_INCLUDE_PATH})
    TARGET_INCLUDE_DIRECTORIES(${F2PYMODULE} PRIVATE ${F2PY_SRC_DIR})
    # ...linked to fortran routines and python 
    TARGET_LINK_LIBRARIES(${F2PYMODULE} ${F2PYMODULE}for ${PYTHON_LIBRARY})

ENDMACRO()



# ----------------------------------------------------------------------------   
INCLUDE(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set GMM_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(F2PY DEFAULT_MSG 
            F2PY_EXECUTABLE)

MESSAGE(STATUS "F2PY_FOUND = ${F2PY_FOUND}")
# ----------------------------------------------------------------------------   
