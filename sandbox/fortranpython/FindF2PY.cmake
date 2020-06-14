# FindF2PY
#
# output variables:
#   F2PY_EXECUTABLE
#   NUMPY_PATH
#   NUMPY_VERSION
#   F2PY_PATH
#   F2PY_SRC_DIR
#

# -- Search for Python (requires cmake>=3.12)
# use Python3_ROOT_DIR if wrong python found (e.g. anaconda)
FIND_PACKAGE(Python3 COMPONENTS Interpreter Development)
SET(Python3_EXECUTABLE ${Python3_EXECUTABLE})
SET(Python3_LIBRARIES ${Python3_LIBRARIES})
SET(Python3_INCLUDE_DIRS ${Python3_INCLUDE_DIRS}) 
SET(PYTHONLIBS_VERSION_STRING ${Python3_VERSION})
MESSAGE(STATUS "Python3_EXECUTABLE=${Python3_EXECUTABLE}")
MESSAGE(STATUS "Python3_LIBRARIES=${Python3_LIBRARIES}")
MESSAGE(STATUS "Python3_INCLUDE_DIRS=${Python3_INCLUDE_DIRS}")
MESSAGE(STATUS "Python3_VERSION=${Python3_VERSION}")

# search for f2py   => F2PY_EXECUTABLE
FIND_PROGRAM(F2PY_EXECUTABLE NAMES f2py f2py3)
MESSAGE("F2PY_EXECUTABLE=${F2PY_EXECUTABLE}")

# Find out the include path of numpy => NUMPY_PATH
EXECUTE_PROCESS(
COMMAND "${Python3_EXECUTABLE}" -c
        "try: import numpy; print(numpy.get_include(), end='')\nexcept:pass\n"
        OUTPUT_VARIABLE NUMPY_PATH)
MESSAGE("NUMPY_PATH=${NUMPY_PATH}")

# And the version of numpy => NUMPY_VERSION
EXECUTE_PROCESS(
COMMAND "${Python3_EXECUTABLE}" -c
        "try: import numpy; print(numpy.__version__, end='')\nexcept:pass\n"
OUTPUT_VARIABLE NUMPY_VERSION)
MESSAGE("NUMPY_VERSION=${NUMPY_VERSION}")

# Find out the include path of numpy/f2py/src => F2PY_PATH
EXECUTE_PROCESS(
COMMAND "${Python3_EXECUTABLE}" -c
        "import os; import numpy.f2py; print(os.path.dirname(numpy.f2py.__file__).replace(os.sep,'/'), end='')"
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

    IF(NOT MSVC) # gcc
        ADD_CUSTOM_COMMAND(
            OUTPUT ${OUTPUTS}
            COMMAND ${F2PY_EXECUTABLE}
            -m ${F2PYMODULE}
            --lower   # gcc cree du lowercase quel que soit le nom dans le code
            ${${FSRCS_LIST}}
            DEPENDS ${${FSRCS_LIST}}
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        )
    ELSE() # MSVC + Intel Fortran => uppercase/no underscore
        ADD_CUSTOM_COMMAND(
            OUTPUT ${OUTPUTS}
            COMMAND ${F2PY_EXECUTABLE}
            -m ${F2PYMODULE}
            #--no-lower
            ${${FSRCS_LIST}}
            DEPENDS ${${FSRCS_LIST}}
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        )
    ENDIF()

    # static library "XXXfor" with the fortran source and fortran wrappers
    ADD_LIBRARY(${F2PYMODULE}for ${${FSRCS_LIST}} ${F_OUTPUTS}) 

    # shared python module XXX.pyd  
    ADD_LIBRARY(${F2PYMODULE} SHARED ${F2PYMODULE}module.c ${F2PY_SRC_DIR}/fortranobject.c ) 
    SET_TARGET_PROPERTIES(${F2PYMODULE} PROPERTIES SUFFIX .pyd)
    SET_TARGET_PROPERTIES(${F2PYMODULE} PROPERTIES PREFIX "")
    TARGET_INCLUDE_DIRECTORIES(${F2PYMODULE} PRIVATE ${Python3_INCLUDE_DIRS})
    TARGET_INCLUDE_DIRECTORIES(${F2PYMODULE} PRIVATE ${F2PY_SRC_DIR})
    TARGET_INCLUDE_DIRECTORIES(${F2PYMODULE} PRIVATE ${NUMPY_PATH})
    IF(MSVC) # MSVC + Intel Fortran => uppercase/no underscore
        target_compile_definitions(${F2PYMODULE} PUBLIC NO_APPEND_FORTRAN)
        target_compile_definitions(${F2PYMODULE} PUBLIC UPPERCASE_FORTRAN)
    ENDIF()
    # ...linked to fortran routines and python 
    TARGET_LINK_LIBRARIES(${F2PYMODULE} ${F2PYMODULE}for ${Python3_LIBRARIES})

ENDMACRO()



# ----------------------------------------------------------------------------   
INCLUDE(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set GMM_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(F2PY DEFAULT_MSG 
            F2PY_EXECUTABLE)

MESSAGE(STATUS "F2PY_FOUND = ${F2PY_FOUND}")
# ----------------------------------------------------------------------------   
