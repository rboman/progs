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


#   set(f2py_module_name "_cylinder_methods")
#   set(fortran_src_file "${CMAKE_CURRENT_SOURCE_DIR}/cylinder_methods.f90")
#
#   set(generated_module_file ${CMAKE_CURRENT_BINARY_DIR}/${f2py_module_name}${PYTHON_EXTENSION_MODULE_SUFFIX})
#
#   add_custom_target(${f2py_module_name} ALL
#     DEPENDS ${generated_module_file}
#     )
#
#   add_custom_command(
#     OUTPUT ${generated_module_file}
#     COMMAND ${F2PY_EXECUTABLE}
#       -m ${f2py_module_name}
#       -c
#       ${fortran_src_file}
#     WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
#     )

set(f2py_module_name vect)
set(generated_module_file ${CMAKE_CURRENT_BINARY_DIR}/${f2py_module_name}${PYTHON_EXTENSION_MODULE_SUFFIX})
set(fortran_src_file norme.f90)

   add_custom_command(
     OUTPUT vectmodule.c
     COMMAND ${F2PY_EXECUTABLE}
       -m ${f2py_module_name}
       ${CMAKE_CURRENT_SOURCE_DIR}/${fortran_src_file}
     WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
     )



ADD_LIBRARY(vect vectmodule.c) 
TARGET_INCLUDE_DIRECTORIES(vect PRIVATE ${PYTHON_INCLUDE_PATH})
