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




find_program(F2PY_EXECUTABLE NAMES f2py f2py${PYTHON_VERSION_MAJOR})

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

   add_custom_command(
     OUTPUT vectmodule.c
     COMMAND ${F2PY_EXECUTABLE}
       -m ${f2py_module_name}
       ${fortran_src_file}
     WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
     )



#ADD_EXECUTABLE(prog01 prog01.f90 progmod.f90) # .f90/.f95 => free format | .f/.for => fixed

#ENABLE_TESTING()
#ADD_TEST(NAME prog01 COMMAND ${EXECUTABLE_OUTPUT_PATH}/prog01
#         WORKING_DIRECTORY ${PROJECT_BINARY_DIR})