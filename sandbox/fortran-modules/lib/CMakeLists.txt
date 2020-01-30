# all file of *.f90 extension will be placed in a variable "src_lib"
file( GLOB_RECURSE

src_lib

*.f90

)

# Must be present if the functions in the target library (=src_lib) are using modules.
include_directories( ${CMAKE_Fortran_MODULE_DIRECTORY} )
# Creating a library whose executable is generated from src_lib source files.
add_library( ${LIB_NAME} SHARED ${src_lib} )