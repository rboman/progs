# # Putting all the .f90 extension files present in the folder into CMAKE variable "modFiles" 
file( GLOB_RECURSE

modFiles

*.f90

)

# Must be present if functions in the target library are using modules.
include_directories( ${CMAKE_Fortran_MODULE_DIRECTORY} )
# instead of generating .mod when the binaries are built, CMAKE builds a static library binary. T
# This library is created, and will be linked to the executable after. (Note that .mod binary files 
# will also be generated, but there are simply used to build the static library)
add_library( ${LIB_MODULES} SHARED ${modFiles} )