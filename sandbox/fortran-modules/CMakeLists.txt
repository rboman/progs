# Project name
set( PROJECT_NAME example_module )
# Name of library containing the mathematical subroutines/functions
set( LIB_NAME calc )
# Name of library created to build the modules
set( LI_MODULES mods )

# Creating a path specifying where CMAKE should search and place modules binaries.
set( CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}" )

# Changing the defaut directory for executable binary (bin/ subfolder instead of src/)
set( EXECUTABLE_OUTPUT_PATH "${CMAKE_BINARY_DIR}/bin" )

# Start CMAKE project:
project(${PROJECT_NAME})    

# Enable fortran language
enable_language(Fortran)

# Ensure CMAKE Version in recent enough
cmake_minimum_required(VERSION 3.0)

# Adding the following subdirectory containing the source files. Attention, the order of the 
# ADD_SUBDIRECTORIES instructions matters (always start with modules for dependencies purposes!)
ADD_SUBDIRECTORY( mods )
ADD_SUBDIRECTORY( lib )
ADD_SUBDIRECTORY( src )

