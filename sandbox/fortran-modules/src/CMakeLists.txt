# Putting all the .f90 files contained in the folder in the variabe "srcFile"
file( GLOB_RECURSE

srcFiles

*.f90

)

# Add an executable target whose source files are represented by srcFiles CMAKE variable.
add_executable( ${PROJECT_NAME} ${srcFiles} )
# Link the main executable with the library 
target_link_libraries( ${PROJECT_NAME} ${LIB_NAME} )
# Link the main executable with the library created when compiling the modules.
target_link_libraries( ${PROJECT_NAME} ${LIB_MODULES} )
