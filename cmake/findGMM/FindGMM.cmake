# FindGMM.cmake - try to find gmm++ headers
#    (RoBo)
# ----------------------------------------------------------------------------   
# input:
#    /
# output:
#    GMM_FOUND             : TRUE/FALSE
#    GMM_INCLUDE_DIRS      : where the gmm/*.h are              [cached]
# ----------------------------------------------------------------------------
# autodetection:
#    utiliser "CMAKE_PREFIX_PATH=c:\local"
#          ou "CMAKE_INCLUDE_PATH=c:\local\include"
#          ou "INCLUDE=c:\local\include"
#   
# ----------------------------------------------------------------------------   

find_path(GMM_INCLUDE_DIRS "gmm/gmm.h")


IF(GMM_INCLUDE_DIRS)
    # try to compile CMake/try_compile/gmm_op.cpp with installed version of gmm
    # NOTE: how to debug "try_compile"? (Windows)
    #    .run: cmake -A x64 --debug-trycompile  (this keeps the temporary solution)
    #    .check the solution in build\gmm_op\CMakeFiles\CMakeTmp
    #    .build it with MSVC and see the errors
    try_compile(_SUCCESS 
                "${CMAKE_BINARY_DIR}/gmm_op" 
                SOURCES "${CMAKE_SOURCE_DIR}/try_compile/gmm_op.cpp"
                CMAKE_FLAGS "-DINCLUDE_DIRECTORIES=${GMM_INCLUDE_DIRS}"
                COMPILE_DEFINITIONS "-D_SCL_SECURE_NO_DEPRECATE"
    )
    IF(_SUCCESS)
        SET(GMM_DEFINITIONS "GMM_HAS_PRINTVECTOR")
    ELSE()
        SET(GMM_DEFINITIONS "")
    ENDIF()

    MESSAGE(STATUS "GMM_DEFINITIONS=${GMM_DEFINITIONS}")
ENDIF()
# ----------------------------------------------------------------------------   

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set GMM_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(GMM DEFAULT_MSG 
				GMM_INCLUDE_DIRS)

#MESSAGE(STATUS "GMM_FOUND = ${GMM_FOUND}")
# ----------------------------------------------------------------------------   
