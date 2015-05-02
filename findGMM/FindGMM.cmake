# FindGMM.cmake - try to find gmm++ headers
#    (RoBo)
# ----------------------------------------------------------------------------   
# input:
#    ENABLE_TBBMALLOC : ON/OFF
# output:
#    GMM_FOUND             : TRUE/FALSE
#    GMM_INCLUDE_DIRS      : where the tbb/*.h are              [cached]
# ----------------------------------------------------------------------------
# autodetection:
#    utiliser "CMAKE_PREFIX_PATH=c:\local"
#          ou "CMAKE_INCLUDE_PATH=c:\local\include"
#          ou "INCLUDE=c:\local\include"
#   
# ----------------------------------------------------------------------------   

find_file(GMM_INCLUDE_DIRS "gmm/gmm.h")

# ----------------------------------------------------------------------------   

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set TBB_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(GMM DEFAULT_MSG 
				GMM_INCLUDE_DIRS)

#MESSAGE(STATUS "GMM_FOUND = ${GMM_FOUND}")
# ----------------------------------------------------------------------------   
