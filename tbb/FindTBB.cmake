# FindTBB.cmake - try to find Intel Threading Building Blocks libraries
#    (RoBo)
# ----------------------------------------------------------------------------   
# input:
#    ENABLE_TBBMALLOC : ON/OFF
# output:
#    TBB_FOUND          : **TODO**
#    TBB_LIBRARIES      : list of tbb libraries to link with 
#    TBB_LIB_PATH       : where the optimized .so/.lib are   [cached]
#    TBB_LIB_PATH_DEBUG : where the debug .so/.lib are       [cached]
#    TBB_INCLUDE_DIRS   : where the tbb/*.h are              [cached]
# ----------------------------------------------------------------------------   
# todo:
#    TBB_DEFINITIONS ? (args suppl en debug)
#
# autodetection:
#    succeeds if tbb.dll/tbb_debug.dll are in the PATH/LD_LIBRARY_PATH
#    otherwise, you may set TBB_LIB_PATH, TBB_LIB_PATH manually
#
# main advantage: 
#    the link wilkl be performed with a library in the PATH
#    => avoids problem when 2 sets of libs are installed in the system
#
# memo:
#    http://www.cmake.org/Wiki/CMake:How_To_Find_Libraries#Writing_find_modules
# ----------------------------------------------------------------------------   

OPTION(ENABLE_TBBMALLOC "Use TBBMALLOC" ON)

# clean cache from previous runs
unset(TBB_TBBDLL       CACHE)
unset(TBB_TBBDLL_DEBUG CACHE)

IF(0)  # Debug
	MESSAGE(STATUS "-------------------------------------")
	MESSAGE(STATUS " FindTBB")
	MESSAGE(STATUS "-------------------------------------")

	MESSAGE(STATUS "CMAKE_SHARED_LIBRARY_SUFFIX=${CMAKE_SHARED_LIBRARY_SUFFIX}")
	MESSAGE(STATUS "CMAKE_SHARED_MODULE_SUFFIX=${CMAKE_SHARED_MODULE_SUFFIX}")
	MESSAGE(STATUS "CMAKE_FIND_LIBRARY_SUFFIX=${CMAKE_FIND_LIBRARY_SUFFIX}")
	MESSAGE(STATUS "TBB_FIND_REQUIRED=${TBB_FIND_REQUIRED}") # automatiquement à 1 si REQUIRED
	MESSAGE(STATUS "TBB_USE_TBBMALLOC=${TBB_USE_TBBMALLOC}")
	MESSAGE(STATUS "WIN32=${WIN32}")
	MESSAGE(STATUS "MSVC=${MSVC}")
	MESSAGE(STATUS "MINGW=${MINGW}")
ENDIF()

# --- Liste des bibliothèques à trouver ---

SET(TBB_LIBRARIES  optimized tbb debug tbb_debug)
IF(ENABLE_TBBMALLOC)
	LIST(APPEND TBB_LIBRARIES optimized tbbmalloc_proxy optimized tbbmalloc)
	LIST(APPEND TBB_LIBRARIES debug tbbmalloc_proxy_debug debug tbbmalloc_debug)
ENDIF()
MESSAGE(STATUS "TBB_LIBRARIES=${TBB_LIBRARIES}")

# ----------------------------------------------------------------------------   
# --- TBB_LIB_PATH ---
# ----------------------------------------------------------------------------   
# method:
#   - look for the dynamic lib in the PATH/LD_LIBRARY_PATH 
#     on linux or win/mingw; this path is correct
#   - on win/msvc, replace "bin" => "lib" in the path previously found

SET(TBB_LIB_PATH TBB_LIB_PATH-NOTFOUND CACHE PATH "where the optimized .so/.lib are")

IF(NOT TBB_LIB_PATH)
	# try to find "tbb.dll/so" from the env variables
	IF(MSVC)
		find_file(TBB_TBBDLL "tbb.dll" PATHS "" )
	ELSE()
		find_library(TBB_TBBDLL "tbb" PATHS "" ENV LD_LIBRARY_PATH)
	ENDIF()
	MESSAGE(STATUS "TBB_TBBDLL=${TBB_TBBDLL}")
	IF("${TBB_TBBDLL}" STREQUAL "TBB_TBBDLL-NOTFOUND")
		MESSAGE(FATAL_ERROR "!!!!!!!! tbb${CMAKE_SHARED_LIBRARY_SUFFIX} should be in PATH/LD_LIBRARY_PATH !!!!!!!!!")
	ENDIF()

	get_filename_component(TBB_DLL_PATH ${TBB_TBBDLL} PATH CACHE)
	unset(TBB_TBBDLL CACHE)

	IF(MSVC)
		STRING(REPLACE "/bin/" "/lib/" TBB_LIB_PATH ${TBB_DLL_PATH})
		SET(TBB_LIB_PATH ${TBB_LIB_PATH} CACHE PATH "where the optimized .so/.lib are" FORCE) 
	ELSE()
		SET(TBB_LIB_PATH ${TBB_DLL_PATH} CACHE PATH "where the optimized .so/.lib are" FORCE) 
	ENDIF()
	unset(TBB_DLL_PATH CACHE)

	MESSAGE(STATUS "TBB_LIB_PATH=${TBB_LIB_PATH}")
ENDIF()

# ----------------------------------------------------------------------------   
# --- TBB_LIB_PATH_DEBUG ---
# ----------------------------------------------------------------------------   
#   same method for debug libs

SET(TBB_LIB_PATH_DEBUG TBB_LIB_PATH_DEBUG-NOTFOUND CACHE PATH "where the debug .so/.lib are")

IF(NOT TBB_LIB_PATH_DEBUG)
	# try to find "tbb_debug.dll/so" from the env variables
	IF(MSVC)
		find_file(TBB_TBBDLL_DEBUG "tbb_debug.dll" PATHS "" )
	ELSE()
		find_library(TBB_TBBDLL_DEBUG "tbb_debug" PATHS "" ENV LD_LIBRARY_PATH)
	ENDIF()
	MESSAGE(STATUS "TBB_TBBDLL_DEBUG=${TBB_TBBDLL_DEBUG}")
	IF(NOT TBB_TBBDLL_DEBUG)
		MESSAGE(FATAL_ERROR "!!!!!!!! tbb_debug${CMAKE_SHARED_LIBRARY_SUFFIX} should be in PATH/LD_LIBRARY_PATH !!!!!!!!!")
	ENDIF()

	get_filename_component(TBB_DLL_PATH_DEBUG ${TBB_TBBDLL_DEBUG} PATH CACHE)
	unset(TBB_TBBDLL_DEBUG CACHE)

	IF(MSVC)
		STRING(REPLACE "/bin/" "/lib/" TBB_LIB_PATH_DEBUG ${TBB_DLL_PATH_DEBUG})
		SET(TBB_LIB_PATH_DEBUG ${TBB_LIB_PATH_DEBUG} CACHE PATH "where the debug .so/.lib are" FORCE)
	ELSE()
		SET(TBB_LIB_PATH_DEBUG ${TBB_DLL_PATH_DEBUG} CACHE PATH "where the debug .so/.lib are" FORCE)
	ENDIF()
	unset(TBB_DLL_PATH_DEBUG CACHE)

	MESSAGE(STATUS "TBB_LIB_PATH_DEBUG=${TBB_LIB_PATH_DEBUG}")
ENDIF()

# ----------------------------------------------------------------------------   
# TBB_INCLUDE_DIRS: search include dir
# ----------------------------------------------------------------------------   

SET(TBB_INCLUDE_DIRS TBB_INCLUDE_DIRS-NOTFOUND CACHE PATH "where the tbb/*.h are")

IF(NOT TBB_INCLUDE_DIRS)
	SET(VAR1 ${TBB_LIB_PATH})
	SET(BASENAME ${VAR1})
	SET(TBB_TBBROOT "TBB_TBBROOT-NOTFOUND")
	WHILE( (NOT TBB_TBBROOT) OR (NOT BASENAME) )
		get_filename_component(DIRNAME ${VAR1} PATH)
		#MESSAGE(STATUS "DIRNAME=${DIRNAME}")	
		get_filename_component(BASENAME ${VAR1} NAME)
		#MESSAGE(STATUS "BASENAME=${BASENAME}") # on remonte jusqu'à un "lib" ou "build"
		
		IF( ("${BASENAME}" STREQUAL "lib") OR ("${BASENAME}" STREQUAL "build") OR ("${BASENAME}" STREQUAL "bin") )
			SET(TBB_TBBROOT ${DIRNAME})
		ELSE()
			SET(VAR1 ${DIRNAME})
			#MESSAGE(STATUS "VAR1=${VAR1}")
		ENDIF()
	ENDWHILE()

	MESSAGE(STATUS "TBB_TBBROOT=${TBB_TBBROOT}")
	MESSAGE(STATUS "TBBROOT=$ENV{TBBROOT}")

	FIND_PATH(TBB_INCLUDE_DIRS 
				 NAMES  tbb/parallel_for.h
				 PATHS "${TBB_TBBROOT}"
				 PATH_SUFFIXES include
				)
	MESSAGE(STATUS "TBB_INCLUDE_DIRS=${TBB_INCLUDE_DIRS}")
ENDIF()


# ----------------------------------------------------------------------------   

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set TBB_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(TBB  DEFAULT_MSG
                                  TBB_LIBRARIES TBB_INCLUDE_DIRS)

# a voir + tard si necessaire
#mark_as_advanced(TBB_INCLUDE_DIRS TBB_LIBRARIES )

