# FindTBB.cmake - try to find Intel Threading Building Blocks libraries
#    (RoBo)
# ----------------------------------------------------------------------------   
# input:
#    ENABLE_TBBMALLOC : ON/OFF
# output:
#    TBB_FOUND             : TRUE/FALSE
#    TBB_LIBRARIES         : list of tbb libraries to link with 
#    TBB_LIB_PATH          : where the optimized .so/.lib are   [cached]
#    TBB_LIB_PATH_DEBUG    : where the debug .so/.lib are       [cached]
#    TBB_INCLUDE_DIRS      : where the tbb/*.h are              [cached]
#    TBB_VERSION_INTERFACE : 
#    TBB_VERSION_MAJOR     : 
#    TBB_VERSION_MINOR     : 
#    TBB_VERSION_STRING    : version such as "4.2"
#    TBB_CXX_FLAGS_DEBUG   : to be added to
# ----------------------------------------------------------------------------   
# todo:
#    TBB_DEFINITIONS ? (args suppl en debug)
#
# autodetection:
#    succeeds if tbb.dll/tbb_debug.dll are in the PATH/LD_LIBRARY_PATH
#    otherwise, you may set TBB_LIB_PATH, TBB_LIB_PATH_DEBUG manually
#
# main advantage: 
#    the link will be always performed with the library corresponding 
#    to dynamic lib the PATH
#    => avoids problem when 2 sets of libs are installed in the system
#
# remarks:
#    $ENV{TBBROOT} is *not* used here because it is rather difficult 
#    to guess the wanted target architecture
#
# tested on:
#    win7-msvc + official tbb binaries
#    winxp-mingw32 + tbb compiled from src
#    linux + installed parallel studio 
#
# memo:
#    http://www.cmake.org/Wiki/CMake:How_To_Find_Libraries#Writing_find_modules
# ----------------------------------------------------------------------------   

set (_VERB 0)  # set to 1 for debugging

OPTION(ENABLE_TBBMALLOC "Use TBBMALLOC" ON)

IF(_VERB)  # Debug
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
	MESSAGE(STATUS "TBBROOT=$ENV{TBBROOT}")

ENDIF()

SET(TBB_CXX_FLAGS_DEBUG "-DTBB_USE_THREADING_TOOLS=1 -DTBB_USE_DEBUG=1")

# --- List of libraries to be found ---

set(_TBB_TBBLIB_NAME    "tbb")
set(_TBB_PROXYLIB_NAME  "tbbmalloc_proxy")
set(_TBB_MALLOCLIB_NAME "tbbmalloc")

set(_TBB_LIBSET1 ${_TBB_TBBLIB_NAME})
set(_TBB_LIBSET2 ${_TBB_PROXYLIB_NAME} ${_TBB_MALLOCLIB_NAME})

# ----------------------------------------------------------------------------   
# --- TBB_LIB_PATH ---
# ----------------------------------------------------------------------------   
# method:
#   - look for the dynamic lib in the PATH/LD_LIBRARY_PATH 
#     on linux or win/mingw; this path is correct
#   - on win/msvc, replace "bin" => "lib" in the path previously found

SET(_TBB_LIB_PATH ${TBB_LIB_PATH})                              # copy cached value
IF(NOT _TBB_LIB_PATH)                                           # work on copy
	# try to find "tbb.dll/so" from the env variables
	#IF(MSVC)
	#	find_file(_TBB_TBBDLL "tbb.dll" PATHS "" )
	#ELSE()
	#	find_library(_TBB_TBBDLL "tbb" PATHS "" ENV LD_LIBRARY_PATH)
	#ENDIF()

	#new
	find_file(_TBB_TBBDLL "${_TBB_TBBLIB_NAME}${CMAKE_SHARED_LIBRARY_SUFFIX}" PATHS "" )

	#MESSAGE(STATUS "_TBB_TBBDLL=${_TBB_TBBDLL}")
	IF(_TBB_TBBDLL)
		get_filename_component(_TBB_DLL_PATH ${_TBB_TBBDLL} PATH CACHE)
		unset(_TBB_TBBDLL CACHE)
	
		IF(MSVC)
			STRING(REPLACE "/bin/" "/lib/" _TBB_LIB_PATH ${_TBB_DLL_PATH})
		ELSE()
			SET(_TBB_LIB_PATH ${_TBB_DLL_PATH}) 
		ENDIF()
		unset(_TBB_DLL_PATH CACHE)
	
		#MESSAGE(STATUS "_TBB_LIB_PATH=${_TBB_LIB_PATH}")
	ELSE()
		MESSAGE(WARNING "!!!!!!!! ${_TBB_TBBLIB_NAME}${CMAKE_SHARED_LIBRARY_SUFFIX} should be in PATH/LD_LIBRARY_PATH !!!!!!!!!")
	ENDIF()
	unset(_TBB_TBBDLL CACHE)
ENDIF()
IF(NOT TBB_LIB_PATH) # set cache if needed
	SET(TBB_LIB_PATH ${_TBB_LIB_PATH} CACHE PATH "where the optimized .so/.lib are")
ENDIF()

# ----------------------------------------------------------------------------   
# --- TBB_LIB_PATH_DEBUG ---
# ----------------------------------------------------------------------------   
#   same method for debug libs

SET(_TBB_LIB_PATH_DEBUG ${TBB_LIB_PATH_DEBUG})                              # copy cached value
IF(NOT _TBB_LIB_PATH_DEBUG)                                                 # work on copy
	# try to find "tbb_debug.dll/so" from the env variables
	IF(MSVC)
		find_file(_TBB_TBBDLL_DEBUG "tbb_debug.dll" PATHS "" )
	ELSE()
		find_library(_TBB_TBBDLL_DEBUG "tbb_debug" PATHS "" ENV LD_LIBRARY_PATH)
	ENDIF()
	#MESSAGE(STATUS "_TBB_TBBDLL_DEBUG=${_TBB_TBBDLL_DEBUG}")
	IF(_TBB_TBBDLL_DEBUG)
		get_filename_component(_TBB_DLL_PATH_DEBUG ${_TBB_TBBDLL_DEBUG} PATH CACHE)
		unset(_TBB_TBBDLL_DEBUG CACHE)
	
		IF(MSVC)
			STRING(REPLACE "/bin/" "/lib/" _TBB_LIB_PATH_DEBUG ${_TBB_DLL_PATH_DEBUG})
		ELSE()
			SET(_TBB_LIB_PATH_DEBUG ${_TBB_DLL_PATH_DEBUG}) 
		ENDIF()
		unset(_TBB_DLL_PATH_DEBUG CACHE)
	
		#MESSAGE(STATUS "_TBB_LIB_PATH_DEBUG=${_TBB_LIB_PATH_DEBUG}")
	ELSE()
		MESSAGE(WARNING "!!!!!!!! tbb_debug${CMAKE_SHARED_LIBRARY_SUFFIX} should be in PATH/LD_LIBRARY_PATH !!!!!!!!!")
	ENDIF()
	unset(_TBB_TBBDLL_DEBUG CACHE)
ENDIF()
IF(NOT TBB_LIB_PATH_DEBUG) # set cache if needed
	SET(TBB_LIB_PATH_DEBUG ${_TBB_LIB_PATH_DEBUG} CACHE PATH "where the debug .so/.lib are")
ENDIF()

# ----------------------------------------------------------------------------   
# TBB_LIBRARIES: search full path of all libraries
# ----------------------------------------------------------------------------   

macro(resolveLibs VARNAME LISTNAME)
	foreach(LIB ${${LISTNAME}})
		# look for optimized version
		find_library(FOUND_LIB ${LIB} PATHS ${TBB_LIB_PATH})
		#MESSAGE(STATUS "FOUND_LIB=${FOUND_LIB}")
		if(FOUND_LIB)
			list(APPEND ${VARNAME} "optimized" ${FOUND_LIB})
		endif()
		unset(FOUND_LIB CACHE)
		# look for debug version
		find_library(FOUND_LIB ${LIB}_debug PATHS ${TBB_LIB_PATH_DEBUG})
		#MESSAGE(STATUS "FOUND_LIB=${FOUND_LIB}")
		if(FOUND_LIB)
			list(APPEND ${VARNAME} "debug" ${FOUND_LIB})
		endif()
		unset(FOUND_LIB CACHE)
	endforeach()
endmacro()

# search for all libs
IF( TBB_LIB_PATH AND TBB_LIB_PATH_DEBUG)
	resolveLibs(_TBB_LIBSET1_DIRS _TBB_LIBSET1)
	resolveLibs(_TBB_LIBSET2_DIRS _TBB_LIBSET2)
ENDIF()

# build full list of libs
set(TBB_LIBRARIES ${_TBB_LIBSET1_DIRS})
IF(ENABLE_TBBMALLOC)
	LIST(APPEND TBB_LIBRARIES ${_TBB_LIBSET2_DIRS} )
ENDIF()

# print for debug
IF(_VERB)
	foreach(LIB ${TBB_LIBRARIES})
		MESSAGE(STATUS "lib=${LIB}")
	endforeach()
ENDIF()

# ----------------------------------------------------------------------------   
# TBB_INCLUDE_DIRS: search include dir
# ----------------------------------------------------------------------------   

SET(_TBB_INCLUDE_DIRS ${TBB_INCLUDE_DIRS})
IF( (NOT _TBB_INCLUDE_DIRS) AND (TBB_LIB_PATH))
	SET(_VAR1 ${TBB_LIB_PATH})
	SET(_BASENAME ${_VAR1})
	SET(_TBB_TBBROOT "_TBB_TBBROOT-NOTFOUND")
	WHILE( (NOT _TBB_TBBROOT) OR (NOT _BASENAME) )
		get_filename_component(_DIRNAME ${_VAR1} PATH)
		#MESSAGE(STATUS "_DIRNAME=${_DIRNAME}")	
		get_filename_component(_BASENAME ${_VAR1} NAME)
		#MESSAGE(STATUS "_BASENAME=${_BASENAME}") # on remonte jusqu'à un "lib" ou "build"
		
		IF( ("${_BASENAME}" STREQUAL "lib") OR ("${_BASENAME}" STREQUAL "build") OR ("${_BASENAME}" STREQUAL "bin") )
			SET(_TBB_TBBROOT ${_DIRNAME})
		ELSE()
			SET(_VAR1 ${_DIRNAME})
		ENDIF()
	ENDWHILE()
	#MESSAGE(STATUS "_TBB_TBBROOT=${_TBB_TBBROOT}")

	FIND_PATH(_TBB_INCLUDE_DIRS 
				 NAMES  tbb/parallel_for.h
				 PATHS "${_TBB_TBBROOT}"
				 PATH_SUFFIXES include
				)
	#MESSAGE(STATUS "_TBB_INCLUDE_DIRS=${_TBB_INCLUDE_DIRS}")
ENDIF()
IF(NOT TBB_INCLUDE_DIRS) # set cache if needed
	SET(TBB_INCLUDE_DIRS ${_TBB_INCLUDE_DIRS} CACHE PATH "where the tbb/*.h are")
ENDIF()
unset(_TBB_INCLUDE_DIRS CACHE)

# ----------------------------------------------------------------------------   
# Versions
# ----------------------------------------------------------------------------  
 
SET(_TBB_VERSION_INTERFACE ${TBB_VERSION_INTERFACE})
IF( (NOT TBB_VERSION_INTERFACE) AND TBB_INCLUDE_DIRS )
	FILE(READ "${TBB_INCLUDE_DIRS}/tbb/tbb_stddef.h" _FILE)
	set(_TBB_VERSION_MAJOR 0)
	set(_TBB_VERSION_MINOR 0)
	STRING(REGEX REPLACE ".*#define TBB_INTERFACE_VERSION ([0-9]+).*" "\\1" _TBB_VERSION_INTERFACE "${_FILE}")
	STRING(REGEX REPLACE ".*#define TBB_VERSION_MAJOR ([0-9]+).*"     "\\1" _TBB_VERSION_MAJOR     "${_FILE}")
	STRING(REGEX REPLACE ".*#define TBB_VERSION_MINOR ([0-9]+).*"     "\\1" _TBB_VERSION_MINOR     "${_FILE}")
	set(_TBB_VERSION_STRING "${_TBB_VERSION_MAJOR}.${_TBB_VERSION_MINOR}")
	unset(_FILE)
ENDIF()
IF(NOT TBB_VERSION_INTERFACE)
	SET(TBB_VERSION_INTERFACE ${_TBB_VERSION_INTERFACE} CACHE STRING "")
	SET(TBB_VERSION_MAJOR ${_TBB_VERSION_MAJOR} CACHE INTERNAL "")
	SET(TBB_VERSION_MINOR ${_TBB_VERSION_MINOR} CACHE INTERNAL "")
	SET(TBB_VERSION_STRING ${_TBB_VERSION_STRING} CACHE STRING "")
	mark_as_advanced(TBB_VERSION_INTERFACE)
	mark_as_advanced(TBB_VERSION_MAJOR)
	mark_as_advanced(TBB_VERSION_MINOR)
	mark_as_advanced(TBB_VERSION_STRING)
ENDIF()
# ----------------------------------------------------------------------------   

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set TBB_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(TBB DEFAULT_MSG 
				TBB_LIBRARIES TBB_INCLUDE_DIRS
				TBB_LIB_PATH TBB_LIB_PATH_DEBUG TBB_VERSION_STRING)

#MESSAGE(STATUS "TBB_FOUND = ${TBB_FOUND}")
# ----------------------------------------------------------------------------   

unset(_VERB)