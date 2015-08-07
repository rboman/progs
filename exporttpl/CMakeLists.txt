# ExportTPL
# sandbox for template instanciation in a DLL

# ----------------------------------------------------------------------------
PROJECT(ExportTPL)
# ----------------------------------------------------------------------------
CMAKE_MINIMUM_REQUIRED(VERSION 2.6)

SET(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin CACHE PATH
                        "Single output directory for building all libraries.")
SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin CACHE PATH
                        "Single output directory for building all executables.")
MARK_AS_ADVANCED(LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)


IF(WIN32)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS -D_SCL_SECURE_NO_DEPRECATE)
ENDIF(WIN32)

# -- C++11

IF( (CMAKE_CXX_COMPILER_ID MATCHES "GNU") OR (CMAKE_CXX_COMPILER_ID MATCHES "Intel") )
	ADD_DEFINITIONS(
	    -std=gnu++0x #c++0x # =c++11 # Or -std=c++0x
	)
ENDIF()


# -- Sub directories
ADD_SUBDIRECTORY( mydll )
ADD_SUBDIRECTORY( myexe )





