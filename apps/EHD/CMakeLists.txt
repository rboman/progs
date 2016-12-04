# ----------------------------------------------------------------------------
PROJECT(EHD)
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

# -- Sub directories

ADD_SUBDIRECTORY( GAUSSLIB )
ADD_SUBDIRECTORY( GaussTest )
ADD_SUBDIRECTORY( SKYLIB )
ADD_SUBDIRECTORY( SkyTest )
ADD_SUBDIRECTORY( EHD )




