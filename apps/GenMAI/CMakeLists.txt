PROJECT(GenMAI)
CMAKE_MINIMUM_REQUIRED(VERSION 2.6)

SET(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin CACHE PATH
                        "Single output directory for building all libraries.")
SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin CACHE PATH
                        "Single output directory for building all executables.")
MARK_AS_ADVANCED(LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)


IF(WIN32)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS -D_SCL_SECURE_NO_DEPRECATE)
ENDIF(WIN32)

if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
    add_compile_options(-Werror)
endif()

CONFIGURE_FILE(${PROJECT_SOURCE_DIR}/genmai_config.h.in
               ${PROJECT_BINARY_DIR}/genmai_config.h)

enable_testing()

ADD_SUBDIRECTORY( src )
ADD_SUBDIRECTORY( _src )

ADD_SUBDIRECTORY( exe )

ADD_SUBDIRECTORY( tests )



###ADD_SUBDIRECTORY( gui )




