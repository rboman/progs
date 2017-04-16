# CMake file for the SWIG interface of genmai

# -- Search for Python
FIND_PACKAGE(PythonLibs REQUIRED)


# -- Search for SWIG
FIND_PACKAGE(SWIG REQUIRED)
IF(CMAKE_GENERATOR MATCHES "Visual Studio") # not MSVC because of nmake & jom
    SET(CMAKE_SWIG_OUTDIR "${EXECUTABLE_OUTPUT_PATH}/$(Configuration)/")
ELSE()
    SET(CMAKE_SWIG_OUTDIR "${EXECUTABLE_OUTPUT_PATH}")
ENDIF()


INCLUDE(${SWIG_USE_FILE})

INCLUDE_DIRECTORIES(${PYTHON_INCLUDE_PATH})

FILE(GLOB SRCS *.h *.cpp *.inl *.swg)
FILE(GLOB ISRCS *.i)

SET(CMAKE_SWIG_FLAGS "")
SET_SOURCE_FILES_PROPERTIES(${ISRCS} PROPERTIES CPLUSPLUS ON)

SET(SWINCFLAGS 
-I${PROJECT_SOURCE_DIR}/utils
-I${PROJECT_SOURCE_DIR}/params
-I${PROJECT_SOURCE_DIR}/export
-I${PROJECT_SOURCE_DIR}/builders
)
SET_SOURCE_FILES_PROPERTIES(${ISRCS} PROPERTIES SWIG_FLAGS "${SWINCFLAGS}")



SWIG_ADD_MODULE(utils python ${ISRCS} ${SRCS})


# ---

INCLUDE_DIRECTORIES( ${PROJECT_SOURCE_DIR}/export
                     ${PROJECT_SOURCE_DIR}/builders
                     ${PROJECT_SOURCE_DIR}/params
                     ${PROJECT_SOURCE_DIR}/utils
                     )

SWIG_LINK_LIBRARIES(utils
                    exporters builders params utils  ${PYTHON_LIBRARIES}
)
