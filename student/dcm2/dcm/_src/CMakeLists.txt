# CMake input file of the SWIG wrapper around "dcm.so"

INCLUDE(${SWIG_USE_FILE})

INCLUDE_DIRECTORIES(${PYTHON_INCLUDE_PATH})

FILE(GLOB SRCS *.h *.cpp *.inl *.swg)
FILE(GLOB ISRCS *.i)

# BUG: UseSWIG.cmake (cmake 3.15) introduit -interface _dcmi_d en debug sous msvc
#      sauf si -interface est deja défini dans les flags...
SET(CMAKE_SWIG_FLAGS "-interface" "_dcmi") 
SET_SOURCE_FILES_PROPERTIES(${ISRCS} PROPERTIES CPLUSPLUS ON)

SET(SWINCFLAGS 
-I${PROJECT_SOURCE_DIR}/dcm/src
)
SET_SOURCE_FILES_PROPERTIES(${ISRCS} PROPERTIES SWIG_FLAGS "${SWINCFLAGS}")


IF(CMAKE_VERSION VERSION_LESS 3.8.0)
    SWIG_ADD_MODULE(dcmi python ${ISRCS} ${SRCS})
ELSE()
    SWIG_ADD_LIBRARY(dcmi LANGUAGE python SOURCES ${ISRCS} ${SRCS})
ENDIF()
MACRO_DebugPostfix(_dcmi) # foire si cmake_policy(SET CMP0078 NEW)

# ---

INCLUDE_DIRECTORIES( ${PROJECT_SOURCE_DIR}/dcm/src
)

SWIG_LINK_LIBRARIES(dcmi 
                    dcm ${PYTHON_LIBRARIES}
)

