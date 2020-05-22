# CMake input file of the SWIG wrapper around "dcm.so"

INCLUDE(${SWIG_USE_FILE})

INCLUDE_DIRECTORIES(${Python3_INCLUDE_DIRS})

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

SWIG_ADD_LIBRARY(dcmi LANGUAGE python SOURCES ${ISRCS} ${SRCS})
MACRO_DebugPostfix(_dcmi) # foire si cmake_policy(SET CMP0078 NEW)

# ---

INCLUDE_DIRECTORIES( ${PROJECT_SOURCE_DIR}/dcm/src )

SWIG_LINK_LIBRARIES(dcmi dcm ${Python3_LIBRARIES})

