# CMake file for the SWIG interface of genmai

INCLUDE(${SWIG_USE_FILE})

INCLUDE_DIRECTORIES(${Python3_INCLUDE_DIRS})

FILE(GLOB SRCS *.h *.cpp *.inl *.swg)
FILE(GLOB ISRCS *.i)

SET_SOURCE_FILES_PROPERTIES(${ISRCS} PROPERTIES CPLUSPLUS ON)

SET(SWINCFLAGS  -I${PROJECT_SOURCE_DIR}/src )
SET_SOURCE_FILES_PROPERTIES(${ISRCS} PROPERTIES SWIG_FLAGS "${SWINCFLAGS}")

SWIG_ADD_LIBRARY(genmai LANGUAGE python SOURCES ${ISRCS} ${SRCS})

# ---

INCLUDE_DIRECTORIES( ${PROJECT_SOURCE_DIR}/src )

SWIG_LINK_LIBRARIES(genmai genmai  ${Python3_LIBRARIES} )
