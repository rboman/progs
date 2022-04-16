# CMake input file of the SWIG wrapper around "gauss.so"
#   Uses new policy CMP0078 (UseSWIG generates standard target names)
#   Requires CMake >=3.13

# INCLUDE(${SWIG_USE_FILE})

FILE(GLOB SRCS *.h *.cpp *.inl *.swg)
FILE(GLOB ISRCS *.i)

SET_SOURCE_FILES_PROPERTIES(${ISRCS} PROPERTIES CPLUSPLUS ON)
SET(CMAKE_SWIG_FLAGS "-interface" "_gaussi") # avoids "import _module_d" with MSVC/Debug
SWIG_ADD_LIBRARY(gaussi LANGUAGE python SOURCES ${ISRCS} ${SRCS})
SET_PROPERTY(TARGET gaussi PROPERTY SWIG_USE_TARGET_INCLUDE_DIRECTORIES ON)
MACRO_DebugPostfix(gaussi)

TARGET_INCLUDE_DIRECTORIES(gaussi PRIVATE ${Python3_INCLUDE_DIRS})
TARGET_LINK_LIBRARIES(gaussi PRIVATE gauss ${Python3_LIBRARIES})
