# CMake input file of the SWIG wrapper around "ehd.so"
#   Uses new policy CMP0078 (UseSWIG generates standard target names)
#   Requires CMake >=3.13

FILE(GLOB SRCS *.h *.cpp *.inl *.swg)
FILE(GLOB ISRCS *.i)

SET_SOURCE_FILES_PROPERTIES(${ISRCS} PROPERTIES CPLUSPLUS ON)
SET(CMAKE_SWIG_FLAGS "-interface" "_ehdi") # avoids "import _module_d" with MSVC/Debug
SWIG_ADD_LIBRARY(ehdi LANGUAGE python SOURCES ${ISRCS} ${SRCS})
SET_PROPERTY(TARGET ehdi PROPERTY SWIG_USE_TARGET_INCLUDE_DIRECTORIES ON)
MACRO_DebugPostfix(ehdi)

TARGET_INCLUDE_DIRECTORIES(ehdi PRIVATE 
    ${PROJECT_SOURCE_DIR}/gauss/_src
    ${PROJECT_SOURCE_DIR}/sky/_src
    ${Python3_INCLUDE_DIRS})
TARGET_LINK_LIBRARIES(ehdi PRIVATE ehd ${Python3_LIBRARIES})
