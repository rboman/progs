cmake_minimum_required(VERSION 2.8.11)
project(Barres CXX)
include(CheckCXXCompilerFlag)
check_cxx_compiler_flag("-std=c++11" STD11CHECK)
IF(STD11CHECK)
    ADD_DEFINITIONS(-std=c++11 )
ENDIF()

FIND_PACKAGE(Qt5 COMPONENTS Widgets REQUIRED)

FILE(GLOB SRCS *.h *.cpp)
set (MOC_HEADERS Barres.h Window.h)

qt5_wrap_cpp (MOC_OUTFILES ${MOC_HEADERS})

ADD_EXECUTABLE(barres ${SRCS} ${MOC_OUTFILES})

if(POLICY CMP0043)
    cmake_policy(SET CMP0043 OLD)
endif()
QT5_USE_MODULES(barres Widgets)

