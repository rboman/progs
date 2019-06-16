CMAKE_MINIMUM_REQUIRED(VERSION 3.1)
PROJECT(ImgFILTER CXX)

FIND_PACKAGE(Qt5 COMPONENTS Widgets REQUIRED)

FILE(GLOB SRCS *.h *.cpp)
SET(MOC_HEADERS imgfilter.h)

qt5_wrap_cpp(MOC_OUTFILES ${MOC_HEADERS})

ADD_EXECUTABLE(imgfilter ${SRCS} ${MOC_OUTFILES})

QT5_USE_MODULES(imgfilter Widgets)

configure_file(config.h.in config.h)
include_directories(${PROJECT_BINARY_DIR})
