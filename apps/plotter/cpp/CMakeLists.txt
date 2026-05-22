cmake_minimum_required(VERSION 3.16)
project(plotter CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Enable automatic compilation of Qt MOC, RCC, and UIC files
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)

# Try to find Qt6, fall back to Qt5 if not found
find_package(Qt6 COMPONENTS Widgets QUIET)
if(Qt6_FOUND)
    message(STATUS "Using Qt6")
    set(QT_LIBRARIES Qt6::Widgets)
else()
    find_package(Qt5 COMPONENTS Widgets REQUIRED)
    message(STATUS "Using Qt5")
    set(QT_LIBRARIES Qt5::Widgets)
endif()

# Source and header files
set(SOURCES
    main.cpp
    core.cpp
    plot2dwidget.cpp
)

set(HEADERS
    core.h
    plot2dwidget.h
)

add_executable(plotter ${SOURCES} ${HEADERS})
target_link_libraries(plotter PRIVATE ${QT_LIBRARIES})
