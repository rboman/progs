cmake_minimum_required(VERSION 3.1)
if(POLICY CMP0053)
  cmake_policy(SET CMP0053 NEW)
endif()
project(QtImageViewer)

find_package(VTK COMPONENTS vtkGUISupportQt )
include(${VTK_USE_FILE})
find_package(Qt5Core REQUIRED QUIET)
find_library( LIB_COCOA cocoa )

add_executable(qtimageviewer main.cxx osxHelper.mm)
qt5_use_modules(qtimageviewer Core Gui Widgets)
target_link_libraries(qtimageviewer ${VTK_LIBRARIES})
