cmake_minimum_required(VERSION 2.8)
 
PROJECT(HistogramXYPlot)
 
find_package(VTK REQUIRED)
include(${VTK_USE_FILE})
 
add_executable(HistogramXYPlot HistogramXYPlot.cpp)
 
if(VTK_LIBRARIES)
    target_link_libraries(HistogramXYPlot ${VTK_LIBRARIES})
else()
    target_link_libraries(HistogramXYPlot vtkHybrid vtkWidgets)
endif()
