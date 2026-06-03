cmake_minimum_required(VERSION 3.16)

project(MailSph LANGUAGES CXX)

include(CMakeToolsHelpers OPTIONAL)  # integration VSCode + CMake

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(VTK_COMPONENTS
    CommonCore
    CommonDataModel
    CommonExecutionModel
    FiltersCore
    FiltersExtraction
    FiltersGeneral
    FiltersHybrid
    FiltersModeling
    IOCore
    IOXML
    InteractionStyle
    InteractionWidgets
    RenderingAnnotation
    RenderingCore
    RenderingOpenGL2
)

find_package(VTK 9.3 REQUIRED COMPONENTS ${VTK_COMPONENTS})

file(GLOB SRCS CONFIGURE_DEPENDS *.h *.cpp)

add_executable(mailsph MACOSX_BUNDLE ${SRCS})
target_link_libraries(mailsph PRIVATE ${VTK_LIBRARIES})

if(MSVC)
    target_compile_definitions(mailsph PRIVATE _CRT_SECURE_NO_WARNINGS _SCL_SECURE_NO_DEPRECATE)
    target_compile_options(mailsph PRIVATE /MP)
endif()

vtk_module_autoinit(
    TARGETS mailsph
    MODULES ${VTK_LIBRARIES}
)
