# Options

OPTION(SPH_USE_GUI "Build Qt/VTK GUI" ON)

SET(LIBNAME "sph")

IF(SPH_USE_GUI)
    SET(CMAKE_AUTOMOC ON)  # before target definition
    # SET(CMAKE_AUTORCC ON)
    # SET(CMAKE_AUTOUIC ON)
ENDIF()

FILE(GLOB SRCS *.h *.cpp *.inl *.hpp)
ADD_LIBRARY(${LIBNAME} SHARED ${SRCS})


# find Eigen
# FIND_PACKAGE(Eigen3 3.4 REQUIRED NO_MODULE)
# set(CMAKE_FIND_DEBUG_MODE TRUE)
FIND_PATH(EIGEN_INCLUDE_DIRS "Eigen/Dense" 
          PATHS "${PROJECT_SOURCE_DIR}/lib/eigen" 
                "/usr/include/eigen3" 
                "c:/msys64/mingw64/include/eigen3")
MESSAGE(STATUS "EIGEN_INCLUDE_DIRS=" ${EIGEN_INCLUDE_DIRS})
IF(NOT EIGEN_INCLUDE_DIRS)
    MESSAGE(FATAL_ERROR "Eigen include dir not found!")
ENDIF()

TARGET_INCLUDE_DIRECTORIES(${LIBNAME} PUBLIC ${EIGEN_INCLUDE_DIRS})
#TARGET_LINK_LIBRARIES(${LIBNAME} Eigen3::Eigen)

# find OpenMP
FIND_PACKAGE(OpenMP)
IF(OPENMP_FOUND)
    MESSAGE (STATUS "OpenMP enabled (CXX_FLAGS=${OpenMP_CXX_FLAGS})")
ELSE()
    MESSAGE (STATUS "OpenMP disabled.")
ENDIF()

IF(OpenMP_CXX_FOUND)
    TARGET_LINK_LIBRARIES(${LIBNAME} PUBLIC OpenMP::OpenMP_CXX)
ENDIF()

TARGET_COMPILE_DEFINITIONS(${LIBNAME} PUBLIC EIGEN_DONT_PARALLELIZE)
# try to increase Eigen performance in Debug mode (inlining and no assertions)
# TARGET_COMPILE_DEFINITIONS(${LIBNAME} PUBLIC $<$<CONFIG:Debug>: EIGEN_NO_DEBUG >)
# TARGET_COMPILE_DEFINITIONS(${LIBNAME} PUBLIC $<$<CONFIG:Debug>: "EIGEN_STRONG_INLINE=__attribute__((always_inline)) inline" > )

TARGET_INCLUDE_DIRECTORIES(${LIBNAME} PUBLIC ${CMAKE_CURRENT_LIST_DIR})

IF(SPH_USE_GUI)

    FILE(GLOB GUI_SRCS gui/*.h gui/*.cpp)
    TARGET_SOURCES(${LIBNAME} PRIVATE ${GUI_SRCS})
    TARGET_INCLUDE_DIRECTORIES(${LIBNAME} PUBLIC ${CMAKE_CURRENT_LIST_DIR}/gui)

    # -- find Qt
    FIND_PACKAGE(Qt6 COMPONENTS Core)
    IF(NOT Qt6_FOUND)
        FIND_PACKAGE(Qt5 5.15 REQUIRED COMPONENTS Core)
    ENDIF()

    # -- find VTK
    FIND_PACKAGE(VTK 9.0 COMPONENTS 
        GUISupportQt
        InteractionWidgets
        FiltersHybrid
        IOExport
        IOXML
        CommonColor
        CommonCore
        FiltersSources
        InteractionStyle
        RenderingContextOpenGL2
        RenderingCore
        RenderingFreeType
        RenderingGL2PSOpenGL2
        RenderingOpenGL2
    )
    IF(NOT VTK_FOUND)
        MESSAGE(FATAL_ERROR "VTK 9 not found!")
    ENDIF()

    # VTK/Qt
    TARGET_LINK_LIBRARIES(${LIBNAME} PUBLIC ${VTK_LIBRARIES} Qt::Widgets)

    vtk_module_autoinit( TARGETS ${LIBNAME} 
                         MODULES ${VTK_LIBRARIES})

ENDIF()

CONFIGURE_FILE(sph_config.h.in sph_config.h)
TARGET_INCLUDE_DIRECTORIES(${LIBNAME} PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
