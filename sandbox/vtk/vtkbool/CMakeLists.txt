# see https://gitlab.kitware.com/vtk/vtk/-/tree/master/Examples/Modules/Wrapping
#
# projet de user-module python VTK
#   - utilise le build system de VTK9 minimum
#   - se construit comme Metafor (dans bin\Release et non bin\Lib\site-packages)
#   - s'installe comme Metafor (dans la racine de CMAKE_INSTALL_PREFIX et non dans bin\Lib\site-packages)
#   - marche en debug
#
# build/Windows
#   cmake -DCMAKE_INSTALL_PREFIX=c:\temp .. && cmake --build . --config Debug --target INSTALL
# test/Windows
#   ..\test_vtkbool.py  
#   python_d ..\test_vtkbool.py  


CMAKE_MINIMUM_REQUIRED(VERSION 3.12 FATAL_ERROR)
PROJECT(vtkbool)

SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin CACHE PATH
                        "Single output directory for building all libraries.")
SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin CACHE PATH
                        "Single output directory for building all executables.")
MARK_AS_ADVANCED(CMAKE_LIBRARY_OUTPUT_DIRECTORY 
                 CMAKE_RUNTIME_OUTPUT_DIRECTORY)

IF(NOT CMAKE_BUILD_TYPE)
    SET( CMAKE_BUILD_TYPE "Release" CACHE STRING  "" FORCE)
ENDIF()

SET(CMAKE_CXX_STANDARD 11)
SET(CMAKE_CXX_STANDARD_REQUIRED ON)

IF(MSVC)
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /MP")
ENDIF()

FIND_PACKAGE(VTK REQUIRED COMPONENTS 
    FiltersSources IOLegacy FiltersExtraction 
    FiltersGeometry FiltersModeling FiltersFlowPaths 
    WrappingPythonCore NO_MODULE)

if(VTK_FOUND AND VTK_VERSION VERSION_GREATER_EQUAL "9.0.0")

    INCLUDE(GNUInstallDirs)
    SET(CMAKE_INSTALL_BINDIR ".") # install .dll in the root of CMAKE_INSTALL_PREFIX (windows)
    SET(CMAKE_INSTALL_LIBDIR ".") # install .so in the root of CMAKE_INSTALL_PREFIX (linux)

    # First we scan the modules in our project to find out the dependency graph
    # between them.

    vtk_module_scan(
        MODULE_FILES "${CMAKE_CURRENT_SOURCE_DIR}/module/vtk.module"
        REQUEST_MODULES vtkBool
        PROVIDES_MODULES modules)

    # Build the module we just scanned.
    SET(BUILD_SHARED_LIBS ON) # si OFF lib statique dans le .pyd (mais warnings) - pas recommandé dans la doc.
    vtk_module_build(MODULES ${modules})

    # wrap VTK module in python (name of target="vtkBoolPython")
    #   (defined in "VTK-9.1.0\lib\cmake\vtk-9.1\vtkModuleWrapPython.cmake")
    vtk_module_wrap_python(
        MODULES ${modules}
        PYTHON_PACKAGE "vtkbool"
        MODULE_DESTINATION "."  # sinon bin\Lib\site-packages 
        USE_DEBUG_SUFFIX ON     # add "_d" for windows/debug
        BUILD_STATIC OFF
        INSTALL_HEADERS OFF)
    #print_target_properties(vtkBoolPython)

    # corrige le build dir du .pyd pour les générateurs multi-config tel Visual Studio
    GET_PROPERTY(_vtk_python_is_multi_config GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
    IF(_vtk_python_is_multi_config)
      FOREACH(_vtk_python_config IN LISTS CMAKE_CONFIGURATION_TYPES)
        STRING(TOUPPER "${_vtk_python_config}" _vtk_python_config_upper)
        SET_PROPERTY(TARGET vtkBoolPython PROPERTY
            "LIBRARY_OUTPUT_DIRECTORY_${_vtk_python_config_upper}" 
            "${CMAKE_BINARY_DIR}/bin/$<CONFIG>/vtkbool")
      ENDFOREACH()
    ELSE()
        SET_PROPERTY(TARGET vtkBoolPython PROPERTY
            "LIBRARY_OUTPUT_DIRECTORY" 
            "${CMAKE_BINARY_DIR}/bin/vtkbool")
    ENDIF()
ENDIF()
