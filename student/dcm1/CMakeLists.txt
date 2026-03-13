cmake_minimum_required(VERSION 3.16)
project(Barres LANGUAGES CXX)

# Set a default build type only for single-config generators (Ninja/Makefiles).
if(NOT CMAKE_CONFIGURATION_TYPES AND NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

set(CMAKE_AUTOMOC ON)

find_package(Qt5 COMPONENTS Widgets LinguistTools REQUIRED)

set(TS_FILES
    i18n/barres_en.ts
)

set(TS_SOURCES
    main.cpp
    Window.cpp
    Window.h
    Barres.cpp
    Barres.h
    MechanismKinematicsSolver.cpp
    MechanismKinematicsSolver.h
    MechanismRenderer.cpp
    MechanismRenderer.h
)

# Extract translatable strings and compile *.qm during build.
qt5_create_translation(QM_FILES
    ${TS_SOURCES}
    ${TS_FILES}
)

add_executable(barres)
target_sources(barres PRIVATE
    main.cpp
    Window.cpp
    Window.h
    Barres.cpp
    Barres.h
    MechanismKinematicsSolver.cpp
    MechanismKinematicsSolver.h
    MechanismRenderer.cpp
    MechanismRenderer.h
    ${TS_FILES}
    ${QM_FILES}
)
target_compile_features(barres PRIVATE cxx_std_11)
target_link_libraries(barres PRIVATE Qt5::Widgets)

# Make runtime translation files available next to the executable.
add_custom_command(TARGET barres POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E make_directory $<TARGET_FILE_DIR:barres>/i18n
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${QM_FILES} $<TARGET_FILE_DIR:barres>/i18n
)

add_executable(mechanism_kinematics_solver_test)
target_sources(mechanism_kinematics_solver_test PRIVATE
    tests/MechanismKinematicsSolver.test.cpp
    MechanismKinematicsSolver.cpp
    MechanismKinematicsSolver.h
)
target_compile_features(mechanism_kinematics_solver_test PRIVATE cxx_std_11)
target_include_directories(mechanism_kinematics_solver_test
                           PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

if(MSVC)
    target_compile_definitions(barres PRIVATE _CRT_SECURE_NO_WARNINGS
                                              _SCL_SECURE_NO_DEPRECATE)
    target_compile_options(barres PRIVATE /MP)

    target_compile_definitions(mechanism_kinematics_solver_test
                               PRIVATE _CRT_SECURE_NO_WARNINGS
                                       _SCL_SECURE_NO_DEPRECATE)
    target_compile_options(mechanism_kinematics_solver_test PRIVATE /MP)
endif()

include(CTest)
if(BUILD_TESTING)
    add_test(NAME mechanism_kinematics_solver_test
             COMMAND mechanism_kinematics_solver_test)
endif()

