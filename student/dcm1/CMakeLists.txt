CMAKE_MINIMUM_REQUIRED(VERSION 3.1)
PROJECT(Barres CXX)

# on Linux, set optimisation by default
IF(NOT CMAKE_BUILD_TYPE)
    SET(CMAKE_BUILD_TYPE "Release" CACHE STRING "" FORCE)
ENDIF()

# compiler settings
IF(CMAKE_CXX_COMPILER_ID MATCHES "MSVC")
    # target-scoped settings are added after targets are declared
ENDIF()

# -- C++11
SET(CMAKE_CXX_STANDARD 11)
SET(CMAKE_CXX_STANDARD_REQUIRED ON)

FIND_PACKAGE(Qt5 COMPONENTS Widgets REQUIRED)

FILE(GLOB SRCS *.h *.cpp)
SET(MOC_HEADERS Barres.h Window.h)

qt5_wrap_cpp (MOC_OUTFILES ${MOC_HEADERS})

ADD_EXECUTABLE(barres ${SRCS} ${MOC_OUTFILES})
TARGET_LINK_LIBRARIES(barres PRIVATE Qt5::Widgets)

ADD_EXECUTABLE(mechanism_kinematics_solver_test
               tests/MechanismKinematicsSolver.test.cpp
               MechanismKinematicsSolver.cpp)
TARGET_INCLUDE_DIRECTORIES(mechanism_kinematics_solver_test
                           PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

IF(CMAKE_CXX_COMPILER_ID MATCHES "MSVC")
    TARGET_COMPILE_DEFINITIONS(barres PRIVATE _CRT_SECURE_NO_WARNINGS
                                              _SCL_SECURE_NO_DEPRECATE)
    TARGET_COMPILE_OPTIONS(barres PRIVATE /MP)

    TARGET_COMPILE_DEFINITIONS(mechanism_kinematics_solver_test
                               PRIVATE _CRT_SECURE_NO_WARNINGS
                                       _SCL_SECURE_NO_DEPRECATE)
    TARGET_COMPILE_OPTIONS(mechanism_kinematics_solver_test PRIVATE /MP)
ENDIF()

ENABLE_TESTING()
ADD_TEST(NAME mechanism_kinematics_solver_test
         COMMAND mechanism_kinematics_solver_test)

