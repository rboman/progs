CMAKE_MINIMUM_REQUIRED(VERSION 3.1)
PROJECT(OLCPGE CXX)

SET(CMAKE_CXX_STANDARD 17)
SET(CMAKE_CXX_STANDARD_REQUIRED ON)

# compiler settings
MESSAGE("CMAKE_CXX_COMPILER_ID=${CMAKE_CXX_COMPILER_ID}")
IF(CMAKE_CXX_COMPILER_ID MATCHES "MSVC")
    # ADD_DEFINITIONS(-DUNICODE)
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /MP")  # parallel build with MSVC
ELSEIF(CMAKE_CXX_COMPILER_ID MATCHES "GNU")
    ADD_DEFINITIONS(-DFORCE_EXPERIMENTAL_FS) 
    ADD_DEFINITIONS(-fpermissive)
    set(THREADS_PREFER_PTHREAD_FLAG ON)
    find_package(Threads REQUIRED)
    find_package(X11 REQUIRED)
    message(STATUS "X11_FOUND = ${X11_FOUND}")
    message(STATUS "X11_INCLUDE_DIR = ${X11_INCLUDE_DIR}")
    message(STATUS "X11_LIBRARIES = ${X11_LIBRARIES}")
    include_directories(${X11_INCLUDE_DIR})
    find_package(OpenGL REQUIRED)
    find_package(PNG REQUIRED)
ENDIF()


FIND_PATH(OLC_INCLUDE_DIRS "olcPixelGameEngine.h" REQUIRED)
MESSAGE(STATUS "OLC_INCLUDE_DIRS=${OLC_INCLUDE_DIRS}")


ADD_EXECUTABLE(etoiles1 etoiles1_pge.cpp)
TARGET_INCLUDE_DIRECTORIES(etoiles1 PUBLIC ${OLC_INCLUDE_DIRS})
IF(CMAKE_CXX_COMPILER_ID MATCHES "GNU")
    target_link_libraries(etoiles1 Threads::Threads PNG::PNG ${OPENGL_LIBRARIES} ${X11_LIBRARIES} stdc++fs)
ENDIF()

# configure_file(config.h.in config.h)
# include_directories(${PROJECT_BINARY_DIR})

