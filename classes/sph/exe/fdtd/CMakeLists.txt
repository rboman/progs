FILE(GLOB SRCS *.h *.cpp)
ADD_EXECUTABLE(fdtd ${SRCS})
INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR}/json ${PROJECT_SOURCE_DIR}/vtl)
TARGET_LINK_LIBRARIES(fdtd json vtl)

# RapidJSON include dir
find_path(RAPIDJSON_INCLUDE_DIRS NAMES "rapidjson/rapidjson.h")
message(STATUS "RAPIDJSON_INCLUDE_DIRS=" ${RAPIDJSON_INCLUDE_DIRS})
if(NOT RAPIDJSON_INCLUDE_DIRS)
    message(FATAL_ERROR "RapidJSON include dir not found!")
endif()
include_directories(${RAPIDJSON_INCLUDE_DIRS})



# tests 

file(GLOB tfiles *.json)
foreach(tfile ${tfiles})
    string(REPLACE "${PROJECT_SOURCE_DIR}/" "" strip ${tfile})
    message(STATUS "Adding test ${strip}")
    add_test(NAME ${strip}
             COMMAND ${CMAKE_BINARY_DIR}/bin/fdtd ${tfile}
            )
endforeach()
