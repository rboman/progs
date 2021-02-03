FILE(GLOB SRCS *.h *.cpp)
ADD_LIBRARY(json SHARED ${SRCS})
INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR}/vtl)
TARGET_LINK_LIBRARIES(json vtl)

# RapidJSON include dir
find_path(RAPIDJSON_INCLUDE_DIRS NAMES "rapidjson/rapidjson.h")
message(STATUS "RAPIDJSON_INCLUDE_DIRS=" ${RAPIDJSON_INCLUDE_DIRS})
if(NOT RAPIDJSON_INCLUDE_DIRS)
    message(FATAL_ERROR "RapidJSON include dir not found!")
endif()
include_directories(${RAPIDJSON_INCLUDE_DIRS})