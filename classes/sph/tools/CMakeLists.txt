FILE(GLOB SRCS *.h *.cpp)
ADD_LIBRARY(tools SHARED ${SRCS})
INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR}/vtl)
TARGET_LINK_LIBRARIES(tools vtl)

