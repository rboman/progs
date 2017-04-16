# Exe 

FILE(GLOB SRCS *.h *.cpp)

ADD_EXECUTABLE(genmai_console ${SRCS})
IF(WIN32)
    SET_TARGET_PROPERTIES(genmai_console PROPERTIES DEBUG_POSTFIX "_d")
ENDIF(WIN32)

# ---

INCLUDE_DIRECTORIES( ${PROJECT_SOURCE_DIR}/utils 
${PROJECT_SOURCE_DIR}/builders
${PROJECT_SOURCE_DIR}/export
${PROJECT_SOURCE_DIR}/params
)

TARGET_LINK_LIBRARIES(genmai_console exporters builders params utils )
