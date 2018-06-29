# Example of compiled problem using "gauss"

FILE(GLOB SRCS *.h *.cpp *.inl *.hpp)

ADD_EXECUTABLE(gauss_app ${SRCS})
MACRO_DebugPostfix(gauss_app)

# ---

INCLUDE_DIRECTORIES( ${PROJECT_SOURCE_DIR}/gauss/src )
TARGET_LINK_LIBRARIES(gauss_app gauss)

#add_test(NAME gauss_app COMMAND ${EXECUTABLE_OUTPUT_PATH}/gauss_app
#         WORKING_DIRECTORY ${PROJECT_BINARY_DIR})

add_test(NAME gauss_app
         COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/run.py --nogui ${EXECUTABLE_OUTPUT_PATH}/gauss_app
         WORKING_DIRECTORY ${PROJECT_BINARY_DIR})