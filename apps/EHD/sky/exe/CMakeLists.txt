# Example of compiled problem using "sky"

FILE(GLOB SRCS *.h *.cpp *.inl *.hpp)

ADD_EXECUTABLE(sky_app ${SRCS})
MACRO_DebugPostfix(sky_app)

TARGET_LINK_LIBRARIES(sky_app sky)

add_test(NAME sky_app
         COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/run.py --nogui ${EXECUTABLE_OUTPUT_PATH}/sky_app
         WORKING_DIRECTORY ${PROJECT_BINARY_DIR})
