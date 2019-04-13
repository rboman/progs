# Example of compiled problem using "ehd"

FILE(GLOB SRCS *.h *.cpp *.inl *.hpp)

ADD_EXECUTABLE(ehd_app ${SRCS})
MACRO_DebugPostfix(ehd_app)

TARGET_LINK_LIBRARIES(ehd_app ehd)

ADD_TEST(NAME ehd_app
         COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/run.py --nogui ${EXECUTABLE_OUTPUT_PATH}/ehd_app
         WORKING_DIRECTORY ${PROJECT_BINARY_DIR})