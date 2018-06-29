# Example of compiled problem using "ehd"

FILE(GLOB SRCS *.h *.cpp *.inl *.hpp)

ADD_EXECUTABLE(ehd_app ${SRCS})
MACRO_DebugPostfix(ehd_app)

# ---

INCLUDE_DIRECTORIES( ${PROJECT_SOURCE_DIR}/ehd/src  
                     ${PROJECT_SOURCE_DIR}/gauss/src  
                     ${PROJECT_SOURCE_DIR}/sky/src 
)
TARGET_LINK_LIBRARIES(ehd_app ehd)

#add_test(NAME ehd_app COMMAND ${EXECUTABLE_OUTPUT_PATH}/ehd_app 
#         WORKING_DIRECTORY ${PROJECT_BINARY_DIR})

add_test(NAME ehd_app
         COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/run.py --nogui ${EXECUTABLE_OUTPUT_PATH}/ehd_app
         WORKING_DIRECTORY ${PROJECT_BINARY_DIR})