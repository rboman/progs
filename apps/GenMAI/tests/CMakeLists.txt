
FILE(GLOB tfiles *.py)
FOREACH(tfile ${tfiles})
    STRING(REPLACE "${PROJECT_SOURCE_DIR}/" "" strip ${tfile})
    MESSAGE(STATUS "Adding test ${strip}")
    ADD_TEST(NAME ${strip}
             COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/run.py ${tfile} --nogui
             WORKING_DIRECTORY ${PROJECT_BINARY_DIR})
ENDFOREACH()
