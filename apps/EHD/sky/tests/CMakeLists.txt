
FILE(GLOB tfiles *.py)
FOREACH(tfile ${tfiles})
    STRING(REPLACE "${PROJECT_SOURCE_DIR}/" "" strip ${tfile})
    MESSAGE(STATUS "Adding test ${strip}")
    ADD_TEST(NAME ${strip}
             COMMAND ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/run.py --nogui ${tfile}
             WORKING_DIRECTORY ${PROJECT_BINARY_DIR})
ENDFOREACH()
