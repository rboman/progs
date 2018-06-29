
file(GLOB tfiles *.py)
foreach(tfile ${tfiles})
    string(REPLACE "${PROJECT_SOURCE_DIR}/" "" strip ${tfile})
    message(STATUS "Adding test ${strip}")
    add_test(NAME ${strip}
             COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/run.py --nogui ${tfile}         
             WORKING_DIRECTORY ${PROJECT_BINARY_DIR})
endforeach()
