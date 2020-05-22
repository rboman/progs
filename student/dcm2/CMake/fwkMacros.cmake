# useful macros/fcts


MACRO(MACRO_AddTest srcDir)
    #message(STATUS "Adding test directory ${srcDir}")
    file(GLOB tfiles RELATIVE ${srcDir} ${srcDir}/*)
    #message(STATUS "tfiles=${tfiles}")
    foreach(tfile ${tfiles})
        set(spath ${srcDir}/${tfile})
        if(NOT IS_DIRECTORY ${spath} AND ${spath} MATCHES ".+\\.py$")
            string(REPLACE "${PROJECT_SOURCE_DIR}/" "" strip ${spath}) 
            message(STATUS "Adding test ${strip}")
            add_test(NAME ${strip} 
                     WORKING_DIRECTORY ${PROJECT_SOURCE_DIR} 
                     COMMAND ${Python3_EXECUTABLE} ${PROJECT_SOURCE_DIR}/run.py --nogui ${strip})
        else()
            MACRO_AddTest(${srcDir}/${tfile})
        endif()
    endforeach()
ENDMACRO()


MACRO(MACRO_DebugPostfix libname)
    IF(MSVC)
        SET_TARGET_PROPERTIES(${libname} PROPERTIES DEBUG_POSTFIX "_d")
    ENDIF(MSVC)
ENDMACRO()
