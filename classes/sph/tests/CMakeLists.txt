ADD_SUBDIRECTORY( test_spoint )

#add_test(NAME test_spoint
#         WORKING_DIRECTORY ${PROJECT_SOURCE_DIR} 
#         COMMAND test_spoint)

add_test(NAME "Test_spoint"
         COMMAND ${CMAKE_COMMAND}
         -Dtest_cmd=${CMAKE_BINARY_DIR}/bin/test_spoint
         -Doutput_blessed=${CMAKE_SOURCE_DIR}/tests/test_spoint/test_spoint00000000.vtk
         -Doutput_test=${CMAKE_BINARY_DIR}/tests/test_spoint00000000.vtk
         -P ${CMAKE_SOURCE_DIR}/cmake/run_test.cmake
)