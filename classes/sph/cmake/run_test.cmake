# adapted from https://cmake.org/pipermail/cmake/2009-July/030595.html


# some argument checking:
# test_cmd is the command to run with all its arguments
if( NOT test_cmd )
   message( FATAL_ERROR "Variable test_cmd not defined" )
endif()
# output_blessed contains the name of the "blessed" output file
if( NOT output_blessed )
   message( FATAL_ERROR "Variable output_blessed not defined" )
endif()
# output_test contains the name of the output file the test_cmd will produce
if( NOT output_test )
   message( FATAL_ERROR "Variable output_test not defined" )
endif()

# -------------------------------------------------------------------

# !probleme si output_blessed/output_test comportent des guillemets

message("RUNNING ${test_cmd}")
execute_process(
     COMMAND ${test_cmd}
     RESULT_VARIABLE test_not_successful 
     #OUTPUT_QUIET 
     #ERROR_QUIET
)
#message("test_not_successful=${test_not_successful}")
if( test_not_successful )
   message( SEND_ERROR "${test_cmd} did not run properly!" )
endif()

message("COMPARING output files:")
message("\toutput_blessed=${output_blessed}")
message("\toutput_test=${output_test}")

execute_process(
     COMMAND ${CMAKE_COMMAND} -E compare_files ${output_blessed} ${output_test}
     RESULT_VARIABLE test_not_successful
     #OUTPUT_QUIET 
     #ERROR_QUIET
)
#message("test_not_successful=${test_not_successful}")

if( test_not_successful )
   message( SEND_ERROR "${output_test} does not match ${output_blessed}!" )
endif()
