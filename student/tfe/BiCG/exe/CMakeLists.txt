

ADD_EXECUTABLE(test_bicg test_bicg.f90) # .f90/.f95 => free format | .f/.for => fixed
TARGET_LINK_LIBRARIES(test_bicg tfe sparskit common)

ADD_EXECUTABLE(test_bicgst test_bicgst.f90) # .f90/.f95 => free format | .f/.for => fixed
TARGET_LINK_LIBRARIES(test_bicgst tfe sparskit common)

ADD_EXECUTABLE(test_cgs test_cgs.for) # .f90/.f95 => free format | .f/.for => fixed
TARGET_LINK_LIBRARIES(test_cgs tfe sparskit common)

# ENABLE_TESTING()
# ADD_TEST(NAME theoric COMMAND ${EXECUTABLE_OUTPUT_PATH}/theoric
#         WORKING_DIRECTORY ${PROJECT_BINARY_DIR})