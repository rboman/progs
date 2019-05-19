

ADD_EXECUTABLE(test_gmres5 test_gmres5.for) # .f90/.f95 => free format | .f/.for => fixed
TARGET_LINK_LIBRARIES(test_gmres5 tfe common)

# ENABLE_TESTING()
# ADD_TEST(NAME theoric COMMAND ${EXECUTABLE_OUTPUT_PATH}/theoric
#         WORKING_DIRECTORY ${PROJECT_BINARY_DIR})