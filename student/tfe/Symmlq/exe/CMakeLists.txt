

ADD_EXECUTABLE(test_symmlq2 test_symmlq2.for) # .f90/.f95 => free format | .f/.for => fixed
TARGET_LINK_LIBRARIES(test_symmlq2 tfe common)

# ENABLE_TESTING()
# ADD_TEST(NAME theoric COMMAND ${EXECUTABLE_OUTPUT_PATH}/theoric
#         WORKING_DIRECTORY ${PROJECT_BINARY_DIR})