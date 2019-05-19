

ADD_EXECUTABLE(test_pcg test_pcg.for) # .f90/.f95 => free format | .f/.for => fixed
TARGET_LINK_LIBRARIES(test_pcg tfe common)
