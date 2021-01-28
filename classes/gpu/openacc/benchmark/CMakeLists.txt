PROJECT(OPENACC CXX)
CMAKE_MINIMUM_REQUIRED(VERSION 3.1)

# build type is "" by default in Linux
IF(NOT CMAKE_BUILD_TYPE)
    SET( CMAKE_BUILD_TYPE "Release" CACHE STRING "" FORCE)
ENDIF()

#FIND_PACKAGE(OpenACC)
#MESSAGE("OpenACC_CXX_FLAGS=${OpenACC_CXX_FLAGS}")
#MESSAGE("OpenACC_C_FLAGS=${OpenACC_C_FLAGS}")
#MESSAGE("OpenACC_SPEC_DATE=${OpenACC_SPEC_DATE}")
#MESSAGE("OpenACC_VERSION=${OpenACC_VERSION}")

#SET(ACCEL cc75) # GeForce 1660 GTX 
SET(ACCEL cc70)  # Titan V (treebeard)


ADD_COMPILE_OPTIONS(-Minfo=accel)

# serial code running on CPU

SET(ACCELFLAG )
ADD_EXECUTABLE( jsolvec_serial jsolvec.cpp)
TARGET_COMPILE_OPTIONS(jsolvec_serial PUBLIC ${ACCELFLAG})
#TARGET_LINK_OPTIONS(jsolveci_serial PUBLIC )${ACCELFLAG} # cmake 3.13
set_property(TARGET jsolvec_serial APPEND_STRING PROPERTY LINK_FLAGS " ${ACCELFLAG}")

# multicore code running on CPU

SET(ACCELFLAG -ta=multicore)
ADD_EXECUTABLE( jsolvec_multicore jsolvec_managed.cpp)
TARGET_COMPILE_OPTIONS(jsolvec_multicore PUBLIC ${ACCELFLAG})
#TARGET_LINK_OPTIONS(jsolvec_multicore PUBLIC )${ACCELFLAG} # cmake 3.13
set_property(TARGET jsolvec_multicore  APPEND_STRING PROPERTY LINK_FLAGS " ${ACCELFLAG}")

# GPU code with automatc management of data

SET(ACCELFLAG -ta=tesla:${ACCEL},managed)
ADD_EXECUTABLE( jsolvec_managed jsolvec_managed.cpp)
TARGET_COMPILE_OPTIONS(jsolvec_managed PUBLIC ${ACCELFLAG})
set_property(TARGET jsolvec_managed APPEND_STRING PROPERTY LINK_FLAGS " ${ACCELFLAG}")

# GPU codes with manual data management (with or without async kernels)

SET(ACCELFLAG -ta=tesla:${ACCEL})
ADD_EXECUTABLE( jsolvec_manual jsolvec_manual.cpp)
TARGET_COMPILE_OPTIONS(jsolvec_manual PUBLIC ${ACCELFLAG})
set_property(TARGET jsolvec_manual APPEND_STRING PROPERTY LINK_FLAGS " ${ACCELFLAG}")

ADD_EXECUTABLE( jsolvec_manual_async jsolvec_manual_async.cpp)
TARGET_COMPILE_OPTIONS(jsolvec_manual_async PUBLIC ${ACCELFLAG})
set_property(TARGET jsolvec_manual_async APPEND_STRING PROPERTY LINK_FLAGS " ${ACCELFLAG}")


