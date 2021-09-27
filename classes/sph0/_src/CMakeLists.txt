# Copyright 2020 University of Liège
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# CMake input file of the SWIG wrapper around "sph.so"

INCLUDE(${SWIG_USE_FILE})

FILE(GLOB SRCS *.h *.cpp *.inl *.swg)
FILE(GLOB ISRCS *.i)

SET(CMAKE_SWIG_FLAGS "")
SET_SOURCE_FILES_PROPERTIES(${ISRCS} PROPERTIES CPLUSPLUS ON)

SET(SWINCFLAGS 
-I${PROJECT_SOURCE_DIR}/sph/src
-I${PROJECT_SOURCE_DIR}/tbox/src
-I${PROJECT_SOURCE_DIR}/tbox/_src
-I${PROJECT_SOURCE_DIR}/fwk/src
-I${PROJECT_SOURCE_DIR}/fwk/_src
-I${EIGEN_INCLUDE_DIRS}
)
SET_SOURCE_FILES_PROPERTIES(${ISRCS} PROPERTIES SWIG_FLAGS "${SWINCFLAGS}")

if (${CMAKE_VERSION} VERSION_LESS "3.8.0")
    SWIG_ADD_MODULE(sphw python ${ISRCS} ${SRCS})
else()
    SWIG_ADD_LIBRARY(sphw LANGUAGE python SOURCES ${ISRCS} ${SRCS})
endif()
MACRO_DebugPostfix(_sphw)

TARGET_INCLUDE_DIRECTORIES(_sphw PRIVATE ${PROJECT_SOURCE_DIR}/fwk/_src
                                         ${PROJECT_SOURCE_DIR}/tbox/_src
                                         ${PYTHON_INCLUDE_PATH}
)

SWIG_LINK_LIBRARIES(sphw 
                    sph tbox fwk ${PYTHON_LIBRARIES}
)
