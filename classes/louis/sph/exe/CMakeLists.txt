# louis++ - main executable

FILE(GLOB SRCS *.h *.cpp)
ADD_EXECUTABLE(louis++ ${SRCS})
TARGET_LINK_LIBRARIES(louis++ PRIVATE sph)
