# Example of compiled problem using "ndh"

FILE(GLOB SRCS *.h *.cpp *.inl *.hpp)

ADD_EXECUTABLE(ndh_app ${SRCS})
MACRO_DebugPostfix(ndh_app)
INCLUDE_DIRECTORIES( ${PROJECT_SOURCE_DIR}/ndh/src )
TARGET_LINK_LIBRARIES(ndh_app ndh)
