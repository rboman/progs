# Example of compiled problem using "sky"

FILE(GLOB SRCS *.h *.cpp *.inl *.hpp)

ADD_EXECUTABLE(sky_app ${SRCS})
MACRO_DebugPostfix(sky_app)

# ---

INCLUDE_DIRECTORIES( ${PROJECT_SOURCE_DIR}/sky/src )
TARGET_LINK_LIBRARIES(sky_app sky)
