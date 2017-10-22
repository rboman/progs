# Example of compiled problem using "gauss"

FILE(GLOB SRCS *.h *.cpp *.inl *.hpp)

ADD_EXECUTABLE(gauss_app ${SRCS})
MACRO_DebugPostfix(gauss_app)

# ---

INCLUDE_DIRECTORIES( ${PROJECT_SOURCE_DIR}/gauss/src )
TARGET_LINK_LIBRARIES(gauss_app gauss)
