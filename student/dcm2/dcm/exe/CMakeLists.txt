# Example of compiled problem using "dcm"

FILE(GLOB SRCS *.h *.cpp *.inl *.hpp)

ADD_EXECUTABLE(dcm_app ${SRCS})
MACRO_DebugPostfix(dcm_app)

# ---

INCLUDE_DIRECTORIES( ${PROJECT_SOURCE_DIR}/dcm/src )
TARGET_LINK_LIBRARIES(dcm_app dcm)
