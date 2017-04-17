# LIB

FILE(GLOB SRCS *.h *.cpp *.inl *.hpp)

ADD_LIBRARY(genmai STATIC ${SRCS})

IF(WIN32)
    SET_TARGET_PROPERTIES(genmai PROPERTIES DEBUG_POSTFIX "_d")
ENDIF(WIN32)

SOURCE_GROUP(base       REGULAR_EXPRESSION ".*\\.(cpp|inl|hpp|h)")
SOURCE_GROUP(exporters  REGULAR_EXPRESSION ".+(Exporter).+")
SOURCE_GROUP(builders   REGULAR_EXPRESSION ".+(Builder).+")
SOURCE_GROUP(params     REGULAR_EXPRESSION ".+(Param).+")
