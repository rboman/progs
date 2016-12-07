#
# $Id: FindIOMP.cmake 2087 2014-10-02 07:02:45Z papeleux $
#
# FindIOMP.cmake - try to find Intel Open MP (IOMP) libs
# for use with mkl 
# by RoBo
#
# --- IOMP_LIB_PATH ---

SET(IOMP_LIB_PATH IOMP_LIB_PATH-NOTFOUND CACHE PATH "")

SET(IOMP_LIB_PATH_DESC "directory containing libiomp*.lib")
SET(IOMP_LIB_PATH_MESG "Set the IOMP_LIB_PATH cmake cache entry to the ${IOMP_LIB_PATH_DESC}")
FIND_PATH(IOMP_LIB_PATH 
             NAMES iomp5
             DOC "The ${IOMP_LIB_PATH_MESG}"
            )
IF(NOT IOMP_LIB_PATH)
    MESSAGE(FATAL_ERROR ${IOMP_LIB_PATH_MESG})
ENDIF(NOT IOMP_LIB_PATH)

# --- IOMP_INCLUDE_PATH ---

SET(IOMP_INCLUDE_PATH IOMP_INCLUDE_PATH-NOTFOUND CACHE PATH "")

SET(IOMP_INCLUDE_PATH_DESC "directory containing omp.h")
SET(IOMP_INCLUDE_PATH_MESG "Set the IOMP_INCLUDE_PATH cmake cache entry to the ${IOMP_INCLUDE_PATH_DESC}")
FIND_PATH(IOMP_INCLUDE_PATH 
             NAMES  omp.h
             DOC "The ${IOMP_DIR_DESCRIPTION}"
            )
IF(NOT IOMP_INCLUDE_PATH)
    MESSAGE(FATAL_ERROR ${IOMP_INCLUDE_PATH_MESG})
ENDIF(NOT IOMP_INCLUDE_PATH)
