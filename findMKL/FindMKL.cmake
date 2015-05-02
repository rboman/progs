#
# $Id: FindMKL.cmake 2209 2015-03-27 15:53:12Z boman $
#
# FindMKL.cmake - try to find MKL libs
# by RoBo
#

# --- MKL_LIB_PATH ---

SET(MKL_LIB_PATH MKL_LIB_PATH-NOTFOUND CACHE PATH "")

SET(MKL_LIB_PATH_DESC "directory containing mkl*.lib")
SET(MKL_LIB_PATH_MESG "Set the MKL_LIB_PATH cmake cache entry to the ${MKL_LIB_PATH_DESC}")
FIND_PATH(MKL_LIB_PATH 
             NAMES mkl_dll.lib mkl_core
             DOC "The ${MKL_LIB_PATH_MESG}"
            )
IF(NOT MKL_LIB_PATH)
    MESSAGE(FATAL_ERROR ${MKL_LIB_PATH_MESG})
ENDIF(NOT MKL_LIB_PATH)

MESSAGE(STATUS "MKL_LIB_PATH=${MKL_LIB_PATH}")

# --- MKL_INCLUDE_PATH ---

SET(MKL_INCLUDE_PATH MKL_INCLUDE_PATH-NOTFOUND CACHE PATH "")

SET(MKL_INCLUDE_PATH_DESC "directory containing mkl_dss.h")
SET(MKL_INCLUDE_PATH_MESG "Set the MKL_INCLUDE_PATH cmake cache entry to the ${MKL_INCLUDE_PATH_DESC}")
FIND_PATH(MKL_INCLUDE_PATH 
             NAMES  mkl_dss.h
             DOC "The ${MKL_DIR_DESCRIPTION}"
            )
IF(NOT MKL_INCLUDE_PATH)
    MESSAGE(FATAL_ERROR ${MKL_INCLUDE_PATH_MESG})
ENDIF(NOT MKL_INCLUDE_PATH)

MESSAGE(STATUS "MKL_INCLUDE_PATH=${MKL_INCLUDE_PATH}")

# --- MKL_LIBS ---

# la lib solver est intégrée à mkl_intel pour MKL >=11.0
IF(CMAKE_SIZEOF_VOID_P MATCHES "8")
    find_library(MKL_SOLVER_LIBRARY
    NAMES mkl_solver_lp64
    PATHS ${MKL_LIB_PATH}
    )
ELSE()
    find_library(MKL_SOLVER_LIBRARY
    NAMES mkl_solver
    PATHS ${MKL_LIB_PATH}
    )
ENDIF()
MESSAGE(STATUS "MKL_SOLVER_LIBRARY=${MKL_SOLVER_LIBRARY}")

# win32 (X86) & mkl < 11 => mkl are not SAFESEH 
# => add linker option to deactivate SAFESEH link ...
IF(WIN32) # Windows 
    IF(${_MACHINE_ARCH_FLAG} MATCHES X86) # 32 bits
        IF (MKL_SOLVER_LIBRARY) # Methode bourrin pour voir si mkl version < 11 : 
                                # en version 11, MKL_SOLVER_LIBRARY est vide !!!
            SET (MKL_LINKER_FLAG "/SAFESEH:NO")
            MESSAGE(STATUS "MKL_LINKER_FLAG=${MKL_LINKER_FLAG}")
        ENDIF()
    ENDIF()
ENDIF()

#
IF(APPLE)
    SET(MKL_LIBS mkl_intel_lp64 mkl_sequential mkl_core CACHE FILEPATH "" FORCE)
ELSE(APPLE)
    IF(WIN32)
        IF(CMAKE_SIZEOF_VOID_P MATCHES "8")
            # windows-x64
            SET(MKL_LIBS mkl_intel_lp64_dll mkl_intel_thread_dll mkl_core_dll libiomp5md  CACHE FILEPATH "" FORCE)
        ELSE()
            # windows-x86
            SET(MKL_LIBS mkl_intel_c_dll mkl_intel_thread_dll mkl_core_dll libiomp5md CACHE FILEPATH "" FORCE) # old
        ENDIF()
    ELSE(WIN32)
        IF(CMAKE_SIZEOF_VOID_P MATCHES "8") # 64bits
            IF(CMAKE_CXX_COMPILER MATCHES "icpc$")
                # linux-x64-icc :  -L$(MKLROOT)/lib/intel64 -lmkl_intel_lp64 -lmkl_core -lmkl_intel_thread -lpthread -lm
                SET(MKL_LIBS mkl_intel_lp64 mkl_intel_thread mkl_core iomp5 pthread CACHE FILEPATH "" FORCE)
            ELSE() # gcc
                IF(MKL_USE_IOMP) # linux gcc & intel threads (libiomp) : -Wl,--no-as-needed -L$(MKLROOT)/lib/intel64 -lmkl_intel_lp64 -lmkl_core -lmkl_intel_thread -liomp5 -ldl -lpthread -lm
                    SET(MKL_LIBS mkl_intel_lp64 mkl_core mkl_intel_thread iomp5 dl pthread m CACHE FILEPATH "" FORCE)  
                ELSE() # linux-x64-gcc - gnu thread (libgomp) :  -Wl,--no-as-needed -L$(MKLROOT)/lib/intel64 -lmkl_intel_lp64 -lmkl_core -lmkl_gnu_thread -ldl -lpthread -lm
                    SET(MKL_LIBS mkl_intel_lp64 mkl_core mkl_gnu_thread gomp dl pthread m CACHE FILEPATH "" FORCE)  
                ENDIF()
                SET(MKL_LINKER_FLAG "-Wl,--no-as-needed")            
            ENDIF()
        ELSE() # 32 bits
            IF(CMAKE_CXX_COMPILER MATCHES "icpc$")
                # linux-x86-icc :  -L$(MKLROOT)/lib/ia32 -lmkl_intel -lmkl_core -lmkl_intel_thread -lpthread -lm
                SET(MKL_LIBS mkl_intel mkl_intel_thread mkl_core iomp5 pthread CACHE FILEPATH "" FORCE)
            ELSE()
                # linux-x86-gcc gnu threads:   -Wl,--no-as-needed -L$(MKLROOT)/lib/ia32 -lmkl_intel -lmkl_core -lmkl_gnu_thread -ldl -lpthread -lm
                # linux-x86-gcc iomp threads:  -Wl,--no-as-needed -L$(MKLROOT)/lib/ia32 -lmkl_intel -lmkl_core -lmkl_intel_thread -liomp5 -ldl -lpthread -lm
                SET(MKL_LIBS mkl_intel mkl_gnu_thread mkl_core iomp5 pthread CACHE FILEPATH "" FORCE)
                SET(MKL_LINKER_FLAG "-Wl,--no-as-needed")    
            ENDIF()
        ENDIF()
    ENDIF(WIN32)
ENDIF(APPLE)

MESSAGE(STATUS "MKL_LINKER_FLAG=${MKL_LINKER_FLAG}")
MESSAGE(STATUS "MKL_LIBS=${MKL_LIBS}")


