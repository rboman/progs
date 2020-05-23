CMAKE_MINIMUM_REQUIRED(VERSION 3.1)
PROJECT(TestSCALAPACK Fortran)

# configure Fortran compiler
GET_FILENAME_COMPONENT(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

IF(Fortran_COMPILER_NAME MATCHES "f95")
    SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall") 
ENDIF()

MESSAGE(STATUS "Fortran_COMPILER_NAME        : " ${Fortran_COMPILER_NAME})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS          : " ${CMAKE_Fortran_FLAGS})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS_RELEASE  : " ${CMAKE_Fortran_FLAGS_RELEASE})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS_DEBUG    : " ${CMAKE_Fortran_FLAGS_DEBUG})

# blas

# ifort...
#SET(LAPACK_LIBRARIES "mkl_intel_lp64_dll.lib mkl_intel_thread_dll.lib mkl_core_dll.lib libiomp5md.lib")

IF("${CMAKE_SIZEOF_VOID_P}" EQUAL "8")
    MESSAGE(STATUS "Target is 64 bits")
    SET(LAPACK_LIBRARIES 
    "C:/Program Files (x86)/IntelSWTools/compilers_and_libraries_2017/windows/mkl/lib/intel64/mkl_scalapack_lp64_dll.lib"
    "C:/Program Files (x86)/IntelSWTools/compilers_and_libraries_2017/windows/mkl/lib/intel64/mkl_blacs_lp64_dll.lib"
    "C:/Program Files (x86)/IntelSWTools/compilers_and_libraries_2017/windows/mkl/lib/intel64/mkl_intel_lp64_dll.lib"
    "C:/Program Files (x86)/IntelSWTools/compilers_and_libraries_2017/windows/mkl/lib/intel64/mkl_core_dll.lib"
    "C:/Program Files (x86)/IntelSWTools/compilers_and_libraries_2017/windows/mkl/lib/intel64/mkl_intel_thread_dll.lib"  
    "C:/Program Files (x86)/IntelSWTools/compilers_and_libraries_2017/windows/compiler/lib/intel64/libiomp5md.lib"
    )
ELSE()
    # mingw (x86)... // works!
    MESSAGE(STATUS "Target is 32 bits")
    SET(LAPACK_LIBRARIES 
    "C:/Program Files (x86)/IntelSWTools/compilers_and_libraries_2017/windows/mkl/lib/ia32_win/mkl_intel_c_dll.lib" 
    "C:/Program Files (x86)/IntelSWTools/compilers_and_libraries_2017/windows/mkl/lib/ia32_win/mkl_intel_thread_dll.lib"
    "C:/Program Files (x86)/IntelSWTools/compilers_and_libraries_2017/windows/mkl/lib/ia32_win/mkl_core_dll.lib"
    "C:/Program Files (x86)/IntelSWTools/compilers_and_libraries_2017/windows/compiler/lib/ia32_win/libiomp5md.lib"
    )
ENDIF()


FILE(GLOB SRCS *.f )      # .f90/.f95 => free format | .f/.for => fixed
ADD_EXECUTABLE(testscalapack ${SRCS}) 
TARGET_LINK_LIBRARIES(testscalapack ${LAPACK_LIBRARIES})