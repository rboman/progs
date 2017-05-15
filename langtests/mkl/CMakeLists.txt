PROJECT(TestDAXPY Fortran C)
CMAKE_MINIMUM_REQUIRED(VERSION 2.6)

# configure Fortran compiler
GET_FILENAME_COMPONENT(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

IF (Fortran_COMPILER_NAME MATCHES "f95")
    SET(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall") 
ENDIF()

MESSAGE(STATUS "Fortran_COMPILER_NAME        : " ${Fortran_COMPILER_NAME})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS          : " ${CMAKE_Fortran_FLAGS})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS_RELEASE  : " ${CMAKE_Fortran_FLAGS_RELEASE})
MESSAGE(STATUS "CMAKE_Fortran_FLAGS_DEBUG    : " ${CMAKE_Fortran_FLAGS_DEBUG})

# blas

# ifort...
#SET(LAPACK_LIBRARIES "mkl_intel_lp64_dll.lib mkl_intel_thread_dll.lib mkl_core_dll.lib libiomp5md.lib")

# mingw (x86)...
SET(LAPACK_LIBRARIES "C:/Program Files (x86)/IntelSWTools/compilers_and_libraries_2017/windows/mkl/lib/ia32_win/mkl_intel_c_dll.lib" 
"C:/Program Files (x86)/IntelSWTools/compilers_and_libraries_2017/windows/mkl/lib/ia32_win/mkl_intel_thread_dll.lib"
#"C:/Program Files (x86)/IntelSWTools/compilers_and_libraries_2017/windows/mkl/lib/ia32_win/mkl_sequential_dll.lib"
"C:/Program Files (x86)/IntelSWTools/compilers_and_libraries_2017/windows/mkl/lib/ia32_win/mkl_core_dll.lib"
"C:/Program Files (x86)/IntelSWTools/compilers_and_libraries_2017.2.187/windows/compiler/lib/ia32_win/libiomp5md.lib"


#"C:/Program Files (x86)/Windows Kits/8.1/Lib/winv6.3/um/x86/bufferoverflow.lib"
#"C:/Program Files (x86)/Windows Kits/8.1/Lib/winv6.3/um/x86/uuid.lib"
)

file(GLOB SRCS *.f90 *.c)      # .f90/.f95 => free format | .f/.for => fixed
ADD_EXECUTABLE(testdaxpy ${SRCS}) 
TARGET_LINK_LIBRARIES(testdaxpy ${LAPACK_LIBRARIES})