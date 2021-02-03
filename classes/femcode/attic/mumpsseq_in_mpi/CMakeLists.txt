# GMM / MUMPS sequentiel mais code MPI

cmake_minimum_required(VERSION 2.7)

project(TestMUMPS CXX)

if(WIN32)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS -D_SCL_SECURE_NO_DEPRECATE)
endif(WIN32)

# mumps/sequentiel
add_definitions(-DGMM_USES_MUMPS)  
#add_definitions(-DGMM_USES_MPI) # GMM sequentiel mais code MPI 

include_directories(${PROJECT_SOURCE_DIR})

# Require MPI for this project
if(WIN32)
  #This is for finding MS-MPI.
  #set(_MPI_PREFIX_PATH)
  #list(APPEND _MPI_PREFIX_PATH "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\MPI;Path]/..")
  set(MPIEXEC "C:/Program Files/Microsoft MPI/Bin/mpiexec.exe")
  # For building MPI programs the selected Visual Studio compiler is used, namely cl.exe.
  # So there is no need to set a specific MPI compiler.
  #set(MPI_CXX_COMPILER "${CMAKE_CXX_COMPILER}")
  set(MPI_CXX_INCLUDE_PATH "C:/Program Files (x86)/Microsoft SDKs/MPI/Include")
  # Make sure the correct libraries (64-bit or 32-bit) are selected.
  # Decide between 32-bit and 64-bit libraries for Microsoft's MPI
  if("${CMAKE_SIZEOF_VOID_P}" EQUAL 8)
    set(MS_MPI_ARCH_DIR x64)
  else()
    set(MS_MPI_ARCH_DIR x86)
  endif()
  set(MPI_CXX_LIBRARIES "C:/Program Files (x86)/Microsoft SDKs/MPI/Lib/${MS_MPI_ARCH_DIR}/msmpi.lib")
  set(MPI_INCLUDE_PATH "${MPI_CXX_INCLUDE_PATH}")
  set(MPI_LIBRARIES "${MPI_CXX_LIBRARIES}")
  set(MPIEXEC_NUMPROC_FLAG "-np" CACHE STRING "Flag used by MPI to specify the number of processes for MPIEXEC; the next option will be the number of processes.")
else()
    find_package(MPI REQUIRED)
endif()

include_directories(${MPI_INCLUDE_PATH})

if(WIN32)
    include_directories("$ENV{MYLOCAL}/WinMumps-4.10/MUMPS_4.10.0/include")
endif(WIN32)

add_executable(testMUMPS testMUMPS.cpp)

TARGET_LINK_LIBRARIES(testMUMPS ${MPI_LIBRARIES})  # ! l'ordre à de l'importance sous win!! (sinon on link tout avec mpiseq de mumps!!)

if(WIN32)
	TARGET_LINK_LIBRARIES(testMUMPS "$ENV{MYLOCAL}/WinMumps-4.10/MUMPS_4.10.0/lib/Release/x64/dmumps_c.lib")
	TARGET_LINK_LIBRARIES(testMUMPS "$ENV{MYLOCAL}/WinMumps-4.10/MUMPS_4.10.0/lib/Release/x64/dmumps_fortran.lib")
	TARGET_LINK_LIBRARIES(testMUMPS "$ENV{MYLOCAL}/WinMumps-4.10/MUMPS_4.10.0/lib/Release/x64/mumps_common_c.lib")
	TARGET_LINK_LIBRARIES(testMUMPS "$ENV{MYLOCAL}/WinMumps-4.10/MUMPS_4.10.0/lib/Release/x64/pord_c.lib")
	TARGET_LINK_LIBRARIES(testMUMPS "$ENV{MYLOCAL}/WinMumps-4.10/MUMPS_4.10.0/lib/Release/x64/libseq_c.lib")
	TARGET_LINK_LIBRARIES(testMUMPS "$ENV{MYLOCAL}/WinMumps-4.10/MUMPS_4.10.0/lib/Release/x64/libseq_fortran.lib")
	TARGET_LINK_LIBRARIES(testMUMPS "C:/Program Files (x86)/Intel/Composer XE 2013 SP1/compiler/lib/intel64/ifconsol.lib")
	TARGET_LINK_LIBRARIES(testMUMPS "C:/Program Files (x86)/Intel/Composer XE 2013 SP1/compiler/lib/intel64/libifcoremd.lib")
	TARGET_LINK_LIBRARIES(testMUMPS "C:/Program Files (x86)/Intel/Composer XE 2013 SP1/compiler/lib/intel64/libifportmd.lib")
	TARGET_LINK_LIBRARIES(testMUMPS "C:/Program Files (x86)/Intel/Composer XE 2013 SP1/compiler/lib/intel64/libmmd.lib")
	TARGET_LINK_LIBRARIES(testMUMPS "C:/Program Files (x86)/Intel/Composer XE 2013 SP1/compiler/lib/intel64/libirc.lib")
	TARGET_LINK_LIBRARIES(testMUMPS "C:/Program Files (x86)/Intel/Composer XE 2013 SP1/compiler/lib/intel64/svml_dispmd.lib")
	
	TARGET_LINK_LIBRARIES(testMUMPS "C:/Program Files (x86)/Intel/Composer XE 2013 SP1/mkl/lib/intel64/mkl_intel_lp64_dll.lib")
	TARGET_LINK_LIBRARIES(testMUMPS "C:/Program Files (x86)/Intel/Composer XE 2013 SP1/mkl/lib/intel64/mkl_intel_thread_dll.lib")
	TARGET_LINK_LIBRARIES(testMUMPS "C:/Program Files (x86)/Intel/Composer XE 2013 SP1/mkl/lib/intel64/mkl_core_dll.lib")
	TARGET_LINK_LIBRARIES(testMUMPS "C:/Program Files (x86)/Intel/Composer XE 2013 SP1/compiler/lib/intel64/libiomp5md.lib")
else(WIN32)
    TARGET_LINK_LIBRARIES(testMUMPS dmumps_seq)  # mumps/sequentiel
endif(WIN32)


