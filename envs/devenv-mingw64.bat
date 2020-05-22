@echo off

:: Environnement:
::     - MinGW-w64 (http://www.mingw.org/) [64 bits!]
::         !pas de zlib dans l'install!
::     - Microsoft MPI
::
:: Compiler d'abord zlib
::   cmake -G "MinGW Makefiles" -DCMAKE_INSTALL_PREFIX=c:/mingw-w64/mingw64 ..
::   mingw32-make -j 6 install
::
:: Exemple:
::   mkdir build
::   cd build
::   cmake -G "MinGW Makefiles" ..
::   mingw32-make -j 6
::   mpiexec -n 6 bin\fdtd_mpi

echo setting MinGW-w64 environment

set INCLUDE=
::set INCLUDE=%INCLUDE%;C:\local\MUMPS\include

set LIB=
::set LIB=c:\local\lib

set OMP_NUM_THREADS=1

set PATH=C:\mingw-w64\mingw64\bin
::set PATH=%PATH%;C:\MinGW\msys\1.0\bin
set PATH=%PATH%;C:\Program Files\CMake\bin\
set PATH=%PATH%;C:\Program Files\Microsoft MPI\Bin\

call "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\mkl\bin\mklvars.bat" intel64 vs2015

%comspec%
