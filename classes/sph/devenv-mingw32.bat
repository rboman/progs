@echo off

:: Environnement:
::     - MinGW32 (http://www.mingw.org/) [32 bits!]
::         !l'install de base oublie zlib-dev et pthread!
::     - Microsoft MPI
::
:: Exemple:
::   mkdir build
::   cd build
::   cmake -G "MinGW Makefiles" ..
::   mingw32-make -j 6
::   mpiexec -n 6 bin\fdtd_mpi

echo setting MinGW32 environment

set INCLUDE=
::set INCLUDE=%INCLUDE%;C:\local\MUMPS\include

set LIB=
::set LIB=c:\local\lib

set OMP_NUM_THREADS=1

set PATH=c:\MinGW\bin
::set PATH=%PATH%;C:\MinGW\msys\1.0\bin
set PATH=%PATH%;C:\Program Files\CMake\bin\
set PATH=%PATH%;C:\Program Files\Microsoft MPI\Bin\

%comspec%
