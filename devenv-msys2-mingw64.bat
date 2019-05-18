@echo off

echo setting msys64\mingw64 environment

set INCLUDE=
::set INCLUDE=%INCLUDE%;C:\local\MUMPS\include

set LIB=
::set LIB=c:\local\lib

set OMP_NUM_THREADS=1

set PATH=C:\msys64\mingw64\bin
::set PATH=%PATH%;C:\MinGW\msys\1.0\bin
set PATH=%PATH%;C:\Program Files\CMake\bin\
set PATH=%PATH%;C:\Program Files\Microsoft MPI\Bin\
set PATH=%PATH%;C:\Program Files\Microsoft VS Code\bin

%comspec%
