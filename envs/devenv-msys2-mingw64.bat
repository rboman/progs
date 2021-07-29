@echo off

echo setting msys64\mingw64 environment

set INCLUDE=
::set INCLUDE=%INCLUDE%;C:\local\MUMPS\include

set LIB=
::set LIB=c:\local\lib

set OMP_NUM_THREADS=1

set PATH=C:\msys64\mingw64\bin;%PATH%
set PATH=%PATH%;C:\msys64\usr\bin
:: note: the path to windows cmake should be *before* the one of MSYS! 

set PATH=%PATH%;%LOCALAPPDATA%\Programs\Microsoft VS Code\bin

%comspec%
