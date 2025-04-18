@echo off

:: build the code on Windows10/11 with MSYS2 UCRT64 (or MINGW64)
::   in a DOS terminal with the cmake from windows
::
:: run this script, then 
::   cd ..
::   mkdir build
::   cmake -G "MSYS Makefiles" ..
::   cmake --build . --target install -- -j 6
::
:: another option would be to run an MSYS2 shell an use
:: the cmake from msys with no argument 
:: (there is no "MSYS Makefiles" here, the default="Unix Makefiles")

echo setting msys64\mingw64 environment in a DOS terminal

@REM set MSYS_FOLDER=C:\msys64
@REM set MSYS_TYPE=ucrt64

:: marc
set MSYS_FOLDER=C:\msys64_2024
set MSYS_TYPE=mingw64

set INCLUDE=

set LIB=

set OMP_NUM_THREADS=1

:: ** do not include %PATH% otherwise Qwt is found from c:\local!
:: set PATH=C:\msys64\mingw64\bin;%PATH% 
:: ** MINGW version
:: set PATH=C:\msys64\mingw64\bin
:: ** UCRT version
set PATH=%MSYS_FOLDER%\%MSYS_TYPE%\bin
set PATH=%PATH%;%MSYS_FOLDER%\usr\bin
set "PATH=%PATH%;%USERPROFILE%\AppData\Local\Programs\Microsoft VS Code\bin"
set PATH=%PATH%;c:\windows\System32
:: xmesher required for LAM3... 
set PATH=%PATH%;c:\local\bin

:: note: the path of windows cmake should be *before* the one of MSYS! 

:: problem with "stdin" 
:: 	( see https://lam3.org/gitlab/lam3/lam3/issues/25 )
set GFORTRAN_UNBUFFERED_PRECONNECTED=y

:: open a *DOS* command prompt
%comspec%



:: MSYS2 packages that should be installed to be run with msys/mingw/python
::   (msys/mingw/python is found by cmake and thus used by ctest)

:: pacman -S mingw64/mingw-w64-x86_64-cmake
:: pacman -S mingw64/mingw-w64-x86_64-python-numpy
:: pacman -S mingw64/mingw-w64-x86_64-python-matplotlib
:: pacman -S mingw64/mingw-w64-x86_64-vtk (installs a lot of packages!)

:: do not use msys/python!