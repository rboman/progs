@echo off
:: This file opens a terminal which allows you to compile the code with
:: Microsoft Visual Studio 2017 (x64) on Windows
::
:: HOW TO USE THIS FILE?
::   [run this file]
::   mkdir build
::   cd build
::   cmake -A x64 ..
::   cmake --build . --config Release
::   [executables are built in the bin/Release folder]
::   ctest -C Release

:: set the location of gmsh SDK / mingw compiler
set "GMSHSDK=%~dp0..\gmsh-sdk"
set "EIGEN=%~dp0..\eigen"
set COMPILERPATH="C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build"

:: perform some tests...
IF NOT EXIST "%GMSHSDK%" (
    ECHO   - gmsh-sdk NOT found in %GMSHSDK%!!
    PAUSE
    EXIT /B
) ELSE (
    ECHO   - gmsh-sdk found.
)

IF NOT EXIST "%EIGEN%" (
    ECHO   - eigen NOT found in %EIGEN%!!
    PAUSE
    EXIT /B
) ELSE (
    ECHO   - eigen found.
)

IF NOT EXIST %COMPILERPATH% (
    ECHO   - compiler NOT found in %COMPILERPATH%!!
    PAUSE
    EXIT /B
) ELSE (
    ECHO   - compiler found.
)

:: where is gmsh.exe and gmsh-**.dll ?
set PATH=%GMSHSDK%\bin;%GMSHSDK%\lib;%PATH%
:: where is gmsh.h ?
set INCLUDE=%EIGEN%;%GMSHSDK%\include;%INCLUDE%
:: where is gmsh.lib ?
set LIB=%GMSHSDK%\lib;%LIB%
:: where is gmsh.py ? (required only if you want to use the python API)
set PYTHONPATH=%GMSHSDK%\lib;%PYTHONPATH%

:: set the environment of the msvc compiler
CD /d "%~dp0"
CD ..
%comspec% /k "%COMPILERPATH%\vcvars64.bat"
