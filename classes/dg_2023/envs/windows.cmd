@echo off
:: This file opens a terminal which allows you to compile the code with
:: a 64-bit mingw compiler
::     https://mingw-w64.org/
::     https://www.msys2.org/
::
:: HOW TO USE THIS FILE?
::   [check the PATHs below]
::   [run this file]
::   mkdir build
::   cd build
::   cmake ..
::   make
::   ctest
::
:: How to clean the "build" folder using cmd line?
::   cd build
::   rd /q /s .

echo setting MinGW64 environment...

:: set the location of gmsh SDK / mingw compiler
set GMSHSDK=%~dp0..\lib\gmsh-sdk
set EIGEN=%~dp0..\lib\eigen
set COMPILERPATH=%USERPROFILE%\mingw64\bin
:: use ninja (UTF-8 chars in USERPROFILE => OK)
set MAKESYSTEM=%~dp0ninja
:: use mingw32-make (UTF-8 chars in USERPROFILE => KO)
:: (uncomment the following line if you want to use classical Makefiles)
::set MAKESYSTEM=%~dp0make

:: perform some tests...
IF NOT EXIST "%GMSHSDK%" (
    ECHO   - gmsh-sdk NOT found in %GMSHSDK%!!
    @REM PAUSE
    @REM EXIT /B
) ELSE (
    ECHO   - gmsh-sdk found in %GMSHSDK%.
)

IF NOT EXIST "%EIGEN%" (
    ECHO   - eigen NOT found in %EIGEN%!!
    @REM PAUSE
    @REM EXIT /B
) ELSE (
    ECHO   - eigen found in %EIGEN%.
)

:: Look for MinGW.
IF NOT EXIST "%COMPILERPATH%" (
    ECHO   - compiler NOT found in %COMPILERPATH%!!
    @REM PAUSE
    @REM EXIT /B
) ELSE (
    ECHO   - compiler found in %COMPILERPATH%.
    set "PATH=%COMPILERPATH%;%PATH%"
)

:: where is gmsh.exe and gmsh-**.dll ?
set PATH=%GMSHSDK%\bin;%GMSHSDK%\lib;%PATH%
:: where is gmsh.h ?
set INCLUDE=%EIGEN%;%GMSHSDK%\include;%INCLUDE%
:: where is gmsh.lib ?
set LIB=%GMSHSDK%\lib;%LIB%
:: where is gmsh.py ? (required only if you want to use the python API)
set PYTHONPATH=%GMSHSDK%\lib;%PYTHONPATH%

:: add current folder to PATH for cmake/make aliases
set PATH=%MAKESYSTEM%;%PATH%

:: clear local vars
set GMSHSDK=
set EIGEN=
set COMPILERPATH=
set MAKESYSTEM=

:: open terminal
CD /d "%~dp0"
CD ..
%comspec% /K
