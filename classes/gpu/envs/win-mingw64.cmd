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
set GMSHSDK=%~dp0..\gmsh-sdk
set EIGEN=%~dp0..\eigen
set COMPILERPATHS=C:\msys64\mingw64\bin;C:\mingw-w64\mingw64\bin

:: perform some tests...
IF NOT EXIST %GMSHSDK% (
    ECHO   - gmsh-sdk NOT found in %GMSHSDK%!!
    PAUSE
    EXIT /B
) ELSE (
    ECHO   - gmsh-sdk found in %GMSHSDK%.
)

IF NOT EXIST %EIGEN% (
    ECHO   - eigen NOT found in %EIGEN%!!
    PAUSE
    EXIT /B
) ELSE (
    ECHO   - eigen found in %EIGEN%.
)

FOR %%a IN (%COMPILERPATHS%) DO (
    IF EXIST %%a (
        ECHO   - compiler found in %%a.
        SET COMPILERPATH=%%a
    ) ELSE (
        IF NOT DEFINED COMPILERPATH (
            ECHO   - compiler not found in %%a.
        )
    )
)
IF NOT DEFINED COMPILERPATH (
    ECHO   =^> compiler NOT found!
    PAUSE
    EXIT /B 
)

:: where is gmsh.exe and gmsh-**.dll ?
set PATH=%GMSHSDK%\bin;%GMSHSDK%\lib;%PATH%
:: where is gmsh.h ?
set INCLUDE=%EIGEN%;%GMSHSDK%\include;%INCLUDE%
:: where is gmsh.lib ?
set LIB=%GMSHSDK%\lib;%LIB%
:: where is gmsh.py ? (required only if you want to use the python API)
set PYTHONPATH=%GMSHSDK%\lib;%PYTHONPATH%
:: add path to the compiler to the PATH
set PATH=%COMPILERPATH%;%PATH%
:: add current folder to PATH for cmake/make aliases
set PATH=%~dp0;%PATH%

:: clear local vars
set GMSHSDK=
set EIGEN=
set COMPILERPATHS=
set COMPILERPATH=

:: open terminal
CD /d "%~dp0"
CD ..
%comspec% /K
