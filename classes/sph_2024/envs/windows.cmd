@echo off
:: This file opens a terminal which allows you to compile the code with
:: a 64-bit mingw compiler
::     https://mingw-w64.org/
::
:: HOW TO USE THIS FILE?
::   install cmake from here: https://cmake.org/
::   install MingW-w64 from here: 
::       https://github.com/Vuniverse0/mingwInstaller/releases/download/1.2.0/mingwInstaller.exe
::       (use the default installation path!)
::   [run this file]
::   mkdir src/build
::   cd build
::   cmake ..
::   make
::
:: How to clean the "build" folder using cmd line?
::   cd build
::   rd /q /s .

echo Setting math0471 environment for Windows
echo ----------------------------------------
:: Note: we start from an *EMPTY PATH* in order to avoid conflicts later
::  (msys2, other gcc such as strawberry perl's, etc.)
::  we do not modify the PATH directly because the "where" command would not be 
::  found later.
set NEWPATH=

:: set the location of mingw compiler
set COMPILERPATH=%USERPROFILE%\mingw64\bin
IF NOT EXIST "%COMPILERPATH%" (
    ECHO ERROR: compiler NOT found in %COMPILERPATH%!!
    ECHO    please install mingw-w64 from here:
    ECHO    https://github.com/Vuniverse0/mingwInstaller/releases/download/1.2.0/mingwInstaller.exe
    ECHO    and keep the default installation path!
    PAUSE
    EXIT /B
)
set "NEWPATH=%COMPILERPATH%"

:: We use Ninja because it handles better UTF-8 characters in USERPROFILE
:: ninja.exe binary is assumed to be pushed in the ninja folder! 
:: The folder also contains aliases for cmake and make.
set MAKESYSTEM=%~dp0ninja
:: (uncomment the following line if you want to use classical Makefiles)
:: set MAKESYSTEM=%~dp0make
set "NEWPATH=%MAKESYSTEM%;%NEWPATH%"

:: ** Look for required utilities in the PATH **

:: bitsadmin is used to download zlib srcs
FOR /F "tokens=* USEBACKQ" %%F IN (`where bitsadmin`) DO ( SET BITSADMINPATH=%%F )
ECHO   BITSADMINPATH    = %BITSADMINPATH%
FOR %%I IN ("%BITSADMINPATH%") DO SET "BITSADMINFOLDER=%%~dpI"
ECHO   BITSADMINFOLDER  = %BITSADMINFOLDER%
set "NEWPATH=%NEWPATH%;%BITSADMINFOLDER%"
set BITSADMINPATH=
set BITSADMINFOLDER=

:: powershell is used to unzip archives such as zlib srcs
FOR /F "tokens=* USEBACKQ" %%F IN (`where powershell`) DO ( SET POWERSHELLPATH=%%F )
ECHO   POWERSHELLPATH   = %POWERSHELLPATH%
FOR %%I IN ("%POWERSHELLPATH%") DO SET "POWERSHELLFOLDER=%%~dpI"
ECHO   POWERSHELLFOLDER = %POWERSHELLFOLDER%
set "NEWPATH=%NEWPATH%;%POWERSHELLFOLDER%"
set POWERSHELLPATH=
set POWERSHELLFOLDER=

:: cmake is used to build zlib and your project
FOR /F "tokens=* USEBACKQ" %%F IN (`where cmake`) DO ( SET CMAKEPATH=%%F )
ECHO   CMAKEPATH        = %CMAKEPATH%
FOR %%I IN ("%CMAKEPATH%") DO SET "CMAKEFOLDER=%%~dpI"
ECHO   CMAKEFOLDER      = %CMAKEFOLDER%
set "NEWPATH=%NEWPATH%;%CMAKEFOLDER%"
set CMAKEPATH=
set CMAKEFOLDER=

:: it is convenient (but not required) to have git in the PATH of the console
FOR /F "tokens=* USEBACKQ" %%F IN (`where git`) DO ( SET GITPATH=%%F )
ECHO   GITPATH          = %GITPATH%
FOR %%I IN ("%GITPATH%") DO SET "GITFOLDER=%%~dpI"
ECHO   GITFOLDER        = %GITFOLDER%
set "NEWPATH=%NEWPATH%;%GITFOLDER%"
set GITPATH=
set GITFOLDER=

:: set the location of zlib (it could be absent for the first run)

set ZLIBFOLDER=%~dp0..\lib\zlib
IF NOT EXIST "%ZLIBFOLDER%" (
    ECHO WARNING: zlib NOT found in %ZLIBFOLDER%!
    ECHO    do not forget to install it by running the following commands:
    ECHO    cd ..\lib && get_zlib.cmd
) 
set "NEWPATH=%ZLIBFOLDER%\bin;%NEWPATH%"
set INCLUDE=%ZLIBFOLDER%\include
set LIB=%ZLIBFOLDER%\lib
set ZLIBFOLDER=

:: set the PATH
set "PATH=%NEWPATH%"
set NEWPATH=

:: display summary

echo.
echo   PATH    = %PATH%
echo   INCLUDE = %INCLUDE%
echo   LIB     = %LIB%
echo.

:: open a sub-terminal
CD /d "%~dp0"
CD ..
%comspec% /K
