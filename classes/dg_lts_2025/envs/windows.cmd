@echo off
:: This file opens a terminal which allows you to compile the code with
:: a 64-bit mingw compiler ( https://mingw-w64.org/ ) 
:: and ninja ( https://ninja-build.org/ )
::
:: HOW TO USE THIS FILE?
::   install cmake from here: https://cmake.org/
::   install MingW-w64 from here: 
::       https://github.com/Vuniverse0/mingwInstaller/releases/download/1.2.0/mingwInstaller.exe
::       (use the default installation path!)
::   [run this file]
::   envs\windows.cmd
::   mkdir src\build
::   cd src\build
::   cmake ..
::   make
::
:: How to clean the "build" folder using cmd line?
::   cd build
::   rd /q /s .
::   [be careful, the last command will remove all files in the current folder!]
::
:: note: this script has been tested with folders containing spaces and accented 
::       characters in their names.
:: -----------------------------------------------------------------------------
::
:: Location of mingw compiler
:: => change this variable if you have installed it in a different folder
:: => or add it to you PATH
set COMPILERPATH=%USERPROFILE%\mingw64\bin
:: -----------------------------------------------------------------------------

echo.
powershell -command "& {Write-Host 'Setting math0471 environment for Windows...' -ForegroundColor DarkGreen}"
:: Note: we start from an *EMPTY PATH* in order to avoid conflicts later
::  (msys2, other gcc such as strawberry perl's, etc.)
::  we do not modify the PATH directly because the "where" command would not be 
::  found later.
set NEWPATH=

:: we also clear INCLUDE, LIB, and PYTHONPATH
set INCLUDE=
set LIB=
set PYTHONPATH=


:: Check  if the folder specified exists. If yes, we use it
IF NOT EXIST "%COMPILERPATH%" (
    powershell -command "& {Write-Host '  NOTE: compiler NOT found in %COMPILERPATH%!' -ForegroundColor DarkYellow}"
    ECHO    Be aware that using a different compiler could lead to problems.
    ECHO    Looking for a gcc compiler in the PATH...
    :: unset COMPILERPATH so that we can find another g++ in the PATH
    set COMPILERPATH=
)

:: Find the path of another g++ from the folders specified in the PATH
FOR /F "tokens=* USEBACKQ" %%F IN (`cmd /C "where g++ 2>nul"`) DO ( SET GCCPATH=%%F )
FOR %%I IN ("%GCCPATH%") DO SET "GCCFOLDER=%%~dpI"
:: we will use this gcc
IF NOT DEFINED COMPILERPATH (
    set COMPILERPATH=%GCCFOLDER%
)
set GCCPATH=
set GCCFOLDER=

:: No compiler found => display error and exit
IF NOT DEFINED COMPILERPATH (
    powershell -command "& {Write-Host '  ERROR: g++ compiler NOT found in the PATH!' -ForegroundColor Red}"
    ECHO    please install mingw-w64 from here:
    ECHO    https://github.com/Vuniverse0/mingwInstaller/releases/download/1.2.0/mingwInstaller.exe
    ECHO    ^(and keep the default installation folder, or add it to your PATH!^)
    PAUSE
    EXIT /B
)

echo   COMPILERPATH     = %COMPILERPATH%

:: Start building a new clean PATH containing only the folders required for
:: the project
set "NEWPATH=%COMPILERPATH%"

:: We use Ninja because it handles better UTF-8 characters in USERPROFILE
:: ninja.exe binary is assumed to be pushed in the ninja folder! 
:: The folder also contains aliases for cmake and make.
set MAKESYSTEM=%~dp0ninja
:: (uncomment the following line if you want to use classical Makefiles)
:: set MAKESYSTEM=%~dp0make
set "NEWPATH=%MAKESYSTEM%;%NEWPATH%"

:: Add the path of the tools (e.g. envs\bin\tee.exe) to the PATH
set "NEWPATH=%~dp0bin;%NEWPATH%"

:: ** Look for required utilities in the PATH **

:: bitsadmin is used to download libraries
FOR /F "tokens=* USEBACKQ" %%F IN (`cmd /C "where bitsadmin 2>nul"`) DO ( SET BITSADMINPATH=%%F )
ECHO   BITSADMINPATH    = %BITSADMINPATH%
FOR %%I IN ("%BITSADMINPATH%") DO SET "BITSADMINFOLDER=%%~dpI"
::ECHO   BITSADMINFOLDER  = %BITSADMINFOLDER%
set "NEWPATH=%NEWPATH%;%BITSADMINFOLDER%"
set BITSADMINPATH=
set BITSADMINFOLDER=

:: powershell is used to unzip archives such as zlib srcs
FOR /F "tokens=* USEBACKQ" %%F IN (`cmd /C "where powershell 2>nul"`) DO ( SET POWERSHELLPATH=%%F )
ECHO   POWERSHELLPATH   = %POWERSHELLPATH%
FOR %%I IN ("%POWERSHELLPATH%") DO SET "POWERSHELLFOLDER=%%~dpI"
::ECHO   POWERSHELLFOLDER = %POWERSHELLFOLDER%
set "NEWPATH=%NEWPATH%;%POWERSHELLFOLDER%"
set POWERSHELLPATH=
set POWERSHELLFOLDER=

:: cmake is used to build your project
FOR /F "tokens=* USEBACKQ" %%F IN (`cmd /C "where cmake 2>nul"`) DO ( SET CMAKEPATH=%%F )
ECHO   CMAKEPATH        = %CMAKEPATH%
IF NOT DEFINED CMAKEPATH (
    powershell -command "& {Write-Host '  WARNING: CMake NOT found in the PATH!' -ForegroundColor DarkYellow}"
)
FOR %%I IN ("%CMAKEPATH%") DO SET "CMAKEFOLDER=%%~dpI"
::ECHO   CMAKEFOLDER      = %CMAKEFOLDER%
set "NEWPATH=%NEWPATH%;%CMAKEFOLDER%"
set CMAKEPATH=
set CMAKEFOLDER=

:: it is convenient (but not required) to have git in the PATH of the console
FOR /F "tokens=* USEBACKQ" %%F IN (`cmd /C "where git 2>nul"`) DO ( SET GITPATH=%%F )
ECHO   GITPATH          = %GITPATH%
IF NOT DEFINED GITPATH (
    powershell -command "& {Write-Host '  WARNING: git NOT found in the PATH!' -ForegroundColor DarkYellow}"
)
FOR %%I IN ("%GITPATH%") DO SET "GITFOLDER=%%~dpI"
::ECHO   GITFOLDER        = %GITFOLDER%
set "NEWPATH=%NEWPATH%;%GITFOLDER%"
set GITPATH=
set GITFOLDER=

:: set the location of gmsh (it could be absent for the first run)
set GMSHFOLDER=%~dp0..\lib\gmsh-sdk
IF NOT EXIST "%GMSHFOLDER%" (
    powershell -command "& {Write-Host '  WARNING: gmsh NOT found in %GMSHFOLDER%!' -ForegroundColor DarkYellow}"
    ECHO      do not forget to install it by running the following commands:
    ECHO      cd ..\lib ^&^& get_gmsh.cmd"
)
set "NEWPATH=%GMSHFOLDER%\bin;%GMSHFOLDER%\lib;%NEWPATH%"
set "INCLUDE=%GMSHFOLDER%\include;%INCLUDE%"
set "LIB=%GMSHFOLDER%\lib;%LIB%"
set "PYTHONPATH=%GMSHFOLDER%\lib;%PYTHONPATH%"
set GMSHFOLDER=

:: set the location of eigen (it could be absent for the first run)
set EIGENFOLDER=%~dp0..\lib\eigen
IF NOT EXIST "%EIGENFOLDER%" (
    powershell -command "& {Write-Host '  WARNING: eigen NOT found in %EIGENFOLDER%!' -ForegroundColor DarkYellow}"
    ECHO      do not forget to install it by running the following commands:
    ECHO      cd ..\lib ^&^& get_eigen.cmd
)
set "INCLUDE=%EIGENFOLDER%;%INCLUDE%"
set EIGENFOLDER=

:: set the PATH
set "PATH=%NEWPATH%"
set NEWPATH=

:: remove extra ';' if it exists
if "%INCLUDE:~-1%"==";" set "INCLUDE=%INCLUDE:~0,-1%"
if "%LIB:~-1%"==";" set "LIB=%LIB:~0,-1%"
if "%PYTHONPATH:~-1%"==";" set "PYTHONPATH=%PYTHONPATH:~0,-1%"
if "%PATH:~-1%"==";" set "PATH=%PATH:~0,-1%"

echo.
powershell -command "& {Write-Host 'Environment:' -ForegroundColor DarkGreen}"

:: pretty-print the path with a line break after each ';' character
echo   PATH    =
FOR %%A IN ("%PATH:;=";"%") DO (
    echo      %%~A
)

:: pretty-print the INCLUDE variable
echo   INCLUDE =
FOR %%A IN ("%INCLUDE:;=";"%") DO (
    echo      %%~A
)

:: pretty-print the LIB variable
echo   LIB     =
FOR %%A IN ("%LIB:;=";"%") DO (
    echo      %%~A
)

:: "/B" is used to exit the script without closing the terminal
:: (same behaviour as "source" in bash or "call" in cmd)
EXIT /B
