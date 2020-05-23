@echo off
:: Visual Studio environment using RB libs

set INCLUDE=%MYLOCAL%\include;%MYLOCAL%\MUMPS\include;%MYLOCAL%\eigen
set LIB=%MYLOCAL%\MUMPS\lib
:: f2py
set PATH=%PATH%;F:\Python37\Scripts

:: olcPixelGameEngine
set "OLCPATH=%~dp0..\externals\olcPixelGameEngine"
IF NOT EXIST "%OLCPATH%" (
    ECHO   - olcPixelGameEngine NOT found in %OLCPATH%!!
) ELSE (
    ECHO   - olcPixelGameEngine found.
    set "INCLUDE=%OLCPATH%;%INCLUDE%"
)

:: swig
set "SWIGPATH=%MYLOCAL%\swig"
IF NOT EXIST "%SWIGPATH%" (
    ECHO    - swig NOT found in %SWIGPATH%!!
) ELSE (
    ECHO   - swig found.
    set "PATH=%PATH%;%MYLOCAL%\swig"
)

:: visual studio [required]
set COMPILERPATH="C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build"
IF NOT EXIST %COMPILERPATH% (
    ECHO   - MSVC compiler NOT found in %COMPILERPATH%!!
    PAUSE
    EXIT /B
) ELSE (
    ECHO   - MSVC compiler found.
)

:: Intel MKL/TBB
set INTELPATH="C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows"
IF NOT EXIST %INTELPATH% (
    ECHO   - INTEL libraries NOT found in %INTELPATH%!!
) ELSE (
    ECHO   - INTEL libraries found.
    call %INTELPATH%\mkl\bin\mklvars.bat intel64 vs2017
    call %INTELPATH%\tbb\bin\tbbvars.bat intel64 vs2017
)

:: run MSVC environment
CD /d "%~dp0"
CD ..
%comspec% /k "%COMPILERPATH%\vcvars64.bat"
