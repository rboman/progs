::@echo off
:: download zlib & extract & build
:: https://eigen.tuxfamily.org/

setlocal
set version=1.3.1
set folder=zlib-%version%
set file=%folder%.zip

:: download sources from github
del %file%  >nul 2>&1
bitsadmin /transfer get_zlib /dynamic /download /priority foreground ^
 "https://github.com/madler/zlib/archive/refs/tags/v%version%.zip" ^
 "%CD%\%file%"

:: unzip sources
rd /Q/S %folder%  >nul 2>&1
powershell.exe -nologo -noprofile ^
 -command "& { Add-Type -A 'System.IO.Compression.FileSystem'; [IO.Compression.ZipFile]::ExtractToDirectory('%CD%\%file%', '%CD%'); }"
del %file%  >nul 2>&1

:: build from source
cd %folder%
mkdir build
cd build

:: note:
::  If the current path contains a hyphen (e.g. "OneDrive - ULiege"), the build 
::  will start winres.exe which produces an error:
::      (gcc.exe: error: unrecognized command-line option '-\').
::  The trick here is to replace windres by a no-op program and create an empty 
::  compiled resource file (zlib1rc.obj)
type nul > noop.cmd
cmake.exe -G"Ninja" -DCMAKE_INSTALL_PREFIX=..\..\zlib ^
          -DCMAKE_BUILD_TYPE=Release ^
          -DCMAKE_RC_COMPILER=noop.cmd ^
          -DZLIB_BUILD_EXAMPLES=OFF ..
if %errorlevel% neq 0 exit /b %errorlevel%
type nul > zlib1rc.obj
cmake.exe --build . --target install
if %errorlevel% neq 0 exit /b %errorlevel%
cd ..\..

:: remove src folder
rd /Q/S %folder% >nul 2>&1

:: In windows, cmake's FindZLIB tries to find separate files for debug and release 
:: versions of zlib (it supposes that zlibd.lib is the debug version of zlib.lib 
:: even if MinGW is used). If a MSVC version of zlibd.lib is found in the PATH, it
:: will use it and make gdb crash when debugging. To avoid this, we need to create
:: a fake zlibd.lib file.

copy zlib\lib\libzlib.dll.a zlib\lib\libzlibd.dll.a
copy zlib\lib\libzlibstatic.a zlib\lib\libzlibstaticd.a

:: clear local vars
endlocal
