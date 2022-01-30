::@echo off
:: download eigen & extract to current folder

set version=3.4.0
set file=eigen-%version%.zip

:: download file
del %file%  >nul 2>&1
bitsadmin /transfer get_eigen /dynamic /download /priority foreground "https://gitlab.com/libeigen/eigen/-/archive/%version%/%file%" "%CD%\%file%"

:: unzip
rd /Q/S eigen-%version%  >nul 2>&1
powershell.exe -nologo -noprofile -command "& { Add-Type -A 'System.IO.Compression.FileSystem'; [IO.Compression.ZipFile]::ExtractToDirectory('%CD%\%file%', '%CD%'); }"
del %file%  >nul 2>&1

:: rename folder
rd /Q/S eigen >nul 2>&1
ren eigen-%version% eigen
