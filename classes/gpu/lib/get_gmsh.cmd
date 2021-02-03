::@echo off
:: download gmsh-sdk & extract to current folder

set gmsh=gmsh-4.7.1-Windows64-sdk
set file=%gmsh%.zip

:: download file
del %file%  >nul 2>&1
bitsadmin /transfer get_gmsh /dynamic /download /priority foreground "http://gmsh.info/bin/Windows/%file%" "%CD%\%file%"

:: unzip
rd /Q/S %gmsh%  >nul 2>&1
powershell.exe -nologo -noprofile -command "& { Add-Type -A 'System.IO.Compression.FileSystem'; [IO.Compression.ZipFile]::ExtractToDirectory('%CD%\%file%', '%CD%'); }"
del %file%  >nul 2>&1

:: rename folder
rd /Q/S gmsh-sdk >nul 2>&1
ren %gmsh% gmsh-sdk

:: copy dll to bin folder so that double-clic on gmsh.exe works!
copy /Y gmsh-sdk\lib\gmsh-4.7.dll gmsh-sdk\bin\
