@echo off
::    - cmake -G "Visual Studio 11 2012 Win64" ..\waves
::    - cmake --build . --config Release

:: MKL + VS en ligne de commande
call "C:\Program Files (x86)\Intel\Composer XE\mkl\bin\intel64\mklvars_intel64.bat"
call "C:\Program Files (x86)\Intel\Composer XE\tbb\bin\tbbvars.bat" intel64 vs2012
%comspec% /K ""C:\Program Files (x86)\Microsoft Visual Studio 11.0\VC\vcvarsall.bat" amd64"
