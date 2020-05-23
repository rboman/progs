@echo off

set PATH=%PATH%;%MYLOCAL%\swigwin-3.0.12
set INCLUDE=%MYLOCAL%\include;%MYLOCAL%\MUMPS\include;%MYLOCAL%\eigen-3.3.2
set LIB=%MYLOCAL%\MUMPS\lib

:: ok (MKL + VS en ligne de commande)

call "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\mkl\bin\mklvars.bat" intel64 vs2017
call "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\tbb\bin\tbbvars.bat" intel64 vs2017
:: call "C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvars64.bat"
:: call "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\bin\ifortvars.bat" intel64 vs2017
call "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\bin\compilervars.bat" intel64 vs2017

%comspec%
