@echo off
:: Visual Studio environment using RB libs

set PATH=%PATH%;%MYLOCAL%\swig
set INCLUDE=%MYLOCAL%\include;%MYLOCAL%\MUMPS\include;%MYLOCAL%\eigen
set LIB=%MYLOCAL%\MUMPS\lib
:: f2py
set PATH=%PATH%;F:\Python37\Scripts

:: ok (MKL + VS en ligne de commande)
call "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\mkl\bin\mklvars.bat" intel64 vs2017
call "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\tbb\bin\tbbvars.bat" intel64 vs2017
%comspec% /k "C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvars64.bat"
