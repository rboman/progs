@echo off
:: Ligne de commande qui va bien pour compiler mes brols
::
:: Utilisation:
::    - installer les libs (dans c:\local p. expl.)
::    - definir la variable d'env MYLOCAL  (=c:\local p. expl. )
::    - double cliquer sur myenv.bat
::    - cd build
::    - cmake -G "Visual Studio 14 2015 Win64" ..  
::        ou plus simplement
::    - cmake -A x64 ..  
::    - cmake --build . --config Release
::    - ctest -C Release
:: Incredibuild (http://www.incredibuild.com/webhelp/#BuildConsole.html)
::    - BuildConsole XXX.sln /rebuild /cfg="Release|x64"

set PATH=%PATH%;%MYLOCAL%\swigwin-3.0.12
set INCLUDE=%MYLOCAL%\include;%MYLOCAL%\MUMPS\include;%MYLOCAL%\eigen-3.3.2;%MYLOCAL%\zlib\include
set LIB=%MYLOCAL%\MUMPS\lib;%MYLOCAL%\zlib\lib

:: ok (MKL + VS en ligne de commande)
:: call "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\mkl\bin\mklvars.bat" intel64 vs2015
:: call "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\tbb\bin\tbbvars.bat" intel64 vs2015
:: %comspec% /k "C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvars64.bat"

:: Parallel Studio 2018
%comspec% /E:ON /V:ON /K ""C:\Program Files (x86)\IntelSWTools\compilers_and_libraries_2018.1.156\windows\bin\ipsxe-comp-vars.bat" intel64 vs2017"
