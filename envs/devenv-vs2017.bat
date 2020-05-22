@echo off
:: Ligne de commande qui va bien pour compiler mes brols
::
:: Utilisation:
::    - installer les libs (dans c:\local p. expl.)
::    - definir la variable d'env MYLOCAL  (=c:\local p. expl. )
::    - double cliquer sur myenv.bat
::    - cd build
::    - cmake -A x64 ..
::    - cmake --build . --config Release -- -j6
::    - ctest -C Release
:: Incredibuild (http://www.incredibuild.com/webhelp/#BuildConsole.html)
::    - BuildConsole Waves.sln /rebuild /cfg="Release|x64"

set PATH=%PATH%;%MYLOCAL%\swig
set INCLUDE=%MYLOCAL%\include;%MYLOCAL%\MUMPS\include;%MYLOCAL%\eigen
set LIB=%MYLOCAL%\MUMPS\lib

::set PATH=%PATH%;C:\Program Files\Git\bin
:: f2py
set PATH=%PATH%;F:\Python37\Scripts


:: ok (MKL + VS en ligne de commande)
call "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\mkl\bin\mklvars.bat" intel64 vs2017
call "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\tbb\bin\tbbvars.bat" intel64 vs2017
%comspec% /k "C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvars64.bat"
