@echo off
:: Ligne de commande qui va bien pour compiler mes brols

::    - mkdir build
::    - cd build
::    - cmake -G "Visual Studio 11 Win64" ..
::    - cmake --build . --config Release

%comspec% /K ""C:\Program Files (x86)\Microsoft Visual Studio 11.0\VC\vcvarsall.bat" amd64"
