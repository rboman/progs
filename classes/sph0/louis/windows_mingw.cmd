@echo off
:: This file opens a terminal which allows you to compile the code with

echo setting MinGW64 environment...
set PATH=%USERPROFILE%\mingw64\bin;%PATH%
set CMAKE_GENERATOR=Ninja

:: open terminal
CD /d "%~dp0"
%comspec% /K
