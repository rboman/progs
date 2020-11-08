Compilation requires:
  * a compiler
  * [CMake](https://cmake.org/)
  * [optional] [zlib](https://www.zlib.net/)
  * MPI

## Linux
```
mkdir build
cd build
cmake ..
make -j 6
```
## Windows/MSVC
* Install [Microsoft Visual Studio Community](https://www.visualstudio.com/fr/)
* Compile/install [zlib](https://www.zlib.net/)
* Install [Microsoft MPI](https://msdn.microsoft.com/en-us/library/bb524831(v=vs.85).aspx)
* Open a "VS20XX x64 Native Tools Command Prompt"
```
mkdir build
cd build
cmake -A 64 ..
```
Open the generated solution in Visual Studio and build it.

## Windows/mingw32 (32bits)
* Install [MinGW](http://www.mingw.org/)
  (install everything including `zlib-dev` and `pthread`)
* check that `c:\MinGW\bin` is in your `PATH`
* Open a command line:
```
mkdir build
cd build
cmake -G "MinGW Makefiles" ..
mingw32-make
```

## Windows/mingw-w64 (64bits)
* Install [mingw-w64](https://mingw-w64.org/). Choose the [builds version](https://mingw-w64.org/doku.php/download/mingw-builds) and then "amd64" during installation and select `C:\mingw-w64` as destination folder.
* check that `C:\mingw-w64\mingw64\bin` is in your `PATH`
* Compile/install [zlib](https://www.zlib.net/)
```
cmake -G "MinGW Makefiles" -DCMAKE_INSTALL_PREFIX=c:/mingw-w64/mingw64 ..
mingw32-make -j 6 install
```
* Open a command line:
```
mkdir build
cd build
cmake -G "MinGW Makefiles" ..
mingw32-make
```