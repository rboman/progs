zlib is a free library that is used by VTL for the compression of data

Compilation from source:

  * Download the source code: https://www.zlib.net/
  * Create a folder `c:\mingw-w64\src`
  * unzip the source code there
  * Create a folder named `c:\mingw-w64\src\zlib-1.2.11\build`
  * Double click on `c:\mingw-w64\mingw-w64.bat`
  * Go to `c:\mingw-w64\src\zlib-1.2.11\build`
  * Be sure that `cmake.exe` is in your `PATH`
  * type:
```
cmake -G "MinGW Makefiles" -DCMAKE_INSTALL_PREFIX=c:/mingw-w64/mingw64 ..
mingw32-make -j 6 install
```
