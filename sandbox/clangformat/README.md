# test clang format

can be downloaded from:
- https://llvm.org/builds/    (version 11)
- c++ extension from vscode: https://code.visualstudio.com/docs/cpp/cpp-ide (version 9 - C:\Users\r_bom\.vscode\extensions\ms-vscode.cpptools-0.28.3\LLVM\bin)
- installé avec vscode (version 6)
```
F:\dev\progs\sandbox\clangformat>where clang-format.exe
C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\Common7\IDE\VC\VCPackages\clang-format.exe
```


doc: https://clang.llvm.org/docs/ClangFormatStyleOptions.html#

```
clang-format -style=llvm -dump-config > .clang-format
```

```
clang-format -i *.cpp

find . -regex '.*\.\(cpp\|hpp\|cu\|c\|h\)' -exec clang-format -style=file -i {} \;
```