# dcm2

## compilation

Example using Windows
```
mkdir build
cd build
cmake -A x64 ..
cmake --build . --config Release
cd ..
```

## run (exe app)

```
build\bin\Release\dcm_app.exe
```

## run (python apps)

Polynome test
```
run.py dcm\tests\testpoly.py
```
Plane test
```
run.py dcm\tests\plane1.py
```
