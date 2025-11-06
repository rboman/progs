# examples/openmp

This folder contains a program to test OpenMP with your compiler. 
This is mainly useful for Apple users.

```
mkdir build
cd build
cmake .. && make
OMP_NUM_THREADS=4 ./omp_hello
```
should display something like:
```
Hello World from thread = 0
Number of threads = 4
Hello World from thread = 1
Hello World from thread = 3
Hello World from thread = 2
```
