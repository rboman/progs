# examples/mpi

This folder contains a program to test MPI with your compiler. 

```
mkdir build
cd build
cmake .. && make
mpirun -np 6 ./mpi_hello  # use "mpiexec" with MS MPI on Windows
```
should display something like:
```
0: We have 6 processors
0: Hello 1! Processor 1 reporting for duty

0: Hello 2! Processor 2 reporting for duty

0: Hello 3! Processor 3 reporting for duty

0: Hello 4! Processor 4 reporting for duty

0: Hello 5! Processor 5 reporting for duty
```
