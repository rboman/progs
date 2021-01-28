# OpenACC codes

References

* https://www.youtube.com/watch?v=PxmvTsrCTZg

```
git clone https://github.com/OpenACCUserGroup/openacc-users-group.git
cd openacc-users-group/Contributed_Sample_Codes/Tutorial1/solver
```
Sequential
```
pgc++ jsolvec.cpp -fast -Minfo=opt
```
Multithread
```
[add pragmas]
pgc++ jsolvec_acc.cpp -fast -Minfo=opt -ta=multicore -Minfo=accel
pgcpuid   # info cpu
./a.out 3000 20000
export ACC_NUM_CORES=6  # equivalent to OMP_NUM_THREADS - by default: nb of physical cores
```
GPU
```
pgaccelinfo # info gpu
pgc++ jsolvec_acc.cpp -fast -Minfo=opt -ta=tesla:cc70,managed -Minfo=accel
./a.out 2000 10000
./a.out 3000 20000
```
