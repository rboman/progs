# examples/gpu

This is an example used to test offloading capabilities of your compiler. 

Manual build/run on lucia
```
module load EasyBuild/2022a
module load Clang/16.0.6-GCCcore-11.3.0-CUDA-11.7.0
clang++ -O3 -fopenmp -fopenmp-targets=nvptx64-nvidia-cuda -o saxpy_gpu saxpy_gpu.cpp
```

Build/run with cmake 
```
export CXX=clang++
cmake . -B build
make -C build
OMP_TARGET_OFFLOAD=MANDATORY ./build/saxpy_gpu
```
