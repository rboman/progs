# file used to compile Metafor using clang on Ubuntu16.04 - garfield

export LIBRARY_PATH=/home/boman/clang/local/tbb/build/linux_intel64_clang_cc4.8_libc2.19_kernel3.13.0_release:/opt/intel/composer_xe_2013_sp1.3.174/compiler/lib/intel64:/opt/intel/composer_xe_2013_sp1.3.174/mkl/lib/intel64
export MIC_LD_LIBRARY_PATH=/opt/intel/composer_xe_2013_sp1.3.174/compiler/lib/mic:/opt/intel/composer_xe_2013_sp1.3.174/mkl/lib/mic
export LD_LIBRARY_PATH=/home/boman/clang/local/tbb/build/linux_intel64_clang_cc4.8_libc2.19_kernel3.13.0_release:/opt/intel/composer_xe_2013_sp1.3.174/compiler/lib/intel64:/opt/intel/composer_xe_2013_sp1.3.174/mkl/lib/intel64:/usr/local/lib/vtk-5.10
export MIC_LIBRARY_PATH=
export CPATH=/opt/intel/composer_xe_2013_sp1.3.174/mkl/include
export TBBROOT=/home/boman/clang/local/tbb
export CC=clang
export CXX=clang++

