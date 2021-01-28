#!/bin/sh

#
# Build GCC with support for offloading to NVIDIA GPUs.
#

work_dir=$HOME/offload/wrk
install_dir=$HOME/offload/install

# Location of the installed CUDA toolkit
cuda=/usr/local/cuda

# Build assembler and linking tools
mkdir -p $work_dir
cd $work_dir
git clone https://github.com/MentorEmbedded/nvptx-tools
cd nvptx-tools
./configure \
    --with-cuda-driver-include=$cuda/include \
    --with-cuda-driver-lib=$cuda/lib64 \
    --prefix=$install_dir
make
make install
cd ..

# Set up the GCC source tree
git clone https://github.com/MentorEmbedded/nvptx-newlib
wget ftp://ftp.lip6.fr/pub/gcc/releases/gcc-9.3.0/gcc-9.3.0.tar.gz
tar xf gcc-9.3.0.tar.gz
ln -s gcc-9.3.0 gcc
#svn co svn://gcc.gnu.org/svn/gcc/tags/gcc_7_2_0_release gcc
cd gcc
contrib/download_prerequisites
ln -s ../nvptx-newlib/newlib newlib
cd ..
target=$(gcc/config.guess)

# Build nvptx GCC
mkdir build-nvptx-gcc
cd build-nvptx-gcc
../gcc/configure \
    --target=nvptx-none --with-build-time-tools=$install_dir/nvptx-none/bin \
    --enable-as-accelerator-for=$target \
    --disable-sjlj-exceptions \
    --enable-newlib-io-long-long \
    --enable-languages="c,c++,fortran,lto" \
    --prefix=$install_dir
make -j20
make install
cd ..

# Build host GCC
mkdir build-host-gcc
cd  build-host-gcc
../gcc/configure \
    --enable-offload-targets=nvptx-none \
    --with-cuda-driver-include=$cuda/include \
    --with-cuda-driver-lib=$cuda/lib64 \
    --disable-bootstrap \
    --disable-multilib \
    --enable-languages="c,c++,fortran,lto" \
    --prefix=$install_dir
make -j20
make install
cd ..

