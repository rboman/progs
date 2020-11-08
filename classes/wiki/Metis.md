# Metis

METIS is a "Fill-reducing Matrix Ordering" library used by [[MUMPS]]. In other words, it allows MUMPS to reorder the equations in order to minimise the number of floating-point operations required during the factorisation phase of the linear system resolution.

ParMETIS is the parallel version (MPI-based) of METIS. [[MUMPS]] can use both libraries.

## Compilation on CECI clusters

  * Be sure that your environment contains the following modules:
```
module load gcc/4.9.2
module load openmpi/1.6.4/gcc-4.9.2
```
  * Download METIS from the [METIS website](http://glaros.dtc.umn.edu/gkhome/metis/metis/download)
  * Download ParMETIS from the [ParMETIS webite](http://glaros.dtc.umn.edu/gkhome/metis/parmetis/download)
  * Unzip and compile both libraries:
```
tar xvzf metis-5.1.0.tar.gz
cd metis-5.1.0
make config prefix=~/local
make -j 6 install
cd ..
tar xvzf parmetis-4.0.3.tar.gz
make config prefix=~/local
make -j 6 install
```
  * Both METIS/ParMETIS libraries are now installed in a folder named `~/local`

## Configure your environment

If you want [[CMake]] to find the location of `metis.h` (header of metis) and `libmetis.a` (METIS library), you need to define 2 environment variables in your `~/.bashrc`

  * Edit your `~/.bashrc`
```
cd
nano .bashrc
```
  * Add the following lines at the end of the file:
```
export INCLUDE=$HOME/local/include:$INCLUDE
export LIB=$HOME/local/lib:$LIB
```

