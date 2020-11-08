MUMPS is a solver of linear systems of equations. MUMPS stands for "MUltifrontal Massively Parallel sparse direct Solver".

## CECI clusters

MUMPS is not directly available on the CECI clusters. In consequence, you need to compile it first.

  * Be sure that your environment contains the following modules:
```
module load gcc/4.9.2
module load openmpi/1.6.4/gcc-4.9.2
module load intel/mkl/64/11.1/2013_sp1.3.174
```
  * Download the source code of MUMPS from [MUMPS website](http://mumps.enseeiht.fr/)
  * uncompress the source using `tar xvzf MUMPS_5.1.2.tar.gz`
  * `cd MUMPS_5.1.2`
  * The compilation uses `make` by it requires a customised `Makefile.inc` that should be present in the root folder of the source code. The required file can be found here: [Makefile.inc.nic4](https://github.com/rboman/math0471/blob/master/mumps/make.inc/Makefile.inc.nic4). It should be renamed `Makefile.inc`
  * Type `make` and the library and the examples should be built.
  * Run the example using the following commands:
