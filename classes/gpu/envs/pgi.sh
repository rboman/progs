export PGI=/opt/pgi
export PATH=/opt/pgi/linux86-64/2019/bin:$PATH
export MANPATH=$MANPATH:/opt/pgi/linux86-64/2019/man
export LM_LICENSE_FILE=$LM_LICENSE_FILE:/opt/pgi/license.dat

export PATH=/opt/pgi/linux86-64/2019/mpi/openmpi-3.1.3/bin:$PATH
export MANPATH=$MANPATH:$PGI/linux86-64/2019/mpi/openmpi-3.1.3/man

export CXX=pgc++
export CC=pgcc
export FC=pgfortran


