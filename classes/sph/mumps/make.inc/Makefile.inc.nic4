# CONFIG file for NIC4 - March 2018
# gfortran
# MKL 2013.SP1 (including BLACS and SCALAPACK)
# OpenMPI
# PORD ordering <= to be changed!
#
#Currently Loaded Modulefiles:
#  1) slurm/14.03.11
#  2) cmake/3.5.2
#  3) gdb/7.8.1
#  4) gcc/4.9.2
#  5) openmpi/1.6.4/gcc-4.9.2
#  6) intel/tbb/64/4.2/2013_sp1.3.174
#  7) intel/mkl/64/11.1/2013_sp1.3.174
#  8) git/2.16.2
#  9) python/2.7.10
#


# Begin orderings
LSCOTCHDIR = /usr/lib
ISCOTCH   = -I/usr/include/scotch

LSCOTCH =
#LSCOTCH   = -L$(LSCOTCHDIR) -lptesmumps -lptscotch -lptscotcherr
#LSCOTCH   = -L$(LSCOTCHDIR) -lesmumps -lscotch -lscotcherr

LPORDDIR = $(topdir)/PORD/lib/
IPORD    = -I$(topdir)/PORD/include/
LPORD    = -L$(LPORDDIR) -lpord

LMETISDIR =
#LMETISDIR = /usr/lib 
IMETIS =
#IMETIS    = -I/usr/include/parmetis
#IMETIS    = -I/usr/include/metis

# LMETIS    = -L$(LMETISDIR) -lparmetis -lmetis
#LMETIS    = -L$(LMETISDIR) -lmetis
LMETIS =

# Corresponding variables reused later
ORDERINGSF =  -Dpord
#ORDERINGSF = -Dmetis -Dpord -Dparmetis -Dscotch -Dptscotch
#ORDERINGSF = -Dmetis -Dpord -Dscotch
ORDERINGSC  = $(ORDERINGSF)

LORDERINGS = $(LMETIS) $(LPORD) $(LSCOTCH)
#LORDERINGS = $(LPORD)
#IORDERINGSF = $(ISCOTCH)
#IORDERINGSC = $(IMETIS) $(IPORD) $(ISCOTCH)
IORDERINGSF = 
IORDERINGSC = $(IPORD)


# End orderings
################################################################################

PLAT    =
LIBEXT  = .a
OUTC    = -o 
OUTF    = -o 
RM = /bin/rm -f
CC = gcc
FC = gfortran
FL = gfortran
AR = ar vr 
RANLIB = ranlib
#LAPACK = -llapack
#SCALAP  = -lscalapack-openmpi -lblacs-openmpi  -lblacsF77init-openmpi -lblacsCinit-openmpi

# from
# https://software.intel.com/en-us/articles/intel-mkl-link-line-advisor
# link statique sinon pas openmpi

SCALAP = 
LAPACK =  ${MKLROOT}/lib/intel64/libmkl_scalapack_lp64.a -Wl,--start-group ${MKLROOT}/lib/intel64/libmkl_gf_lp64.a ${MKLROOT}/lib/intel64/libmkl_gnu_thread.a ${MKLROOT}/lib/intel64/libmkl_core.a ${MKLROOT}/lib/intel64/libmkl_blacs_openmpi_lp64.a -Wl,--end-group -lgomp -lpthread -lm -ldl

INCPAR = -I/cm/shared/apps/openmpi/1.6.4/gcc-4.9.2/include

#LIBPAR = $(SCALAP) $(LAPACK)  -lmpi -lmpi_f77
# utiliser "mpif90 --showme:link" (ou "mpif77"? mpifort n'existe pas)

LIBPAR = $(SCALAP) $(LAPACK) -pthread -I/cm/shared/apps/openmpi/1.6.4/gcc-4.9.2/lib -L/cm/shared/apps/openmpi/1.6.4/gcc-4.9.2/lib -lmpi_f90 -lmpi_f77 -lmpi -losmcomp -lrdmacm -libverbs -lrt -lnsl -lutil -lm -lpsm_infinipath -ldl -lm -lnuma -Wl,--export-dynamic -lrt -lnsl -lutil -lm -ldl

#INCSEQ = -I$(topdir)/libseq
#LIBSEQ  = $(LAPACK) -L$(topdir)/libseq -lmpiseq

#LIBBLAS = -lblas
#LIBOTHERS = -lpthread

#Preprocessor defs for calling Fortran from C (-DAdd_ or -DAdd__ or -DUPPER)
CDEFS   = -DAdd_

#Begin Optimized options
# uncomment -fopenmp in lines below to benefit from OpenMP
OPTF    = -m64 -I${MKLROOT}/include -O -fopenmp 
OPTL    = -m64 -I${MKLROOT}/include -O -fopenmp
OPTC    = -m64 -I${MKLROOT}/include -O -fopenmp
#End Optimized options
 
INCS = $(INCPAR)
LIBS = $(LIBPAR)
LIBSEQNEEDED =
