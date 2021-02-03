// /usr/bin/mpirun -np 6 ./testMUMPS 
// chaque cpu resout le MEME systeme et renvoie la ieme inconnue.
// utiliser 6 process pour voir toute la solution.

// expl: (windows avec msmpi):
//   D:\dev\femcode\mumpsseq_in_mpi\build>mpiexec -np 6 Release\testMUMPS.exe
//   0: We have 6 processors
//   0: Processor 1: 2.800000
//   0: Processor 2: 0.000000
//   0: Processor 3: -0.600000
//   0: Processor 4: -0.200000
//   0: Processor 5: 0.142857


#include <mpi.h>
#include <gmm/gmm.h>
#include <gmm/gmm_MUMPS_interface.h>
#ifdef GMM_USES_MPI
#error "MUMPS should be do not work with MUMPS/MPI"
#endif

#include <stdio.h>
#include <string.h>

#define BUFSIZE 128
#define TAG 0

void testMUMPS()
{
   char buff[BUFSIZE];

   int i;
   MPI_Status stat;

   int numprocs;
   int myid;

   /* find out how big the SPMD world is */
   MPI_Comm_size(MPI_COMM_WORLD,&numprocs);

   /* and this processes' rank is */
   MPI_Comm_rank(MPI_COMM_WORLD,&myid);
   
   if(myid == 0)
    {
        printf("%d: We have %d processors\n", myid, numprocs);
        for(i=1;i<numprocs;i++)
        {
            sprintf(buff, "Hello %d! ", i);
            MPI_Send(buff, BUFSIZE, MPI_CHAR, i, TAG, MPI_COMM_WORLD);
        }
        for(i=1;i<numprocs;i++)
        {
            MPI_Recv(buff, BUFSIZE, MPI_CHAR, i, TAG, MPI_COMM_WORLD, &stat);
            printf("%d: %s\n", myid, buff);
        }
    }
    else
    {
        /* receive from rank 0: */
        MPI_Recv(buff, BUFSIZE, MPI_CHAR, 0, TAG, MPI_COMM_WORLD, &stat);
        double A[5][5] = {  { 1, 3, 0, 9, 0},
                            { 1, 2, 0, 9, 0},
                            { 1, 2, 3, 0, 0},
                            { 3, 2, 11, 4, 0},
                            { 0, 2, 0, 0, 7 }};
       
        int n=5;
       
        gmm::row_matrix< gmm::wsvector<double> > K(n,n);
       
        for(int i=0; i<n; ++i)
            for(int j=0; j<n; ++j)
                //if(A[i][j]!=0.0 || A[j][i] != 0.0)
                K(i, j) = A[i][j];
       
        gmm::csr_matrix<double> Kcsr;    
        gmm::copy(K, Kcsr); 
       
        std::vector<double> b(n), x(n);
        for(int i=0; i<n; ++i)
            b[i]=1.0;
       
        // solver MUMPS
        gmm::MUMPS_solve(Kcsr, x, b);
       
        sprintf(buff, "Processor %d: %f", myid, x[myid-1]);
       
        /* send to rank 0: */
        MPI_Send(buff, BUFSIZE, MPI_CHAR, 0, TAG, MPI_COMM_WORLD);
    }
   
}


int main(int argc, char *argv[])
{
    int ierr = MPI_Init(&argc,&argv);
    if(ierr!=MPI_SUCCESS) {
      fprintf(stderr, "MPI Init failure!\n");
      abort();
    }

    try
    {
        testMUMPS();
    }
    GMM_STANDARD_CATCH_ERROR;

    MPI_Finalize();
    return 0;
}

