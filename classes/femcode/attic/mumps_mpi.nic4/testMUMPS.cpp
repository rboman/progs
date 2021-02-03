// /usr/bin/mpirun -np 6 ./testMUMPS 
// chaque cpu resout le meme systeme et renvoie la ieme inconnue.


#include <gmm/gmm.h>
#include <gmm/gmm_MUMPS_interface.h>

#include <stdio.h>
#include <string.h>
#include <mpi.h>
#define BUFSIZE 128
#define TAG 0

void testMUMPS()
{

   char idstr[32];
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
       
       double A[5][5] = { { 1, 3, 0, 9, 0},
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
    int rc = MPI_Init(&argc,&argv);
    if(rc!=MPI_SUCCESS) {
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

