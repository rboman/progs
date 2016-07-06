#include "testSwig.h"

int testSwig(MPI_Comm comm, int a)
{
    int sum, size, rank;
    
    MPI_Comm_size(comm, &size);
    MPI_Comm_rank(comm, &rank);
    MPI_Allreduce(&a, &sum, 1, MPI_INT, MPI_SUM, comm);
    return sum;
}
