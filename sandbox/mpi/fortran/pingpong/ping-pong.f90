! mpiexec -n 4 ./ping-pong

program pingpong
    use mpi
    implicit none
    integer :: ierr, comm, rank, rankP1, rankM1
    integer :: rreq, sreq, nprocs
    integer :: stat(MPI_STATUS_SIZE)
    integer :: data_to_be_sent, data_received

    comm = MPI_COMM_WORLD

    ! init communicator
    call MPI_init(ierr)
    call MPI_COMM_RANK(comm, rank, ierr)
    call MPI_COMM_SIZE(comm, nprocs, ierr)

    rankP1 = MOD(rank + 1, nprocs)           ! rank of the receiver (rank+1)
    rankM1 = MOD(nprocs + rank - 1, nprocs)  ! rank of the sender (rank-1)

    ! j'envoie au rank+1 son propre rank (facile à vérifier)
    data_to_be_sent = rankP1

    print "('rank #',I2,' wants to send ',I2,' to ','rank ',I2)", rank, data_to_be_sent, rankP1
    call MPI_Issend(data_to_be_sent, 1, MPI_INTEGER, rankP1, 0, comm, sreq, ierr)
    print "('rank #',I2,' wants to receive something from rank ',I2)", rank, rankM1
    call MPI_Irecv(data_received, 1, MPI_INTEGER, rankM1, 0, comm, rreq, ierr)
    print "('rank #',I2,' waits for data')", rank
    call MPI_Wait(rreq, stat, ierr)
    print "('rank #',I2,' waits for its send operation to be done')", rank
    call MPI_Wait(sreq, stat, ierr)

    ! on attend tous les threads pour afficher
    call MPI_BARRIER(comm, ierr)
    print "('summary: rank #',I2,' recieved ', I2)", rank, data_received 

    call MPI_FINALIZE(ierr)

end program pingpong