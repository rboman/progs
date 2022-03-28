! mkdir build
! cd build
! cmake .. && make && mpirun -np 2 ./pingpong 

program pingpong
      
    use mpi
    implicit none
    integer :: ierr, comm, rank, rankP1
    integer :: rreq, sreq, nprocs, iproc
    integer :: stat(MPI_STATUS_SIZE)
    integer :: icomm, ncomm           ! n. of communication step
    ! initial data of each process to be
    ! spread in all the communications
    integer :: proc_data
    integer :: init_data
    integer :: proc_sum

    icomm = 1
    proc_data = 0

    comm = MPI_COMM_WORLD
    call MPI_init(ierr)
    call MPI_COMM_RANK(comm, rank, ierr)
    call MPI_COMM_SIZE(comm, nprocs, ierr)
    !print *, 'rank = ', rank, ' nprocs = ', nprocs, ' ierr = ', ierr, 'icomm = ', icomm

    if ( rank == 0 ) then
        proc_data = 99
        print *, '0, sending...'
        call MPI_Issend(proc_data, 1, MPI_INTEGER, icomm, 0, comm, sreq, ierr)
        print *, '0, waiting...'
        call MPI_Wait(sreq, stat, ierr)
        print *, '0, done...'
    end if 


    if ( rank == icomm ) then
        print *,'1, receiving...'
        call MPI_Irecv(proc_data, 1, MPI_INTEGER, 0, 0, comm, rreq, ierr)
        print *,'1, waiting...'
        call MPI_Wait(rreq, stat, ierr)
        print *, '1, done... received', proc_data
    end if 

    call MPI_FINALIZE(ierr)

end program 