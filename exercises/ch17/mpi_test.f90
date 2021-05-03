! This program tests using mpi, which can be compiled either with mpif90 or mpiifort

PROGRAM hello_world
    implicit none

    include 'mpif.h'

    integer process_Rank, size_Of_Cluster, ierror, i

    call MPI_INIT(ierror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, size_Of_Cluster, ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, process_Rank, ierror)

    
    do i = 0, size_Of_Cluster-1
        
        if(i == process_Rank) then
            print *, "Hello World from process: ", process_Rank, " of ", size_Of_Cluster
        end if
        
        call MPI_BARRIER(MPI_COMM_WORLD, ierror)
        
    end do
    
    call MPI_FINALIZE(ierror)



END PROGRAM