program open_mp_test

! This program exhibits how to use OpenMP to run parallel threads from a SINGLE node

! ifort uses -qopenmp and gfortran uses -fopenmp

! The number of threads to be run is dictated by the environmental variable $OMP

use OMP_LIB
use euler
use iso_fortran_env

    implicit none

    type(Timer) :: clock
    integer :: this_thread, num_threads
    integer(16) :: total_sum = 0, partial_sum, i

    call clock%start
    
    num_threads = omp_get_max_threads()
    print *, "Num threads: ", num_threads

    !$OMP PARALLEL PRIVATE(this_thread, partial_sum) SHARED(total_sum)

        this_thread = omp_get_thread_num()

            do i = 0, num_threads-1 
                if (this_thread == i) then                
                    print *, "Hello from process ", this_thread
                end if
                !$OMP BARRIER
            end do

        partial_sum = 0

        !$OMP do 
        do i = 1, 10000000000_16

            partial_sum = partial_sum + i

        end do

        do i = 0, num_threads-1
            if (this_thread == i) then
                print *, "Partial sum of thread ", this_thread, ": ", partial_sum
            end if
            !$OMP BARRIER
        end do

        !$OMP CRITICAL

            total_sum = total_sum + partial_sum
            
        !$OMP END CRITICAL
            
            
    !$OMP END PARALLEL
    print *, "Total sum: ", total_sum

    


    call clock%stop

    print *, "Cpu time: ", clock%ctime_elapsed()
    print *, "Wall time: ", clock%wtime_elapsed()


end program