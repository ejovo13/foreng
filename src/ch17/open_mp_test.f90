program open_mp_test

! This program exhibits how to use OpenMP to run parallel threads from a SINGLE node

! ifort uses -qopenmp and gfortran uses -fopenmp

! The number of threads to be run is dictated by the environmental variable $OMP


use OMP_LIB

    !$OMP PARALLEL

        print *, "Hello from process ", omp_get_thread_num(), " of ", omp_get_num_threads()

    !$OMP END PARALLEL


end program