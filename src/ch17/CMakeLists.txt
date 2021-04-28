
# find_package(Coarray REQUIRED)

find_package(MPI REQUIRED)
find_package(OpenMP REQUIRED)
# find_library(eulerf REQUIRED HINTS /home/ejovo13/lib)

include_directories(${MPI_Fortran_INCLUDE_DIRS})
add_executable(mpi_test mpi_test.f90)
target_link_libraries(mpi_test ${MPI_Fortran_LIBRARIES})

# if(MPI_COMPILE_FLAGS)
#   set_target_properties(mpi_test PROPERTIES
#     COMPILE_FLAGS "${MPI_Fortran_COMPILE_FLAGS}")
# endif()

# if(MPI_LINK_FLAGS)
#   set_target_properties(mpi_test PROPERTIES
#     LINK_FLAGS "${MPI_Fortran_LINK_FLAGS}")
# endif()

add_executable(open_mp_test open_mp_test.f90)
target_link_libraries(open_mp_test PUBLIC OpenMP::OpenMP_Fortran /home/ejovo13/lib/libeulerf.so)
target_include_directories(open_mp_test PUBLIC /home/ejovo13/inc/Fortran_modules)


# if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU") 
#     # Using GNU
#     target_compile_options(open_mp_test PUBLIC -fopenmp)
# elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
#     # Using intel
#     target_compile_options(open_mp_test PUBLIC -qopenmp)
# endif()


# link_directories(/home/ejovo13/Programming/Projecet_Euler/bin/lib)

# add_executable(coarrays coarray_playground.f90)

# include_directories(/home/ejovo13/Programming/Projecet_Euler/src/lib)


# target_compile_options(coarrays PUBLIC -coarray)
# target_link_libraries(coarrays eulerf)