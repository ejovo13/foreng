add_library( 
    
    foreng_numeric 

    numeric.f90 
    numeric_calculus.f90 
    numeric_matrices.f90 
    numeric_matrices_gauss.f90
    numeric_matrices_inverse.f90
    numeric_regression.f90 
    numeric_conversion.f90


)

target_link_libraries(foreng_numeric foreng_env)