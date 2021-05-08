add_library( 
    
    foreng_io

    io.f90
    io_string.f90

)

target_link_libraries(foreng_io foreng_env)