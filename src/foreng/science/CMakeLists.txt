add_library( 
    
    foreng_science 

    science.f90 
    science_temp.f90
    science_const.f90
    science_elec.f90
    science_phys.f90

)

target_link_libraries(foreng_science foreng_env)