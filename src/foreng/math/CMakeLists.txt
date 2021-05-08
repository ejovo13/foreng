add_library( 
    
    foreng_math 

    math.f90 
    math_trig.f90 
    math_sets.f90 
    math_misc.f90 
    math_stats.f90 
    math_prob.f90 
    math_geom.f90

)

target_link_libraries(foreng_math foreng_env)