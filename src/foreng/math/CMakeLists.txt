add_library( 
    
    math 

    math.f90 
    math_trig.f90 
    math_sets.f90 
    math_misc.f90 
    math_stats.f90 
    math_prob.f90 
    math_conv.f90 
    math_geom.f90

)

target_link_libraries(math foreng_env)