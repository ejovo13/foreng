add_library( 
    
    foreng_science 

    OBJECT

    science.f90 
    science_temp.f90
    science_const.f90
    science_elec.f90
    science_phys.f90

)

target_link_libraries(foreng_science foreng_env)

# install(TARGETS foreng_science
#     EXPORT foreng_science_targets)

# install(EXPORT foreng_science_targets
#     FILE foreng_science_targets.cmake
#     NAMESPACE foreng_science::
#     DESTINATION /usr/local/lib/cmake/foreng
# )