add_library(foreng_env foreng_env.f90)
add_library(foreng foreng.f90)

add_subdirectory(math)
add_subdirectory(io)
add_subdirectory(numeric)
add_subdirectory(science)

target_link_libraries(foreng foreng_math foreng_io foreng_numeric foreng_science)

