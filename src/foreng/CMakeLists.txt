add_library(foreng_env foreng_env.f90)
add_library(foreng foreng.f90)

add_subdirectory(math)

target_link_libraries(foreng math)

