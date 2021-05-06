add_library(foreng_env foreng_env.f90)
add_library(foreng foreng.f90)
add_library(math math.f90 math_trig.f90 math_sets.f90 math_misc.f90 math_stats.f90 math_prob.f90 math_conv.f90 math_geom.f90)

target_link_libraries(math foreng_env)
target_link_libraries(foreng math)


enable_testing()

add_executable(trig_test trig_test.f90)
add_executable(sets_test sets_test.f90)
add_executable(misc_test misc_test.f90)
add_executable(stats_test stats_test.f90)
add_executable(prob_test prob_test.f90)

target_link_libraries(trig_test foreng)
target_link_libraries(sets_test foreng)
target_link_libraries(misc_test foreng)
target_link_libraries(stats_test foreng)
target_link_libraries(prob_test foreng)

add_test(NAME foreng_test COMMAND trig_test)
