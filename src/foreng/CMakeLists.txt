add_library(math math.f90 math_trig.f90 math_sets.f90)
add_library(foreng foreng.f90)

target_link_libraries(foreng math)


enable_testing()

add_executable(trig_test trig_test.f90)
add_executable(sets_test sets_test.f90)
target_link_libraries(trig_test foreng)
target_link_libraries(sets_test foreng)

add_test(NAME foreng_test COMMAND trig_test)