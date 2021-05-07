set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${MY_BIN}/tests)

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

add_test(NAME trig_test COMMAND trig_test)
add_test(NAME sets_test COMMAND sets_test)
add_test(NAME misc_test COMMAND misc_test)
add_test(NAME stats_test COMMAND stats_test)
add_test(NAME prob_test COMMAND prob_test)