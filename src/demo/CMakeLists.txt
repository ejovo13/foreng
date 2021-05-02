add_executable(chapter_demos demo.f90)
target_link_libraries(chapter_demos ch4)
target_link_libraries(chapter_demos ch5)

set(DEMO_DIR ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/demo)

set_target_properties(chapter_demos PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${DEMO_DIR})

configure_file(5_4.dat ${DEMO_DIR} COPYONLY)
configure_file(5_5.dat ${DEMO_DIR} COPYONLY)