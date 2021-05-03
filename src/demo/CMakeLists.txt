add_executable(chapter_demos demo.f90)
target_link_libraries(chapter_demos all_chapters)

set(DEMO_DIR ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/demo)
make_directory(${DEMO_DIR})

set_target_properties(chapter_demos PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${DEMO_DIR})

configure_file(5_4.dat ${DEMO_DIR} COPYONLY)
configure_file(5_5.dat ${DEMO_DIR} COPYONLY)