program error_test

    ! This program allows me to observe how the error function works

    integer, allocatable, dimension(:) :: unknown_array
    allocate(unknown_array(1))

    error stop "You can't do that!"

end program