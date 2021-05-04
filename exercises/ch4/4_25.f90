!! Bacterial Growth 
!! Bacterium in medium A reproduce every 90 minutes. medium B reproduce every 120 minutes.
program bac_growth

    integer :: minute = 0
    integer, parameter :: MED_A_REP_TIME = 90, MED_B_REP_TIME = 120 !! Reproduction time in minutes
    integer, parameter :: EXPERIMENT_TIME = 24*60 !! Run time in minutes
    integer, parameter :: init_A = 1, init_B = 1
    integer, parameter :: SIX_HOURS = 6*60

    integer :: nb_medium_A = init_A, nb_medium_B = init_B !! Number of bacteria in medium


    222 format("| Time (hr) | Medium A | Medium B |")
    111 format("|", 5X, I0, T13, "|", X, I0, T24,"|", X, I0, T35, "|")
    333 format(35("="))

    print 222
    print 333
    print 111, 0, nb_medium_A, nb_medium_B
    
    do minute = 1, EXPERIMENT_TIME
        
        if (mod(minute, MED_A_REP_TIME) == 0) then
            nb_medium_A = nb_medium_A * 2
        end if
        
        if (mod(minute, MED_B_REP_TIME) == 0) then
            nb_medium_B = nb_medium_B * 2
        end if
        
        if (mod(minute, SIX_HOURS) == 0) then
            
            print 111, minute/60, nb_medium_A, nb_medium_B
            
            
        end if
        
    end do
    
    print 333
    
    
end program