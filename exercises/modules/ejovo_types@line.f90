submodule (ejovo_types) line

contains 

    module procedure print

        print 99, line1%m, line1%b
        99 format ("y = ", F10.3, "x + ", F10.3)

    end procedure

end submodule