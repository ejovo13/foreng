! What is meant by each of the following array terms? (a) size, (b) shape, (c) extent, (d)
! rank, (e) conformable

program array_definitions

    integer, parameter :: DEFINITION_LENGTH = 65
    character(DEFINITION_LENGTH) :: sizeDef, shapeDef, extentDef, rankDef, conformableDef

    sizeDef = "Number of elements of A"
    shapeDef = "Return the dimensions of A"
    extentDef = "The number of elements in a single dimension"
    rankDef = "The number of dimensions"
    conformableDef = "Two arrays that have the same shape and extent are conformable"

    print 100, sizeDef, shapeDef, extentDef, rankDef, conformableDef
    100 Format("size: ", T30, A, /, "shape: ", T30, A, /, "extent: ", T30, A, /, "rank: ", T30, A, /, "conformable: ", T30, A)
    
end program