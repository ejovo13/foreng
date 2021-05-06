module foreng_env
!! Top level module for all of the useful functions written for Fortran for Scientists and Engineers

use iso_fortran_env, only: real64, real32, int64, int32, int16

implicit none

! public :: sgl, dbl, short, long, pi, pi_s
public int128

 
!     integer, parameter :: SGL = real32
!     integer, parameter :: DBL = real64
!     integer, parameter :: SHORT = int16
!     integer, parameter :: LONG = int64

    real(real64), parameter :: PI_64 = 3.141592653589793
    real(real64), parameter :: PI = PI_64
    real(real32), parameter :: PI_32 = 3.14159265
    integer, parameter :: int128 = selected_int_kind(38)

!=============================================================================!
!=                      List of incorporated modules                         =!
!=============================================================================!

! Math
!   _trig
!   _sets


end module