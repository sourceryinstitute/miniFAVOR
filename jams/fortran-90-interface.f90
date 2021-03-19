program main
  implicit none
  !complex

  ! real f ! Fortran 77

  interface
    real function f(a)
      real a
    end function
  end interface

  print *, f(.false.)
end program
