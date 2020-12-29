module input_m
  implicit none

  private
  public :: read_IN

  interface

    module subroutine read_IN(fn_IN, n_IN, n_ECHO, &
        a, b, nsim, ntime, details, Cu_ave, Ni_ave, Cu_sig, Ni_sig, fsurf, RTndt0, stress, temp)
      implicit none

      integer, intent(in) :: n_IN, n_ECHO
      character(len=64), intent(in) :: fn_IN
      real, intent(out) :: a, b, Cu_ave, Ni_ave, Cu_sig, Ni_sig, fsurf, RTndt0
      integer, intent(out) :: nsim, ntime
      logical, intent(out) :: details
      real, allocatable, intent(out) :: stress(:), temp(:)
    end subroutine

  end interface

end module input_m
