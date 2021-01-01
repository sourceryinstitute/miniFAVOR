module input_data_m
  implicit none

  private
  public :: input_data_t

  type input_data_t
  contains
    procedure :: define
  end type

  interface

    module subroutine define(self, fn_IN, &
        a, b, nsim, ntime, details, Cu_ave, Ni_ave, Cu_sig, Ni_sig, fsurf, RTndt0, stress, temp)
      implicit none

      class(input_data_t), intent(out) :: self
      character(len=64), intent(in) :: fn_IN
      real, intent(out) :: a, b, Cu_ave, Ni_ave, Cu_sig, Ni_sig, fsurf, RTndt0
      integer, intent(out) :: nsim, ntime
      logical, intent(out) :: details
      real, allocatable, intent(out) :: stress(:), temp(:)
    end subroutine

  end interface

end module input_data_m
