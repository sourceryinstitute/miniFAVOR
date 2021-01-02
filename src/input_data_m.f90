module input_data_m
  implicit none

  private
  public :: input_data_t

  type input_data_t
    private
    real :: a_, b_
    integer :: nsim_, ntime_
  contains
    procedure :: define
    procedure :: a
    procedure :: b
    procedure :: nsim
    procedure :: ntime
  end type

  interface

    module subroutine define(self, fn_IN, &
        details, Cu_ave, Ni_ave, Cu_sig, Ni_sig, fsurf, RTndt0, stress, temp)
      implicit none

      class(input_data_t), intent(out) :: self
      character(len=64), intent(in) :: fn_IN
      real, intent(out) :: Cu_ave, Ni_ave, Cu_sig, Ni_sig, fsurf, RTndt0
      logical, intent(out) :: details
      real, allocatable, intent(out) :: stress(:), temp(:)
    end subroutine

    pure module function a(self) result(self_a)
      implicit none
      class(input_data_t), intent(in) :: self
      real self_a
    end function

    pure module function b(self) result(self_b)
      implicit none
      class(input_data_t), intent(in) :: self
      real self_b
    end function

    pure module function nsim(self) result(self_nsim)
      implicit none
      class(input_data_t), intent(in) :: self
      integer self_nsim
    end function

    pure module function ntime(self) result(self_ntime)
      implicit none
      class(input_data_t), intent(in) :: self
      integer self_ntime
    end function

  end interface

end module input_data_m
