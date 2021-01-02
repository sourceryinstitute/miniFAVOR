module input_data_m
  implicit none

  private
  public :: input_data_t

  type input_data_t
    private
    real :: a_, b_, Cu_ave_, Ni_ave_, Cu_sig_, Ni_sig_
    integer :: nsim_, ntime_
    logical :: details_
  contains
    procedure :: define
    procedure :: a
    procedure :: b
    procedure :: nsim
    procedure :: ntime
    procedure :: details
    procedure :: Cu_ave
    procedure :: Cu_sig
    procedure :: Ni_ave
    procedure :: Ni_sig
  end type

  interface

    module subroutine define(self, fn_IN, fsurf, RTndt0, stress, temp)

      implicit none

      class(input_data_t), intent(out) :: self
      character(len=64), intent(in) :: fn_IN
      real, intent(out) :: fsurf, RTndt0
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

    pure module function details(self) result(self_details)
      implicit none
      class(input_data_t), intent(in) :: self
      logical self_details
    end function

    pure module function Cu_ave(self) result(self_Cu_ave)
      implicit none
      class(input_data_t), intent(in) :: self
      real self_Cu_ave
    end function

    pure module function Cu_sig(self) result(self_Cu_sig)
      implicit none
      class(input_data_t), intent(in) :: self
      real self_Cu_sig
    end function

    pure module function Ni_ave(self) result(self_Ni_ave)
      implicit none
      class(input_data_t), intent(in) :: self
      real self_Ni_ave
    end function

    pure module function Ni_sig(self) result(self_Ni_sig)
      implicit none
      class(input_data_t), intent(in) :: self
      real self_Ni_sig
    end function

  end interface

end module input_data_m
