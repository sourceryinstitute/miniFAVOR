module input_data_m
  implicit none

  private
  public :: input_data_t

  type input_data_t
    private
    real :: a_, b_, Cu_ave_, Ni_ave_, Cu_sig_, Ni_sig_, fsurf_, RTndt0_
    real, allocatable, dimension(:) :: stress_, temp_
    integer :: nsim_, ntime_
    logical :: details_
  contains
    procedure :: norm
    procedure :: subtract
    generic :: operator(-) => subtract
    procedure :: define
    procedure :: broadcast
    procedure :: a
    procedure :: b
    procedure :: nsim
    procedure :: ntime
    procedure :: details
    procedure :: Cu_ave
    procedure :: Cu_sig
    procedure :: Ni_ave
    procedure :: Ni_sig
    procedure :: fsurf
    procedure :: RTndt0
    procedure :: stress
    procedure :: temp
    procedure :: assign
    generic :: assignment(=) => assign
  end type

  interface input_data_t

    pure module function default_input_data_t() result(new_input_data_t)
      implicit none
      type(input_data_t) new_input_data_t
    end function

  end interface

  interface

    pure module function norm(self) result(norm_of_self)
      implicit none
      class(input_data_t), intent(in) :: self
      real norm_of_self
    end function

    pure module function subtract(self, rhs) result(difference)
      implicit none
      class(input_data_t), intent(in) :: self
      type(input_data_t), intent(in) :: rhs
      type(input_data_t) difference
    end function

    module subroutine define(self, fn_IN)
      implicit none
      class(input_data_t), intent(out) :: self
      character(len=*), intent(in) :: fn_IN
    end subroutine

    module subroutine broadcast(self, source_image)
      implicit none
      class(input_data_t), intent(inout) :: self
      integer, intent(in) :: source_image
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

    pure module function fsurf(self) result(self_fsurf)
      implicit none
      class(input_data_t), intent(in) :: self
      real self_fsurf
    end function

    pure module function RTndt0(self) result(self_RTndt0)
      implicit none
      class(input_data_t), intent(in) :: self
      real self_RTndt0
    end function

    pure module function stress(self) result(self_stress)
      implicit none
      class(input_data_t), intent(in) :: self
      real, allocatable :: self_stress(:)
    end function

    pure module function temp(self) result(self_temp)
      implicit none
      class(input_data_t), intent(in) :: self
      real, allocatable :: self_temp(:)
    end function

    pure module subroutine assign(lhs, rhs)
      class(input_data_t), intent(inout) :: lhs
      type(input_data_t), intent(in) :: rhs
    end subroutine
  end interface

end module input_data_m
