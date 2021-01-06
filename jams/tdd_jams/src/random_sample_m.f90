module random_sample_m
  implicit none

  private

  type, public :: random_sample_t
    private
    logical :: defined = .false.
  contains
    procedure :: user_defined
    procedure :: define
  end type

  interface

    pure module function user_defined(self) result(self_defined)
      implicit none
      class(random_sample_t), intent(in) :: self
      logical self_defined
    end function

    module subroutine define(self)
      implicit none
      class(random_sample_t), intent(out) :: self
    end subroutine

  end interface

end module

submodule(random_sample_m) random_sample_s
  implicit none

contains

    module procedure user_defined
      self_defined = self%defined
    end procedure

    module procedure define
      self%defined = .true.
    end procedure

end submodule
