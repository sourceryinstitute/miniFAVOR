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

 ! interface

 !   pure module function user_defined(self) result(is_defined)
 !     implicit none
 !     class(random_sample_t), intent(in) :: self
 !     logical is_defined
 !   end function

 ! end interface

contains

    pure module function user_defined(self) result(self_defined)
      implicit none
      class(random_sample_t), intent(in) :: self
      logical self_defined
      self_defined = self%defined
    end function

    module subroutine define(self)
      implicit none
      class(random_sample_t), intent(out) :: self
      self%defined = .true.
    end subroutine

end module random_sample_m
