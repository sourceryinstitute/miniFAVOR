module object_m
  implicit none

  private

  type, public :: object_t
    private
    logical :: defined = .false.
  contains
    procedure :: user_defined
    procedure :: mark_as_defined
  end type

  interface

    pure module function user_defined(self) result(self_defined)
      implicit none
      class(object_t), intent(in) :: self
      logical self_defined
    end function

    pure module subroutine mark_as_defined(self)
      implicit none
      class(object_t), intent(inout) :: self
    end subroutine

  end interface

end module
