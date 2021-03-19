module oracle_m
  use object_m, only: object_t
  implicit none

  private
  public :: oracle_t

  type, extends(object_t), abstract :: oracle_t
    private
  contains
    procedure(norm_interface), deferred :: norm
    procedure(subtract_interface), deferred :: subtract
    generic :: operator(-) => subtract
    procedure :: within_tolerance
  end type

  abstract interface

    function norm_interface(self) result(norm_of_self)
      import oracle_t
      implicit none
      class(oracle_t), intent(in) :: self
      real norm_of_self
    end function

    function subtract_interface(self, rhs) result(difference)
      import oracle_t
      implicit none
      class(oracle_t), intent(in) :: self, rhs
      class(oracle_t), allocatable :: difference
    end function

  end interface

  interface

    module function within_tolerance(self, reference, tol)
      implicit none
      class(oracle_t), intent(in) :: self, reference
      real, intent(in) :: tol
      logical within_tolerance
    end function

  end interface

end module
