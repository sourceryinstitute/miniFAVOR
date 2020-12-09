module randomness_m
  use object_interface, only : object
  implicit none
  private
  public :: random_samples_t

  type, extends(object) :: random_samples_t
    real :: Cu_sig_local
    real :: Cu_local
    real :: Ni_local
    real :: phi
  contains
    procedure :: define
    procedure :: write_formatted
  end type

  interface

    module subroutine define(self)
      implicit none
      class(random_samples_t), intent(out) :: self
    end subroutine

    module subroutine write_formatted(self, unit, iotype, v_list, iostat, iomsg)
      implicit none
      class(random_samples_t), intent(in) :: self
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg
    end subroutine

  end interface

end module
