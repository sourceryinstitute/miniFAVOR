module random_samples_m
  use object_interface, only : object_t
  implicit none
  private
  public :: random_samples_t

  type, extends(object_t) :: random_samples_t
    private
    real :: Cu_sig_local_
    real :: Cu_local_
    real :: Ni_local_
    real :: phi_
  contains
    procedure :: define
    procedure :: write_formatted
    procedure :: Cu_sig_local
    procedure :: Cu_local
    procedure :: Ni_local
    procedure :: phi
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

    pure module function Cu_sig_local(self) result(self_Cu_sig_local)
      implicit none
      class(random_samples_t), intent(in) :: self
      real self_Cu_sig_local
    end function

    pure module function Cu_local(self) result(self_Cu_local)
      implicit none
      class(random_samples_t), intent(in) :: self
      real self_Cu_local
    end function

    pure module function Ni_local(self) result(self_Ni_local)
      implicit none
      class(random_samples_t), intent(in) :: self
      real self_Ni_local
    end function

    elemental module function phi(self) result(self_phi)
      implicit none
      class(random_samples_t), intent(in) :: self
      real self_phi
    end function

  end interface

end module
