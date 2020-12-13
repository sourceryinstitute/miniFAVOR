module material_content_m
  implicit none

  private
  public :: material_content_t

  type material_content_t
    !! Elemental content
    private
    real Cu_ !! copper
    real Ni_ !! nickel
  contains
    procedure :: Cu
    procedure :: Ni
  end type

  interface material_content_t

    pure module function user_define(Cu, Ni) result(new_material_content_t)
      !! Define new material_content_t
      implicit none
      real, intent(in) :: Cu !! copper
      real, intent(in) :: Ni !! nickel
      type(material_content_t) new_material_content_t
    end function

  end interface

  interface

    module subroutine write_formatted(self, unit, iotype, v_list, iostat, iomsg)
      implicit none
      class(material_content_t), intent(in) :: self
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg
    end subroutine

    pure module function Cu(self) result(my_Cu)
      class(material_content_t), intent(in) :: self
      real my_Cu
    end function

    pure module function Ni(self) result(my_Ni)
      class(material_content_t), intent(in) :: self
      real my_Ni
    end function

  end interface

end module
