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

    elemental module function sample_chem(Cu_ave, Ni_ave, Cu_sig, Ni_sig, samples) result(material_content)
      ! Contstruct a new material_content_t by sampling the copper and nickel contents based on
      ! the nominal value and the standard deviation
      use randomness_m, only : random_samples_t
      implicit none
      type(material_content_t) material_content
      real, intent(in) :: Cu_ave, Ni_ave, Cu_sig, Ni_sig
      type(random_samples_t), intent(in) :: samples
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

    elemental module function Cu(self) result(my_Cu)
      class(material_content_t), intent(in) :: self
      real my_Cu
    end function

    elemental module function Ni(self) result(my_Ni)
      class(material_content_t), intent(in) :: self
      real my_Ni
    end function

  end interface

end module
