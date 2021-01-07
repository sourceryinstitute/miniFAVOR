module material_content_m
  use object_m, only : object_t
  use random_sample_m, only: random_sample_t
  implicit none

  private
  public :: material_content_t

  type, extends(object_t) :: material_content_t
    private
    real :: Cu_ave, Ni_ave, Cu_sig, Ni_sig
    type(random_sample_t) :: samples
  contains
    procedure :: Ni
    procedure :: Cu
  end type

  interface material_content_t ! Generic interface (Fortran 90/2003)

    pure module function sample_chem(Cu_ave, Ni_ave, Cu_sig, Ni_sig, samples) result(new_material_content_t)
      implicit none
      real, intent(in) :: Cu_ave, Ni_ave, Cu_sig, Ni_sig
      type(random_sample_t), intent(in) :: samples
      type(material_content_t) new_material_content_t
    end function

  end interface

  interface

    pure module function Ni(self) result(my_Ni)
      !! Result is the nickel content
      class(material_content_t), intent(in) :: self
      real my_Ni
    end function

    pure module function Cu(self) result(my_Cu)
      !! Result is the copper content
      class(material_content_t), intent(in) :: self
      real my_Cu
    end function

  end interface

end module material_content_m
