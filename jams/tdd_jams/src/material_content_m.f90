module material_content_m
  use object_m, only : object_t
  use random_sample_m, only: random_sample_t
  implicit none

  private
  public :: material_content_t

  type, extends(object_t) :: material_content_t
  end type

  interface material_content_t ! Generic interface (Fortran 90/2003)

    pure module function sample_chem(Cu_ave, Ni_ave, Cu_sig, Ni_sig, samples) result(new_material_content_t)
      implicit none
      real, intent(in) :: Cu_ave, Ni_ave, Cu_sig, Ni_sig
      type(random_sample_t), intent(in) :: samples
      type(material_content_t) new_material_content_t
    end function

  end interface

end module material_content_m
