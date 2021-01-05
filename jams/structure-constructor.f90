module material_m
  implicit none

  private ! information hiding

  type, public :: material_content_t ! encapsulate
    real :: Ni=0.1, Cu=0.2
  end type

end module

program main
  use material_m, only : material_content_t
  implicit none
  type(material_content_t) :: vessel = material_content_t(Cu=0.4, Ni=0.3)

  real, parameter :: pi=3.141592654

  print *, material_content_t()
  print *, vessel
  print *, vessel%Ni, vessel%Cu

end program main
