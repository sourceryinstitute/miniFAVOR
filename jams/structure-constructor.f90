module material_m
  implicit none

  private ! information hiding

  type, public :: material_content_t ! encapsulate
    real :: Ni=0.1, Cu=0.2
  end type

end module

program main
  use material_m, only : material_content_t
  use iso_fortran_env, only : real64
  implicit none
  type(material_content_t) :: vessel = material_content_t(Cu=0.4, Ni=0.3)

  real, parameter :: pi=3.141592654

  integer, parameter :: rkind = SELECTED_REAL_KIND (6, 70)
  real(rkind) y
  real(real64) z

  print *, material_content_t()
  print *, vessel
  print *, vessel%Ni, vessel%Cu

end program main
