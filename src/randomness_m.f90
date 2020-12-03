module randomness_m
  implicit none
  private
  public :: random_samples_t

  type :: random_samples_t
    real :: Cu_sig_local
    real :: Cu_local
    real :: Ni_local
    real :: phi
  contains
    procedure :: define
  end type

  interface
    module subroutine define(self)
      implicit none
      class(random_samples_t), intent(out) :: self
    end subroutine
  end interface

end module
