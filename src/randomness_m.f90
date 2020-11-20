module randomness_m
  implicit none
  private
  public :: random_samples_t, get_samples

  type :: random_samples_t
    real :: Cu_sig_local
    real :: Cu_local
    real :: Ni_local
    real :: phi
  end type
contains
  function get_samples() result(samples)
    type(random_samples_t) :: samples

    ! These must be called in this order or the results will change
    call random_number(samples%Cu_sig_local)
    call random_number(samples%Cu_local)
    call random_number(samples%Ni_local)
    call random_number(samples%phi)
  end function
end module
