submodule(randomness_m) randomness_s
  implicit none

contains
  module procedure define
    ! These must be called in this order or the results will change
    call random_number(self%Cu_sig_local)
    call random_number(self%Cu_local)
    call random_number(self%Ni_local)
    call random_number(self%phi)
  end procedure
end submodule
