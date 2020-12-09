submodule(randomness_m) randomness_s
  implicit none

contains

  module procedure define
    ! These must be called in this order or the results will change
    call random_number(self%Cu_sig_local)
    call random_number(self%Cu_local)
    call random_number(self%Ni_local)
    call random_number(self%phi)
    call self%mark_as_defined
  end procedure

  module procedure write_formatted
  end procedure
end submodule
