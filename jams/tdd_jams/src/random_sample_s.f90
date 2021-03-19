submodule(random_sample_m) random_sample_s
  implicit none

contains

    module procedure phi
      self_phi = self%phi_
    end procedure

    module procedure Ni_local
      self_Ni_local = self%Ni_local_
    end procedure

    module procedure Cu_local
      self_Cu_local = self%Cu_local_
    end procedure

    module procedure Cu_sig_local
      self_Cu_sig_local = self%Cu_sig_local_
    end procedure

    module procedure define
      logical, save :: first_call=.true.

      if (first_call) then
        first_call=.false.
        block
          integer i, num_seeds
          call random_seed(size=num_seeds)
          call random_seed(put=[(i, i=1, num_seeds)])
        end block
      end if

      ! These must be called in this order or the results will change
      call random_number(self%Cu_sig_local_)
      call random_number(self%Cu_local_)
      call random_number(self%Ni_local_)
      call random_number(self%phi_)

      call self%mark_as_defined
    end procedure

end submodule
