submodule(random_samples_m) random_samples_s
  use assertions_interface,only : assert
  implicit none

contains

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

  module procedure write_formatted
    integer, parameter :: success=0

    select case(iotype)
    case('LISTDIRECTED')
      write(unit,*) "random_samples_t(Cu_sig_local=", self%Cu_sig_local_, ", Cu_local=", self%Cu_local_, &
                    ", Ni_local=", self%Ni_local_, ", phi=", self%phi_,")"
      iostat = success
    case default
      block
        use iso_fortran_env, only : IOSTAT_INQUIRE_INTERNAL_UNIT
        integer, parameter :: iotype_not_supported=99
        call assert(iotype_not_supported/=IOSTAT_INQUIRE_INTERNAL_UNIT, "standard-conforming iostat_value")
        iostat = iotype_not_supported
        iomsg = "iotype not supported"
      end block
    end select

  end procedure

  module procedure Cu_sig_local
    self_Cu_sig_local = self%Cu_sig_local_
  end procedure

  module procedure Cu_local
    self_Cu_local = self%Cu_local_
  end procedure

  module procedure Ni_local
    self_Ni_local = self%Ni_local_
  end procedure

  module procedure phi
    self_phi = self%phi_
  end procedure

end submodule
