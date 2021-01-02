submodule(input_data_m) input_data_s
  implicit none

contains

    module procedure define

      character(len=64) :: fn_ECHO
      integer :: i, n_IN, n_ECHO

      !Open input file and create echo file
      open (newunit=n_IN, file=fn_IN, status='old', form='formatted')
      fn_ECHO = fn_IN(1:index(fn_IN, '.in')-1)//'.echo'
      open (newunit=n_ECHO, file=fn_ECHO, status='unknown', form='formatted')

      !Read and echo crack depth and vessel thickness
      read (n_IN, *) self%a_, self%b_
      write (n_ECHO, '(a25,f10.3,a)') 'Crack Depth: ', self%a(), ' in'
      write (n_ECHO, '(a25,f10.3,a)') 'Vessel Thickness: ', self%b(), ' in'

      !Read and echo number of simulations to be performed andnumber of time steps
      read (n_IN, *) self%nsim_, self%ntime_
      write (n_ECHO, '(a25,i10)') 'Number of Simulations: ', self%nsim()
      write (n_ECHO, '(a25,i10)') 'Number of Time Steps: ', self%ntime()

      !Read in and echo type of output to be written
      read (n_IN, *) self%details_
      write (n_ECHO, '(a25,l10)') 'Detailed output: ', self%details()

      !Read and echo embrittlement inputs
      read (n_IN, *) self%Cu_ave_, self%Ni_ave_, self%Cu_sig_, self%Ni_sig_, self%fsurf_, self%RTndt0_
      write (n_ECHO, '(a25,f10.3,a)') 'Copper Content: ', self%Cu_ave(), ' %'
      write (n_ECHO, '(a25,f10.3,a)') 'Nickel Content: ', self%Ni_ave(), ' %'
      write (n_ECHO, '(a25,f10.3,a)') 'Copper Content STDEV: ', self%Cu_sig(), ' %'
      write (n_ECHO, '(a25,f10.3,a)') 'Nickel Content STDEV: ', self%Ni_sig(), ' %'
      write (n_ECHO, '(a25,f10.3,a)') 'ID Surface Fluence: ', self%fsurf(), ' n/cm^2'
      write (n_ECHO, '(a25,f10.3,a)') 'Unirradiated RTndt: ', self%RTndt0(), ' degF'

      !Allocate stress and temperature arrays
      allocate(self%stress_(self%ntime()))
      allocate(self%temp_(self%ntime()))

      !Read and echo stress and temerature inputs
      write (n_ECHO, '(a)') 'Stress (ksi),      Temperature (degF)'
      read_transient: do  i = 1, self%ntime()
          read (n_IN, *) self%stress_(i), self%temp_(i)
          write (n_ECHO, '(f10.3,9x,f10.3)') self%stress_(i), self%temp_(i)
      end do read_transient

    end procedure define

    module procedure assign
      lhs%a_       = rhs%a_
      lhs%b_       = rhs%b_
      lhs%Cu_ave_  = rhs%Cu_ave_
      lhs%Ni_ave_  = rhs%Ni_ave_
      lhs%Cu_sig_  = rhs%Cu_sig_
      lhs%Ni_sig_  = rhs%Ni_sig_
      lhs%fsurf_   = rhs%fsurf_
      lhs%RTndt0_  = rhs%RTndt0_
      lhs%stress_  = rhs%stress_
      lhs%temp_    = rhs%temp_
      lhs%nsim_    = rhs%nsim_
      lhs%ntime_   = rhs%ntime_
      lhs%details_ = rhs%details_
    end procedure

    module procedure broadcast

      integer size_stress, size_temp
      type(input_data_t) message ! work around gfortran lack of support for polymorphic co_broadcast argument

      associate(me => this_image())

        if (me == source_image) then
          size_stress = size(self%stress_)
          size_temp   = size(self%temp_)
        end if

        call co_broadcast(size_stress, source_image)
        call co_broadcast(size_temp, source_image)

        if (me == source_image) then
          message = self
        else
          allocate(message%stress_(size_stress))
          allocate(message%temp_(size_temp))
        end if

        call co_broadcast(message, source_image)

        if (me /= source_image) self = message

      end associate

    end procedure

    module procedure a
       self_a = self%a_
    end procedure

    module procedure b
       self_b = self%b_
    end procedure

    module procedure nsim
       self_nsim = self%nsim_
    end procedure

    module procedure ntime
       self_ntime = self%ntime_
    end procedure

    module procedure details
       self_details = self%details_
    end procedure

    module procedure Cu_ave
       self_Cu_ave = self%Cu_ave_
    end procedure

    module procedure Cu_sig
       self_Cu_sig = self%Cu_sig_
    end procedure

    module procedure Ni_ave
       self_Ni_ave = self%Ni_ave_
    end procedure

    module procedure Ni_sig
       self_Ni_sig = self%Ni_sig_
    end procedure

    module procedure fsurf
       self_fsurf = self%fsurf_
    end procedure

    module procedure RTndt0
       self_RTndt0 = self%RTndt0_
    end procedure

    module procedure stress
       self_stress = self%stress_
    end procedure

    module procedure temp
       self_temp = self%temp_
    end procedure

end submodule input_data_s
