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

    module procedure default_input_data_t
      new_input_data_t%a_       = 0.
      new_input_data_t%b_       = 0.
      new_input_data_t%Cu_ave_  = 0.
      new_input_data_t%Ni_ave_  = 0.
      new_input_data_t%Cu_sig_  = 0.
      new_input_data_t%Ni_sig_  = 0.
      new_input_data_t%fsurf_   = 0.
      new_input_data_t%RTndt0_  = 0.
      new_input_data_t%stress_  = [0.]
      new_input_data_t%temp_    = [0.]
      new_input_data_t%nsim_    = 0
      new_input_data_t%ntime_   = 0
      new_input_data_t%details_ = .true.
    end procedure

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

    module procedure norm
      norm_of_self = maxval(abs([ &
        self%a_, self%b_, self%Cu_ave_, self%Ni_ave_, self%Cu_sig_, self%Ni_sig_, self%fsurf_ , self%RTndt0_, self%stress_, &
        self%temp_, real(self%nsim_), real(self%ntime_), merge(0.,huge(0.), self%details_) &
      ]))
    end procedure

    module procedure subtract
      difference%a_       = self%a_       - rhs%a_
      difference%b_       = self%b_       - rhs%b_
      difference%Cu_ave_  = self%Cu_ave_  - rhs%Cu_ave_
      difference%Ni_ave_  = self%Ni_ave_  - rhs%Ni_ave_
      difference%Cu_sig_  = self%Cu_sig_  - rhs%Cu_sig_
      difference%Ni_sig_  = self%Ni_sig_  - rhs%Ni_sig_
      difference%fsurf_   = self%fsurf_   - rhs%fsurf_
      difference%RTndt0_  = self%RTndt0_  - rhs%RTndt0_
      difference%stress_  = self%stress_  - rhs%stress_
      difference%temp_    = self%temp_    - rhs%temp_
      difference%nsim_    = self%nsim_    - rhs%nsim_
      difference%ntime_   = self%ntime_   - rhs%ntime_
      difference%details_ = self%details_ .eqv. rhs%details_
    end procedure

    module procedure broadcast

      integer size_stress, size_temp

      associate(me => this_image())

        if (me == source_image) then
          size_stress = size(self%stress_)
          size_temp   = size(self%temp_)
        end if

        call co_broadcast(size_stress, source_image)
        call co_broadcast(size_temp, source_image)

        if (me /= source_image) then
          allocate(self%stress_(size_stress))
          allocate(self%temp_(size_temp))
        end if
       end associate

       workarounds: &
       block

         logical, parameter :: opencoarrays_issue_727_fixed = .false.

         if (.not. opencoarrays_issue_727_fixed) then
          ! work around OpenCoarrays issue 727 (https://github.com/sourceryinstitute/OpenCoarrays/issues/727)
          call broadcast_components
         else
           select type(self)
             ! work around gfortran lack of support for polymorphic co_broadcast argument
             type is(input_data_t)
               call co_broadcast(self, source_image)
             class default
               error stop "input_data_type_t%broadcast: unsupported type"
           end select
         end if

       end block workarounds

#ifdef FORD
    end procedure
#else
    contains
#endif

      subroutine broadcast_components()
        call co_broadcast(self%a_, source_image)
        call co_broadcast(self%b_, source_image)
        call co_broadcast(self%Cu_ave_, source_image)
        call co_broadcast(self%Ni_ave_, source_image)
        call co_broadcast(self%Cu_sig_, source_image)
        call co_broadcast(self%Ni_sig_, source_image)
        call co_broadcast(self%fsurf_, source_image)
        call co_broadcast(self%RTndt0_, source_image)
        call co_broadcast(self%stress_, source_image)
        call co_broadcast(self%temp_, source_image)
        call co_broadcast(self%nsim_, source_image)
        call co_broadcast(self%ntime_, source_image)
        call co_broadcast(self%details_, source_image)
      end subroutine

#ifndef FORD
    end procedure
#endif

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
