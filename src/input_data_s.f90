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
      read (n_IN, *) nsim, ntime
      write (n_ECHO, '(a25,i10)') 'Number of Simulations: ', nsim
      write (n_ECHO, '(a25,i10)') 'Number of Time Steps: ', ntime

      !Read in and echo type of output to be written
      read (n_IN, *) details
      write (n_ECHO, '(a25,l10)') 'Detailed output: ', details

      !Read and echo embrittlement inputs
      read (n_IN, *) Cu_ave, Ni_ave, Cu_sig, Ni_sig, fsurf, RTndt0
      write (n_ECHO, '(a25,f10.3,a)') 'Copper Content: ', Cu_ave, ' %'
      write (n_ECHO, '(a25,f10.3,a)') 'Nickel Content: ', Ni_ave, ' %'
      write (n_ECHO, '(a25,f10.3,a)') 'Copper Content STDEV: ', Cu_sig, ' %'
      write (n_ECHO, '(a25,f10.3,a)') 'Nickel Content STDEV: ', Ni_sig, ' %'
      write (n_ECHO, '(a25,f10.3,a)') 'ID Surface Fluence: ', fsurf, ' n/cm^2'
      write (n_ECHO, '(a25,f10.3,a)') 'Unirradiated RTndt: ', RTndt0, ' degF'

      !Allocate stress and temperature arrays
      allocate(stress(ntime))
      allocate(temp(ntime))

      !Read and echo stress and temerature inputs
      write (n_ECHO, '(a)') 'Stress (ksi),      Temperature (degF)'
      read_transient: do  i = 1, ntime
          read (n_IN, *) stress(i), temp(i)
          write (n_ECHO, '(f10.3,9x,f10.3)') stress(i), temp(i)
      end do read_transient

    end procedure define

    module procedure a
       self_a = self%a_
    end procedure

    module procedure b
       self_b = self%b_
    end procedure

end submodule input_data_s
