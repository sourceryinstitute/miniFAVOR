submodule(input_m) input_s
  implicit none

contains

    module procedure read_IN

      character(len=64) :: fn_ECHO
      integer :: i

      !Open input file and create echo file
      open (unit=n_IN, file=fn_IN, status='old', form='formatted')
      fn_ECHO = fn_IN(1:index(fn_IN, '.in')-1)//'.echo'
      open (unit=n_ECHO, file=fn_ECHO, status='unknown', form='formatted')

      !Read and echo crack depth and vessel thickness
      read (n_IN, *) a, b
      write (n_ECHO, '(a25,f10.3,a)') 'Crack Depth: ', a, ' in'
      write (n_ECHO, '(a25,f10.3,a)') 'Vessel Thickness: ', b, ' in'

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

    end procedure read_IN

end submodule input_s
