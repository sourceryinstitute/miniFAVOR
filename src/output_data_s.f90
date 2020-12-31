submodule(output_data_m) output_data_s

  implicit none

contains

  module procedure write_OUT

      character(len=64) :: fn_OUT, fn_DAT
      integer :: i, n_OUT, n_DAT

      !Open output file
      fn_OUT = fn_IN(1:index(fn_IN, '.in')-1)//'.out'
      open (newunit=n_OUT, file=fn_OUT, status='unknown', form='formatted')

      !Write out important outputs summary
      write (n_OUT, '(a)') 'MiniFAVOR Output Summary'
      write (n_OUT, '(a)') '/Key inputs/'
      write (n_OUT, '(a25,f10.3,a)') 'Crack Depth: ', a, ' in'
      write (n_OUT, '(a25,f10.3,a)') 'Vessel Thickness: ', b, ' in'
      write (n_OUT, '(a25,i10)') 'Number of Simulations: ', nsim
      write (n_OUT, '(a25,f10.3,a)') 'Copper Content: ', Cu_ave, ' %'
      write (n_OUT, '(a25,f10.3,a)') 'Nickel Content: ', Ni_ave, ' %'
      write (n_OUT, '(a25,f10.3,a)') 'Copper Content STDEV: ', Cu_sig, ' %'
      write (n_OUT, '(a25,f10.3,a)') 'Nickel Content STDEV: ', Ni_sig, ' %'
      write (n_OUT, '(a25,f10.3,a)') 'ID Surface Fluence: ', fsurf, ' n/cm^2'
      write (n_OUT, '(a25,f10.3,a)') 'Unirradiated RTndt: ', RTndt0, ' degF'
      write (n_OUT, '(a)') '/Results/'
      write (n_OUT, '(a25,f10.3)') 'Final CPI: ', CPI_avg(nsim)
      write (n_OUT, '(a25,f10.3,a)') 'Minimum crack tip RTndt: ', &
          minval(R_Tndt), ' degF'
      write (n_OUT, '(a25,f10.3,a)') 'Maximum crack tip  RTndt: ', &
          maxval(R_Tndt), ' degF'
      write (n_OUT, '(a25,f10.3,a)') 'Average crack tip RTndt: ', &
          sum(R_Tndt)/nsim, ' degF'

      !Write out detailed output to data file
      if (details) then
          fn_DAT = fn_IN(1:index(fn_IN, '.in')-1)//'.dat'
          open (newunit=n_DAT, file=fn_DAT, status='unknown', form='formatted')
          write (n_DAT, '(a)') 'MiniFAVOR Detailed Output'
          write (n_DAT, '(a)') '/Applied SIF (ksi*in^0.5)/'
          write_SIF: do  i = 1, ntime
              write (n_DAT, '(f10.3)') K_hist(i)
          end do write_SIF
          write (n_DAT, '(a)') '/Chemistry Results'
          write (n_DAT, '(a)') 'Cu content (%),  Ni Content (%), Chemistry Factor CF'
          write_chem: do  i = 1, nsim
              write (n_DAT, '(3f10.3)') Chemistry_content(i,1), Chemistry_content(i,2), Chemistry_factor(i)
          end do write_chem
          write (n_DAT, '(a)') '/Vessel CPI data'
          write (n_DAT, '(a)') 'Vessel RTndt (degF),  Vessel CPI, Cumulative Average CPI'
          write_CPI: do  i = 1, nsim
              write (n_DAT, '(3f10.3)') R_Tndt(i), CPI(i), CPI_avg(i)
          end do write_CPI
      end if


  end procedure write_OUT

end submodule output_data_s
