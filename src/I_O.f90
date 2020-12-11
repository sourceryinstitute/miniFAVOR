module I_O

implicit none

    contains

    subroutine read_IN(fn_IN, n_IN, n_ECHO, &
        a, b, nsim, ntime, details, Cu_ave, Ni_ave, Cu_sig, Ni_sig, fsurf, RTndt0, stress, temp)

        !Variables
        integer, intent(in) :: n_IN, n_ECHO
        character(len=64), intent(in) :: fn_IN
        real, intent(out) :: a, b, Cu_ave, Ni_ave, Cu_sig, Ni_sig, fsurf, RTndt0
        integer, intent(out) :: nsim, ntime
        logical, intent(out) :: details
        real, allocatable, intent(out) :: stress(:), temp(:)

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

    end subroutine read_IN

    subroutine write_OUT(fn_IN, n_OUT, n_DAT, &
        a, b, nsim, ntime, details, Cu_ave, Ni_ave, Cu_sig, Ni_sig, fsurf, RTndt0, &
        R_Tndt, CPI, CPI_avg, K_hist, Chemistry_content, Chemistry_factor)

        !Variables
        character(len=64), intent(in) :: fn_IN
        integer, intent(in) :: n_OUT, n_DAT
        real, intent(in) :: a, b, Cu_ave, Ni_ave, Cu_sig, Ni_sig, fsurf, RTndt0
        integer, intent(in) :: nsim, ntime
        logical, intent(in) :: details
        real, intent(in) :: K_hist(:), Chemistry_content(:,:), Chemistry_factor(:)
        real, intent(in) :: R_Tndt(:)
        real, intent(in) :: CPI(:)
        real, intent(in) :: CPI_avg(:)

        character(len=64) :: fn_OUT, fn_DAT
        integer :: i

        !Open output file
        fn_OUT = fn_IN(1:index(fn_IN, '.in')-1)//'.out'
        open (unit=n_OUT, file=fn_OUT, status='unknown', form='formatted')

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
            open (unit=n_DAT, file=fn_DAT, status='unknown', form='formatted')
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


    end subroutine write_OUT


end module I_O
