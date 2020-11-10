module I_O
    
    implicit none
    
    contains
        subroutine read_IN(fn_IN, n_IN)
            
            use inputs_h
            
            !Variables
            integer :: i, n_IN
            character(len=64) :: fn_IN
            
            !Open input file
            open (unit=n_IN, file=fn_IN, status='old', form='formatted')
            
            !Read crack depth and vessel thickness
            read (n_IN, '(2f10.3)') a, b
            
            !Read number of simulations to be performed andnumber of time steps
            read (n_IN, '(2i10)') nsim, ntime
            
            !Allocate stress and temperature arrays
            allocate(stress(ntime))
            allocate(temp(ntime))
            
            !Read stress and temerature inputs
            read_transient: do  i = 1, ntime
                read (n_IN, '(2f10.3)') stress(i), temp(i)
            end do read_transient
            
            !Read embrittlement inputs
            read (n_IN, '(4f10.3)') Cu, Ni, fsurf, RTndt0
            
        end subroutine read_IN
end module I_O