!  miniFAVOR.f90 
!
!  FUNCTIONS:
!  miniFAVOR - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: miniFAVOR
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program miniFAVOR
    
    use I_O, only: read_IN
    use inputs_h, only: nsim, ntime

    implicit none

    ! Variables
    character(len=64) :: fn_IN
    integer, parameter :: n_IN = 15
    integer :: i, j
    
    ! Body of miniFAVOR
    
    !Get input file name
    print *, 'Input file name:'
    read (*,'(a)') fn_IN
    
    !Read input file
    call read_IN(fn_IN, n_IN)
    
    !Start looping over number of simulations
    Vessel_loop: do i = 1, nsim
        !Start time loop
        Time_loop: do j = 1, ntime
            !time dependent statements
        end do Time_loop
    end do Vessel_loop
    
    
    end program miniFAVOR

