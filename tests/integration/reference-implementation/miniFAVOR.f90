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
    
    use I_O
    use inputs_h
    use outputs_h
    use calc_RTndt
    use calc_K
    use calc_cpi

    implicit none

    ! Variables
    character(len=64) :: fn_IN
    integer, parameter :: n_IN = 15
    integer, parameter :: n_ECHO = n_IN + 1
    integer, parameter :: n_OUT = n_IN + 2
    integer, parameter :: n_DAT = n_IN + 3
    integer :: i, j
    
    ! Body of miniFAVOR
    
    !Get input file name
    print *, 'Input file name:'
    read (*,'(a)') fn_IN
    
    !Read input file
    call read_IN(fn_IN, n_IN, n_ECHO)
    
    !Allocate output arrays
    allocate(K_hist(ntime))
    allocate(Chemistry(nsim,3))
    allocate(cpi_hist(nsim, ntime))
    allocate(CPI_results(nsim,3))
    
    !Initialize output arrays
    K_hist =  0.0
    Chemistry = 0.0
    cpi_hist = 0.0
    CPI_results = 0.0
    
    !Calculate applied stress intensity factor (SIF)
    SIF_loop: do j = 1, ntime
        K_hist(j) = Ki_t(a, b, stress(j))
    end do SIF_loop
        
    !Start looping over number of simulations
    Vessel_loop: do i = 1, nsim
        
        !Sample chemistry: Chemistry(i,1) is Cu content, Chemistry(i,2) is Ni content
        call sample_chem(Cu_ave, Ni_ave, Cu_sig, Ni_sig, Chemistry(i,1), Chemistry(i,2))
        
        !Calculate chemistry factor: Chemistry(i,3) is chemistry factor
        Chemistry(i,3) = CF(Chemistry(i,1), Chemistry(i,2))
        
        !Calculate RTndt for this vessel trial: CPI_results(i,1) is RTndt
        CPI_results(i,1) = RTndt(a, Chemistry(i,3), fsurf, RTndt0)
        
        !Start time loop
        Time_loop: do j = 1, ntime
            !Calculate instantaneous cpi(t)
            cpi_hist(i,j) = cpi_t(K_hist(j), CPI_results(i,1), temp(j))
        end do Time_loop
        
        !Calculate CPI for vessel 'i'
        CPI_results(i,2) = maxval(cpi_hist(i,:))
        
        !Calculate moving average CPI for trials executed so far
        CPI_results(i,3) = sum(CPI_results(:,2))/i
                
    end do Vessel_loop
    
    call write_OUT(fn_IN, n_OUT, n_DAT)
    
    end program miniFAVOR