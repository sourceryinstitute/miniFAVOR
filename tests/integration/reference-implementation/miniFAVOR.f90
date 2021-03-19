!
!     (c) 2020-2021 Sourcery, Inc.
!     This software was developed for the U.S. Nuclear Regulatory Commission (US NRC) under contract # 31310020D0006:
!     "Technical Assistance in Support of NRC Nuclear Regulatory Research for Materials, Waste, and Reactor Programs"
!
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

module reference_miniFAVOR_m

contains
  subroutine reference_miniFAVOR
    use reference_inputs_h
    use reference_outputs_h
    use reference_calc_RTndt
    use reference_calc_K
    use reference_calc_cpi

    implicit none

    ! Variables
    integer :: i, j

    ! Body of miniFAVOR
    block
      integer i, num_seeds
      call random_seed(size=num_seeds)
      call random_seed(put=[(i, i=1, num_seeds)])
    end block

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

  end subroutine
end module
