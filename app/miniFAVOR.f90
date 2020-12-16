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

    use I_O, only : read_In, write_Out
    use calc_RTndt, only : RTndt, CF
    use calc_K, only : Ki_t
    use calc_cpi, only : cpi_t
    use randomness_m, only: random_samples_t
    use material_content_m, only: material_content_t
    use data_partition_interface, only : data_partition_t => data_partition

    implicit none

    ! Variables
    character(len=64) :: fn_IN
    integer, parameter :: n_IN = 15
    integer, parameter :: n_ECHO = n_IN + 1
    integer, parameter :: n_OUT = n_IN + 2
    integer, parameter :: n_DAT = n_IN + 3
    integer :: i, j
    type(random_samples_t), allocatable :: samples(:)
    type(data_partition_t) data_partition

    ! Inputs
    real :: a, b
    integer :: nsim, ntime
    logical :: details
    real, dimension(:), allocatable :: stress, temp
    real :: Cu_ave, Ni_ave, Cu_sig, Ni_sig, fsurf, RTndt0

    ! Outputs
    real, allocatable :: K_hist(:)
    real, allocatable :: R_Tndt(:)
    real, dimension(:,:), allocatable :: cpi_hist

    ! Body of miniFAVOR

    !Get input file name
    print *, 'Input file name:'
    read (*,'(a)') fn_IN

    !Read input file
    call read_IN(fn_IN, n_IN, n_ECHO, &
        a, b, nsim, ntime, details, Cu_ave, Ni_ave, Cu_sig, Ni_sig, fsurf, RTndt0, stress, temp)

    !Allocate output arrays
    allocate(cpi_hist(nsim, ntime))
    allocate(R_Tndt(nsim), samples(nsim))

    !Calculate applied stress intensity factor (SIF)
    K_hist = Ki_t(a, b, stress)

    call data_partition%define_partitions(cardinality=nsim)

    associate(me=>this_image(), nimages=>num_images())

      print *,"image", me, ":", data_partition%first(me), "-", data_partition%last(me)

      ! This cannot be parallelized or reordered without the results changing
      do i = 1, nsim
        call samples(i)%define()
      end do

      !Sample chemistry: assign Cu content and Ni content
      associate(material_content => material_content_t(Cu_ave, Ni_ave, Cu_sig, Ni_sig, samples))

          associate(Chemistry_factor => CF(material_content%Cu(), material_content%Ni()))

      !Start looping over number of simulations
      do i = 1, nsim

          !Calculate RTndt for this vessel trial: CPI_results(i,1) is RTndt
          R_Tndt(i) = RTndt(a, Chemistry_factor(i), fsurf, RTndt0, samples(i)%phi())

      end do

      !Start looping over number of simulations
      Vessel_loop: do i = 1, nsim
          Time_loop: do j = 1, ntime
              !Calculate instantaneous cpi(t)
              cpi_hist(i,j) = cpi_t(K_hist(j), R_Tndt(i), temp(j))
          end do Time_loop
      end do Vessel_loop

      associate(CPI => [(maxval(cpi_hist(i,:)), i=1,nsim)])

      ! Moving average CPI for all trials
      associate(CPI_avg => [(sum(CPI(1:i))/i, i=1,nsim)])

      block
        integer, parameter :: nmaterials=2

        associate(content => reshape([material_content%Cu(),material_content%Ni()], [nsim, nmaterials] ))
          call write_OUT(fn_IN, n_OUT, n_DAT, &
            a, b, nsim, ntime, details, Cu_ave, Ni_ave, Cu_sig, Ni_sig, fsurf, RTndt0, &
            R_Tndt, CPI, CPI_avg, K_hist, content, Chemistry_factor)
        end associate
      end block
            end associate
          end associate
        end associate
      end associate
    end associate

    end program miniFAVOR
