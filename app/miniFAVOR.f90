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

    use calc_RTndt, only : RTndt, CF
    use calc_K, only : Ki_t
    use calc_cpi, only : cpi_t
    use random_samples_m, only: random_samples_t
    use material_content_m, only: material_content_t
    use data_partition_interface, only : data_partition_t => data_partition
    use input_data_m, only : input_data_t
    use output_data_m, only : write_OUT
    use iso_fortran_env, only : input_unit

    implicit none

    ! Variables
    character(len=64) :: fn_IN
    integer, parameter :: input_unit_reader=1
    integer :: i, j
    type(random_samples_t), allocatable :: samples(:)
    type(data_partition_t) data_partition
    type(input_data_t) input_data

    ! Inputs
    real, dimension(:), allocatable :: stress, temp
    real :: fsurf, RTndt0

    ! Outputs
    real, dimension(:,:), allocatable :: cpi_hist

    ! Body of miniFAVOR

    associate(me=>this_image())

      !Get input file name
      if  (me==input_unit_reader) then
        print *, 'Input file name:'
        read (input_unit,'(a)') fn_IN
      end if

      !Read input file
      call input_data%define(fn_IN, fsurf, RTndt0, stress, temp)

      !Calculate applied stress intensity factor (SIF)
      associate( &
        K_hist => Ki_t(input_data%a(), input_data%b(), stress), &
        nsim => input_data%nsim(), &
        ntime => input_data%ntime() &
      )

        call data_partition%define_partitions(cardinality=nsim)

        allocate(samples(nsim))
        do i = 1, nsim ! This cannot be parallelized or reordered without the results changing
          call samples(i)%define()
        end do

        !Sample chemistry: assign Cu content and Ni content
        associate( &
          material_content => &
          material_content_t(input_data%Cu_ave(), input_data%Ni_ave(), input_data%Cu_sig(),input_data%Ni_sig(), samples) &
        )
          associate(Chemistry_factor => CF(material_content%Cu(), material_content%Ni()))
            !Calculate RTndt for this vessel trial: CPI_results(i,1) is RTndt
            associate(R_Tndt => RTndt(input_data%a(), Chemistry_factor, fsurf, RTndt0, samples%phi()))
              !Start looping over number of simulations
              allocate(cpi_hist(nsim, ntime))
              do concurrent(i = 1:nsim, j = 1:ntime)
                ! Instantaneous cpi(t)
                cpi_hist(i,j) = cpi_t(K_hist(j), R_Tndt(i), temp(j))
              end do
              associate(CPI => [(maxval(cpi_hist(i,:)), i=1,nsim)])
                ! Moving average CPI for all trials
                associate(CPI_avg => [(sum(CPI(1:i))/i, i=1,nsim)])
                  block
                    integer, parameter :: nmaterials=2

                    associate(content => reshape([material_content%Cu(),material_content%Ni()], [nsim, nmaterials] ))
                      call write_OUT(fn_IN, &
                        input_data%a(), input_data%b(), nsim, ntime, input_data%details(), &
                        input_data%Cu_ave(), input_data%Ni_ave(), input_data%Cu_sig(), input_data%Ni_sig(), fsurf, RTndt0, &
                        R_Tndt, CPI, CPI_avg, K_hist, content, Chemistry_factor)
                    end associate

                  end block
                end associate
              end associate
            end associate
          end associate
        end associate
      end associate
    end associate

    end program miniFAVOR
