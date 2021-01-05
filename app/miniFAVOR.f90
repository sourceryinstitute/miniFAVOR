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
    use input_data_m, only : input_data_t
    use output_data_m, only : output_data_t
    use detailed_output_m, only : detailed_output_t
    use iso_fortran_env, only : input_unit

    implicit none

    ! Variables
    character(len=64) :: fn_IN
    integer, parameter :: input_unit_reader=1, output_writer=1
    integer :: i
    type(random_samples_t), allocatable :: samples(:)
    type(input_data_t) input_data

    ! Body of miniFAVOR

    associate(me=>this_image())

      !Get input file name
      if  (me==input_unit_reader) then
        print *, 'Input file name:'
        read (input_unit,'(a)') fn_IN
        !Read input file
        call input_data%define(fn_IN)
      end if

      call input_data%broadcast(source_image = input_unit_reader)

      !Calculate applied stress intensity factor (SIF)
      associate(nsim => input_data%nsim())

        allocate(samples(nsim))
        do i = 1, nsim ! This cannot be parallelized or reordered without the results changing
          call samples(i)%define()
        end do

        associate( &
            output_data => output_data_t(input_data, samples), &
            base_name => fn_IN(1:index(fn_IN, '.in')-1) &
        )
          if (me==output_writer) then
            block
              integer unit

              open(newunit=unit,  file=base_name//".out", status='unknown')
              write(unit, '(DT)') output_data
              close(unit)
              if (input_data%details()) then
                  open(newunit=unit,  file=base_name//".dat", status='unknown')
                  write(unit, '(DT)') detailed_output_t(output_data)
                  close(unit)
              end if
            end block
          end if
        end associate
      end associate
    end associate

    end program miniFAVOR
