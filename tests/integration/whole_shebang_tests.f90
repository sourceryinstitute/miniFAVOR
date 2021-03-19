module whole_shebang_tests
  !
  !     (c) 2020-2021 Sourcery, Inc.
  !     This software was developed for the U.S. Nuclear Regulatory Commission (US NRC) under contract # 31310020D0006:
  !     "Technical Assistance in Support of NRC Nuclear Regulatory Research for Materials, Waste, and Reactor Programs"
  !
  use input_data_m, only : input_data_t
  use output_data_m, only : output_data_t
  use random_samples_m, only : random_samples_t
  use vegetables, only  : result_t, test_item_t, it, assert_equals, describe, assert_that
  use iso_fortran_env, only : real64
  implicit none
contains

  function check_whole_shebang() result(result_)
    type(result_t) result_
    type(input_data_t) input_data
    type(output_data_t) output_data, reference_data
    type(random_samples_t), allocatable :: samples(:)
    integer, parameter :: input_file_reader=1
    integer i

    if (this_image()==input_file_reader) call input_data%define("test.in")
    call input_data%broadcast(source_image = input_file_reader)

    associate(nsim => input_data%nsim())
      allocate(samples(nsim))
      do i = 1, nsim ! This cannot be parallelized or reordered without the results changing
        call samples(i)%define()
      end do
    end associate

    output_data = output_data_t(input_data, samples) ! invoke generic interface for the whole_shebang function
    reference_data = reference_wrapper(input_data)
    result_ = assert_that(output_data%within_tolerance(reference_data, tolerance=1.E-06))
  end function

  function test_output_data() result(test_item)
    type(test_item_t) test_item

    test_item = describe( &
      "whole_shebang", &
      [it( &
        "matches a reference implementation", &
        check_whole_shebang)] &
    )
  end function

  function reference_wrapper(input_data) result(output_data)
    use reference_miniFAVOR_m, only: reference_miniFAVOR
    use reference_inputs_h, only: a, b, nsim, ntime, details, stress, temp, Cu_ave, Ni_ave, Cu_sig, Ni_sig, fsurf, RTndt0
    use reference_outputs_h, only: K_hist, Chemistry, CPI_results

    type(input_data_t), intent(in) :: input_data
    type(output_data_t) :: output_data

    a = input_data%a()
    b = input_data%b()
    nsim = input_data%nsim()
    ntime = input_data%ntime()
    details = input_data%details()
    stress = input_data%stress()
    temp = input_data%temp()
    Cu_ave = input_data%Cu_ave()
    Ni_ave = input_data%Ni_ave()
    Cu_sig = input_data%Cu_sig()
    Ni_sig = input_data%Ni_sig()
    fsurf = input_data%fsurf()
    RTndt0 = input_data%RTndt0()

    call reference_miniFAVOR

    output_data = output_data_t( &
        input_data, CPI_results(:,1), K_hist, Chemistry(:,1:2), Chemistry(:,3), CPI_results(:,2), CPI_results(:,3))
  end function
end module whole_shebang_tests
