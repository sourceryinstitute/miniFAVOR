module whole_shebang_tests
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
    reference_data = output_data
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

end module whole_shebang_tests
