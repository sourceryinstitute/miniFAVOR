module output_data_tests
  use output_data_m, only : output_data_t
  use vegetables, only  : result_t, test_item_t, it, assert_equals, describe, assert_that
  use iso_fortran_env, only : real64
  implicit none
contains

  function check_norm_of_difference() result(result_)
    type(result_t) result_
    type(output_data_t) output_data, difference

    output_data = output_data_t()
    difference = output_data - output_data
    result_ = assert_equals(0._real64, real(difference%norm(), real64))

  end function

  function test_output_data() result(test_item)
    type(test_item_t) test_item

    test_item = describe( &
      "output_data_t", &
      [it( &
        "norm of difference between an object and itself is zero", &
       check_norm_of_difference)] &
    )
  end function

end module output_data_tests
