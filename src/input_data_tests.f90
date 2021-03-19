module input_data_tests
  use input_data_m, only : input_data_t
  use vegetables, only  : result_t, test_item_t, it, describe, assert_equals, assert_that
  use iso_fortran_env, only : real64
  implicit none
contains

  function check_norm_of_difference() result(result_)
    type(result_t) result_
    type(input_data_t) input_data, difference

    input_data = input_data_t()

    difference = input_data - input_data
    result_ = assert_equals(0._real64, real(difference%norm(), real64))
  end function

  function test_input_data() result(test_item)
    type(test_item_t) test_item

    test_item = describe( &
      "input_data_t", &
      [it( &
        "norm of difference between an object and itself is zero", &
       check_norm_of_difference)] &
    )
  end function

end module input_data_tests
