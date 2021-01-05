module random_samples_tests
  use random_samples_m, only : random_samples_t
  use vegetables, only  : result_t, test_item_t, it, assert_that, describe
  implicit none
contains

  function check_definition() result(result_)
    type(result_t) result_
    type(random_samples_t) random_samples

    call random_samples%define
    result_ = assert_that(random_samples%user_defined())
  end function

  function test_random_samples() result(test_item)
    type(test_item_t) test_item

    test_item = describe( &
      "random_sample_t", &
      [it( &
        "is marked as user-defined after calling define()", &
       check_definition)] &
    )
  end function

end module random_samples_tests
