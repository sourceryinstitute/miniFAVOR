module random_sample_test
  implicit none
  private
  public :: test_random_sample
contains
  function test_random_sample() result(tests)
    use vegetables, only: test_item_t, describe, it

    type(test_item_t) :: tests

    tests = describe( &
        "random_sample_t", &
        [ it("is not marked as user_defined initially", check_not_user_defined) &
        , it("is marked as user_defined after calling define", check_user_defined) &
        ])
  end function

  function check_not_user_defined() result(result_)
    use random_sample_m, only: random_sample_t
    use vegetables, only: result_t, assert_not

    type(result_t) :: result_

    type(random_sample_t) :: sample

    result_ = assert_not(sample%user_defined())
  end function

  function check_user_defined() result(result_)
    use random_sample_m, only: random_sample_t
    use vegetables, only: result_t, assert_that

    type(result_t) :: result_

    type(random_sample_t) :: sample

    call sample%define()
    result_ = assert_that(sample%user_defined())
  end function
end module
