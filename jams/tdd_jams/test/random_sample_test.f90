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
        , it("once defined, has components between 0-1", check_components) &
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

  function check_components() result(result_)
    use random_sample_m, only: random_sample_t
    use vegetables, only: result_t, assert_that

    type(result_t) :: result_

    type(random_sample_t) :: sample

    call sample%define()

    result_ = &
        assert_that(sample%Cu_sig_local() > 0.0 .and. sample%Cu_sig_local() < 1.0, "Cu_sig_local") &
        .and.assert_that(sample%Cu_local() > 0.0 .and. sample%Cu_local() < 1.0, "Cu_local") &
        .and.assert_that(sample%Ni_local() > 0.0 .and. sample%Ni_local() < 1.0, "Ni_local") &
        .and.assert_that(sample%phi() > 0.0 .and. sample%phi() < 1.0, "phi")
  end function
end module
