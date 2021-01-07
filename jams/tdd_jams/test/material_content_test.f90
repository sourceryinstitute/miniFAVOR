module material_content_test
  use material_content_m, only: material_content_t
  use random_sample_m, only: random_sample_t
  use vegetables, only: test_item_t, describe, it, result_t, assert_that, assert_not
  implicit none
  private
  public :: test_material_content

contains
  function test_material_content() result(tests)

    type(test_item_t) :: tests

    tests = describe( &
        "material_content_t", &
        [ it("is not marked as user_defined initially", check_not_user_defined) &
        , it("is marked as user_defined after being constructed", check_user_defined) &
        ])
  end function

  function check_not_user_defined() result(result_)

    type(result_t) :: result_

    type(material_content_t) :: content

    result_ = assert_not(content%user_defined())
  end function

  function check_user_defined() result(result_)

    type(result_t) :: result_

    type(material_content_t) :: content
    type(random_sample_t) :: samples

    call samples%define()

    content = material_content_t( &
        Cu_ave = 0.1, Ni_ave = 0.2, Cu_sig = 0.01, Ni_sig = 0.01, samples)

    result_ = assert_that(content%user_defined())
  end function
end module
