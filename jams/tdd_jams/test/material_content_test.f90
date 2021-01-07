module material_content_test
  use material_content_m, only: material_content_t
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
        , it("is marked as user_defined after calling define", check_user_defined) &
        ])
  end function

  function check_not_user_defined() result(result_)

    type(result_t) :: result_

    type(material_content_t) :: sample

    result_ = assert_not(sample%user_defined())
  end function

  function check_user_defined() result(result_)

    type(result_t) :: result_

    type(material_content_t) :: sample

    call sample%define()
    result_ = assert_that(sample%user_defined())
  end function
end module
