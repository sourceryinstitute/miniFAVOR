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
        , it("contents are not negative", check_contents_not_negative) &
        , it("two material contents are within 6 sigma of eachother", check_difference) &
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
        Cu_ave = 0.1, Ni_ave = 0.2, Cu_sig = 0.01, Ni_sig = 0.01, samples=samples)

    result_ = assert_that(content%user_defined())
  end function

  function check_contents_not_negative() result(result_)
    type(result_t) :: result_

    type(material_content_t) :: content
    type(random_sample_t) :: samples

    call samples%define()

    content = material_content_t( &
        Cu_ave = 0.1, Ni_ave = 0.2, Cu_sig = 0.01, Ni_sig = 0.01, samples=samples)

    result_ = &
        assert_that(content%Cu() > 0.0, "Cu") &
        .and.assert_that(content%Ni() > 0.0, "Ni")
  end function

  function check_difference() result(result_)
    type(result_t) :: result_

    real, parameter :: CU_SIG = 0.01, NI_SIG = 0.01, CU_AVE = 0.1, NI_AVE = 0.2
    type(material_content_t) :: content1
    type(random_sample_t) :: samples1
    type(material_content_t) :: content2
    type(random_sample_t) :: samples2

    call samples1%define()
    call samples2%define()

    content1 = material_content_t( &
        Cu_ave = CU_AVE, Ni_ave = NI_AVE, Cu_sig = CU_SIG, Ni_sig = NI_SIG, samples=samples1)
    content2 = material_content_t( &
        Cu_ave = CU_AVE, Ni_ave = NI_AVE, Cu_sig = CU_SIG, Ni_sig = NI_SIG, samples=samples2)

    result_ = assert_that(content1%within_tolerance(content2, 6.*max(CU_SIG, NI_SIG)))
  end function
end module
