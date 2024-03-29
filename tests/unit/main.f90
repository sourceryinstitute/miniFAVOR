!
!     (c) 2020-2021 Sourcery, Inc.
!     This software was developed for the U.S. Nuclear Regulatory Commission (US NRC) under contract # 31310020D0006:
!     "Technical Assistance in Support of NRC Nuclear Regulatory Research for Materials, Waste, and Reactor Programs"
!
program main
  !! Generated by make_vegetable_driver. DO NOT EDIT
    implicit none

    call run()
contains
    subroutine run()
        use input_data_tests, only: &
                input_data_tests_test_input_data => test_input_data
        use output_data_tests, only: &
                output_data_tests_test_output_data => test_output_data
        use random_samples_tests, only: &
                random_samples_tests_test_random_samples => test_random_samples
        use vegetables, only: test_item_t, test_that, run_tests

        type(test_item_t) :: tests
        type(test_item_t) :: individual_tests(3)

        individual_tests(1) = input_data_tests_test_input_data()
        individual_tests(2) = output_data_tests_test_output_data()
        individual_tests(3) = random_samples_tests_test_random_samples()
        tests = test_that(individual_tests)

        call run_tests(tests)
    end subroutine
end program
