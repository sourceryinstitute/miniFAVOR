!
!     (c) 2020-2021 Sourcery, Inc.
!     This software was developed for the U.S. Nuclear Regulatory Commission (US NRC) under contract # 31310020D0006:
!     "Technical Assistance in Support of NRC Nuclear Regulatory Research for Materials, Waste, and Reactor Programs"
!
module reference_calc_K

implicit none

    contains

    !Function to calculate K(t)
    function Ki_t(a, b, stress)

        use reference_constants_h, only: Pi

        !Variables
        real :: Ki_t
        real, intent(in) :: a, b, stress

        !Calculate Ki_t
        Ki_t = stress*sqrt(Pi*a)*   &
            (1.122-0.231*(a/b)+10.55*(a/b)**2-21.71*(a/b)**3+30.382*(a/b)**4)

    end function Ki_t

end module
