!
!     (c) 2020-2021 Sourcery, Inc.
!     This software was developed for the U.S. Nuclear Regulatory Commission (US NRC) under contract # 31310020D0006:
!     "Technical Assistance in Support of NRC Nuclear Regulatory Research for Materials, Waste, and Reactor Programs"
!
module reference_calc_RTndt

implicit none

    contains

    !RTndt_x calculation
    function RTndt(a, CF, fsurf, RTndt0)

        !Variables
        real :: RTndt, D_RTepi, D_RTndt, phi, f
        real, intent(in) :: a, CF, fsurf, RTndt0

        !Calculate D_RTepi
        call RANDOM_NUMBER(phi)
        D_RTepi = -29.5+78.0*(-log(1-phi))**(1/1.73)

        !Calculate D_RTndt
        f = fsurf*exp(-0.24*a)
        D_RTndt = CF*f**(0.28-0.10*log10(f))

        !Calculate the RTndt
        RTndt = RTndt0 + D_RTepi + D_RTndt

    end function RTndt

    !This function calculates the weld chemistry factor given the copper and nickel contents
    function CF(Cu, Ni)

        use reference_constants_h, only: CF_weld

        !Variables
        real :: CF
        real, intent(in) :: Cu, Ni
        integer :: Cu_int, Ni_int
        real :: CF_1, CF_2

        !Calculate indexes for copper interpolation:
        !  multiply the Cu-% by 100 and take the integer truncation to find interpolation bounds
        !  truncate interpolation between 0% and 0.40%
        Cu_int = int(Cu*100)
        if (Cu_int < 0) then
            Cu_int = 0
        else if (Cu_int > 40) then
            Cu_int = 40
        end if

        !Calculate indexes for nickel interpolation:
        !  multiply the Ni-% by 100 and take the integer truncation to find interpolation bounds
        !  truncate interpolation between 0% and 1.20%
        Ni_int = int(Ni*100)
        if (Ni_int < 0) then
            Ni_int = 0
        else if (Ni_int > 120) then
            Ni_int = 120
        end if
        !Nickel contents in CF_weld are at intervals of 0.20% nickel
        Ni_int = int(Ni_int/20)+1

        !Bi-linear interpolation
        if (Cu <= 0.0 .or. Cu >= 0.40) then !only interpolate on nickel
            select case (Ni_int)
            case (7)
                CF = CF_weld(Cu_int, Ni_int)
            case default
                CF = CF_weld(Cu_int,Ni_int) + &
                (Ni-0.2*(Ni_int-1))/0.2 * (CF_weld(Cu_int,Ni_int+1)-CF_weld(Cu_int,Ni_int))
            end select
        else
            !First, interpolate on copper
            select case (Ni_int)
            case (7)
                CF = CF_weld(Cu_int, Ni_int) + &
                (Cu-0.01*(Cu_int))/0.01 * (CF_weld(Cu_int+1,Ni_int)-CF_weld(Cu_int+1,Ni_int))
            case default
                CF_1 = CF_weld(Cu_int,Ni_int) + &
                    (Cu-0.01*(Cu_int))/0.01 * (CF_weld(Cu_int+1,Ni_int)-CF_weld(Cu_int+1,Ni_int))
                CF_2 = CF_weld(Cu_int,Ni_int+1) + &
                    (Cu-0.01*(Cu_int))/0.01 * (CF_weld(Cu_int+1,Ni_int+1)-CF_weld(Cu_int+1,Ni_int+1))
            !Second, interpolate on nickel
            CF = CF_1 + (Ni-0.2*(Ni_int-1))/0.2 * (CF_2-CF_1)
            end select
        end if

    end function CF

    !This subroutine samples the copper and nickel contents based on the nominal value
    !and the standard deviation
    subroutine sample_chem(Cu_ave, Ni_ave, Cu_sig, Ni_sig, Cu_local, Ni_local)

        !Variables
        real, intent(in) :: Cu_ave, Ni_ave, Cu_sig, Ni_sig
        real, intent(out) :: Cu_local, Ni_local
        real :: u, Cu_bar, Cu_sig_star, Cu_sig_local

        !Sample local copper content based on weld copper sampling procedure
        Cu_bar = Cu_ave * Cu_sig
        Cu_sig_star = min(0.0718*Cu_ave, 0.0185)
        call RANDOM_NUMBER(u)
        Cu_sig_local = Cu_bar + Cu_sig_star*sqrt(2.0)*erfc(2*u-1)
        call RANDOM_NUMBER(u)
        Cu_local = Cu_ave + Cu_sig_local*sqrt(2.0)*erfc(2*u-1)

        !Sample local nickel content based on weld nickel heat 34B009 & W5214 procedure
        call RANDOM_NUMBER(u)
        Ni_local = Ni_ave + Ni_sig*sqrt(2.0)*erfc(2*u-1)

    end subroutine sample_chem

end module
