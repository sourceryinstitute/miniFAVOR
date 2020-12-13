module calc_RTndt
  use assertions_interface, only : assert
  use material_content_m, only : material_content_t

implicit none

    contains

    pure function RTndt(a, CF, fsurf, RTndt0, phi)

        real :: RTndt
        real, intent(in) :: a, CF, fsurf, RTndt0, phi

        associate( &
          D_RTepi => -29.5+78.0*(-log(1-phi))**(1/1.73), &
          f => fsurf*exp(-0.24*a) &
        )
          associate(D_RTndt => CF*f**(0.28-0.10*log10(f)))

          RTndt = RTndt0 + D_RTepi + D_RTndt
       end associate
       end associate

    end function RTndt

    !This function calculates the weld chemistry factor given the copper and nickel contents
    pure function CF(Cu, Ni)

        use constants_h, only: CF_weld

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
    pure function sample_chem(Cu_ave, Ni_ave, Cu_sig, Ni_sig, samples) result(material_content)
        use randomness_m, only : random_samples_t

        !Variables
        type(random_samples_t), intent(in) :: samples
        real, intent(in) :: Cu_ave, Ni_ave, Cu_sig, Ni_sig
        real :: Cu_local, Ni_local
        real :: Cu_bar, Cu_sig_star, Cu_sig_local
        type(material_content_t) material_content

        ! Requires
        call assert(samples%user_defined(), "random_samples_t%sample_chem: samples%user_defined()")

        !Sample local copper content based on weld copper sampling procedure
        Cu_bar = Cu_ave * Cu_sig
        Cu_sig_star = min(0.0718*Cu_ave, 0.0185)
        Cu_sig_local = Cu_bar + Cu_sig_star*sqrt(2.0)*erfc(2*samples%Cu_sig_local()-1)
        Cu_local = Cu_ave + Cu_sig_local*sqrt(2.0)*erfc(2*samples%Cu_local()-1)

        !Sample local nickel content based on weld nickel heat 34B009 & W5214 procedure
        Ni_local = Ni_ave + Ni_sig*sqrt(2.0)*erfc(2*samples%Ni_local()-1)

        material_content = material_content_t(Cu=Cu_local, Ni=Ni_local)

    end function sample_chem

end module calc_RTndt
