module calc_RTndt
  use assertions_interface, only : assert
  use material_content_m, only : material_content_t

  implicit none

contains

  elemental function RTndt(a, CF, fsurf, RTndt0, phi)

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
  elemental function CF(Cu, Ni)

    use constants_h, only: CF_weld

    real :: CF
    real, intent(in) :: Cu, Ni

    truncate_and_interpolate: &
    associate( &
      Cu_int => rounded_and_bounded(Cu*100, bounds=[0, 40]), &
      Ni_int => int(rounded_and_bounded(Ni*100, bounds=[0, 120])/20) + 1 &! Nickel contents in CF_weld are at intervals of 0.20%
    )
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
          associate( &
            CF_1 => CF_weld(Cu_int,Ni_int) + (Cu-0.01*(Cu_int))/0.01 * (CF_weld(Cu_int+1,Ni_int)-CF_weld(Cu_int+1,Ni_int)), &
            CF_2 => CF_weld(Cu_int,Ni_int+1) + (Cu-0.01*(Cu_int))/0.01 * (CF_weld(Cu_int+1,Ni_int+1)-CF_weld(Cu_int+1,Ni_int+1)) &
          )
            !Second, interpolate on nickel
            CF = CF_1 + (Ni-0.2*(Ni_int-1))/0.2 * (CF_2-CF_1)
          end associate
        end select
      end if
    end associate truncate_and_interpolate

  contains

    pure function rounded_and_bounded(unbounded_value, bounds)
      integer rounded_and_bounded
      real, intent(in) :: unbounded_value
      integer, intent(in) :: bounds(:)
      integer, parameter :: end_points=2

      call assert(size(bounds)==end_points, "Calc_RTndt|bounded_value: size(bounds)==end_points")

      associate(floor_=>bounds(1), ceiling_=>bounds(2))

        if (unbounded_value < floor_) then
          rounded_and_bounded = floor_
        else if (unbounded_value > ceiling_) then
          rounded_and_bounded  = ceiling_
        else
          rounded_and_bounded = int(unbounded_value)
        end if

      end associate

    end function

  end function CF

end module calc_RTndt
