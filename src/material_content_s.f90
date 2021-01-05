submodule(material_content_m) material_content_s
  use assertions_interface, only : assert
  implicit none

contains

  module procedure write_formatted
    integer, parameter :: success=0

    select case(iotype)
    case('LISTDIRECTED')
      write(unit,*) "material_local_t(Cu_=", self%Cu_, ", Ni_=", self%Ni_, ")"
      iostat = success
    case default
      block
        use iso_fortran_env, only : IOSTAT_INQUIRE_INTERNAL_UNIT
        integer, parameter :: iotype_not_supported=99
        call assert(iotype_not_supported/=IOSTAT_INQUIRE_INTERNAL_UNIT, "standard-conforming iostat_value")
        iostat = iotype_not_supported
        iomsg = "iotype not supported"
      end block
    end select

  end procedure

  module procedure Cu
     my_Cu = self%Cu_
  end procedure

  module procedure Ni
     my_Ni = self%Ni_
  end procedure

  module procedure sample_chem

    call assert(samples%user_defined(), "material_content_s|sample_chem: samples%user_defined()")

    associate( &
      Cu_bar => Cu_ave * Cu_sig, &
      Cu_sig_star => min(0.0718*Cu_ave, 0.0185) &
    )
      associate(Cu_sig_local => Cu_bar + Cu_sig_star*sqrt(2.0)*erfc(2*samples%Cu_sig_local()-1))
        !Sample local copper content based on weld copper sampling procedure
        !Sample local nickel content based on weld nickel heat 34B009 & W5214 procedure
        associate( &
          Cu_local => Cu_ave + Cu_sig_local*sqrt(2.0)*erfc(2*samples%Cu_local()-1), &
          Ni_local => Ni_ave + Ni_sig*sqrt(2.0)*erfc(2*samples%Ni_local()-1) &
        )
          material_content = material_content_t(Cu_=Cu_local, Ni_=Ni_local)
        end associate
      end associate
    end associate

  end procedure

end submodule
