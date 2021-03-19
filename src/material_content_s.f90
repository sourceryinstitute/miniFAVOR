!
!     (c) 2020-2021 Sourcery, Inc.
!     This software was developed for the U.S. Nuclear Regulatory Commission (US NRC) under contract # 31310020D0006:
!     "Technical Assistance in Support of NRC Nuclear Regulatory Research for Materials, Waste, and Reactor Programs"
!
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

  module procedure gather

    real, allocatable, dimension(:) :: Cu_local, Ni_local

    allocate(Cu_local(size(material_content)), Ni_local(size(material_content)))

    associate(me => this_image())
      associate(my_first => data_partition%first(me), my_last => data_partition%last(me))
        Cu_local(my_first:my_last) = material_content(my_first:my_last)%Cu()
        Ni_local(my_first:my_last) = material_content(my_first:my_last)%Ni()
      end associate
    end associate

    call data_partition%gather(Cu_local, dim=1)
    call data_partition%gather(Ni_local, dim=1)

    material_content%Cu_ = Cu_local
    material_content%Ni_ = Ni_local

  end procedure

end submodule
