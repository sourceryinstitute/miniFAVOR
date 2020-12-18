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

  module procedure user_define
     new_material_content_t%Cu_ = Cu
     new_material_content_t%Ni_ = Ni
  end procedure

  module procedure Cu
     my_Cu = self%Cu_
  end procedure

  module procedure Ni
     my_Ni = self%Ni_
  end procedure

end submodule
