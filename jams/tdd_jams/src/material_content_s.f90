submodule(material_content_m) material_content_s
  use assertions_m, only : assert
  implicit none

contains

  module procedure sample_chem
    ! Requirements
    call assert(samples%user_defined(), "sample_chem: samples%user_defined()")
    call assert(all([Cu_ave, Ni_ave, Cu_sig, Ni_sig]>0.), "sample_chem: all([Cu_ave, Ni_ave, Cu_sig, Ni_sig]>0.)")

    new_material_content_t%Cu_ave = Cu_ave
    new_material_content_t%Ni_ave = Ni_ave
    new_material_content_t%Cu_sig = Cu_sig
    new_material_content_t%Ni_sig = Ni_sig
    new_material_content_t%samples = samples
    call new_material_content_t%mark_as_defined

    ! Assurances
    call assert(new_material_content_t%user_defined(), "sample_chem: new_material_content_t%user_defined()")

  end procedure

  module procedure Cu
    ! call assert(self%samples%user_defined(), "material_content_s|Cu: samples%user_defined()")

    associate( &
      Cu_bar => self%Cu_ave * self%Cu_sig, &
      Cu_sig_star => min(0.0718*self%Cu_ave, 0.0185) &
    )
      associate(Cu_sig_local => Cu_bar + Cu_sig_star*sqrt(2.0)*erfc(2*self%samples%Cu_sig_local()-1))
        !Sample local copper content based on weld copper sampling procedure
        !Sample local nickel content based on weld nickel heat 34B009 & W5214 procedure
        my_Cu = self%Cu_ave + Cu_sig_local*sqrt(2.0)*erfc(2*self%samples%Cu_local()-1)
      end associate
    end associate
  end procedure

  module procedure Ni
    ! call assert(self%samples%user_defined(), "material_content_s|Ni: samples%user_defined()")

    my_Ni = self%Ni_ave + self%Ni_sig*sqrt(2.0)*erfc(2*self%samples%Ni_local()-1)
  end procedure

end submodule
