submodule(material_content_m) material_content_s
  use assertions_m, only : assert
  implicit none

contains

  module procedure subtract
    difference%Cu_ = self%Cu_ - rhs%Cu_
    difference%Ni_ = self%Ni_ - rhs%Ni_
  end procedure

  module procedure norm
    norm_of_self = maxval(abs([self%Cu_, self%Ni_]))
  end procedure

  module procedure sample_chem
    ! Requirements
    call assert(samples%user_defined(), "sample_chem: samples%user_defined()")
    call assert(all([Cu_ave, Ni_ave, Cu_sig, Ni_sig]>0.), "sample_chem: all([Cu_ave, Ni_ave, Cu_sig, Ni_sig]>0.)")

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
          new_material_content_t = material_content_t(Cu_=Cu_local, Ni_=Ni_local)
        end associate
      end associate
    end associate

    call new_material_content_t%mark_as_defined

    ! Assurances
    call assert(new_material_content_t%user_defined(), "sample_chem: new_material_content_t%user_defined()")

  end procedure

  module procedure Cu
    call assert(self%user_defined(), "material_content_s|Cu: self%user_defined()")
    my_Cu = self%Cu_
  end procedure

  module procedure Ni
    call assert(self%user_defined(), "material_content_s|Ni: self%user_defined()")
    my_Ni = self%Ni_
  end procedure

end submodule
