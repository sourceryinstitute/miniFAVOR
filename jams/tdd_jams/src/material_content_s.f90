submodule(material_content_m) material_content_s
  implicit none

contains

  module procedure sample_chem
    call new_material_content_t%mark_as_defined
  end procedure

  module procedure Cu
    my_Cu =  0.5
  end procedure

  module procedure Ni
    my_Ni =  0.5
  end procedure

end submodule
