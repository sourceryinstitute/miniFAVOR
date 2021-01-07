submodule(material_content_m) material_content_s
  implicit none

contains

  module procedure sample_chem
    call new_material_content_t%mark_as_defined
  end procedure

end submodule
