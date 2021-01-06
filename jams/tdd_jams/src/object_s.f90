submodule(object_m) object_s
  implicit none

contains

    module procedure user_defined
      self_defined = self%defined
    end procedure

    module procedure mark_as_defined
      self%defined = .true.
    end procedure

end submodule
