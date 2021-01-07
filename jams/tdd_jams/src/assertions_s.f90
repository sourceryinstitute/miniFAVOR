submodule(assertions_m) assertions_s
  implicit none

  logical, parameter :: assertions = .true.

contains

    module procedure assert
      if (assertions) then
        if (.not. assertion) error stop description
      end if
    end procedure

end submodule
