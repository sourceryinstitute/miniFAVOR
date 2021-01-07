submodule(oracle_m) oracle_s
  implicit none

contains

    module procedure within_tolerance
      associate(difference => self - reference)
        within_tolerance = difference%norm() < tol
      end associate
    end procedure

end submodule
