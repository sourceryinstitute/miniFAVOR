submodule(oracle_m) oracle_s
  implicit none

contains

    module procedure within_tolerance
      class(oracle_t), allocatable :: difference
      difference = self - reference
      within_tolerance = difference%norm() < tol
    end procedure

end submodule
