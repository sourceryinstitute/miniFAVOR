module random_sample_m
  use object_m, only: object_t
  implicit none

  private

  type, public, extends(object_t) :: random_sample_t
    private
    real phi_
    real Ni_local_
    real Cu_local_
    real Cu_sig_local_
  contains
    procedure :: phi
    procedure :: Ni_local
    procedure :: Cu_local
    procedure :: Cu_sig_local
    procedure :: define
  end type

  interface

    pure module function phi(self) result(self_phi)
      implicit none
      class(random_sample_t), intent(in) :: self
      real self_phi
    end function

    pure module function Ni_local(self) result(self_Ni_local)
      implicit none
      class(random_sample_t), intent(in) :: self
      real self_Ni_local
    end function

    pure module function Cu_local(self) result(self_Cu_local)
      implicit none
      class(random_sample_t), intent(in) :: self
      real self_Cu_local
    end function

    pure module function Cu_sig_local(self) result(self_Cu_sig_local)
      implicit none
      class(random_sample_t), intent(in) :: self
      real self_Cu_sig_local
    end function

    module subroutine define(self)
      implicit none
      class(random_sample_t), intent(out) :: self
    end subroutine

  end interface

end module
