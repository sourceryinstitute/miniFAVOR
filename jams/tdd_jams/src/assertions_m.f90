module assertions_m
  implicit none
  private
  public :: assert

  interface

    pure module subroutine assert(assertion, description)
      logical, intent(in) :: assertion
      character(len=*), intent(in) :: description
    end subroutine

  end interface
end module assertions_m
