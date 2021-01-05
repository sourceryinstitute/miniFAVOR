module detailed_output_m
  use output_data_m, only : output_data_t
  implicit none

  private

  type, public, extends(output_data_t) :: detailed_output_t
  contains
    procedure :: write_formatted
  end type

  interface detailed_output_t

    module function new_detailed_output(output_data)
      type(output_data_t), intent(in) :: output_data
      type(detailed_output_t) new_detailed_output
    end function

  end interface

  interface

    module subroutine write_formatted(self, unit, iotype, v_list, iostat, iomsg)
      implicit none
      class(detailed_output_t), intent(in) :: self
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg
    end subroutine

  end interface

end module detailed_output_m
