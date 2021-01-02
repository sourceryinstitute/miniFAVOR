module output_data_m
  use input_data_m, only : input_data_t
  implicit none

  private
  public :: write_OUT

  interface

    module subroutine write_OUT(fn_IN, input_data, R_Tndt, CPI, CPI_avg, K_hist, Chemistry_content, Chemistry_factor)
      implicit none
      character(len=64), intent(in) :: fn_IN
      type(input_data_t), intent(in) :: input_data
      real, intent(in) :: K_hist(:), Chemistry_content(:,:), Chemistry_factor(:)
      real, intent(in) :: R_Tndt(:)
      real, intent(in) :: CPI(:)
      real, intent(in) :: CPI_avg(:)
    end subroutine write_OUT

  end interface

end module output_data_m
