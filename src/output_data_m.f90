module output_data_m
  use input_data_m, only : input_data_t
  implicit none

  private
  public :: write_OUT

  interface

    module subroutine write_OUT(base_name, input_data, R_Tndt, CPI, CPI_avg, K_hist, Chemistry_content, Chemistry_factor)
      implicit none
      character(len=*), intent(in) :: base_name
      type(input_data_t), intent(in) :: input_data
      real, intent(in) :: K_hist(:), Chemistry_content(:,:), Chemistry_factor(:)
      real, intent(in) :: R_Tndt(:)
      real, intent(in) :: CPI(:)
      real, intent(in) :: CPI_avg(:)
    end subroutine write_OUT

  end interface

end module output_data_m
