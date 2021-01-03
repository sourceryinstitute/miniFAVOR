module output_data_m
  use input_data_m, only : input_data_t
  use object_interface, only : object_t
  implicit none

  private

  type, public, extends(object_t) :: output_data_t
    private
    type(input_data_t) input_data_
    real ,allocatable :: R_Tndt_(:)
    real ,allocatable :: K_hist_(:), Chemistry_content_(:,:), Chemistry_factor_(:)
    real ,allocatable :: CPI_(:)
    real ,allocatable :: CPI_avg_(:)
  contains
    procedure :: write_formatted
    procedure :: R_Tndt
    procedure :: K_hist
    procedure :: Chemistry_content
    procedure :: Chemistry_factor
    procedure :: CPI
    procedure :: CPI_avg
    procedure :: nsim
    procedure :: ntime
  end type

  interface output_data_t

    pure module function new_output_data(input_data, R_Tndt, K_hist, Chemistry_content, Chemistry_factor, CPI, CPI_avg)
      type(input_data_t), intent(in) :: input_data
      real, intent(in) :: R_Tndt(:)
      real, intent(in) :: K_hist(:), Chemistry_content(:,:), Chemistry_factor(:)
      real, intent(in) :: CPI(:)
      real, intent(in) :: CPI_avg(:)
      type(output_data_t) new_output_data
    end function

  end interface

  interface

    module subroutine write_formatted(self, unit, iotype, v_list, iostat, iomsg)
      implicit none
      class(output_data_t), intent(in) :: self
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg
    end subroutine

    module function R_Tndt(self) result(my_R_Tndt)
      implicit none
      class(output_data_t), intent(in) :: self
      real, allocatable :: my_R_Tndt(:)
    end function

    module function K_hist(self) result(my_K_hist)
      implicit none
      class(output_data_t), intent(in) :: self
      real, allocatable :: my_K_hist(:)
    end function

    module function Chemistry_content(self) result(my_Chemistry_content)
      implicit none
      class(output_data_t), intent(in) :: self
      real, allocatable :: my_Chemistry_content(:,:)
    end function

    module function Chemistry_factor(self) result(my_Chemistry_factor)
      implicit none
      class(output_data_t), intent(in) :: self
       real, allocatable :: my_Chemistry_factor(:)
    end function

    module function CPI(self) result(my_CPI)
      implicit none
      class(output_data_t), intent(in) :: self
      real ,allocatable :: my_CPI(:)
    end function

    module function CPI_avg(self) result(my_CPI_avg)
      implicit none
      class(output_data_t), intent(in) :: self
      real ,allocatable :: my_CPI_avg(:)
    end function

    module function nsim(self) result(my_nsim)
      implicit none
      class(output_data_t), intent(in) :: self
      integer my_nsim
    end function

    module function ntime(self) result(my_ntime)
      implicit none
      class(output_data_t), intent(in) :: self
      integer my_ntime
    end function

  end interface

end module output_data_m
