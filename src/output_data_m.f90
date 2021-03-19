!
!     (c) 2020-2021 Sourcery, Inc.
!     This software was developed for the U.S. Nuclear Regulatory Commission (US NRC) under contract # 31310020D0006:
!     "Technical Assistance in Support of NRC Nuclear Regulatory Research for Materials, Waste, and Reactor Programs"
!
module output_data_m
  use input_data_m, only : input_data_t
  use oracle_interface, only : oracle_t
  use random_samples_m, only: random_samples_t
  implicit none

  private

  type, public, extends(oracle_t) :: output_data_t
    private
    type(input_data_t) input_data_
    real ,allocatable :: R_Tndt_(:)
    real ,allocatable :: K_hist_(:), Chemistry_content_(:,:), Chemistry_factor_(:)
    real ,allocatable :: CPI_(:)
    real ,allocatable :: CPI_avg_(:)
  contains
    procedure :: assign
    generic :: assignment(=) => assign
    procedure :: norm
    procedure :: subtract
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

    pure module function default_constructor() result(new_output_data_t)
      implicit none
      type(output_data_t) new_output_data_t
    end function

    module function whole_shebang(input_data, random_samples) result(new_output_data)
      implicit none
      type(input_data_t), intent(in) :: input_data
      type(random_samples_t), intent(in) :: random_samples(:)
      type(output_data_t) :: new_output_data
    end function

    pure module function new_output_data(input_data, R_Tndt, K_hist, Chemistry_content, Chemistry_factor, CPI, CPI_avg)
      implicit none
      type(input_data_t), intent(in) :: input_data
      real, intent(in) :: R_Tndt(:)
      real, intent(in) :: K_hist(:), Chemistry_content(:,:), Chemistry_factor(:)
      real, intent(in) :: CPI(:)
      real, intent(in) :: CPI_avg(:)
      type(output_data_t) new_output_data
    end function

  end interface

  interface

    module subroutine assign(self, rhs)
      implicit none
      class(output_data_t), intent(inout) :: self
      class(oracle_t), intent(in) :: rhs
    end subroutine

    module function subtract(self, rhs) result(difference)
      !! result has components corresponding to subtracting rhs's components fron self object's components
      implicit none
      class(output_data_t), intent(in) :: self
      class(oracle_t), intent(in) :: rhs
      class(oracle_t), allocatable :: difference
    end function

    pure module function norm(self) result(norm_of_self)
      !! result is a norm of the array formed by concatenating the real components of self object
      implicit none
      class(output_data_t), intent(in) :: self
      real norm_of_self
    end function

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
