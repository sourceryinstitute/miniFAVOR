module output_m
  implicit none

  private
  public :: write_OUT

  interface

    module subroutine write_OUT(fn_IN, n_OUT, n_DAT, &
        a, b, nsim, ntime, details, Cu_ave, Ni_ave, Cu_sig, Ni_sig, fsurf, RTndt0, &
        R_Tndt, CPI, CPI_avg, K_hist, Chemistry_content, Chemistry_factor)
      implicit none

      character(len=64), intent(in) :: fn_IN
      integer, intent(in) :: n_OUT, n_DAT
      real, intent(in) :: a, b, Cu_ave, Ni_ave, Cu_sig, Ni_sig, fsurf, RTndt0
      integer, intent(in) :: nsim, ntime
      logical, intent(in) :: details
      real, intent(in) :: K_hist(:), Chemistry_content(:,:), Chemistry_factor(:)
      real, intent(in) :: R_Tndt(:)
      real, intent(in) :: CPI(:)
      real, intent(in) :: CPI_avg(:)

    end subroutine write_OUT

  end interface

end module output_m
