module calc_cpi

  implicit none

contains

  ! calculate cpi(t)
  pure function cpi_t(K, RTndt, T)

    real :: cpi_t
    real, intent(in) :: K, RTndt, T
    real, parameter :: cKic = 4.0

    associate( &
      aKic => 19.35+8.335*exp(0.02254*(T-RTndt)), &
      bKic => 15.61+50.132*exp(0.008*(T-RTndt)) &
    )
      cpi_t = merge(0.0,1-exp(-((K-aKic)/bKic)**cKic), K < aKic)
    end associate

  end function cpi_t

end module calc_cpi
