module inputs_h
    implicit none
    real :: a, b
    integer :: nsim, ntime
    real, dimension(:), allocatable :: stress, temp
    real :: Cu, Ni, fsurf, RTndt0
end module inputs_h