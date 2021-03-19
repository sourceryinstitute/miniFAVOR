!
!     (c) 2020-2021 Sourcery, Inc.
!     This software was developed for the U.S. Nuclear Regulatory Commission (US NRC) under contract # 31310020D0006:
!     "Technical Assistance in Support of NRC Nuclear Regulatory Research for Materials, Waste, and Reactor Programs"
!
module reference_inputs_h
    implicit none
    real :: a, b
    integer :: nsim, ntime
    logical :: details
    real, dimension(:), allocatable :: stress, temp
    real :: Cu_ave, Ni_ave, Cu_sig, Ni_sig, fsurf, RTndt0
end module

module reference_constants_h
    implicit none
    real, parameter :: Pi = 4.D0*DATAN(1.D0)
    real, dimension(0:40,7), parameter :: CF_weld = reshape( (/    &
         20,  20,  21,  22,  24,  26,  29,  32,  36,  40, &
         44,  49,  52,  58,  61,  66,  70,  75,  79,  83, &
         88,  92,  97, 101, 105, 110, 113, 119, 122, 128, &
        131, 136, 140, 144, 149, 153, 158, 162, 166, 171, &
        175,                                              &
         20,  20,  26,  35,  43,  49,  52,  55,  58,  61, &
         65,  68,  72,  76,  79,  84,  88,  92,  95, 100, &
        104, 108, 112, 117, 121, 126, 130, 134, 138, 142, &
        146, 151, 155, 160, 164, 168, 172, 177, 182, 185, &
        189,                                              &
         20,  20,  27,  41,  54,  67,  77,  85,  90,  94, &
         97, 101, 103, 106, 109, 112, 115, 119, 122, 126, &
        129, 133, 137, 140, 144, 148, 151, 155, 160, 164, &
        167, 172, 175, 180, 184, 187, 191, 196, 200, 203, &
        207,                                              &
         20,  20,  27,  41,  54,  68,  82,  95, 106, 115, &
        122, 130, 135, 139, 142, 146, 149, 151, 154, 157, &
        160, 164, 167, 169, 173, 176, 180, 184, 187, 191, &
        194, 198, 202, 205, 209, 212, 216, 220, 223, 227, &
        231,                                              &
         20,  20,  27,  41,  54,  68,  82,  95, 108, 122, &
        133, 144, 153, 162, 168, 175, 178, 184, 187, 191, &
        194, 197, 200, 203, 206, 209, 212, 216, 218, 222, &
        225, 228, 231, 234, 238, 241, 245, 248, 250, 254, &
        257,                                              &
         20,  20,  27,  41,  54,  68,  82,  95, 108, 122, &
        135, 148, 161, 172, 182, 191, 199, 207, 214, 220, &
        223, 229, 232, 236, 239, 243, 246, 249, 251, 254, &
        257, 260, 263, 266, 269, 272, 275, 278, 281, 285, &
        288,                                              &
         20,  20,  27,  41,  54,  68,  82,  95, 108, 122, &
        135, 148, 161, 176, 188, 200, 211, 221, 230, 238, &
        245, 252, 257, 263, 268, 272, 276, 280, 284, 287, &
        290, 293, 296, 299, 302, 305, 308, 311, 314, 317, &
        320 /), (/ 41, 7/))
end module

module reference_outputs_h
    implicit none
    real, dimension(:), allocatable :: K_hist
    real, dimension(:,:), allocatable :: Chemistry
    real, dimension(:,:), allocatable :: cpi_hist
    real, dimension(:,:), allocatable :: CPI_results
end module
