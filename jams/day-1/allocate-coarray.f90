! Compile with the Intel compiler using
! ifort -coarray=shared -coarray-num-image=3 <file-name>
! to set the number of images at compile-time.
! Override the number of images with an environment variable FOR_COARRAY_NUM_IMAGES
program main
  implicit none
  real, allocatable :: array(:)[:]

  print *,"Image ", this_image()," before allocate"
  allocate(array(10)[*]) ! implicit synchronization
  print *,"Image ", this_image()," after allocate"
end program main
