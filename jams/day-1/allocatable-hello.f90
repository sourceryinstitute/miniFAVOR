program main
  implicit none
  integer, parameter :: max_greeting_length=64
  integer image
  character(len=max_greeting_length), allocatable :: greeting(:)[:]
  allocate(greeting(num_images())[*])
  write(greeting(this_image()),*) "Hello from image ",this_image()," of ", num_images()
  greeting(this_image())[1] = greeting (this_image())
  sync all
  if (this_image()==1) then
    do concurrent(image=1:num_images())
      print *,greeting(image)
    end do
  end if
end program
