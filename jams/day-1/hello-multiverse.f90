program main
  implicit none
  integer, parameter :: max_greeting_length=64
  integer image
  character(len=max_greeting_length) greeting[*]
  write(greeting,*) "Hello from image ",this_image()," of ", num_images()
  sync all
  if (this_image()==1) then
    do concurrent(image=1:num_images())
      print *,greeting[image]
    end do
  end if
end program
