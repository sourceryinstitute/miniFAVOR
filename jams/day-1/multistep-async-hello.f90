program main
  use iso_fortran_env, only : event_type
  implicit none
  type(event_type), allocatable :: greeting_ready(:)[:]
  type(event_type) ok_to_overwrite[*]
  integer, parameter :: greeter=1, max_greeting_length=64, nsteps=5
  character(len=max_greeting_length) greeting[*]
  integer step


  associate(me=>this_image(), n=>num_images()) ! n is immutable within this block

    allocate(greeting_ready(2:n)[*])

    do step=1,nsteps
      if (me/=greeter) then
        if (step/=1) event wait(ok_to_overwrite)
        write(greeting, *) "Hello from image ",me," of ", n
        event post(greeting_ready(me)[greeter])
      else
        print *,"Hello from image ",me," of ", n
        block
          logical greeting_not_printed(2:n)
          integer count, image

          greeting_not_printed = .true.

          spin: &
          do while(any(greeting_not_printed))
            do image=2,n
              call event_query(greeting_ready(image), count)
              if (greeting_not_printed(image) .and. count>0) then
                event wait(greeting_ready(image))
                print *,greeting[image]
                event post(ok_to_overwrite[image])
                greeting_not_printed(image) = .false.
              end if
            end do
          end do spin
        end block
      end if
    end do

  end associate

end program main
