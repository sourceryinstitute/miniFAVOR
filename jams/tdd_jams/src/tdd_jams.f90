module tdd_jams
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, tdd_jams!"
  end subroutine say_hello
end module tdd_jams
