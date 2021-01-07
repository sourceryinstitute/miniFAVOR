module material_content_m
  use object_m, only : object_t
  implicit none

  private
  public :: material_content_t

  type, extends(object_t) :: material_content_t
  end type

end module material_content_m
