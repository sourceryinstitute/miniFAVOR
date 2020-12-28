add_library(iso_varying_string
  iso_varying_string/src/ISO_VARYING_STRING.f90
)

add_library(strff
  strff/src/strff.f90
)
target_link_libraries(strff
  PRIVATE iso_varying_string
)

add_library(vegetables
  vegetables/src/vegetables.F90
)
target_link_libraries(vegetables
  PRIVATE strff
  PRIVATE iso_varying_string
)

add_library(parff
 parff/src/parff.f90
)
target_link_libraries(parff
  PRIVATE iso_varying_string
  PRIVATE strff
)

add_executable(make_vegetables_driver
  vegetables/app/main.f90
  vegetables/app/make_driver_m.f90
)
target_link_libraries(make_vegetables_driver
  PRIVATE parff
  PRIVATE strff
  PRIVATE iso_varying_string
)

add_library(sourcery
  sourcery/src/array_functions_implementation.f90
  sourcery/src/array_functions_interface.f90
  sourcery/src/assertions_implementation.F90
  sourcery/src/assertions_interface.F90
  sourcery/src/co_object_implementation.f90
  sourcery/src/co_object_interface.f90
  sourcery/src/data-partition-implementation.F90
  sourcery/src/data-partition-interface.f90
  sourcery/src/emulated_intrinsics_implementation.F90
  sourcery/src/emulated_intrinsics_interface.F90
  sourcery/src/object_implementation.f90
  sourcery/src/object_interface.f90
  sourcery/src/string_functions_implementation.f90
  sourcery/src/string_functions_interface.f90
)
