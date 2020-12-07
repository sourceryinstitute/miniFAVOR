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
  vegetables/src/vegetables.f90
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
