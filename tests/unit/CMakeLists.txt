add_executable(unit_tests
  main.f90
  random_samples_tests.f90
)
target_link_libraries(unit_tests
  PRIVATE minifav
  PRIVATE vegetables
)
