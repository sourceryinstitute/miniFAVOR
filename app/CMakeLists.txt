add_executable(miniFAVOR
  miniFAVOR.f90
)

target_link_libraries(miniFAVOR
  PRIVATE minifav
)
