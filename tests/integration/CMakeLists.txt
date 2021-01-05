foreach(file test.in)
  configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/${file}"
    "${CMAKE_CURRENT_BINARY_DIR}/${file}"
    COPYONLY
  )
endforeach()

add_subdirectory(reference-implementation)

add_executable(test_whole_shebang
  main.f90
  whole_shebang_tests.f90
)
target_link_libraries(test_whole_shebang
  PRIVATE minifav
  PRIVATE vegetables
  PRIVATE reference_miniFAVOR
)
