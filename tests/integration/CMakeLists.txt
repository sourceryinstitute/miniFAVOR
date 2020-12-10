foreach(file test.in test.dat.orig test.echo.orig test.out.orig)
  configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/${file}"
    "${CMAKE_CURRENT_BINARY_DIR}/${file}"
    COPYONLY
  )
endforeach()

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/run_test.sh-template"
  "${CMAKE_CURRENT_BINARY_DIR}/run_test.sh"
  @ONLY
)
