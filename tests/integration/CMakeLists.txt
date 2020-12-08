foreach(file test.in test.dat.orig test.echo.orig test.out.orig run_test.sh)
  configure_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/${file}"
    "${CMAKE_CURRENT_BINARY_DIR}/${file}"
    @ONLY
  )
endforeach()
