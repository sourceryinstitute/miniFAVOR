cmake_minimum_required(VERSION 3.16)

set (FAVOR_VERSION "0.0.1")
set (ALLOWED_BUILD_TYPES "Debug" "Release")

get_property(multi_config GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
if(multi_config)
  set (CMAKE_CONFIGURATION_TYPES "${ALLOWED_BUILD_TYPES}" CACHE STRING "list of supported configuration types" FORCE)
else()
  set (CMAKE_BUILD_TYPE "Debug" CACHE STRING "Select which configuration to build.")
  set_property (CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS ${ALLOWED_BUILD_TYPES})
endif()

string  (JOIN "\n " GREETING "Version: ${FAVOR_VERSION}\n Build type: ${CMAKE_BUILD_TYPE}\n")
message (STATUS "\n${GREETING}")

project(favor
  VERSION "${FAVOR_VERSION}"
  LANGUAGES Fortran
  DESCRIPTION "Fracture Analysis of Vessels Oak Ridge (Mini App)"
)

if ("${PROJECT_SOURCE_DIR}" STREQUAL "${PROJECT_BINARY_DIR}")
  message(FATAL_ERROR
    "\n"
    "Error: PROJECT_SOURCE_DIR matches PROJECT_BINARY_DIR: ${PROJECT_BINARY_DIR}\n"
    "This project does not support identical build and source paths.\n"
    "Please delete ${PROJECT_SOURCE_DIR}/CMakeCache.txt and ${PROJECT_SOURCE_DIR}/CMakeFiles/"
    "and then build in a different directory.  This may be accomplished in a bash shell by executing\n"
    "  rm -r CMakeCache.txt CMakeFiles/\n"
    "  mkdir build\n"
    "  cd build\n"
    "  export FC=gfortran\n"
    "  cmake .. \n"
    "or by substituting the appropriate syntax for shells other than bash."
  )
endif()

option(ENFORCE_FORTRAN_2018 "Instruct the compiler to return error messages for code not comforming to Fortran 2018")

if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
  if(${ENFORCE_FORTRAN_2018})
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -std=f2018")
  else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall")
  endif()
elseif(CMAKE_Fortran_COMPILER_ID MATCHES Intel)
  if(${ENFORCE_FORTRAN_2018})
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} --standard-semenatics -stand f18")
  else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} --standard-semenatics")
  endif()
elseif(CMAKE_Fortran_COMPILER_ID MATCHES NAG)
  if(${ENFORCE_FORTRAN_2018})
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -f2018")
  else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}")
  endif()
endif()

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/mod")

include(CheckFortranSourceCompiles)

check_fortran_source_compiles("
  program main
  integer :: i
  i = 0
  error stop i
  end program
"
  HAVE_NON_CONSTANT_ERROR_STOP
  SRC_EXT ".f90")
if(HAVE_NON_CONSTANT_ERROR_STOP)
  add_definitions(-DHAVE_NON_CONSTANT_ERROR_STOP)
endif()

check_fortran_source_compiles("
  program main
  contains
  pure function foo() result(res)
    error stop 'Error stop is supported in pure functions (F2018)'
  end function
  end program
"
  HAVE_ERROR_STOP_IN_PURE
  SRC_EXT ".f90"
  )
if(HAVE_ERROR_STOP_IN_PURE)
  add_definitions(-DHAVE_ERROR_STOP_IN_PURE)
endif()

foreach(directory app src)
  add_subdirectory("${directory}")
endforeach()
