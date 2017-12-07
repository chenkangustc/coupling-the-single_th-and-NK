# ==============================================================================
# cmake file for DAISY code
# version 1.0.0
# 2017-02-25
# ==============================================================================
cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project(DAISY Fortran)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)
include_directories(${CMAKE_BINARY_DIR}/include)

# WIN32_/WIN64_/LINUX_
if(MSVC_IDE OR UNIX)
    if(MSVC_IDE)
        if(CMAKE_CL_64)
            set(WIN64_ TRUE)
            set(prefix "lib-64bit")
            set(prehdf5 "lib-64bit")
        else()
            set(WIN32_ TRUE)
            set(prefix "lib-32bit")
            set(prehdf5 "lib-32bit")
        endif()
    else()
        set(LINUX_ TRUE)
        set(prefix "lib-linux")
        set(prehdf5 "/apps/compile/hdf5/1.8.16-intel-16.0.0")
    endif()
else()
    message("Their are no libraries for ${CMAKE_SYSTEM}")
endif()
set(presrc "lib-src")

message("Cmake version: ${CMAKE_VERSION}")
message("Platform: ${CMAKE_SYSTEM}")
message("Sources: ${CMAKE_SOURCE_DIR}/src")
message("Build types: ${CMAKE_CONFIGURATION_TYPES}")

# ==============================================================================
# libraries
# pre-compiled binary libraries or build from src
option(USE_COMPILED_LAPACK  "Whether or not to use a pre-compiled LAPACK library"  ON)
option(USE_COMPILED_MATHLIB "Whether or not to use a pre-compiled MATHLIB library" ON)
option(USE_COMPILED_REFPROP "Whether or not to use a pre-compiled REFPROP library" ON)
option(USE_COMPILED_XMLFOX  "Whether or not to use a pre-compiled XMLFOX library"  ON)

# hdf5
if(WIN32_ OR WIN64_)
    message("HDF5 library: ${CMAKE_SOURCE_DIR}/${prehdf5}")
    set(inc_hdf5    "${prehdf5}/hdf5/include") 
    set(lib_hdf5    "${prehdf5}/hdf5/lib") 
    link_directories(${CMAKE_SOURCE_DIR}/${lib_hdf5})
    list(APPEND libraries "hdf5")
    list(APPEND libraries "hdf5_fortran")
    list(APPEND libraries "hdf5_hl")
    list(APPEND libraries "hdf5_hl_fortran")
elseif(LINUX_)
    message("HDF5 library: ${prehdf5}")
    set(inc_hdf5    "${prehdf5}/include") 
    set(lib_hdf5    "${prehdf5}/lib") 
    link_directories(${lib_hdf5})
    list(APPEND libraries "hdf5")
    list(APPEND libraries "hdf5_fortran")
    list(APPEND libraries "hdf5_hl")
    list(APPEND libraries "hdf5hl_fortran")
else()
endif()

# lapack
if(USE_COMPILED_LAPACK)
    message("LAPACK library: ${CMAKE_SOURCE_DIR}/${prefix}/lapack")
    set(inc_lapack  "${prefix}/lapack/include") 
    set(lib_lapack  "${prefix}/lapack/lib") 
    link_directories(${CMAKE_SOURCE_DIR}/${lib_lapack})
else()
    message("LAPACK library: ${CMAKE_SOURCE_DIR}/${presrc}/lapack")
    add_subdirectory(${presrc}/lapack)
endif()
list(APPEND libraries "blas")
list(APPEND libraries "lapack")

# mathlib
if(USE_COMPILED_MATHLIB)
    message("MATHLIB library: ${CMAKE_SOURCE_DIR}/${prefix}/mathlib")
    set(inc_mathlib  "${prefix}/mathlib/include") 
    set(lib_mathlib  "${prefix}/mathlib/lib") 
    link_directories(${CMAKE_SOURCE_DIR}/${lib_mathlib})
else()
    message("MATHLIB library: ${CMAKE_SOURCE_DIR}/${presrc}/mathlib")
    add_subdirectory(${presrc}/mathlib)
endif()
list(APPEND libraries "mathlib")

# refprop
if(USE_COMPILED_REFPROP)
    message("REFPROP library: ${CMAKE_SOURCE_DIR}/${prefix}/refprop")
    set(inc_refprop  "${prefix}/refprop/include") 
    set(lib_refprop  "${prefix}/refprop/lib") 
    link_directories(${CMAKE_SOURCE_DIR}/${lib_refprop})
else()
    message("REFPROP library: ${CMAKE_SOURCE_DIR}/${presrc}/refprop")
    add_subdirectory(${presrc}/refprop)
endif()
list(APPEND libraries "refprop")

# xmlfox
if(USE_COMPILED_XMLFOX)
    message("XMLFOX library: ${CMAKE_SOURCE_DIR}/${prefix}/xmlfox")
    set(inc_xmlfox  "${prefix}/xmlfox/include") 
    set(lib_xmlfox  "${prefix}/xmlfox/lib") 
    link_directories(${CMAKE_SOURCE_DIR}/${lib_xmlfox})
else()
    message("XMLFOX library: ${CMAKE_SOURCE_DIR}/${presrc}/xmlfox")
    add_subdirectory(${presrc}/xmlfox)
endif()
list(APPEND libraries "fox_common")
list(APPEND libraries "fox_dom")
list(APPEND libraries "fox_fsys")
list(APPEND libraries "fox_sax")
list(APPEND libraries "fox_utils")
list(APPEND libraries "fox_wxml")

# ==============================================================================
# compiler
if(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
    if(WIN32_ OR WIN64_)
        list(APPEND f90flags "/Qopenmp")
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /STACK:400000000")
        set(CMAKE_EXE_LINKER_FLAGS_DEBUG "${CMAKE_EXE_LINKER_FLAGS_DEBUG} /NODEFAULTLIB:MSVCRT.lib")
#        if(USE_COMPILED_LAPACK OR USE_COMPILED_MATHLIB OR USE_COMPILED_REFPROP OR USE_COMPILED_XMLFOX)
#            set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /NODEFAULTLIB:MSVCRT.lib") 
#            set(CMAKE_EXE_LINKER_FLAGS_DEBUG "${CMAKE_EXE_LINKER_FLAGS_DEBUG} /NODEFAULTLIB:LIBCMT.lib") 
#            set(CMAKE_EXE_LINKER_FLAGS_DEBUG "${CMAKE_EXE_LINKER_FLAGS_DEBUG} /NODEFAULTLIB:libifcoremt.lib") 
#        else()
#            set(CMAKE_EXE_LINKER_FLAGS_DEBUG "${CMAKE_EXE_LINKER_FLAGS_DEBUG} /NODEFAULTLIB:MSVCRT.lib")
#        endif()
    elseif(LINUX_)
        list(APPEND f90flags "-openmp")
        list(APPEND ldflags "-openmp")
    else()
    endif()
else()
    message("Their are no libraries for ${CMAKE_Fortran_COMPILER_ID}")
endif()

# openmp
#find_package(OpenMP REQUIRED)
#if(OPENMP_FOUND)
#    message("OpenMP support")
#    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
#endif()

# ==============================================================================
set(program "DAISY")
file(GLOB source 
    src/driver/*.f90 
    src/frame/*.f90 
    src/header/*.f90 
    src/inout/*.f90 
    src/NK2solver/*.f90 
    src/solver/*.f90 
    src/thermal/*.f90 
)

add_executable(${program} ${source})

if(CMAKE_VERSION VERSION_LESS 2.8.11)
  include_directories(${inc_hdf5})
  if(USE_COMPILED_LAPACK)
    include_directories(${inc_lapack})
  endif()
  if(USE_COMPILED_MATHLIB)
    include_directories(${inc_mathlib})
  endif()
  if(USE_COMPILED_REFPROP)
    include_directories(${inc_refprop})
  endif()
  if(USE_COMPILED_XMLFOX)
    include_directories(${inc_xmlfox})
  endif()
else()
  target_include_directories(${program} PUBLIC ${inc_hdf5})
  if(USE_COMPILED_LAPACK)
    target_include_directories(${program} PUBLIC ${inc_lapack})
  endif()
  if(USE_COMPILED_MATHLIB)
    target_include_directories(${program} PUBLIC ${inc_mathlib})
  endif()
  if(USE_COMPILED_REFPROP)
    target_include_directories(${program} PUBLIC ${inc_refprop})
  endif()
  if(USE_COMPILED_XMLFOX)
    target_include_directories(${program} PUBLIC ${inc_xmlfox})
  endif()
endif()

if(CMAKE_VERSION VERSION_LESS 2.8.12)
  string(REPLACE ";" " " f90flags "${f90flags}")
  set_property(TARGET ${program} PROPERTY COMPILE_FLAGS "${f90flags}")
else()
  target_compile_options(${program} PUBLIC ${f90flags})
endif()

target_link_libraries(${program} ${ldflags} ${libraries})
