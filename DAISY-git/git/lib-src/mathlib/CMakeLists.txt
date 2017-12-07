set(libsrcs
    self_lebedev.f90
    self_minpack.f90
    self_quadpack.f90
    self_spline.f90
    self_vode.f90
)

add_library(
  mathlib STATIC
  ${libsrcs}
)

