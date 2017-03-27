module mo_types

  use, intrinsic :: iso_c_binding, only: c_short, c_int, c_long_long, c_float, c_double

  implicit none

  integer, parameter :: i8  = SELECTED_INT_KIND(2)
  integer, parameter :: i16 = c_short
  integer, parameter :: i32 = c_int
  integer, parameter :: i64 = c_long_long
  integer, parameter :: f32 = c_float
  integer, parameter :: f64 = c_double

end module mo_types
