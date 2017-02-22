!> \file mo_kind.f90

!> \brief Define number representations

!> \details This module declares the desired ranges and precisions of the number representations,
!> It confirms mostly with the nrtype module of Numerical Recipes in f90.

!> \authors Juliane Mai, Matthias Cuntz, Nov 2011
!> \date Nov 2011


module mo_kind

  !  Number model from which the SELECTED_REAL_KIND are requested:
  !                   4 byte REAL      8 byte REAL
  !          IEEE:    precision =  6   precision =   15
  !                   exponent  = 37   exponent  =  307
  !          CRAY:        -            precision =   13
  !                                    exponent  = 2465
  
  ! Written  Juliane Mai, Matthias Cuntz, Nov 2011
  ! Modified Matthias Cuntz, Nov 2011 - private/public
  !                                   - documentation
  !                                   - removed tab characters
  !          Matthias Cuntz, May 2014 - iso_fortran_env and iso_c_binding
  !          David Schaefer, Mar 2016 - cleanup, changed c_long to c_long_long  
  !          David Schaefer, Feb 2017 - changed names
  !
  ! License
  ! -------
  ! GNU Lesser General Public License http://www.gnu.org/licenses/


  ! Does not work with compilers intel v11 and sun v12.2
  use, intrinsic :: iso_c_binding,   only: &
             c_short, c_int, c_long_long, c_float, c_double, c_float_complex, c_double_complex, c_bool

  implicit none

  !> 1 Byte Integer Kind
  integer, parameter :: i8  = SELECTED_INT_KIND(2)
  integer, parameter :: i16 = c_short
  integer, parameter :: i32 = c_int
  integer, parameter :: i64 = c_long_long
  integer, parameter :: f32 = c_float
  integer, parameter :: f64 = c_double

end module mo_kind
