!> \file mo_kind.f90

!> \brief Define number representations

!> \details This module declares the desired ranges and precisions of the number representations,
!> such as single precision or double precision, 32-bit or 46-bit integer, etc.
!> It confirms mostly with the nrtype module of Numerical Recipes in f90.

!> \authors Juliane Mai, Matthias Cuntz, Nov 2011
!> \date 2011-2014
!> \copyright GNU Lesser General Public License http://www.gnu.org/licenses/

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
! License
! -------
! This file is part of the UFZ Fortran library.

! The UFZ Fortran library is free software: you can redistribute it and/or modify
! it under the terms of the GNU Lesser General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.

! The UFZ Fortran library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
! GNU Lesser General Public License for more details.

! You should have received a copy of the GNU Lesser General Public License
! along with the UFZ Fortran library (cf. gpl.txt and lgpl.txt).
! If not, see <http://www.gnu.org/licenses/>.

! Copyright 2011-2014 Matthias Cuntz, Juliane Mai

MODULE mo_kind

  ! Does not work with compilers intel v11 and sun v12.2
  use, intrinsic :: iso_c_binding,   only: &
             c_short, c_int, c_long_long, c_float, c_double, c_float_complex, c_double_complex, c_bool

  IMPLICIT NONE

  !> 1 Byte Integer Kind
  INTEGER, PARAMETER :: i1  = SELECTED_INT_KIND(2)
  INTEGER, PARAMETER :: i2  = c_short
  INTEGER, PARAMETER :: i4  = c_int
  INTEGER, PARAMETER :: i8  = c_long_long
  INTEGER, PARAMETER :: sp  = c_float
  INTEGER, PARAMETER :: dp  = c_double
  INTEGER, PARAMETER :: spc = c_float_complex
  INTEGER, PARAMETER :: dpc = c_double_complex
  INTEGER, PARAMETER :: lgt = KIND(.true.)

END MODULE mo_kind
