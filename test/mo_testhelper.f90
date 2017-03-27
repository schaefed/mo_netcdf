module mo_testhelper

  use mo_types

  implicit none
  
  interface full
     module procedure full1dI32, full2dI32, full3dI32
  end interface full
contains 

  function full1dI32(len, val) result(out)
    integer(i32), intent(in) :: len, val
    integer(i32)             :: out(len)
    out = val
  end function full1dI32

  function full2dI32(len1, len2, val) result(out)
    integer(i32), intent(in) :: len1, len2, val
    integer(i32)             :: out(len1, len2)
    out = val
  end function full2dI32

  function full3dI32(len1, len2, len3, val) result(out)
    integer(i32), intent(in) :: len1, len2, len3, val
    integer(i32)             :: out(len1, len2, len3)
    out = val
  end function full3dI32

end module mo_testhelper
