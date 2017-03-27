module mo_testing

  use mo_types
  use mo_string

  implicit none

  interface assertEqual
     module procedure assertEqualScalarsI32
  end interface assertEqual

contains

  subroutine exitOut(msg)
    character(len=*), intent(in) :: msg

    print*, msg
    stop 1
  end subroutine exitOut

  subroutine assertEqualScalarsI32(first, second, msg)
    integer(i32),     intent(in)           :: first, second
    character(len=*), intent(in), optional :: msg
    type(String) :: tmp

    if (first /= second) then

       if (present(msg)) then
          tmp = String(msg)
          print*, tmp%get()
       end if

       tmp = String("AssertionError: Values are not equal: ") &
            + String(first) + " - " + String(second)

       call exitOut(tmp%get())
    end if
  end subroutine assertEqualScalarsI32

end module mo_testing
  
