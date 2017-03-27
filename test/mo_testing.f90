module mo_testing

  use mo_types
  use mo_string

  implicit none

  interface assertEqual
     module procedure assertEqualScalarsI32, assertEqual1DI32, assertEqual2DI32, &
         assertEqual3DI32
  end interface assertEqual

contains

  subroutine exitOut(msg)
    type(string), intent(in) :: msg
    call printString(msg)
    stop 1
  end subroutine exitOut

  subroutine assertEqualScalarsI32(first, second, msg)
    integer(i32),     intent(in)           :: first, second
    character(len=*), intent(in), optional :: msg
    type(String)                           :: tmp

    if (first /= second) then

       if (present(msg)) then
          call printString(String(msg))
       end if

       tmp = String("AssertionError: Values are not equal: ") &
            + String(first) + " - " + String(second)

       call exitOut(tmp)
    end if
  end subroutine assertEqualScalarsI32

  subroutine assertEqual1DI32(first, second, msg)
    integer(i32), intent(in)               :: first(:), second(:)
    character(len=*), intent(in), optional :: msg

    if (.not. (all(first == second))) then

       if (present(msg)) then
          call printString(String(msg))
       end if
       
       call exitOut(String("AssertionError: Not all array values are equal"))
     end if
   end subroutine assertEqual1DI32

   subroutine assertEqual2DI32(first, second, msg)
    integer(i32), intent(in)               :: first(:,:), second(:,:)
    character(len=*), intent(in), optional :: msg

    if (.not. (all(first == second))) then

       if (present(msg)) then
          call printString(String(msg))
       end if
       
       call exitOut(String("AssertionError: Not all array values are equal"))
     end if
   end subroutine assertEqual2DI32

   subroutine assertEqual3DI32(first, second, msg)
    integer(i32), intent(in)               :: first(:,:,:), second(:,:,:)
    character(len=*), intent(in), optional :: msg

    if (.not. (all(first == second))) then

       if (present(msg)) then
          call printString(String(msg))
       end if
       
       call exitOut(String("AssertionError: Not all array values are equal"))
     end if
   end subroutine assertEqual3DI32

   ! subroutine assertEqual3D2DI32(first, second, msg)
   !  integer(i32), intent(in)               :: first(:,:,:), second(:,:)
   !  character(len=*), intent(in), optional :: msg

   !  ! if (.not. (all(first == second))) then

   !  !    if (present(msg)) then
   !  !       call printString(String(msg))
   !  !    end if
       
   !  !    call exitOut(String("AssertionError: Not all array values are equal"))
   !  !  end if
   ! end subroutine assertEqual3D2DI32


! subroutine assertEqual1dScalarI32(array, scalar, msg)
  !   integer(i32), intent(in)               :: array(:), scalar
  !   character(len=*), intent(in), optional :: msg
  !   type(String)                           :: tmp

  !   if (.not. (all(array == scalar))) then

  !      if (present(msg)) then
  !         call printString(String(msg))
  !      end if
       
  !      tmp = String("AssertionError: Not all array values are equal to: ") + String(scalar)
  !      call exitOut(tmp)
  !    end if

  !  end subroutine assertEqual1dScalarI32

end module mo_testing
  
