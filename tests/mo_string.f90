module mo_string

  use mo_types

  implicit none
 
  interface toString
     module procedure toStringI32
  end interface toString

  type String
     character(:), allocatable :: data

     contains
       procedure, public :: get
  end type String

  interface String
     procedure newStringEmpty, newStringChar, newStringI32
  end interface String

  interface operator(+)
     procedure concatStringString, concatStringChar
  end interface operator(+)

contains

  function newStringEmpty() result(out)
    type(String)                 :: out

    out%data = ""
  end function newStringEmpty
 
  function newStringChar(arg) result(out)
    character(len=*), intent(in) :: arg
    type(String)                 :: out

    ! if (present(arg)) then
    out%data = arg
    ! else
    !    out%data = ""
    ! end if
  end function newStringChar

  function newStringI32(arg) result(out)
    integer(i32), intent(in) :: arg
    type(String)             :: out
    
    out%data = trim(adjustl(toString(arg)))
  end function newStringI32


  function concatStringString(left, right) result(out)
    class(String), intent(in) :: left, right
    type(String) :: out

    out = String(left%data//right%data)
  end function concatStringString
  
  function concatStringChar(left, right) result(out)
    class(String), intent(in)    :: left
    character(len=*), intent(in) :: right
    type(String) :: out

    out = String(left%data//right)
  end function concatStringChar


  function get(self) result(out)
    class(String), intent(in) :: self
    character(:), allocatable :: out
    
    out = self%data
  end function get

  subroutine printString(str)
    type(String), intent(in) :: str

    print*, str%get()
  end subroutine printString

  function joinStrings(strings, sep) result(out)
    type(String),     intent(in)           :: strings(:)
    character(len=*), intent(in), optional :: sep
    integer(i32)                           :: i
    type(String)                           :: tmp
    type(String)                           :: out

    if (.not.(present(sep))) then
       tmp = String("")
    else
       tmp = String(sep)
    end if

    out = String()
    do i=1,size(strings)
       out = out + strings(i)
       if (i .le. size(strings)-1) then
          out = out + tmp
       end if
    end do
    
  end function joinStrings


  function toStringI32(arg) result(out)
    integer(i32), intent(in) :: arg
    character(len=256)       :: out
    write (out, *) arg
  end function toStringI32

end module mo_string
