module mo_slice

  use, intrinsic :: iso_c_binding, only: c_int

  implicit none

  integer, parameter :: i32 = c_int

  type :: Slice
     integer(i32) :: start =  1
     integer(i32) :: stop  = -1
     integer(i32) :: step  =  1

   contains
     procedure :: update

  end type Slice

  interface Slice
     procedure newSlice
  end interface Slice

contains

  function newSlice(start, stop, step)
    integer(i32), intent(in), optional :: start, stop, step
    type(Slice)                        :: newSlice

    if (present(start)) then
       newSlice%start = start
    end if

    if (present(stop)) then
       newSlice%stop = stop
    end if

    if (present(step)) then
       newSlice%step = step
    end if

  end function newSlice
  
  function update(self, length)
    class(Slice), intent(in) :: self
    integer(i32), intent(in) :: length
    type(Slice)              :: update
    
    update = self
    if (update%stop < 1) then
       update%stop = length + update%stop + 1
    end if

  end function update
  
end module mo_slice
  
