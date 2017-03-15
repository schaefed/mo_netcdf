program test

  use mo_slice, only: Slice

  implicit none

  type(Slice) :: slc, slc1
  
  slc = Slice()
  ! slc = Slice(stop=3)

  slc1 = slc%update(20)
  print*, slc1%start, slc1%stop , slc1%step


end program test
