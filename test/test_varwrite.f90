program test_write

  use mo_netcdf, only: NcDataset, NcVariable
  use mo_types
  use mo_testing
  use mo_string
  
  type(NcDataset)   :: nc
  type(NcVariable)  :: var

  integer(i32), parameter   :: nx=10, ny=20, ntime=8
  integer(i32)              :: val, x(nx), y(ny), time(ntime), tstep(nx, ny)
  integer(i32), allocatable :: vals1d(:), vals2d(:,:)

  nc  = NcDataset("writetest.nc", "w")

  var = nc%setVariable(&
       "data", &
       "i32",  &
       [nc%setDimension("x", nx), nc%setDimension("y", ny), nc%setDimension("time")] &
  )

  !! set and get a single value
  !! i.e. (5,5)
  call var%putData(42, start=[5,5,1])
  call nc%sync()
  call var%getData(val, start=[5,5,1])
  call assertEqual(val, 42, "Failed to set a scalar value!")

  !! set and get the all values for the first row in the third timestep
  !! i.e. (:,1,3)
  x = 21
  call var%putData(x, start=[1,1,3])
  call nc%sync()
  call var%getData(vals1d, start=[1,1,3], cnt=[nx,1,1])
  call assertEqual(vals1d, x, "Failed to set values of a 1D array")

  !! set and get the all values for the third column in the sixth timestep
  !! i.e. (3,:,6)
  y = 84
  call var%putData(y, start=[3,1,6], cnt=[1,ny])
  call nc%sync()
  call var%getData(vals1d, start=[3,1,6], cnt=[1,ny])
  call assertEqual(vals1d, y, "Failed to set values of a 1D array")

  !! set and get the all values for the fourth timestep
  !! i.e. (:,:,4)
  tstep = 168
  call var%putData(tstep, start=[1,1,4])
  call nc%sync()
  call var%getData(vals2d, start=[1,1,4], cnt=[nx,ny])
  call assertEqual(vals2d, tstep, "Failed to set values of a 2D array")

  !! set and get the all values for sixth column, fourth row and every second timestep
  ! !! (4,6,1::2)
  ! time = 336
  ! call var%putData(time, start=[6,4], cnt=[1,1,ntime], stride=[1,1,2] )
  ! call nc%sync()
  ! call var%getData(vals3d, start=[6,4], cnt=[1,1,ntime], stride=[1,1,2])
  ! call assertEqual(vals3d, tstep, "Failed to set values of a 3D array")
  
  call nc%close()

end program test_write
