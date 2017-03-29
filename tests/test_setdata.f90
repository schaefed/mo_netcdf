program test_putdata

  use mo_netcdf, only: NcDataset, NcVariable
  use mo_types
  use mo_assert
  use mo_string
  use mo_testhelper
  
  type(NcDataset)   :: nc
  type(NcVariable)  :: var

  integer(i32), parameter   :: nx=10, ny=20, ntime=8
  integer(i32)              :: val
  integer(i32), allocatable :: write1d(:), write2d(:,:), write3d(:,:,:)
  integer(i32), allocatable :: read1d(:), read2d(:,:), read3d(:,:,:)

  nc  = NcDataset("test_setdata.nc", "w")

  var = nc%setVariable(&
       "data", &
       "i32",  &
       [nc%setDimension("x", nx), nc%setDimension("y", ny), nc%setDimension("time")] &
  )

  ! set and get a single value
  ! i.e. (5,5)
  call var%setData(42, start=[5,5])
  call var%getData(val, start=[5,5])
  call assertEqual(val, 42, "Failed to set a scalar value!")

  ! set and get the all values for the first row in the third timestep
  ! i.e. (:,1,3)
  write1d = full(nx, 21_i32)
  call var%setData(write1d, start=[1,1,3])
  call var%getData(read1d, start=[1,1,3], cnt=[nx,1,1])
  call assertEqual(read1d, write1d, "Failed to set values of a 1D array")

  ! set and get the all values for the third column in the sixth timestep
  ! i.e. (3,:,6)
  write1d = full(ny, 84_i32)
  call var%setData(write1d, start=[3,1,6], cnt=[1,ny,1])
  call var%getData(read1d, start=[3,1,6], cnt=[1,ny,1])
  call assertEqual(read1d, write1d, "Failed to set values of a 1D array")

  ! set and get the all values for sixth column, fourth row and every second timestep
  ! (4,6,1::2)
  write1d = full(ntime, 168_i32)
  call var%setData(write1d, start=[6,4], cnt=[1,1,ntime], stride=[1,1,2] )
  call var%getData(read1d, start=[6,4], cnt=[1,1,ntime], stride=[1,1,2])
  call assertEqual(read1d, write1d, "Failed to set values of a 3D array")
 
  ! set and get the all values for the fourth timestep
  ! i.e. (:,:,4)
  write2d = full(nx, ny, 168)
  call var%setData(write2d, start=[1,1,4])
  call var%getData(read2d, start=[1,1,4], cnt=[nx,ny,1])
  call assertEqual(read2d, write2d, "Failed to set values of a 2D array")

  ! set and get the all values for every third timestep
  ! i.e. (:,:,1::2)
  write3d = full(nx, ny, ntime, 336)
  call var%setData(write3d, cnt=[nx,ny,ntime], stride=[1,1,2] )
  call var%getData(read3d, cnt=[nx,nx,ntime], stride=[1,1,2])
  call assertEqual(read3d, write3d, "Failed to set values of a 3D array")

  call nc%close()

end program test_putdata
