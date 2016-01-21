program example_mo_netcdf

  use mo_kind  , only : i4, sp, dp
  use mo_netcdf, only: NcDataset, NcDimension, NcVariable

  implicit none
  
  type(NcDataset)   :: nc
  type(NcDimension) :: dim1, dim2, dim3
  type(NcVariable)  :: var

  ! some data
  integer(i4), parameter :: nx=10, ny=20, ntime=8
  integer(i4)            :: data(nx, ny, ntime), i

  data = 42

  ! open a dataset in write mode
  ! args:
  !     filename
  !     mode ("w": write, "r": read-only, "a": read-write)
  nc = NcDataset("test.nc", "w") 

  ! create dimensions
  ! args:
  !     dimension name 
  !     dimension length (< 0 for an unlimited dimension)
  dim1 = nc%setDimension("time", -1)
  dim2 = nc%setDimension("y", ny)
  dim3 = nc%setDimension("x", nx)

  ! set a variable
  ! args:
  !     variable name
  !     data type (currently available: "i8", "i16", "i32", "f32", "f64")
  !     dimensions array
  var = nc%setVariable("data", "i32", (/dim3, dim2, dim1/))

  ! define a fill value
  ! args:
  !     one of the supported datatypes
  call var%setFillValue(-9999)

  ! add variable attributes
  ! args:
  !    name
  !    any of the supported datatypes
  call var%setAttribute("attr1", "42")
  call var%setAttribute("attr2", 42)
  call var%setAttribute("attr3", 42.0)

  ! write data
  ! args:
  !     data: scalar/array of any of the supported datatypes
  !     start  (optional): array of indices where the the first/only data value will be written
  !     count  (optional): array of integers specifying the number of indices along selected along each dimension
  !     stride (optional): array of integers specifying the sampling interval of each dimension 
  !     map    (optional): see nf90_put_var 
  ! write the entire variable
  call var%setData(data)
  ! write one value at position (5,5,1) (indexing: x, y, time)
  call var%setData(21, start=(/5,5,1/))
  ! write on 2D array into the 3D data array at position (1,1,4)
  call var%setData(data(:,:,1)/2, start=(/1,1,4/))

  ! dynamically append some data along the time dimension
  do i = ntime+2, ntime+12
     call var%setData(data(:,:,1)+i,start=(/1,1,i/))
  end do

  ! add a global attribute
  ! args:
  !    name
  !    any of the supported datatypes
  call nc%setAttribute("author", "Alfred Mustermann")

  ! close the dataset
  call nc%close()

end program example_mo_netcdf
