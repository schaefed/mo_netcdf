program example_write

  use mo_netcdf, only: NcDataset, NcDimension, NcVariable, NcGroup

  implicit none
  
  type(NcDataset)   :: nc
  type(NcDimension) :: dim1, dim2, dim3
  type(NcVariable)  :: var
  type(NcGroup)     :: grp

  ! some data
  integer, parameter :: nx=10, ny=20, ntime=8
  integer            :: data(nx, ny, ntime), i

  data = 42

  ! open a dataset in write mode
  ! args:
  !     filename
  !     mode ("w": write, "r": read-only, "a": read-write)
  nc  = NcDataset("test.nc", "w")
  grp = nc%setGroup("group")

  ! create dimensions
  ! args:
  !     dimension name 
  !     dimension length (< 0 for an unlimited dimension)
  dim1 = grp%setDimension("time", -1)
  dim2 = grp%setDimension("y", ny)
  dim3 = grp%setDimension("x", nx)

  ! set a variable
  ! args:
  !     variable name
  !     data type (currently available: "i8", "i16", "i32", "f32", "f64")
  !     dimensions array
  var = grp%setVariable("data", "i32", (/dim3, dim2, dim1/))

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
  call var%putData(data)
  ! write one value at position (5,5,1) (indexing: x, y, time)
  call var%putData(21, start=(/5,5,1/))
  ! write a 2D array into the 3D data array at position (1,1,4)
  call var%putData(data(:,:,1)/2, start=(/1,1,4/))

  ! dynamically append some data along the time dimension
  do i = ntime+2, ntime+12
     call var%putData(data(:,:,1)+i, start=(/1,1,i/))
  end do

  ! add a group attribute, attributes can be set to NcDataset, NcGroup and NcVariable
  ! args:
  !    name
  !    any of the supported datatypes
  call grp%setAttribute("auxiliar author", "Ricardo Torres")
  ! add a global attribute
  ! args:
  !    name
  !    any of the supported datatypes
  call nc%setAttribute("author", "David Schaefer")

  ! close the dataset
  call nc%close()

end program example_write
