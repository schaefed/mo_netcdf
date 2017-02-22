program example_read

  use mo_kind, only: i32
  use mo_netcdf, only: NcDataset, NcDimension, NcVariable, NcGroup

  type(NcDataset)   :: nc
  type(NcVariable)  :: var
  type(NcGroup)     :: grp
  
  integer(i32), allocatable :: data(:,:,:)
  integer(i32)              :: att_val
  character(len=80)         :: author1, author2
  
  ! open a dataset in read-only mode
  nc = NcDataset("test.nc", "r")

  ! open the group where the data is
  grp = nc%getGroup("group")
  
  ! access a variable
  ! args:
  !     variable name
  var = grp%getVariable("data")
  
  ! read data
  ! args:
  !     data: allocatable array of any of the supported datatypes
  !     start  (optional): array of indices of the first value to be read
  !     count  (optional): array of integers specifying the number of elements to read along each dimension
  !     stride (optional): array of integers specifying the sampling interval of each dimension 
  !     map    (optional): see nf90_put_var 
  ! read the entire variable
  call var%getData(data)

  ! read all but the first 10 time steps
  call var%getData(data, start=(/1,1,10/))

  ! read the first 5 columns and rows of the first 10 timesteps
  call var%getData(data, cnt=(/5,5,10/))

  ! read every second timestep starting from the 3
  call var%getData(data, start=(/1,1,3/), stride=(/1,1,2/))
  
  ! read attributes
  call var%getAttribute('attr2', att_val)
  call grp%getAttribute('auxiliar author', author2)
  call nc %getAttribute('author', author1)
  
  call nc%close()
  
end program example_read
