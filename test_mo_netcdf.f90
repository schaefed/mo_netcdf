! ---------------------------------------------------------------------------------------
!
! mo_netcdf test program
!
! author: David Schaefer
!
! created: June 2015
!
! ----------------------------------------------------------------------------------------

program test_mo_netcdf


  use mo_kind  , only : i4, sp, dp
  use mo_netcdf, only : NcDataset, NcDimension, NcVariable
  ! use mo_utils , only : equal
  
  implicit none

  ! logical                  :: correct
  ! integer(i4),  parameter  :: ntime=30, ny=15, nx=25, nadd=12

  ! character(*), parameter  :: fname="netcdf_make_check_test_file"
  ! character(*), parameter  :: vname_time="time", vname_lat="lat", vname_lon="lon", vname_data="data"
  ! character(64)            :: wavalue, ravalue
  
  ! type(NcDataset)          :: nc
  ! type(NcDimension)        :: dim_x, dim_y, dim_time
  ! type(NcVariable)         :: var_lon, var_lat, var_time, var_data

  ! integer(i4)              :: wtime(ntime+nadd),  i
  ! integer(i4), allocatable :: rtime(:)
  ! real(sp)                 :: wlat(nx,ny), wlon(nx,ny)
  ! real(sp),    allocatable :: rlat(:,:),rlon(:,:)
  ! real(dp)                 :: wdata(nx,ny,ntime+nadd), wfvalue, rfvalue
  ! real(dp),    allocatable :: rdata(:,:,:)

  ! ------------------------------------------
  ! 0. Initialization ...
  ! ------------------------------------------


  type(NcDataset)   :: nc
  type(NcDimension) :: dim1, dim2, dim3
  type(NcVariable)  :: var

  ! some data
  integer, parameter :: nx=10, ny=20, ntime=8
  integer            :: data(nx, ny, ntime), i, j
  integer, allocatable :: rdata(:,:,:)

  data = 42
  
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
  !     stride (optional): array of integers specifying the sampling interval alonf each dimension 
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

  call nc%close()


  nc = NcDataset("test.nc", "r")
  var = nc%getVariable("data")

  ! read the entire variable
  call var%getData(rdata)
  ! read all but the first 10 time steps
  call var%getData(rdata, start=(/1,1,10/))
  ! read the first 5 columns and rows of the first 10 timesteps
  call var%getData(rdata, count=(/5,5,10/))
  ! read every second timestep starting from the 3
  call var%getData(rdata, start=(/1,1,3/), stride=(/1,1,2/))

  
  call nc%close()
 
  ! ! assume the program is correct up to this point ...
  ! correct = .true.
  
  ! ! generate some dummy data
  ! wfvalue = -9999._dp
  ! wavalue = "David Schaefer"
  ! wtime = (/ (i, i = 1, ntime+nadd ) /)
  ! do i=1, nx
  !    wlon(i,:) = real(i-0.5_sp,sp)
  ! end do
  ! do i=1, ny
  !    wlat(:,i) = real(i-0.5_sp,sp)
  ! end do  
  ! do i=1, ntime+nadd
  !    wdata(:,:,i) = (wlon + wlat) * i
  ! end do

  ! ! --------------------------------------------
  ! ! 1. Set a file and dynamically append data
  ! ! --------------------------------------------
  
  ! ! 1.1 set a file
  ! nc = NcDataset(fname, "w")

  ! ! set dimensions
  ! dim_x    = nc%setDimension("x",    nx) ! lenght < 0 -> unlimited dimension
  ! dim_y    = nc%setDimension("y",    ny)
  ! dim_time = nc%setDimension("time", -1)

  ! ! set variables
  ! var_time = nc%setVariable(vname_time, "i32", (/dim_time/))
  ! var_lat  = nc%setVariable(vname_lat,  "f32", (/dim_x, dim_y/))
  ! var_lon  = nc%setVariable(vname_lon , "f32", (/dim_x, dim_y/))
  ! var_data = nc%setVariable(vname_data, "f64", (/dim_x, dim_y, dim_time/))

  ! ! add some variable attributes
  ! call var_time%setAttribute("units", "days since 1989-12-31 12:00:00")

  ! ! set fill value before any data is written
  ! call var_data%setFillValue(wfvalue)
  
  ! ! write data of static variables
  ! call var_lat%setData(wlat)
  ! call var_lon%setData(wlon)
    
  ! ! append data within a loop
  ! do i=1, ntime
  !    call var_time%setData(wtime(i),     start=(/i/))
  !    call var_data%setData(wdata(:,:,i), start=(/1,1,i/))
  ! end do

  ! ! add some more variable attributes
  ! call var_data%setAttribute("units",   "mm/d")
  ! call var_data%setAttribute("scaling", 0.1_dp)

  ! ! add global attributes
  ! call nc%setAttribute("Author", wavalue)
  ! call nc%setAttribute("Year",   2099_i4)

  ! ! close the file
  ! call nc%close()

  
  ! ! 1.2. Read the written data
  ! ! --------------------------

  ! ! open dataset
  ! nc = NcDataset(fname,"r")

  ! ! acces the variable
  ! var_time = nc%getVariable(vname_time)
  ! var_lat  = nc%getVariable(vname_lat)
  ! var_lon  = nc%getVariable(vname_lon)
  ! var_data = nc%getVariable(vname_data)
  
  ! ! read the data
  ! call var_time%getData(rtime)
  ! call var_lat%getData(rlat)
  ! call var_lon%getData(rlon)
  ! call var_data%getData(rdata)

  ! ! read the fill value
  ! call var_data%getFillValue(rfvalue)

  ! ! read a global attribute
  ! call nc%getAttribute("Author", ravalue)

  ! ! close dataset
  ! call nc%close()

  ! ! 1.3 Check
  ! ! ---------
  ! if (.not. all(rtime .eq. wtime(1:ntime)))        correct = .false.
  ! if (.not. all(equalSp(rlat, wlat)))                correct = .false.
  ! if (.not. all(equalSp(rlon, wlon)))                correct = .false.
  ! if (.not. all(equalDp(rdata, wdata(:,:,1:ntime)))) correct = .false.
  ! if (.not. equalDp(rfvalue, rfvalue))               correct = .false.
  ! if (.not. (ravalue .eq. wavalue))                correct = .false.

  ! ! --------------------------------
  ! ! 2. Append to an existing dataset
  ! ! --------------------------------
  ! !
  ! ! 2.1 Write data
  ! ! --------------
  ! !
  ! ! open dataset
  ! nc = NcDataset(fname,"a")

  ! ! acces variable
  ! var_time = nc%getVariable(vname_time)
  ! var_data = nc%getVariable(vname_data)

  ! ! append data within a loop
  ! do i=ntime+1, ntime+nadd
  !    call var_time%setData(wtime(i),     start=(/i/))
  !    call var_data%setData(wdata(:,:,i), start=(/1,1,i/))
  ! end do

  ! ! close dataset
  ! call nc%close()
  
  ! ! 2.2 Read the appended data
  ! ! ---------------------------
  ! !
  ! ! open dataset
  ! nc = NcDataset(fname,"r")

  ! ! acces the variable
  ! var_time = nc%getVariable(vname_time)
  ! var_data = nc%getVariable(vname_data)

  ! ! read the appended chunk of data
  ! call var_time%getData(rtime, start=(/ntime+1/), count=(/nadd/))
  ! call var_data%getData(rdata, start=(/1,1,ntime+1/), count=(/nx,ny,nadd/))

  ! ! close dataset
  ! call nc%close()

  ! ! 2.3 Check
  ! ! ---------
  ! if (.not. all(rtime(1:nadd) == wtime(ntime+1:ntime+nadd)))              correct = .false.
  ! if (.not. all(equalDp(rdata(:,:,1:nadd), wdata(:,:,ntime+1:ntime+nadd)))) correct = .false.
  
  
  ! ! ----------------------------------
  ! ! 3. Dump some data - the short form
  ! ! ----------------------------------

  ! ! 3.1 Write data

  ! ! Fast dump
  ! ! open a file
  ! nc = NcDataset(fname, "w")

  ! ! set variable and dimensions
  ! var_data = nc%setVariable(vname_data, "f64", (/ &
  !      nc%setDimension("x", size(wdata,1)),       &
  !      nc%setDimension("y", size(wdata,2)),       &
  !      nc%setDimension("time", -1) /)             &
  !      )
  
  ! ! write data
  ! call var_data%setData(wdata)

  ! ! close the file
  ! call nc%close()

  
  ! ! Fast dump with additional time dimension
  ! ! open a file
  ! nc = NcDataset(fname, "w")

  ! ! set variable and dimensions
  ! dim_time = nc%setDimension("time", -1)
  ! var_time = nc%setVariable(vname_time, "i32", (/dim_time/))
  ! var_data = nc%setVariable(vname_data, "f64", (/ &
  !      nc%setDimension("x", nx),              &
  !      nc%setDimension("y", ny),              &
  !      dim_time /)      &
  !      )
  
  ! ! write data
  ! call var_time%setData(wtime)
  ! call var_data%setData(wdata)

  ! ! close the file
  ! call nc%close()

  
  ! ! 3.2 Read the dumped data
  ! ! --------------------------
  ! ! open dataset
  ! nc = NcDataset(fname,"r")

  ! ! ! acces the variable
  ! var_time = nc%getVariable(vname_time)
  ! var_data = nc%getVariable(vname_data)
  
  ! ! read the data
  ! call var_time%getData(rtime)
  ! call var_data%getData(rdata)

  ! ! close dataset
  ! call nc%close()

  ! ! 3.3 check
  ! ! ---------
  ! if (.not. all(rtime == wtime))      correct = .false.
  ! if (.not. all(equalDp(rdata, wdata))) correct = .false.

  ! ! --------------------------------------------------------------------------------------
  ! ! The moment of truth ...
  ! if (correct) then
  !    print*, "mo_netcdf is o.k."
  ! else
  !    print*, "mo_netcdf failed."
  ! endif

contains

  elemental function equalDp(a, b)

    implicit none

    real(dp), intent(in) :: a, b
    logical              :: equalDp

    if ((epsilon(1.0_dp)*abs(b) - abs(a-b)) < 0.0_dp) then
       equalDp = .false.
    else
       equalDp = .true.
    endif

  end function equalDp


  elemental function equalSp(a, b)

    implicit none

    real(sp), intent(in) :: a, b
    logical              :: equalSp

    if ((epsilon(1.0_sp)*abs(b) - abs(a-b)) < 0.0_sp) then
       equalSp = .false.
    else
       equalSp = .true.
    endif

  end function equalSp


  
end program test_mo_netcdf
