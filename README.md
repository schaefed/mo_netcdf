#mo_netcdf

#General
A somewhat object-oriented wrapper around the NetCDF Fortran 90 interface.

#Requirements
- NetCDF fortran 90 interface

#Compiler support
The module is tested with the following compilers:
- GNU gfortran 4.8
- Intel ifort 13.1 
- NAG nagfor 6.0
- PGI pgfortran 15.9

#Usage
More detailed information about routine/interface signatures can be found in mo_netcdf.f90

Write a netcdf file:

```fortran
use mo_netcdf, only: NcDataset, NcDimension, NcVariable

type(NcDataset)   :: nc
type(NcDimension) :: dim1, dim2, dim3
type(NcVariable)  :: var

! some data
integer, parameter :: nx=10, ny=20, ntime=8
integer            :: data(nx, ny, ntime), i

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
```

Read data from file:

```fortran

use mo_netcdf, only: NcDataset, NcDimension, NcVariable

type(NcDataset)   :: nc
type(NcVariable)  :: var

integer, allocatable :: data(:,:,:)

! open a dataset in read(-write) mode
nc = NcDataset("test.nc", "r")

! access a variable
! args:
!     variable name
var = nc%getVariable("data")

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
call var%getData(data, count=(/5,5,10/))
! read every second timestep starting from the 3
call var%getData(data, start=(/1,1,3/), stride=(/1,1,2/))

call nc%close()

```

# Restrictions
The current implementation provides a fairly larg subset of the NetCDF Fortran 90 interface as 
described in the [User's Guide](http://www.csar.cfs.ac.uk/user_information/software/environment/guide.book.pdf).
Beside some minor functionality (e.g. ```nf90_rename_var```) the most important shortcoming is the missing 
group support. This can and will be fixed as soon as this functionality is needed by the author or contributed.


