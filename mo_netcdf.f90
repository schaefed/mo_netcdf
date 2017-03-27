!> \file mo_netcdf.f90

!> \brief NetCDF Fortran 90 interface wrapper

!> \details A wrapper around the NetCDF Fortran 90 interface.
!
!> \authors David Schaefer
!> \date Jun 2015

module mo_netcdf

  ! This module provides a thin wrapper around the NetCDF Fortran 90 interface,
  ! following a object-oriented approach.

  ! Written  David Schaefer, Jun 2015
  ! Modified Matthias Cuntz, Jan 2016 - compiled with PGI Fortran rev 15.9 - no automatic allocation of left-hand-side
  ! Modified Ricardo Torres, Feb 2017 - add derived type NcGroup and NcAttributable. NcAttributable is the base derived type,
  !                                     NcGroup and NcVariable are extended from it. NcDataset is extended from NcGroup. No more
  !                                     duplicated routines to set attributes.

  ! License
  ! -------
  ! GNU Lesser General Public License http://www.gnu.org/licenses/

  use, intrinsic :: iso_c_binding, only: c_short, c_int, c_long_long, c_float, c_double
       
  use netcdf,          only: &
       nf90_open, nf90_close, nf90_strerror, nf90_def_dim, nf90_def_var,   &
       nf90_put_var, nf90_get_var, nf90_put_att, nf90_get_att,             &
       nf90_inquire, nf90_inq_dimid, nf90_inquire_dimension,               &
       nf90_inq_varid, nf90_inq_varids, nf90_inquire_variable, nf90_inquire_attribute,     &
       nf90_inq_ncid, nf90_inq_grp_parent, nf90_inq_grpname, nf90_def_grp, &
       nf90_rename_dim, nf90_rename_var, nf90_rename_att, nf90_sync,       &
       NF90_OPEN, NF90_NETCDF4, NF90_CREATE, NF90_WRITE, NF90_NOWRITE,     &
       NF90_BYTE, NF90_SHORT, NF90_INT, NF90_FLOAT, NF90_DOUBLE,           &
       NF90_FILL_BYTE, NF90_FILL_SHORT, NF90_FILL_INT, NF90_FILL_FLOAT , NF90_FILL_DOUBLE, &
       NF90_NOERR, NF90_UNLIMITED, NF90_GLOBAL, NF90_SHARE, NF90_HDF5, &
       NF90_64BIT_OFFSET, NF90_CLASSIC_MODEL


  implicit none

  ! --------------------------------------------------------------------------------------
  integer, parameter :: i8  = SELECTED_INT_KIND(2)
  integer, parameter :: i16 = c_short
  integer, parameter :: i32 = c_int
  integer, parameter :: i64 = c_long_long
  integer, parameter :: f32 = c_float
  integer, parameter :: f64 = c_double


  type, abstract :: NcBase

     integer(i32) :: id

   contains

     procedure(getNameInterface), deferred   :: getName
     procedure(getParentInterface), deferred :: getParent

  end type NcBase
  
  type, abstract, extends(NcBase) :: NcAttributable

   contains

     procedure, public  :: hasAttribute
     procedure, public  :: renameAttribute
     
     procedure, private :: getAttributableIds
     procedure, private :: setAttributeChar
     procedure, private :: setAttributeI8
     procedure, private :: setAttributeI16
     procedure, private :: setAttributeI32
     procedure, private :: setAttributeF32
     procedure, private :: setAttributeF64

     procedure, private :: getAttributeChar
     procedure, private :: getAttributeI8
     procedure, private :: getAttributeI16
     procedure, private :: getAttributeI32
     procedure, private :: getAttributeF32
     procedure, private :: getAttributeF64

     generic, public :: setAttribute => &
          setAttributeChar,  &
          setAttributeI8,    &
          setAttributeI16,   &
          setAttributeI32,   &
          setAttributeF32,   &
          setAttributeF64

     generic, public :: getAttribute => &
          getAttributeChar,  &
          getAttributeI8,    &
          getAttributeI16,   &
          getAttributeI32,   &
          getAttributeF32,   &
          getAttributeF64
  end type NcAttributable

  ! --------------------------------------------------------------------------------------

  type, extends(NcAttributable) :: NcGroup

  contains

     ! getter
     procedure, private :: getVariableIds
     procedure, public  :: getVariables
     procedure, public  :: getUnlimitedDimension
     procedure, public  :: getNoVariables

     procedure, private :: getDimensionByName
     procedure, private :: getDimensionById

     procedure, public  :: getParent => getGroupParent
     procedure, public  :: getName => getGroupName
     procedure, public  :: getGroup => getGroupByName
     procedure, public  :: getVariable => getVariableByName
     generic,   public  :: getDimension => &
          getDimensionById, &
          getDimensionByName

     ! checker
     procedure, public  :: hasVariable
     procedure, public  :: hasDimension
     procedure, public  :: hasGroup
     procedure, public  :: isUnlimited => isDatasetUnlimited

     ! setter
     procedure, public  :: setGroup
     procedure, private :: setLimitedDimension
     procedure, private :: setUnlimitedDimension
     generic, public    :: setDimension => &
          setLimitedDimension, &
          setUnlimitedDimension
     procedure, private :: setVariableWithTypes
     procedure, private :: setVariableWithNames
     procedure, private :: setVariableWithIds

     generic,   public  :: setVariable => &
          setVariableWithNames, &
          setVariableWithTypes, &
          setVariableWithIds

  end type NcGroup

  interface NcGroup
     procedure newNcGroup
  end interface NcGroup

  ! --------------------------------------------------------------------------------------

  type, extends(NcGroup) :: NcDataset

     character(256) :: fname !> Filename of the opened dataset
     character(1)   :: mode  !> File open mode

   contains

     procedure, public :: sync
     procedure, public :: close

  end type NcDataset

  interface NcDataset
     procedure newNcDataset
  end interface NcDataset

! --------------------------------------------------------------------------------------

  type, extends(NcBase) :: NcDimension

     type(NcGroup)     :: parent  !> The dimension's parent

   contains

     procedure, public :: renameDimension
     procedure, public :: getParent => getDimensionParent
     procedure, public :: getName => getDimensionName
     procedure, public :: getLength => getDimensionLength

     procedure, public :: isUnlimited => isUnlimitedDimension

  end type NcDimension

  interface NcDimension
     procedure newNcDimension
  end interface NcDimension

! --------------------------------------------------------------------------------------

  type, extends(NcAttributable) :: NcVariable

     type(NcGroup)      :: parent   !> The variables's parent

   contains


     procedure, public  :: renameVariable
     procedure, public  :: getParent => getVariableParent
     procedure, public  :: getName => getVariableName
     
     procedure, private :: putDataScalarI8
     procedure, private :: putData1dI8
     procedure, private :: putData2dI8
     procedure, private :: putData3dI8
     procedure, private :: putData4dI8
     procedure, private :: putData5dI8
     procedure, private :: putDataScalarI16
     procedure, private :: putData1dI16
     procedure, private :: putData2dI16
     procedure, private :: putData3dI16
     procedure, private :: putData4dI16
     procedure, private :: putData5dI16
     procedure, private :: putDataScalarI32
     procedure, private :: putData1dI32
     procedure, private :: putData2dI32
     procedure, private :: putData3dI32
     procedure, private :: putData4dI32
     procedure, private :: putData5dI32
     procedure, private :: putDataScalarF32
     procedure, private :: putData1dF32
     procedure, private :: putData2dF32
     procedure, private :: putData3dF32
     procedure, private :: putData4dF32
     procedure, private :: putData5dF32
     procedure, private :: putDataScalarF64
     procedure, private :: putData1dF64
     procedure, private :: putData2dF64
     procedure, private :: putData3dF64
     procedure, private :: putData4dF64
     procedure, private :: putData5dF64

     procedure, private :: getDataScalarI8
     procedure, private :: getData1dI8
     procedure, private :: getData2dI8
     procedure, private :: getData3dI8
     procedure, private :: getData4dI8
     procedure, private :: getData5dI8
     procedure, private :: getDataScalarI16
     procedure, private :: getData1dI16
     procedure, private :: getData2dI16
     procedure, private :: getData3dI16
     procedure, private :: getData4dI16
     procedure, private :: getData5dI16
     procedure, private :: getDataScalarI32
     procedure, private :: getData1dI32
     procedure, private :: getData2dI32
     procedure, private :: getData3dI32
     procedure, private :: getData4dI32
     procedure, private :: getData5dI32
     procedure, private :: getDataScalarF32
     procedure, private :: getData1dF32
     procedure, private :: getData2dF32
     procedure, private :: getData3dF32
     procedure, private :: getData4dF32
     procedure, private :: getData5dF32
     procedure, private :: getDataScalarF64
     procedure, private :: getData1dF64
     procedure, private :: getData2dF64
     procedure, private :: getData3dF64
     procedure, private :: getData4dF64
     procedure, private :: getData5dF64

     procedure, private :: setVariableFillValueI8
     procedure, private :: setVariableFillValueI16
     procedure, private :: setVariableFillValueI32
     procedure, private :: setVariableFillValueF32
     procedure, private :: setVariableFillValueF64

     procedure, private :: getVariableFillValueI8
     procedure, private :: getVariableFillValueI16
     procedure, private :: getVariableFillValueI32
     procedure, private :: getVariableFillValueF32
     procedure, private :: getVariableFillValueF64

     procedure, public  :: getNoDimensions

     procedure, public  :: getDimensions => getVariableDimensions

     procedure, public  :: getShape      => getVariableShape

     procedure, public  :: getDtype      => getVariableDtype

     procedure, public  :: isUnlimited   => isUnlimitedVariable

     generic, public :: putData => &
          putDataScalarI8, &
          putData1dI8, &
          putData2dI8, &
          putData3dI8, &
          putData4dI8, &
          putData5dI8, &
          putDataScalarI16, &
          putData1dI16, &
          putData2dI16, &
          putData3dI16, &
          putData4dI16, &
          putData5dI16, &
          putDataScalarI32, &
          putData1dI32, &
          putData2dI32, &
          putData3dI32, &
          putData4dI32, &
          putData5dI32, &
          putDataScalarF32, &
          putData1dF32, &
          putData2dF32, &
          putData3dF32, &
          putData4dF32, &
          putData5dF32, &
          putDataScalarF64, &
          putData1dF64, &
          putData2dF64, &
          putData3dF64, &
          putData4dF64, &
          putData5dF64

     generic, public :: getData => &
          getDataScalarI8, &
          getData1dI8, &
          getData2dI8, &
          getData3dI8, &
          getData4dI8, &
          getData5dI8, &
          getDataScalarI16, &
          getData1dI16, &
          getData2dI16, &
          getData3dI16, &
          getData4dI16, &
          getData5dI16, &
          getDataScalarI32, &
          getData1dI32, &
          getData2dI32, &
          getData3dI32, &
          getData4dI32, &
          getData5dI32, &
          getDataScalarF32, &
          getData1dF32, &
          getData2dF32, &
          getData3dF32, &
          getData4dF32, &
          getData5dF32, &
          getDataScalarF64, &
          getData1dF64, &
          getData2dF64, &
          getData3dF64, &
          getData4dF64, &
          getData5dF64

     generic, public :: setFillValue => &
          setVariableFillValueI8,  &
          setVariableFillValueI16, &
          setVariableFillValueI32, &
          setVariableFillValueF32, &
          setVariableFillValueF64

     generic, public :: getFillValue => &
          getVariableFillValueI8,  &
          getVariableFillValueI16, &
          getVariableFillValueI32, &
          getVariableFillValueF32, &
          getVariableFillValueF64

  end type NcVariable

  interface NcVariable
    procedure newNcVariable
  end interface NcVariable
  ! --------------------------------------------------------------------------------------

  ! abstract interfaces
  interface
     function getNameInterface(self)
       import NcBase
       class(NcBase), intent(in) :: self
       character(len=256)        :: getNameInterface
     end function getNameInterface

     function getParentInterface(self)
       import NcBase, NcGroup
       class(NcBase), intent(in) :: self
       type(NcGroup)             :: getParentInterface
     end function getParentInterface
  end interface

  interface operator (==)
     procedure equalNcBases
  end interface operator (==)

contains

  function newNcDataset(fname, fmode, cmode)
    character(*), intent(in)              :: fname
    character(1), intent(in)              :: fmode
    character(*), intent(inout), optional :: cmode
    integer(i32)                          :: status
    type(NcDataset)                       :: newNcDataset
    
    select case(fmode)
    case("w")
       status = nf90_create(trim(fname), getCreationMode(cmode), newNcDataset%id)
    case("r")
       status = nf90_open(trim(fname), NF90_NOWRITE, newNcDataset%id)
    case("a")
       status = nf90_open(trim(fname), NF90_WRITE, newNcDataset%id)
    case default
       write(*,*) "Mode argument must be in 'w','r','a' ! "
       stop 1
    end select
    call check(status,"Failed to open file: " // fname)

    newNcDataset%fname = fname
    newNcDataset%mode  = fmode
  end function newNcDataset

  function newNcVariable(id, parent)
    integer(i32) , intent(in) :: id
    type(NcGroup), intent(in) :: parent
    type(NcVariable)          :: newNcVariable

    newNcVariable%id     = id
    newNcVariable%parent = parent
  end function newNcVariable

  function newNcDimension(id, parent)
    integer(i32) , intent(in) :: id
    type(NcGroup), intent(in) :: parent
    type(NcDimension)         :: newNcDimension   

    newNcDimension%id     = id
    newNcDimension%parent = parent
  end function newNcDimension

  function newNcGroup(id)
    integer(i32)    , intent(in) :: id
    type(NcGroup)                :: newNcGroup 

    newNcGroup%id = id
  end function newNcGroup

  subroutine sync(self)
    class(NcDataset) :: self

    call check(nf90_sync(self%id), "Failed to sync file: "//self%fname)
  end subroutine sync

  subroutine close(self)
    class(NcDataset) :: self

    call check(nf90_close(self%id), "Failed to close file: "//self%fname)
  end subroutine close

  function setGroup(self, name)
    class(NcGroup), intent(inout) :: self
    character(*)  , intent(in)    :: name
    integer(i32)                  :: id
    type(NcGroup)                 :: setGroup

    call check(nf90_def_grp(self%id, name, id), "Failed to create new group: " // name)
    setGroup = NcGroup(id)
  end function setGroup

  function getGroupParent(self)
    class(NcGroup), intent(in) :: self
    integer(i32)               :: id
    type(NcGroup)              :: getGroupParent

    call check(nf90_inq_grp_parent(self%id, id), "Failed to get parent group of: " // self%getName())
    getGroupParent = NcGroup(id)
  end function getGroupParent

  function getGroupName(self)
    class(NcGroup), intent(in) :: self
    character(256)             :: getGroupName

    call check(nf90_inq_grpname(self%id, getGroupName), "Failed to inquire group name")
  end function getGroupName

  function getNoVariables(self)
    class(NcGroup), intent(in) :: self
    integer(i32)               :: getNoVariables

    call check(nf90_inquire(self%id, nvariables=getNoVariables), "Failed inquire number of variables")
  end function getNoVariables

  function getDimensionParent(self)
    class(NcDimension), intent(in) :: self
    type(NcGroup)                  :: getDimensionParent

    getDimensionParent = self%parent
  end function getDimensionParent

  function getVariableParent(self)
    class(NcVariable), intent(in) :: self
    type(NcGroup)                  :: getVariableParent

    getVariableParent = self%parent
  end function getVariableParent

  function getVariableIds(self)
    class(NcGroup), intent(in)              :: self
    integer(i32), dimension(:), allocatable :: getVariableIds
    integer(i32)                            :: tmp
    
    allocate(getVariableIds(self%getNoVariables()))
    call check(nf90_inq_varids(self%id, tmp, getVariableIds), "Failed to inquire variable ids")
  end function getVariableIds
  
  function getVariables(self)
    class(NcGroup), intent(in)                  :: self
    type(NcVariable), dimension(:), allocatable :: getVariables
    integer(i32), dimension(:), allocatable     :: varids
    integer(i32)                                :: i, nvars

    nvars = self%getNoVariables()
    allocate(getVariables(nvars), varids(nvars))

    varids = self%getVariableIds()
    do i=1,size(varids)
       getVariables(i) = NcVariable(varids(i), self)
    end do
    
  end function getVariables
  
  function getDimensionName(self)
    class(NcDimension), intent(in) :: self
    character(len=256)             :: getDimensionName

    call check(nf90_inquire_dimension(self%parent%id, self%id, name=getDimensionName), &
         "Failed to inquire dimension name")
  end function getDimensionName

  function getDimensionLength(self)
    class(NcDimension), intent(in) :: self
    integer(i32)                    :: getDimensionLength

    call check(nf90_inquire_dimension(self%parent%id,self%id,len=getDimensionLength),&
         "Failed to inquire dimension: "//self%getName())
  end function getDimensionLength

  function isDatasetUnlimited(self)
    class(NcGroup), intent(in)   :: self
    logical                      :: isDatasetUnlimited
    integer(i32)                  :: dimid

    call check(nf90_inquire(self%id,unlimitedDimId=dimid), &
         "Failed to inquire group "//self%getName())
    isDatasetUnlimited = (dimid .ne. -1)
  end function isDatasetUnlimited

  function getUnlimitedDimension(self)
    class(NcGroup), intent(in)   :: self
    type(NcDimension)            :: getUnlimitedDimension
    integer(i32)                  :: dimid

    call check(nf90_inquire(self%id,unlimitedDimId=dimid), &
         "Failed to inquire group "//self%getName())

    if (dimid .eq. -1) then
       write(*,*) "Dataset has no unlimited dimension"
       stop 1
    end if

    getUnlimitedDimension = self%getDimension(dimid)
  end function getUnlimitedDimension

  function equalNcBases(left, right)
    class(NcBase), intent(in) :: left, right
    logical                   :: equalNcBases
    
    equalNcBases = (left%id .eq. right%id)
  end function equalNcBases

  function isUnlimitedDimension(self)
    class(NcDimension), intent(in) :: self
    logical                        :: isUnlimitedDimension

    isUnlimitedDimension = .false.
    if (self%parent%isUnlimited()) then
       isUnlimitedDimension = (self == self%parent%getUnlimitedDimension())
    end if
  end function isUnlimitedDimension

   
  function setUnlimitedDimension(self, name)
    class(NcGroup), intent(in)           :: self
    character(*)  , intent(in)           :: name
    type(NcDimension)                    :: setUnlimitedDimension
    integer(i32)                         :: id, dimlength

    setUnlimitedDimension = self%setLimitedDimension(name, -1)
  end function setUnlimitedDimension
 
  function setLimitedDimension(self, name, length)
    class(NcGroup), intent(in) :: self
    character(*)  , intent(in) :: name
    integer(i32)  , intent(in) :: length
    type(NcDimension)          :: setLimitedDimension
    integer(i32)               :: id, dimlength

    if (length .le. 0) then
       dimlength = NF90_UNLIMITED
    else
       dimlength = length
    end if

    call check(nf90_def_dim(self%id, name, dimlength, id), &
         "Failed to create dimension: " // name)

    setLimitedDimension = NcDimension(id,self)
  end function setLimitedDimension

  function hasVariable(self, name)
    class(NcGroup), intent(in) :: self
    character(*)    , intent(in) :: name
    logical                      :: hasVariable
    integer(i32)                  :: tmpid

    hasVariable = (nf90_inq_varid(self%id,name,tmpid) .eq. NF90_NOERR)
  end function hasVariable

  function hasDimension(self, name)
    class(NcGroup), intent(in) :: self
    character(*)    , intent(in) :: name
    logical                      :: hasDimension
    integer(i32)                  :: tmpid

    hasDimension = (nf90_inq_dimid(self%id,name,tmpid) .eq. NF90_NOERR)
  end function hasDimension

  function hasGroup(self, name)
    class(NcGroup), intent(in) :: self
    character(*)    , intent(in) :: name
    logical                      :: hasGroup
    integer(i32)                  :: tmpid

    hasGroup = (nf90_inq_ncid(self%id,name,tmpid) .eq. NF90_NOERR)
  end function hasGroup

  function setVariableWithIds(self, name, dtype, dimensions, contiguous, &
       chunksizes, deflate_level, shuffle, fletcher32, endianness, &
       cache_size, cache_nelems, cache_preemption)
    class(NcGroup), intent(in)           :: self
    character(*)    , intent(in)           :: name
    character(*)    , intent(in)           :: dtype
    integer(i32)     , intent(in)           :: dimensions(:)
    logical         , intent(in), optional :: contiguous,shuffle, fletcher32
    integer(i32)     , intent(in), optional :: endianness,deflate_level,cache_size, &
         cache_nelems, cache_preemption, chunksizes(:)
    type(NcVariable)                       :: setVariableWithIds
    integer(i32)                            :: varid, status

    status = nf90_def_var(self%id, name, getDtypeFromString(dtype), dimensions, varid, contiguous, &
         chunksizes, deflate_level, shuffle, fletcher32, endianness, &
         cache_size, cache_nelems, cache_preemption)
    call check(status, "Failed to create variable: " // name)
    setVariableWithIds = NcVariable(varid, self)
  end function setVariableWithIds

  function setVariableWithNames(self, name, dtype, dimensions, contiguous, &
       chunksizes, deflate_level, shuffle, fletcher32, endianness, &
       cache_size, cache_nelems, cache_preemption)

    class(NcGroup), intent(in)              :: self
    character(*)    , intent(in)              :: name
    character(*)    , intent(in)              :: dtype
    character(*)    , intent(in)              :: dimensions(:)
    logical         , intent(in), optional    :: contiguous,shuffle, fletcher32
    integer(i32)     , intent(in), optional    :: endianness,deflate_level,cache_size, &
         cache_nelems, cache_preemption, chunksizes(:)
    type(NcVariable)                          :: setVariableWithNames
    type(NcDimension)                         :: dim
    integer(i32)                               :: i, dimids(size(dimensions))

    do i = 1,size(dimensions)
       dim = self%getDimension(dimensions(i))
       dimids(i) = dim%id
    end do

    setVariableWithNames = setVariableWithIds(self, name, dtype, dimids, contiguous, &
         chunksizes, deflate_level, shuffle, fletcher32, endianness, &
         cache_size, cache_nelems, cache_preemption)
  end function setVariableWithNames

  function setVariableWithTypes(self, name, dtype, dimensions, contiguous, &
       chunksizes, deflate_level, shuffle, fletcher32, endianness, &
       cache_size, cache_nelems, cache_preemption)
    class(NcGroup)   , intent(in)              :: self
    character(*)     , intent(in)              :: name
    character(*)     , intent(in)              :: dtype
    type(NcDimension), intent(in)              :: dimensions(:)
    logical          , intent(in), optional    :: contiguous,shuffle, fletcher32
    integer(i32)      , intent(in), optional    :: endianness,deflate_level,cache_size, &
         cache_nelems, cache_preemption, chunksizes(:)
    type(NcVariable)                           :: setVariableWithTypes
    type(NcDimension)                          :: dim
    integer(i32)                                :: i, dimids(size(dimensions))

    do i = 1,size(dimensions)
       dim = dimensions(i)
       dimids(i) = dim%id
    end do

    setVariableWithTypes = setVariableWithIds(self, name, dtype, dimids, contiguous, &
         chunksizes, deflate_level, shuffle, fletcher32, endianness, &
         cache_size, cache_nelems, cache_preemption)
  end function setVariableWithTypes

  function getDimensionById(self, id)
    class(NcGroup), intent(in) :: self
    integer(i32)                  :: id
    type(NcDimension)            :: getDimensionById
    character(32)                :: msg, name

    write(msg,*) id
    call check(nf90_inquire_dimension(self%id,id,name), &
         "Could not inquire dimension: " // msg)
    getDimensionById = NcDimension(id,self)
  end function getDimensionById

  function getDimensionByName(self, name)
    class(NcGroup), intent(in) :: self
    character(*)                 :: name
    type(NcDimension)            :: getDimensionByName
    integer(i32)                  :: id

    call check(nf90_inq_dimid(self%id,name,id), &
         "Could not inquire dimension: " // name)
    getDimensionByName = self%getDimensionById(id)
  end function getDimensionByName

  function getGroupByName(self, name)
    class(NcGroup), intent(in) :: self
    character(*)  , intent(in) :: name
    type(NcGroup)              :: getGroupByName
    integer(i32)                :: id

    call check(nf90_inq_ncid(self%id, name, id), &
         "Could not inquire variable: " // name)
    getGroupByName = NcGroup(id)
  end function getGroupByName

  function getVariableByName(self, name)
    class(NcGroup), intent(in) :: self
    character(*)    , intent(in) :: name
    type(NcVariable)             :: getVariableByName
    integer(i32)                  :: id

    call check(nf90_inq_varid(self%id, name, id), &
         "Could not inquire variable: " // name)
    getVariableByName = NcVariable(id, self)

  end function getVariableByName

  function getVariableName(self)
    class(NcVariable), intent(in) :: self
    character(len=256)            :: getVariableName

    call check(nf90_inquire_variable(self%parent%id, self%id, name=getVariableName), &
         "Could not inquire variable name")
  end function getVariableName

  function getNoDimensions(self)
    class(NcVariable), intent(in) :: self
    integer(i32)                   :: getNoDimensions

    call check(nf90_inquire_variable(self%parent%id,self%id,ndims=getNoDimensions), &
         "Could not inquire variable: " // self%getName())
  end function getNoDimensions

  function getVariableDimensions(self)
    class(NcVariable), intent(in)  :: self
    type(NcDimension), allocatable :: getVariableDimensions(:)
    integer(i32)      , allocatable :: dimids(:)
    integer(i32)                    :: ii , ndims

    ndims = self%getNoDimensions()
    allocate(dimids(ndims), getVariableDimensions(ndims))
    call check(nf90_inquire_variable(self%parent%id,self%id,dimids=dimids), &
         "Could not inquire variable: " // self%getName())

    do ii=1,ndims
       getVariableDimensions (ii) = self%parent%getDimension(dimids(ii))
    end do
  end function getVariableDimensions

  function getVariableShape(self)
    class(NcVariable), intent(in)  :: self
    integer(i32)      , allocatable :: getVariableShape(:)
    type(NcDimension), allocatable :: dims(:)
    integer(i32)                    :: ii, ndims

    ndims = self%getNoDimensions()
    allocate(getVariableShape(ndims), dims(ndims))

    dims = self%getDimensions()
    do ii = 1,size(dims)
       getVariableShape(ii) = dims(ii)%getLength()
    end do
  end function getVariableShape

  function getVariableDtype(self)
    class(NcVariable), intent(in) :: self
    integer(i32)                   :: dtype
    character(3)                  :: getVariableDtype

    call check(nf90_inquire_variable(self%parent%id,self%id,xtype=dtype),&
         "Could not inquire variable: " // self%getName())
    getVariableDtype = getDtypeFromInteger(dtype)
  end function getVariableDtype

  function isUnlimitedVariable(self)
    class(NcVariable), intent(in)  :: self
    logical                        :: isUnlimitedVariable
    type(NcDimension), allocatable :: dims(:)
    type(NcDimension)              :: dim
    integer(i32)                    :: ii

    allocate(dims(self%getNoDimensions()))

    isUnlimitedVariable = .false.
    dims = self%getDimensions()

    do ii = 1,size(dims)
       dim = dims(ii)
       if (dim%isUnlimited()) then
          isUnlimitedVariable = .true.
       end if
    end do
  end function isUnlimitedVariable

  logical function hasAttribute(self,name)
    class(NcAttributable), intent(inout) :: self
    character(*)     , intent(in) :: name
    integer(i32)                   :: status

    select type (self)
    class is (NcGroup)
        status = nf90_inquire_attribute(self%id, NF90_GLOBAL, name)
    class is (NcVariable)
        status = nf90_inquire_attribute(self%parent%id, self%id, name)
    end select
    
    hasAttribute = (status .eq. NF90_NOERR)
  end function hasAttribute

  subroutine setAttributeChar(self, name, data)
    class(NcAttributable), intent(in) :: self
    character(*)         , intent(in) :: name
    character(*)         , intent(in) :: data
    integer(i32)                      :: ids(2)

    ids = self%getAttributableIds()
    call check(nf90_put_att(ids(1), ids(2), name, data), &
         "Failed to write attribute: " // name)

  end subroutine setAttributeChar

  subroutine setAttributeI8(self, name, data)
    class(NcAttributable), intent(in) :: self
    character(*)         , intent(in) :: name
    integer(i8)          , intent(in) :: data
    integer(i32)                      :: ids(2)

    ids = self%getAttributableIds()
    call check(nf90_put_att(ids(1), ids(2), name, data), &
         "Failed to write attribute: " // name)

  end subroutine setAttributeI8

  subroutine setAttributeI16(self, name, data)
    class(NcAttributable), intent(in) :: self
    character(*)         , intent(in) :: name
    integer(i16)         , intent(in) :: data
    integer(i32)                      :: ids(2)

    ids = self%getAttributableIds()
    call check(nf90_put_att(ids(1), ids(2), name, data), &
         "Failed to write attribute: " // name)

  end subroutine setAttributeI16

  subroutine setAttributeI32(self, name, data)
    class(NcAttributable), intent(in) :: self
    character(*)         , intent(in) :: name
    integer(i32)         , intent(in) :: data
    integer(i32)                      :: ids(2)

    ids = self%getAttributableIds()
    call check(nf90_put_att(ids(1), ids(2), name, data), &
         "Failed to write attribute: " // name)

  end subroutine setAttributeI32

  subroutine setAttributeF32(self, name, data)
    class(NcAttributable), intent(in) :: self
    character(*)         , intent(in) :: name
    real(f32)            , intent(in) :: data
    integer(i32)                      :: ids(2)

    ids = self%getAttributableIds()
    call check(nf90_put_att(ids(1), ids(2), name, data), &
         "Failed to write attribute: " // name)

  end subroutine setAttributeF32

  subroutine setAttributeF64(self, name, data)
    class(NcAttributable), intent(in) :: self
    character(*)         , intent(in) :: name
    real(f64)            , intent(in) :: data
    integer(i32)                      :: ids(2)

    ids = self%getAttributableIds()
    call check(nf90_put_att(ids(1), ids(2), name, data), &
         "Failed to write attribute: " // name)

  end subroutine setAttributeF64

  subroutine getAttributeChar(self, name, avalue)
    class(NcAttributable), intent(in)  :: self
    character(*)         , intent(in)  :: name
    character(*)         , intent(out) :: avalue
    integer(i32)                       :: length, ids(2)

    ids = self%getAttributableIds()
    call check(nf90_inquire_attribute(ids(1), ids(2), name, len=length),&
         "Could not inquire attribute " // name)
    call check(nf90_get_att(ids(1), ids(2), name, avalue), &
         "Could not read attribute "//name)
  end subroutine getAttributeChar

  subroutine getAttributeI8(self, name, avalue)
    class(NcAttributable) , intent(in)  :: self
    character(*)          , intent(in)  :: name
    integer(i8)           , intent(out) :: avalue
    integer(i32)                        :: length, ids(2)

    ids = self%getAttributableIds()
    call check(nf90_inquire_attribute(ids(1), ids(2), name, len=length),&
         "Could not inquire attribute " // name)
    call check(nf90_get_att(ids(1), ids(2), name, avalue), &
         "Could not read attribute "//name)

  end subroutine getAttributeI8

  subroutine getAttributeI16(self, name, avalue)
    class(NcAttributable) , intent(in)  :: self
    character(*)          , intent(in)  :: name
    integer(i16)          , intent(out) :: avalue
    integer(i32)                        :: length, ids(2)

    ids = self%getAttributableIds()
    call check(nf90_inquire_attribute(ids(1), ids(2), name, len=length),&
         "Could not inquire attribute " // name)
    call check(nf90_get_att(ids(1), ids(2), name, avalue), &
         "Could not read attribute "//name)

  end subroutine getAttributeI16

  subroutine getAttributeI32(self, name, avalue)
    class(NcAttributable) , intent(in)  :: self
    character(*)          , intent(in)  :: name
    integer(i32)          , intent(out) :: avalue
    integer(i32)                        :: length, ids(2)

    ids = self%getAttributableIds()
    call check(nf90_inquire_attribute(ids(1), ids(2), name, len=length),&
         "Could not inquire attribute " // name)
    call check(nf90_get_att(ids(1), ids(2), name, avalue), &
         "Could not read attribute "//name)

  end subroutine getAttributeI32

  subroutine getAttributeF32(self, name, avalue)
    class(NcAttributable) , intent(in)  :: self
    character(*)          , intent(in)  :: name
    real(f32)             , intent(out) :: avalue
    integer(i32)                        :: length, ids(2)

    ids = self%getAttributableIds()
    call check(nf90_inquire_attribute(ids(1), ids(2), name, len=length),&
         "Could not inquire attribute " // name)
    call check(nf90_get_att(ids(1), ids(2), name, avalue), &
         "Could not read attribute "//name)

  end subroutine getAttributeF32

  subroutine getAttributeF64(self, name, avalue)
    class(NcAttributable) , intent(in)  :: self
    character(*)          , intent(in)  :: name
    real(f64)             , intent(out) :: avalue
    integer(i32)                        :: length, ids(2)

    ids = self%getAttributableIds()
    call check(nf90_inquire_attribute(ids(1), ids(2), name, len=length),&
         "Could not inquire attribute " // name)
    call check(nf90_get_att(ids(1), ids(2), name, avalue), &
         "Could not read attribute "//name)

 end subroutine getAttributeF64

  function getAttributableIds(self)
    class(NcAttributable), intent(in) :: self
    integer(i32)                      :: getAttributableIds(2)
    select type(self)
    class is (NcGroup)
       getAttributableIds(1) = self%id
       getAttributableIds(2) = NF90_GLOBAL
    class is (NcVariable)
       getAttributableIds(1) = self%parent%id
       getAttributableIds(2) = self%id
    end select
  end function getAttributableIds

  subroutine renameAttribute(self, oldname, newname)
    class(NcAttributable), intent(inout) :: self
    character(len=*), intent(in)         :: oldname, newname
    integer(i32)                         :: ids(2)
    ids = self%getAttributableIds()
    call check(nf90_rename_att(ids(1), ids(2), oldname, newname), "Failed to rename attribute: " // oldname)
  end subroutine renameAttribute

  subroutine renameVariable(self, name)
    class(NcVariable), intent(inout) :: self
    character(len=*),  intent(in)    :: name
    call check(nf90_rename_var(self%parent%id, self%id, name), "Failed to rename variable: " // self%getName())
  end subroutine renameVariable

  subroutine renameDimension(self, name)
    class(NcDimension), intent(inout) :: self
    character(len=*),  intent(in)     :: name
    call check(nf90_rename_dim(self%parent%id, self%id, name), "Failed to rename dimension: " // self%getName())
  end subroutine renameDimension

  subroutine setVariableFillValueI8(self, fvalue)
    class(NcVariable), intent(inout)  :: self
    integer(i8)      , intent(in)  :: fvalue

    if (.not. self%hasAttribute("_FillValue")) then
       call self%setAttribute("_FillValue",fvalue)
    end if
  end subroutine setVariableFillValueI8
  
  subroutine setVariableFillValueI16(self, fvalue)
    class(NcVariable), intent(inout)  :: self
    integer(i16)      , intent(in)  :: fvalue

    if (.not. self%hasAttribute("_FillValue")) then
       call self%setAttribute("_FillValue",fvalue)
    end if
  end subroutine setVariableFillValueI16

  subroutine setVariableFillValueI32(self, fvalue)
    class(NcVariable), intent(inout)  :: self
    integer(i32)      , intent(in)  :: fvalue

    if (.not. self%hasAttribute("_FillValue")) then
       call self%setAttribute("_FillValue",fvalue)
    end if
  end subroutine setVariableFillValueI32

  subroutine setVariableFillValueF32(self, fvalue)
    class(NcVariable), intent(inout)  :: self
    real(f32)         , intent(in)  :: fvalue

    if (.not. self%hasAttribute("_FillValue")) then
       call self%setAttribute("_FillValue",fvalue)
    end if
  end subroutine setVariableFillValueF32

  subroutine setVariableFillValueF64(self, fvalue)
    class(NcVariable), intent(inout)  :: self
    real(f64)         , intent(in)  :: fvalue

    if (.not. self%hasAttribute("_FillValue")) then
       call self%setAttribute("_FillValue",fvalue)
    end if
  end subroutine setVariableFillValueF64

  subroutine getVariableFillValueI8(self, fvalue)
    class(NcVariable), intent(inout)  :: self
    integer(i8)      , intent(out) :: fvalue

    if (self%hasAttribute("_FillValue")) then
       call self%getAttribute("_FillValue", fvalue)
    else
       fvalue = NF90_FILL_BYTE
    end if
  end subroutine getVariableFillValueI8

  subroutine getVariableFillValueI16(self, fvalue)
    class(NcVariable), intent(inout)  :: self
    integer(i16)      , intent(out) :: fvalue

    if (self%hasAttribute("_FillValue")) then
       call self%getAttribute("_FillValue", fvalue)
    else
       fvalue = NF90_FILL_SHORT
    end if
  end subroutine getVariableFillValueI16

  subroutine getVariableFillValueI32(self, fvalue)
    class(NcVariable), intent(inout)  :: self
    integer(i32)      , intent(out) :: fvalue

    if (self%hasAttribute("_FillValue")) then
       call self%getAttribute("_FillValue", fvalue)
    else
       fvalue = NF90_FILL_INT
    end if
  end subroutine getVariableFillValueI32

  subroutine getVariableFillValueF32(self, fvalue)
    class(NcVariable), intent(inout)  :: self
    real(f32)         , intent(out) :: fvalue

    if (self%hasAttribute("_FillValue")) then
       call self%getAttribute("_FillValue", fvalue)
    else
       fvalue = NF90_FILL_FLOAT
    end if
  end subroutine getVariableFillValueF32

  subroutine getVariableFillValueF64(self, fvalue)
    class(NcVariable), intent(inout)  :: self
    real(f64)         , intent(out) :: fvalue

    if (self%hasAttribute("_FillValue")) then
       call self%getAttribute("_FillValue", fvalue)
    else
       fvalue = NF90_FILL_DOUBLE
    end if
  end subroutine getVariableFillValueF64

  subroutine putDataScalarI8(self, values, start)
    class(NcVariable), intent(in)           :: self
    integer(i8)      , intent(in)           :: values
    integer(i32)      , intent(in), optional :: start(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putDataScalarI8

  subroutine putData1dI8(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in)           :: self
    integer(i8)      , intent(in)           :: values(:)
    integer(i32)      , intent(in), optional :: start(:), cnt(:), stride(:), map(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putData1dI8

  subroutine putData2dI8(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in)           :: self
    integer(i8)      , intent(in)           :: values(:,:)
    integer(i32)      , intent(in), optional :: start(:), cnt(:), stride(:), map(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putData2dI8

  subroutine putData3dI8(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in)           :: self
    integer(i8)      , intent(in)           :: values(:,:,:)
    integer(i32)      , intent(in), optional :: start(:), cnt(:), stride(:), map(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putData3dI8

  subroutine putData4dI8(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in)           :: self
    integer(i8)      , intent(in)           :: values(:,:,:,:)
    integer(i32)      , intent(in), optional :: start(:), cnt(:), stride(:), map(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putData4dI8

  subroutine putData5dI8(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in)           :: self
    integer(i8)      , intent(in)           :: values(:,:,:,:,:)
    integer(i32)      , intent(in), optional :: start(:), cnt(:), stride(:), map(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putData5dI8

  subroutine putDataScalarI16(self, values, start)
    class(NcVariable), intent(in)           :: self
    integer(i16)      , intent(in)           :: values
    integer(i32)      , intent(in), optional :: start(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putDataScalarI16

  subroutine putData1dI16(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in)           :: self
    integer(i16)      , intent(in)           :: values(:)
    integer(i32)      , intent(in), optional :: start(:), cnt(:), stride(:), map(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putData1dI16

  subroutine putData2dI16(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in)           :: self
    integer(i16)      , intent(in)           :: values(:,:)
    integer(i32)      , intent(in), optional :: start(:), cnt(:), stride(:), map(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putData2dI16

  subroutine putData3dI16(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in)           :: self
    integer(i16)      , intent(in)           :: values(:,:,:)
    integer(i32)      , intent(in), optional :: start(:), cnt(:), stride(:), map(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putData3dI16

  subroutine putData4dI16(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in)           :: self
    integer(i16)      , intent(in)           :: values(:,:,:,:)
    integer(i32)      , intent(in), optional :: start(:), cnt(:), stride(:), map(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putData4dI16

  subroutine putData5dI16(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in)           :: self
    integer(i16)      , intent(in)           :: values(:,:,:,:,:)
    integer(i32)      , intent(in), optional :: start(:), cnt(:), stride(:), map(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putData5dI16

  subroutine putDataScalarI32(self, values, start)
    class(NcVariable), intent(in)           :: self
    integer(i32)      , intent(in)           :: values
    integer(i32)      , intent(in), optional :: start(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putDataScalarI32

  subroutine putData1dI32(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in)           :: self
    integer(i32)      , intent(in)           :: values(:)
    integer(i32)      , intent(in), optional :: start(:), cnt(:), stride(:), map(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putData1dI32

  subroutine putData2dI32(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in)           :: self
    integer(i32)      , intent(in)           :: values(:,:)
    integer(i32)      , intent(in), optional :: start(:), cnt(:), stride(:), map(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putData2dI32

  subroutine putData3dI32(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in)           :: self
    integer(i32)      , intent(in)           :: values(:,:,:)
    integer(i32)      , intent(in), optional :: start(:), cnt(:), stride(:), map(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putData3dI32

  subroutine putData4dI32(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in)           :: self
    integer(i32)      , intent(in)           :: values(:,:,:,:)
    integer(i32)      , intent(in), optional :: start(:), cnt(:), stride(:), map(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putData4dI32

  subroutine putData5dI32(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in)           :: self
    integer(i32)      , intent(in)           :: values(:,:,:,:,:)
    integer(i32)      , intent(in), optional :: start(:), cnt(:), stride(:), map(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putData5dI32

  subroutine putDataScalarF32(self, values, start)
    class(NcVariable), intent(in)           :: self
    real(f32)         , intent(in)           :: values
    integer(i32)      , intent(in), optional :: start(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putDataScalarF32

  subroutine putData1dF32(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in)           :: self
    real(f32)         , intent(in)           :: values(:)
    integer(i32)      , intent(in), optional :: start(:), cnt(:), stride(:), map(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putData1dF32

  subroutine putData2dF32(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in)           :: self
    real(f32)         , intent(in)           :: values(:,:)
    integer(i32)      , intent(in), optional :: start(:), cnt(:), stride(:), map(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putData2dF32

  subroutine putData3dF32(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in)           :: self
    real(f32)         , intent(in)           :: values(:,:,:)
    integer(i32)      , intent(in), optional :: start(:), cnt(:), stride(:), map(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putData3dF32

  subroutine putData4dF32(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in)           :: self
    real(f32)         , intent(in)           :: values(:,:,:,:)
    integer(i32)      , intent(in), optional :: start(:), cnt(:), stride(:), map(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putData4dF32

  subroutine putData5dF32(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in)           :: self
    real(f32)         , intent(in)           :: values(:,:,:,:,:)
    integer(i32)      , intent(in), optional :: start(:), cnt(:), stride(:), map(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putData5dF32

  subroutine putDataScalarF64(self, values, start)
    class(NcVariable), intent(in)           :: self
    real(f64)         , intent(in)           :: values
    integer(i32)      , intent(in), optional :: start(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putDataScalarF64

  subroutine putData1dF64(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in)           :: self
    real(f64)         , intent(in)           :: values(:)
    integer(i32)      , intent(in), optional :: start(:), cnt(:), stride(:), map(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putData1dF64

  subroutine putData2dF64(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in)           :: self
    real(f64)         , intent(in)           :: values(:,:)
    integer(i32)      , intent(in), optional :: start(:), cnt(:), stride(:), map(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putData2dF64

  subroutine putData3dF64(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in)           :: self
    real(f64)         , intent(in)           :: values(:,:,:)
    integer(i32)      , intent(in), optional :: start(:), cnt(:), stride(:), map(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putData3dF64

  subroutine putData4dF64(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in)           :: self
    real(f64)         , intent(in)           :: values(:,:,:,:)
    integer(i32)      , intent(in), optional :: start(:), cnt(:), stride(:), map(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putData4dF64

  subroutine putData5dF64(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in)           :: self
    real(f64)         , intent(in)           :: values(:,:,:,:,:)
    integer(i32)      , intent(in), optional :: start(:), cnt(:), stride(:), map(:)

    call check( nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
         "Failed to write data into variable: " // trim(self%getName()))
  end subroutine putData5dF64

  subroutine getDataScalarI8(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    integer(i8)      , intent(out)              :: data
    integer(i8)                                 :: tmp(1)

    call check (nf90_get_var(self%parent%id, self%id, tmp, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
    data = tmp(1)
  end subroutine getDataScalarI8

  subroutine getData1dI8(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    integer(i8)      , intent(out), allocatable :: data(:)
    integer(i32)                   , allocatable :: datashape(:)

    allocate(datashape(1))
    datashape = getReadDataShape(self, 1, start, cnt, stride)

    allocate(data(datashape(1)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
  end subroutine getData1dI8

  subroutine getData2dI8(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    integer(i8)      , intent(out), allocatable :: data(:,:)
    integer(i32)                   , allocatable :: datashape(:)

    allocate(datashape(2))
    datashape = getReadDataShape(self, 2, start, cnt, stride)

    allocate(data(datashape(1), datashape(2)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
  end subroutine getData2dI8

  subroutine getData3dI8(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    integer(i8)      , intent(out), allocatable :: data(:,:,:)
    integer(i32)                   , allocatable :: datashape(:)

    allocate(datashape(3))
    datashape = getReadDataShape(self, 3, start, cnt, stride)

    allocate(data(datashape(1), datashape(2), datashape(3)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
  end subroutine getData3dI8

  subroutine getData4dI8(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    integer(i8)      , intent(out), allocatable :: data(:,:,:,:)
    integer(i32)                   , allocatable :: datashape(:)

    allocate(datashape(4))
    datashape = getReadDataShape(self, 4, start, cnt, stride)

    allocate(data(datashape(1), datashape(2), datashape(3), datashape(4)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
  end subroutine getData4dI8

  subroutine getData5dI8(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    integer(i8)      , intent(out), allocatable :: data(:,:,:,:,:)
    integer(i32)                   , allocatable :: datashape(:)

    allocate(datashape(5))
    datashape = getReadDataShape(self, 5, start, cnt, stride)

    allocate(data(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
  end subroutine getData5dI8

  subroutine getDataScalarI16(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    integer(i16)      , intent(out)              :: data
    integer(i16)                                 :: tmp(1)

    call check (nf90_get_var(self%parent%id, self%id, tmp, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
    data = tmp(1)
  end subroutine getDataScalarI16

  subroutine getData1dI16(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    integer(i16)      , intent(out), allocatable :: data(:)
    integer(i32)                   , allocatable :: datashape(:)

    allocate(datashape(1))
    datashape = getReadDataShape(self, 1, start, cnt, stride)

    allocate(data(datashape(1)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
  end subroutine getData1dI16

  subroutine getData2dI16(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    integer(i16)      , intent(out), allocatable :: data(:,:)
    integer(i32)                   , allocatable :: datashape(:)

    allocate(datashape(2))
    datashape = getReadDataShape(self, 2, start, cnt, stride)

    allocate(data(datashape(1), datashape(2)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
  end subroutine getData2dI16

  subroutine getData3dI16(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    integer(i16)      , intent(out), allocatable :: data(:,:,:)
    integer(i32)                   , allocatable :: datashape(:)

    allocate(datashape(3))
    datashape = getReadDataShape(self, 3, start, cnt, stride)

    allocate(data(datashape(1), datashape(2), datashape(3)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
  end subroutine getData3dI16

  subroutine getData4dI16(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    integer(i16)      , intent(out), allocatable :: data(:,:,:,:)
    integer(i32)                   , allocatable :: datashape(:)

    allocate(datashape(4))
    datashape = getReadDataShape(self, 4, start, cnt, stride)

    allocate(data(datashape(1), datashape(2), datashape(3), datashape(4)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
  end subroutine getData4dI16

  subroutine getData5dI16(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    integer(i16)      , intent(out), allocatable :: data(:,:,:,:,:)
    integer(i32)                   , allocatable :: datashape(:)

    allocate(datashape(5))
    datashape = getReadDataShape(self, 5, start, cnt, stride)

    allocate(data(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
  end subroutine getData5dI16

  subroutine getDataScalarI32(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    integer(i32)      , intent(out)              :: data
    integer(i32)                                 :: tmp(1)

    call check (nf90_get_var(self%parent%id, self%id, tmp, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
    data = tmp(1)
  end subroutine getDataScalarI32

  subroutine getData1dI32(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    integer(i32)      , intent(out), allocatable :: data(:)
    integer(i32)                   , allocatable :: datashape(:)
    
    allocate(datashape(1))
    datashape = getReadDataShape(self, 1, start, cnt, stride)
    
    allocate(data(datashape(1)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
  end subroutine getData1dI32

  subroutine getData2dI32(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    integer(i32)      , intent(out), allocatable :: data(:,:)
    integer(i32)                   , allocatable :: datashape(:)

    allocate(datashape(2))
    datashape = getReadDataShape(self, 2, start, cnt, stride)

    allocate(data(datashape(1), datashape(2)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
  end subroutine getData2dI32

  subroutine getData3dI32(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    integer(i32)      , intent(out), allocatable :: data(:,:,:)
    integer(i32)                   , allocatable :: datashape(:)

    allocate(datashape(3))
    datashape = getReadDataShape(self, 3, start, cnt, stride)

    allocate(data(datashape(1), datashape(2), datashape(3)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
  end subroutine getData3dI32

  subroutine getData4dI32(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    integer(i32)      , intent(out), allocatable :: data(:,:,:,:)
    integer(i32)                   , allocatable :: datashape(:)

    allocate(datashape(4))
    datashape = getReadDataShape(self, 4, start, cnt, stride)

    allocate(data(datashape(1), datashape(2), datashape(3), datashape(4)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
  end subroutine getData4dI32

  subroutine getData5dI32(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional   :: start(:), cnt(:), stride(:), map(:)
    integer(i32)      , intent(out), allocatable :: data(:,:,:,:,:)
    integer(i32)                   , allocatable :: datashape(:)

    allocate(datashape(5))
    datashape = getReadDataShape(self, 5, start, cnt, stride)

    allocate(data(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
  end subroutine getData5dI32

  subroutine getDataScalarF32(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)             :: self
    integer(i32)      , intent(in) , optional  :: start(:), cnt(:), stride(:), map(:)
    real(f32)         , intent(out)            :: data
    real(f32)                                  :: tmp(1)

    call check (nf90_get_var(self%parent%id, self%id, tmp, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
    data = tmp(1)
  end subroutine getDataScalarF32

  subroutine getData1dF32(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    real(f32)         , intent(out), allocatable :: data(:)
    integer(i32)                   , allocatable :: datashape(:)

    allocate(datashape(1))
    datashape = getReadDataShape(self, 1, start, cnt, stride)

    allocate(data(datashape(1)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
  end subroutine getData1dF32

  subroutine getData2dF32(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    real(f32)         , intent(out), allocatable :: data(:,:)
    integer(i32)                   , allocatable :: datashape(:)

    allocate(datashape(2))
    datashape = getReadDataShape(self, 2, start, cnt, stride)

    allocate(data(datashape(1), datashape(2)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
  end subroutine getData2dF32

  subroutine getData3dF32(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    real(f32)         , intent(out), allocatable :: data(:,:,:)
    integer(i32)                   , allocatable :: datashape(:)

    allocate(datashape(3))
    datashape = getReadDataShape(self, 3, start, cnt, stride)

    allocate(data(datashape(1), datashape(2), datashape(3)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
  end subroutine getData3dF32

  subroutine getData4dF32(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    real(f32)         , intent(out), allocatable :: data(:,:,:,:)
    integer(i32)                   , allocatable :: datashape(:)

    allocate(datashape(4))
    datashape = getReadDataShape(self, 4, start, cnt, stride)

    allocate(data(datashape(1), datashape(2), datashape(3), datashape(4)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
  end subroutine getData4dF32

  subroutine getData5dF32(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    real(f32)         , intent(out), allocatable :: data(:,:,:,:,:)
    integer(i32)                   , allocatable :: datashape(:)

    allocate(datashape(5))
    datashape = getReadDataShape(self, 5, start, cnt, stride)

    allocate(data(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
  end subroutine getData5dF32

  subroutine getDataScalarF64(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)             :: self
    integer(i32)      , intent(in) , optional  :: start(:), cnt(:), stride(:), map(:)
    real(f64)         , intent(out)            :: data
    real(f64)                                  :: tmp(1)

    call check (nf90_get_var(self%parent%id, self%id, tmp, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
    data = tmp(1)
  end subroutine getDataScalarF64

  subroutine getData1dF64(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    real(f64)         , intent(out), allocatable :: data(:)
    integer(i32)                   , allocatable :: datashape(:)

    allocate(datashape(1))
    datashape = getReadDataShape(self, 1, start, cnt, stride)
    
    allocate(data(datashape(1)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
  end subroutine getData1dF64

  subroutine getData2dF64(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    real(f64)         , intent(out), allocatable :: data(:,:)
    integer(i32)                   , allocatable :: datashape(:)

    allocate(datashape(2))
    datashape = getReadDataShape(self, 2, start, cnt, stride)

    allocate(data(datashape(1), datashape(2)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
  end subroutine getData2dF64

  subroutine getData3dF64(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    real(f64)         , intent(out), allocatable :: data(:,:,:)
    integer(i32)                   , allocatable :: datashape(:)

    allocate(datashape(3))
    datashape = getReadDataShape(self, 3, start, cnt, stride)

    allocate(data(datashape(1), datashape(2), datashape(3)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
  end subroutine getData3dF64

  subroutine getData4dF64(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    real(f64)         , intent(out), allocatable :: data(:,:,:,:)
    integer(i32)                   , allocatable :: datashape(:)

    allocate(datashape(4))
    datashape = getReadDataShape(self, 4, start, cnt, stride)

    allocate(data(datashape(1), datashape(2), datashape(3), datashape(4)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
  end subroutine getData4dF64

  subroutine getData5dF64(self, data, start, cnt, stride, map)
    class(NcVariable), intent(in)               :: self
    integer(i32)      , intent(in) , optional    :: start(:), cnt(:), stride(:), map(:)
    real(f64)         , intent(out), allocatable :: data(:,:,:,:,:)
    integer(i32)                   , allocatable :: datashape(:)

    allocate(datashape(5))
    datashape = getReadDataShape(self, 5, start, cnt, stride)

    allocate(data(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
         "Could not read data from variable: "//trim(self%getName()))
  end subroutine getData5dF64

  function getReadDataShape(var, datarank, instart, incnt, instride)
    type(NcVariable), intent(in)           :: var
    integer(i32)     , intent(in)           :: datarank
    integer(i32)     , intent(in), optional :: instart(:), incnt(:), instride(:)
    integer(i32)     , allocatable          :: readshape(:)
    integer(i32)                            :: naxis
    integer(i32)                            :: getReadDataShape(datarank)

    readshape = var%getShape()
    
    if (present(incnt)) then
       readshape = incnt
    else
       if (present(instart)) then
          readshape(:size(instart)) = readshape(:size(instart)) - (instart - 1)
       end if
       if (present(instride)) then
          readshape(:size(instride)) = readshape(:size(instride)) / instride
       end if
    end if
    
    naxis = count(readshape .gt. 1)
    
    if (all(readshape .eq. 1)) then
       ! return 1-element array
       getReadDataShape(:) = 1 !readshape(1:datarank+1)
    else if (size(readshape) .eq. datarank) then
       ! sizes fit
       getReadDataShape = readshape
    else if (naxis .eq. datarank) then
       getReadDataShape = pack(readshape, readshape .gt. 1)
    ! else if (naxis .lt. datarank) then
       ! would be nice...
    else
       write(*,*) "Given data reading parameters do not match output variable rank!"
       stop 1
    end if
       
  end function getReadDataShape

  function getDtypeFromString(dtype)
    integer(i32)          :: getDtypeFromString
    character(*)         :: dtype

    select case(dtype)
    case("f32")
       getDtypeFromString = NF90_FLOAT
    case("f64")
       getDtypeFromString = NF90_DOUBLE
    case("i8")
       getDtypeFromString = NF90_BYTE
    case("i16")
       getDtypeFromString = NF90_SHORT
    case("i32")
       getDtypeFromString = NF90_INT
    case default
       write(*,*) "Datatype not understood: ", dtype
       stop 1
    end select
  end function getDtypeFromString

  function getDtypeFromInteger(dtype)
    character(3) :: getDtypeFromInteger
    integer(i32)  :: dtype

    select case(dtype)
    case(NF90_FLOAT)
       getDtypeFromInteger = "f32"
    case(NF90_DOUBLE)
       getDtypeFromInteger = "f64"
    case(NF90_BYTE)
       getDtypeFromInteger = "i8"
    case(NF90_SHORT)
       getDtypeFromInteger = "i16"
    case(NF90_INT)
       getDtypeFromInteger = "i32"
    case default
       write(*,*) "Datatype not understood: ", dtype
       stop 1
    end select
  end function getDtypeFromInteger

  function getCreationMode(cmode)
    character(*), intent(in), optional :: cmode
    integer(i32)                       :: getCreationMode
    character(256)                     :: mode

    if (.not. (present(cmode))) then
       mode = "NETCDF4"
    else
       mode = cmode
    end if

    select case(trim(mode))
    case ("NETCDF4")
       getCreationMode = NF90_NETCDF4
    case ("SHARE")
       getCreationMode = NF90_SHARE
    case ("CLASSIC")
       getCreationMode = NF90_CLASSIC_MODEL
    case ("HDF5")
       getCreationMode = NF90_HDF5
    case ("64BIT_OFFSET")
       getCreationMode = NF90_64BIT_OFFSET
    case default
       print*, "Creation mode not understood: " // trim(mode)
       stop 1
    end select
       
  end function getCreationMode

  subroutine check(status, msg)
    integer(i32) , intent(in) :: status
    character(*), intent(in) :: msg

    if (status .ne. NF90_NOERR) then
       write(*,*) msg
       write(*,*) nf90_strerror(status)
       stop 1
    end if
  end subroutine check

end module mo_netcdf

