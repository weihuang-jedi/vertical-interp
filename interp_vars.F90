!----------------------------------------------------------------------------------------
subroutine output_header(grid, flnm, nalt, dz)
  
   use netcdf
   use module_grid

   implicit none

   type(gfsgrid),    intent(inout) :: grid
   character(len=*), intent(in)    :: flnm
   integer,          intent(in)    :: nalt
   real,             intent(in)    :: dz

   integer :: i, rc

   print *, 'Enter generate_header'
   print *, 'flnm = ', trim(flnm)
  
   grid%nalt = nalt
   allocate(grid%alt(grid%nalt))

   do i = 1, grid%nalt
      grid%alt(i) = real(i-1)*dz
   end do

   call create_coord(grid, flnm)

   call create_var_attr(grid)

  !End define mode.
   rc = nf90_enddef(grid%ncid)
   if(rc /= nf90_noerr) then
      write(unit=0, fmt='(a,i6,a)') "Problem to enddef ncid: <", grid%ncid, ">."
      write(unit=0, fmt='(2a)') "Error status: ", trim(nf90_strerror(rc))
      write(unit=0, fmt='(3a, i4)') &
           "Stop in file: <", __FILE__, ">, line: ", __LINE__
      stop
   end if

   print *, 'Leave generate_header'

end subroutine output_header

!---------------------------------------------------------------------------
subroutine create_global_attr(ncid, filename, title)

   implicit none

   integer, intent(in) :: ncid
   character(len = *), intent(in) :: filename, title

  !put global attributes
   call nc_putGlobalCharAttr(ncid, 'filename', trim(filename))
   call nc_putGlobalCharAttr(ncid, 'title', trim(title))

end subroutine create_global_attr

!----------------------------------------------------------------------------------------
subroutine create_coord(grid, flnm)

   use netcdf
   use module_grid

   implicit none

   type(gfsgrid),    intent(inout) :: grid
   character(len=*), intent(in)    :: flnm

   integer :: i, nd, rc, ncid

   integer, dimension(2) :: dimids

   logical :: fileExists

   print *, 'Enter create_coord'
   print *, 'flnm = ', trim(flnm)

   print *, 'grid%nlon = ',  grid%nlon
   print *, 'grid%nlat = ',  grid%nlat
   print *, 'grid%nalt = ',  grid%nalt
   print *, 'grid%npre = ',  grid%npre

   print *, 'grid%lon = ',  grid%lon
   print *, 'grid%lat = ',  grid%lat
   print *, 'grid%alt = ',  grid%alt
   print *, 'grid%pre = ',  grid%pre

   rc = nf90_noerr

   grid%output_flnm = trim(flnm)

  !Create the file. 
   inquire(file=trim(flnm), exist=fileExists)
   if (fileExists) then
      open(unit=1234, iostat=rc, file=trim(flnm), status='old')
      if(rc == 0) close(1234, status='delete')
   end if

   rc = nf90_create(trim(flnm), NF90_NETCDF4, ncid)
   call check_status(rc)

   grid%ncid = ncid
   print *, 'ncid = ', ncid

   rc = nf90_def_dim(ncid, 'lon', grid%nlon, grid%dimidlon)
   call check_status(rc)
   rc = nf90_def_dim(ncid, 'lat', grid%nlat, grid%dimidlat)
   call check_status(rc)
   rc = nf90_def_dim(ncid, 'alt', grid%nalt, grid%dimidalt)
   call check_status(rc)

   call create_global_attr(ncid, flnm, 'Data in Height level')

   dimids(1) = grid%dimidlon
   nd = 1
!--Field lon
   call nc_putAxisAttr(ncid, nd, dimids, NF90_REAL, &
                      'lon', &
                      "Lontitude Coordinate", &
                      "degree_east", &
                      "Longitude" )

   dimids(1) = grid%dimidlat
!--Field lat
   call nc_putAxisAttr(ncid, nd, dimids, NF90_REAL, &
                      'lat', &
                      "Latitude Coordinate", &
                      "degree_north", &
                      "Latitude" )

   dimids(1) = grid%dimidalt
!--Field alt
   call nc_putAxisAttr(ncid, nd, dimids, NF90_REAL, &
                      'alt', &
                      "Altitude Coordinate", &
                      "upward", &
                      "Altitude" )

  !write lon
   call nc_put1Dvar0(ncid, 'lon', grid%lon, 1, grid%nlon)

  !write lat
   call nc_put1Dvar0(ncid, 'lat', grid%lat, 1, grid%nlat)

  !write alt
   call nc_put1Dvar0(ncid, 'alt', grid%alt, 1, grid%nalt)

  !print *, 'Leave create_coord'
end subroutine create_coord

!-------------------------------------------------------------------------------------
subroutine create_var_attr(grid)

   use netcdf
   use module_grid

   implicit none

   type(gfsgrid), intent(inout) :: grid

   integer, dimension(6) :: dimids
   integer :: rc, nd, i
   integer :: missing_int
   real    :: missing_real
   character(len=80) :: long_name, units, coordinates

  !print *, 'Enter create_fv_core_var_attr'

   missing_real = -1.0e38
   missing_int = -999999

   do i = 1, grid%nVars
      if(grid%vars(i)%nDims < 2) then
         cycle
      end if

      long_name = trim(grid%vars(i)%varname)
      units = 'unknown'
      coordinates = 'alt lat lon'
      dimids(1) = grid%dimidlon
      dimids(2) = grid%dimidlat
      dimids(3) = grid%dimidalt
      nd = 3

      if('TMP_P0_L1_GLL0' == trim(grid%vars(i)%varname)) then
         coordinates = 'lat lon'
         nd = 2
         long_name = 'Ground or water surface temperature'
         units = 'K'
     !else if('TMP_P0_L1_GLL0' == trim(grid%vars(i)%varname)) then
      else
         cycle
      end if

      call nc_putAttr(grid%ncid, nd, dimids, NF90_REAL, &
                      trim(grid%vars(i)%varname), &
                      trim(long_name), trim(units), &
                      trim(coordinates), missing_real)
   end do

end subroutine create_var_attr

!----------------------------------------------------------------------------------------
subroutine interpolation(grid)

   use netcdf
   use module_grid

   implicit none

   type(gfsgrid), intent(inout) :: grid

   integer :: i, n, rc

   real, dimension(grid%nlon, grid%nlat, grid%nalt) :: var3d

   print *, 'Enter interpolation'

   do i = 1, grid%nVars
      if(grid%vars(i)%nDims < 2) cycle

      rc = nf90_inquire_variable(grid%fileid, grid%varids(i), &
               ndims=grid%vars(i)%nDims, natts=grid%vars(i)%nAtts)
      call check_status(rc)

      print *, 'Var No. ', i, ': ndims = ', grid%vars(i)%nDims
      print *, 'Var No. ', i, ': name: ', trim(grid%vars(i)%varname)

      rc = nf90_inquire_variable(grid%fileid, grid%varids(i), &
               name=grid%vars(i)%varname)
      call check_status(rc)

     !call interp2hgt(grid, var3d)

   end do

   print *, 'Leave interpolation'
end subroutine interpolation

!----------------------------------------------------------------------
subroutine interp2hgt(grid, var3d)

  use module_grid

  implicit none

  type(gfsgrid), intent(in) :: grid
  real, dimension(grid%nlon, grid%nlat, grid%nalt), intent(out) :: var3d

  integer :: i, j, k, n

  real :: dz

  do j = 1, grid%nlat
  do i = 1, grid%nlon
  do k = 1, grid%nalt
     if(grid%alt(k) <= grid%hgt(i,j,grid%npre)) then
        var3d(i, j, k) = grid%var3d(i, j, grid%npre)
     else if(grid%alt(k) >= grid%hgt(i,j,1)) then
        var3d(i, j, k) = grid%var3d(i, j, 1)
     else
        do n = 2, grid%npre
           if(grid%alt(k) >= grid%hgt(i,j,n)) then
              dz = (grid%alt(k) - grid%hgt(i,j,n)) &
                 / (grid%hgt(i,j,n-1) - grid%hgt(i,j,n))
              var3d(i, j, k) = dz*grid%var3d(i, j, n-1) &
                 + (1.0-dz)*grid%var3d(i, j, n)
              exit
           end if
        end do
     end if
  end do
  end do
  end do

end subroutine interp2hgt

