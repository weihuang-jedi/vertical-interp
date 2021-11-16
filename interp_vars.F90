!----------------------------------------------------------------------------------------
subroutine initialize_outgrid(ingrid, outgrid, flnm, nalt, dz)
  
   use netcdf
   use module_grid

   implicit none

   type(gfsgrid),    intent(inout) :: ingrid
   type(gfsgrid),    intent(out)   :: outgrid
   character(len=*), intent(in)    :: flnm
   integer,          intent(in)    :: nalt
   real,             intent(in)    :: dz

   integer :: i, rc

   print *, 'Enter generate_header'
   print *, 'flnm = ', trim(flnm)
  
   ingrid%nlat = nalt
   allocate(ingrid%alt(ingrid%nalt))

   outgrid%nlat = nalt
   allocate(outgrid%alt(outgrid%nalt))

   do i = 1, outgrid%nalt
      ingrid%alt(i) = real(i-1)*dz
      outgrid%alt(i) = real(i-1)*dz
   end do

   call create_coord(ingrid, outgrid, flnm)

   call create_var_attr(ingrid, outgrid)

  !End define mode.
   rc = nf90_enddef(outgrid%ncid)
   if(rc /= nf90_noerr) then
      write(unit=0, fmt='(a,i6,a)') "Problem to enddef ncid: <", outgrid%ncid, ">."
      write(unit=0, fmt='(2a)') "Error status: ", trim(nf90_strerror(rc))
      write(unit=0, fmt='(3a, i4)') &
           "Stop in file: <", __FILE__, ">, line: ", __LINE__
      stop
   end if

   print *, 'Leave generate_header'

end subroutine initialize_outgrid

!----------------------------------------------------------------------------------------
subroutine finalize_outgrid(grid)

   use netcdf
   use module_grid

   implicit none

   type(gfsgrid), intent(inout) :: grid
   integer :: rc

   rc =  nf90_close(grid%ncid)
   call check_status(rc)

   call finalize_ingrid(grid)

   print *, 'Finished Write to file: ', trim(grid%filename)

end subroutine finalize_outgrid

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
subroutine create_coord(ingrid, outgrid, flnm)

   use netcdf
   use module_grid

   implicit none

   type(gfsgrid),    intent(inout) :: ingrid
   type(gfsgrid),    intent(inout) :: outgrid
   character(len=*), intent(in)    :: flnm

   integer :: i, nd, rc, ncid

   integer, dimension(2) :: dimids

   logical :: fileExists

   print *, 'Enter create_coord'
   print *, 'flnm = ', trim(flnm)

   print *, 'outgrid%nlon = ',  outgrid%nlon
   print *, 'outgrid%nlat = ',  outgrid%nlat
   print *, 'outgrid%nalt = ',  outgrid%nalt
   print *, 'outgrid%npre = ',  outgrid%npre

   print *, 'outgrid%lon = ',  outgrid%lon
   print *, 'outgrid%lat = ',  outgrid%lat
   print *, 'outgrid%alt = ',  outgrid%alt
   print *, 'outgrid%pre = ',  outgrid%pre

   rc = nf90_noerr

   outgrid%filename = trim(flnm)

  !Create the file. 
   inquire(file=trim(flnm), exist=fileExists)
   if (fileExists) then
      open(unit=1234, iostat=rc, file=trim(flnm), status='old')
      if(rc == 0) close(1234, status='delete')
   end if

   rc = nf90_create(trim(flnm), NF90_NETCDF4, ncid)
   call check_status(rc)

   outgrid%ncid = ncid
   print *, 'ncid = ', ncid

   rc = nf90_def_dim(ncid, 'lon', outgrid%nlon, outgrid%dimidlon)
   call check_status(rc)
   rc = nf90_def_dim(ncid, 'lat', outgrid%nlat, outgrid%dimidlat)
   call check_status(rc)
   rc = nf90_def_dim(ncid, 'alt', outgrid%nalt, outgrid%dimidalt)
   call check_status(rc)

   call create_global_attr(ncid, flnm, 'Data in Height level')

   dimids(1) = outgrid%dimidlon
   nd = 1
!--Field lon
   call nc_putAxisAttr(ncid, nd, dimids, NF90_REAL, &
                      'lon', &
                      "Lontitude Coordinate", &
                      "degree_east", &
                      "Longitude" )

   dimids(1) = outgrid%dimidlat
!--Field lat
   call nc_putAxisAttr(ncid, nd, dimids, NF90_REAL, &
                      'lat', &
                      "Latitude Coordinate", &
                      "degree_north", &
                      "Latitude" )

   dimids(1) = outgrid%dimidalt
!--Field alt
   call nc_putAxisAttr(ncid, nd, dimids, NF90_REAL, &
                      'alt', &
                      "Altitude Coordinate", &
                      "upward", &
                      "Altitude" )

  !write lon
   call nc_put1Dvar0(ncid, 'lon', outgrid%lon, 1, outgrid%nlon)

  !write lat
   call nc_put1Dvar0(ncid, 'lat', outgrid%lat, 1, outgrid%nlat)

  !write alt
   call nc_put1Dvar0(ncid, 'alt', outgrid%alt, 1, outgrid%nalt)

  !print *, 'Leave create_coord'
end subroutine create_coord

!-------------------------------------------------------------------------------------
subroutine create_var_attr(ingrid, outgrid)

   use netcdf
   use module_grid

   implicit none

   type(gfsgrid), intent(inout) :: ingrid
   type(gfsgrid), intent(inout) :: outgrid

   integer, dimension(6) :: dimids
   integer :: rc, nd, i
   integer :: missing_int
   real    :: missing_real
   character(len=80) :: long_name, units, coordinates

  !print *, 'Enter create_fv_core_var_attr'

   missing_real = -1.0e38
   missing_int = -999999

   do i = 1, ingrid%nVars
      if(ingrid%vars(i)%nDims < 2) then
         cycle
      end if

      long_name = trim(ingrid%vars(i)%varname)
      units = 'unknown'
      coordinates = 'alt lat lon'
      dimids(1) = outgrid%dimidlon
      dimids(2) = outgrid%dimidlat
      dimids(3) = outgrid%dimidalt
      nd = 3

      if('TMP_P0_L1_GLL0' == trim(ingrid%vars(i)%varname)) then
         coordinates = 'lat lon'
         nd = 2
         long_name = 'Ground or water surface temperature'
         units = 'K'
     !else if('TMP_P0_L1_GLL0' == trim(ingrid%vars(i)%varname)) then
      else
         cycle
      end if

      call nc_putAttr(outgrid%ncid, nd, dimids, NF90_REAL, &
                      trim(ingrid%vars(i)%varname), &
                      trim(long_name), trim(units), &
                      trim(coordinates), missing_real)
   end do

end subroutine create_var_attr

!----------------------------------------------------------------------------------------
subroutine interpolation(ingrid, outgrid)

   use netcdf
   use module_grid

   implicit none

   type(gfsgrid), intent(in)    :: ingrid
   type(gfsgrid), intent(inout) :: outgrid

   integer :: i, n, rc

   real, dimension(outgrid%nlon, outgrid%nlat, outgrid%nalt) :: var3d

   print *, 'Enter interpolation'

   do i = 1, ingrid%nVars
      if(ingrid%vars(i)%nDims < 2) cycle

      rc = nf90_inquire_variable(ingrid%fileid, ingrid%varids(i), &
               ndims=ingrid%vars(i)%nDims, natts=ingrid%vars(i)%nAtts)
      call check_status(rc)

      print *, 'Var No. ', i, ': ndims = ', ingrid%vars(i)%nDims
      print *, 'Var No. ', i, ': name: ', trim(tile(n)%vars(i)%varname)

      rc = nf90_inquire_variable(ingrid%fileid, ingrid%varids(i), &
               name=ingrid%vars(i)%varname)
      call check_status(rc)

   end do

   print *, 'Leave interpolation'
end subroutine interp2outgridgrid

!----------------------------------------------------------------------
subroutine interp3dvar(ingrid, outgrid, var3d)

  use module_grid

  implicit none

  type(gfsgrid), intent(in) :: ingrid
  type(gfsgrid), intent(in) :: outgrid
  real, dimension(outgrid%nlon, outgrid%nlat, outgrid%nlev), intent(out) :: var3d

  integer :: i, j, k, n, ik, jk, m
  real :: w

  do jk = 1, outgrid%nlat
  do ik = 1, outgrid%nlon
     do k = 1, outgrid%nlev
        var3d(ik, jk, k) = 0.0
     end do

     do m = 1, outgrid%npnt
        n = outgrid%tile(ik, jk, m)
        i = outgrid%ilon(ik, jk, m)
        j = outgrid%jlat(ik, jk, m)
        w = outgrid%wgt(ik, jk, m)

        do k = 1, outgrid%nlev
           var3d(ik, jk, k) = var3d(ik, jk, k) + w*tile(n)%var3d(i, j, k)
        end do
     end do
  end do
  end do

end subroutine interp3dvar

