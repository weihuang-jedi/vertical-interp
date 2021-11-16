!--------------------------------------------------------------------
PROGRAM verticalinterp

   use namelist_module
   use module_grid

   IMPLICIT NONE

   type(gfsgrid) :: ingrid, outgrid
   integer :: n

   call read_namelist('input.nml')

   call initialize_ingrid(ingrid, input_flnm)

   call initialize_outgrid(ingrid, outgrid, output_flnm, nalt, dz)

   call interpolation(ingrid, outgrid)

   call finalize_outgrid(outgrid)

   call finalize_ingrid(ingrid)

END PROGRAM verticalinterp

