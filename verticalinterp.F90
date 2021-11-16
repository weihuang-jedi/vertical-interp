!--------------------------------------------------------------------
PROGRAM verticalinterp

   use namelist_module
   use module_grid

   IMPLICIT NONE

   type(gfsgrid) :: grid
   integer :: n

   call read_namelist('input.nml')

   call initialize_grid(grid, input_flnm)

   call output_header(grid, output_flnm, nalt, dz)

   call interpolation(grid)

   call finalize_grid(grid)

END PROGRAM verticalinterp

