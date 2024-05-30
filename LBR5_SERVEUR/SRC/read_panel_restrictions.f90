subroutine read_panel_restrictions(nel)

use param_section

implicit double precision (a-h,o-z)

call read_comput_points(nel)

if (m1_read.eq.1) call read_struct_restrictions(nel)

!219  continue  

call save_restrictions(nel)
call print_restrictions(nel)

!if(nsolm_fatigue.gt.0) then
   call read_fatigue_restrictions(nel)
!endif

!read(iu_10(iboat),*) !TODO A supprimer (fred)
!read(iu_10(iboat),*) !TODO A supprimer (fred)

call read_geom_restrictions(nel)
call read_slosh_restrictions(nel)

return
end
