subroutine read_fatigue_restrictions(nel)

use param_section

implicit double precision (a-h,o-z)

character*80 title_

if (itype(nel).ne.5) then

	read (iu_10(iboat),*,end=900) title_ 
	read (iu_10(iboat),*,end=900) panneau(nel).cjmax_fatigue

endif

return

! -------------------------------------------------------------------------------
! -------------------------------------------------------------------------------
900 write(* ,*)'erreur de lecture : "end of file"'
    write(iu_11(iboat),*)'erreur de lecture : "end of file"'
    write(iu_14(iboat),*)'erreur de lecture : "end of file"'
    write(*,*) 'stop'
    read(*,*)
    stop
end
