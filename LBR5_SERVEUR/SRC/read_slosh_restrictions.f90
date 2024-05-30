subroutine read_slosh_restrictions(nel)

use param_section

implicit double precision (a-h,o-z)

! ====================================================================================================
! lecture des sollicitations
!  nsloshm = nbre de cas de contraintes de sloshing pour ce panneau
!  nslosh = liste des contraintes de sloshing étudiées.
!  slosh = valeur des contraintes de sloshing
! ====================================================================================================

if (itype(nel).ne.5) then
	read (iu_10(iboat),*,end=900) !Lecture de Sloshing Constraint
	read(iu_10(iboat),*) panneau(nel).nsloshm,(panneau(nel).nslosh(i),i=1,panneau(nel).nsloshm)
	if (panneau(nel).nsloshm.gt.0) read(iu_10(iboat),*) panneau(nel).press_slosh

	!Calcul des contraintes et des sensibilités
	if (iopti.gt.0) call sloshing(nel)
endif

m4cont(nel)=panneau(nel).nsloshm !

return

900 write(* ,*)'Erreur de lecture : "end of file"'

end
