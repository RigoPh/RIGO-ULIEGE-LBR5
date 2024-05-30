subroutine rewind_all()

use param_section

rewind iu_10(iboat)
!rewind iu_32(iboat) 
if(ibusc.ne.0) rewind iu_20(iboat)
if(ibusc.ne.0) rewind iu_33(iboat)
rewind iu_21(iboat) 
!rewind iu_26(iboat)   	
rewind iu_27(iboat)	
rewind iu_30(iboat)	
!rewind iu_100(iboat)

close (iu_13(iboat))	

return
end

subroutine rewind_to_first_panel()


use PARAM_SECTION_VECTOR
use param_section

do iboat=1,nfile
	!On passe les 1ères lignes pour arriver aux panneaux
	rewind (iu_10(iboat))
	read(iu_10(iboat),'(12(/))')	!Saut des 13 premières lignes

	do i=1,nbrMat_vector(iboat)
	   read(iu_10(iboat),'(13(/))') !Saut des 14 paramètres du panneau
	enddo

	read(iu_10(iboat),'(4(/))')	    !Saut de 5 paramètres globaux

	do i=1,nsolm_vector(iboat)
	   read(iu_10(iboat),'(1(/))')	!Saut de 2 paramètres globaux
	enddo


    !do i=1,nsolm_fatigue_vector(iboat)
	!   read(iu_10(iboat),*)	    !Saut de tirant(i)
	!enddo

	do i=1,nsolm_vector(iboat)
		read(iu_10(iboat),*)	    !Saut de lignes !Saut de tirant
	enddo

    read(iu_10(iboat),'(5(/))')	!Saut de 6 paramètres globaux

enddo

return
end
