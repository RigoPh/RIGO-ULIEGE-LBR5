subroutine check_multi()

!**************************************************************
!
!	Teste :
!		- si les fonctions objectifs des différentes structures
!		  optimisées sont compatibles.
!		
!
!**************************************************************

use param_section, ONLY : nfile,langue,iopti
use PARAM_SECTION_VECTOR, ONLY : icout_vector

integer*4 i,j


if (iopti.gt.0) then

! Test si les fonctions objectifs des différentes structures
do i=1,nfile
	do j=i+1,nfile
		if (icout_vector(i).ne.icout_vector(j)) then
			if (langue.eq.1) then
				write(*,'(a,i2,a,i2)') '!!! Fonction objectif differente entre la structure',i,' et la structure',j
				write(*,'(a)') 'Veuillez corriger vos donnees - appuyer sur ENTER pour poursuivre'
			endif
			if (langue.eq.2) then
				write(*,'(a,i2,a,i2)') '!!! Objective function different between the structure',i,' and the structure',j
				write(*,'(a)') 'Please correct your data - press ENTER to resume'
			endif
			read(*,*)
			stop
		endif
	enddo
enddo

endif

return

end
