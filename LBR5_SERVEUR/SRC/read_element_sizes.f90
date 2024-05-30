subroutine read_element_sizes()
!**************************************************************************************************
!
! Cette sous-routine va passer en revue tout le fichier d'input pour lire les �l�ments n�cessaires
! � l'allocation de certains vecteurs (sp�cialement des pointeurs de la structure panneau)
!
!**************************************************************************************************

use param_section
integer*4 i,j,nel

! Lecture des donn�es d'en-t�te
!read(

! Lecture par panneau

do nel=1,neto !!! neto pas connu

	do j=1,9
		read(iu_10(iboat),*)	! Saut des 13 premi�res lignes par panneau pour arriver aux d�finitions des param�tres relatifs � la fatigue
	enddo
	read(iu_10(iboat),*) mt !car en fct du nombre de traverse, on lit + ou - beaucoup de lignes
	
	do i=1,mt
		read(iu_10(iboat),*)
		!read(iu_10(iboat),*)
	enddo
	read(iu_10(iboat),*) 		!kcor : fatigue Corrosion factor				!fat_new
	read(iu_10(iboat),*) panneau(nel).I_Fat_Stif		!iboat!!!!!!
	if (panneau(nel).I_Fat_Stif.eq.1) then
		read(iu_10(iboat),*) panneau(nel).nbr_Fat_Stif  ! A stocker dans un autre endroit ??? Probablement !!!
														! en double ??????
		do i=1,panneau(nel).nbr_Fat_Stif
			read(iu_10(iboat),*)
		enddo
	endif

	if (panneau(nel).I_Fat_Plate.eq.1) then
		read(iu_10(iboat),*) panneau(nel).nbr_Fat_Plate
		do i=1,panneau(nel).nbr_Fat_Plate
			read(iu_10(iboat),*)
		enddo
	endif

	if (panneau(nel).I_Fat_Plate_Frame.eq.1) then
		read(iu_10(iboat),*) panneau(nel).nbr_Fat_Plate_Frame
		do i=1,panneau(nel).nbr_Fat_Plate_Frame
			read(iu_10(iboat),*)
		enddo
	endif

	if (panneau(nel).I_Fat_Frame.eq.1) then
		read(iu_10(iboat),*) panneau(nel).nbr_Fat_Frame
		do i=1,panneau(nel).nbr_Fat_Frame
			read(iu_10(iboat),*)
		enddo
	endif


enddo

!!! panneau pas encore allou� normalement !!!

!!! Rewind jusqu'� la bonne ligne !!!!


return
end
