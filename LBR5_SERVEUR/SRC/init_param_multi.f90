subroutine init_param_multi()

use param_section

implicit double precision (a-h,o-z)
!character*80 texte

!Définition de la langue TODO la mettre dans le fichier d'input
langue=1

! ====================================================================================================
! number of sections needed
! ====================================================================================================

open(unit=8,file='init.dat',status='old',err=100)

!read(8,'(a)')       texte
read(8,*) nfile

! ====================================================================================================
! read section filename
! ====================================================================================================

call allocate_param_multi()

do iboat=1,nfile
	read(8,'(a)') section_files(iboat)
	! rem.: on extrait la partie qui se termine à .txt, pour ne pas avoir les commentaires
	section_file=section_files(iboat)
	ind = INDEX (section_file,'.txt')
	if (ind.eq.0) then
		if (langue.eq.1) then
			write(*,'(a,i2,a)') '!!! Attention ! Le nom du fichier de la structure',iboat,' ne se termine pas par .txt'
			write(*,'(a)') 'Veuillez corriger vos donnees dans le fichier init.dat - appuyer sur ENTER pour poursuivre'
		endif
		if (langue.eq.2) then
			write(*,'(a,i2,a)') '!!! Attention ! The structure''s filename ',iboat,' has not the extension .txt'
			write(*,'(a)') 'Please correct your data in the file init.dat - press ENTER to resume'
		endif
		read(*,*)
		stop
	endif
	section_files(iboat) = trim(section_file(1:ind+3))
	!section_files(iboat)=section_files(iboat)
	read (8,*) key_gui(iboat)
enddo

! Test si les noms de fichiers sont différents
do iboat=1,nfile
	do j=iboat+1,nfile
		if (section_files(iboat).eq.section_files(j)) then
			if (langue.eq.1) then
				write(*,'(a,i2,a,i2,a)') '!!! Nom de la structure',iboat,' et de la structure',j,' sont identiques !'
				write(*,'(a)') 'Veuillez corriger vos donnees - appuyer sur ENTER pour poursuivre'
			endif
			if (langue.eq.2) then
				write(*,'(a,i2,a,i2,a)') '!!! Name of the structure',i,' and the structure',j,' are identical!'
				write(*,'(a)') 'Please correct your data - press ENTER to resume'
			endif
			read(*,*)
			stop
		endif
	enddo
enddo
!

read (8,*,err=101) iopti,iteram
read (8,'(a)') egalt_multi_filename	!SCAN ('ASTRING', 'ST') has the value 2. 
i = SCAN (egalt_multi_filename,'.dat', BACK = .TRUE.)
if (i.eq.0) then !TODO semble ne pas marcher top en fait (ça ne marche pas quand le string a des espaces
	write(*,*) 'The file Init.dat must contain a restriction equality file name (.dat) in the fifth line'
	write(*,*) 'Please correct your data - press Enter to resume'
	read(*,*)
	stop
else
egalt_multi_filename = egalt_multi_filename(1:i)
endif
egalt_multi_filename = trim(egalt_multi_filename)
close(8)

open(6800,file=egalt_multi_filename)                        

itera    =0						
ico      =0
iter1    =iteram+1
nsolmax  =20  !nombre max de cas de charges retenus pour l'analyse 
              !is est limité à 10 à cause de sensh(13,4,9,nsol) 
              !                        et de dzsn (8*neto,9*neto,nsol)
mt_max   =10  !nbre max de traverses

if (idebug.eq.1) iu_conlin_global=8000


! ====================================================================================================
! vérification des signatures gui et solveur
! ====================================================================================================

!On ne passe pas par sécurité quand on debug !!!
#if DEBUGFRED
!	call security()
#endif

return

! ====================================================================================================
! Message d'erreur 
! ====================================================================================================
100 continue

if (langue.eq.1) then
	write(*,'(a)') 'Stop : le fichier init.dat manque !'
	write(*,'(a)') 'Veuillez corriger vos donnees - appuyer sur ENTER pour poursuivre'
endif
if (langue.eq.2) then
	write(*,'(a)') 'Stop: the "init.dat" file is missing!'
	write(*,'(a)') 'Please correct your data - press ENTER to resume'
endif
read(*,*)

stop 1

101 continue
if (langue.eq.1) then
	write(*,'(a)') '!!! Le parametre iopti est mal defini dans le fichier init.dat !!!'
	write(*,'(a)') 'Veuillez corriger vos donnees - appuyer sur ENTER pour poursuivre'
endif
if (langue.eq.2) then
	write(*,'(a)') '!!! Parameter iopti is bad defined into the file init.dat !!!'
	write(*,'(a)') 'Please correct your data - press ENTER to resume'
endif
read(*,*)

stop 1

end





