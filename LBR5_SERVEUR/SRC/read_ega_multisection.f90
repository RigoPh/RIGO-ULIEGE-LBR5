subroutine read_ega_multisection()
!************************************************************
!
!	Lecture des restrictions d'égalités entre structures
!
!************************************************************

use param_section

use PARAM_SECTION_VECTOR, ONLY : neto_vector


implicit double precision (a-h,o-z)


! ====================================================================================================
! gather global restrictions from each section
! ====================================================================================================


read(6800,*,end=900) negalt_multisection !Renvoi à 900 pour créer alors le fichier avec 0 dedans !

goto 901

900 rewind 6800
write (6800,*) 0

901 negalt_global=sum(negalt)+negalt_multisection !TODO : ligne suivante normalement
!negalt_global = negalt_multisection

IF (.NOT. ALLOCATED(mega_global)) allocate (mega_global(negalt_multisection,6))
IF (.NOT. ALLOCATED(ega_global)) allocate (ega_global(negalt_multisection))

mega_global(:,:)=0

! ====================================================================================================
! read restrictions between each section
! ====================================================================================================

do i=1,negalt_multisection
   read(6800,*,end=903) mega_global(i,1:4),ega_global(i),mega_global(i,5:6)  !,num_boat(i,1:2)
   if (mega_global(i,5).gt.nfile.or.mega_global(i,6).gt.nfile) then
		if (langue.eq.1) then
			write(*,*) 'Le fichier de definition des restrictions d''egalites contient des restrictions relatives a des structures non definies !'
			write(*,*) 'Veuillez corriger vos donnees - appuyer sur ENTER pour poursuivre'
		elseif (langue.eq.2) then
			write(*,*) 'File for defining equality restrictions contains constraints relating to bad structure number!'
			write(*,*) 'Please correct your data - press ENTER to resume'
		endif
		read(*,*)
		stop
   endif
enddo

! Les variables dépendantes sélectionnées ne peuvent être des variables dépendantes des sous-structures !!!
! (sinon conflit possible) => TEST si cest ok
do i=1,negalt_multisection
	do j=1,ntot(mega_global(i,5))
		if ((mega_global(i,1).eq.mega(j,1,mega_global(i,5))).and.(mega_global(i,2).eq.mega(j,2,mega_global(i,5)))) then
		!   num ref restri multi = num ref restri local      and  num pan multi = num pan local
			if (langue.eq.1) then
				write(*,'(a,i3,a)') '! Restriction d''egalites multi-structure ',i,' ne peut contenir une variable qui est deja &
definie comme variable dependante dans sa structure !'
				write(*,*) 'Veuillez corriger vos donnees - appuyer sur ENTER pour poursuivre'
			elseif (langue.eq.2) then
				write(*,'(a,i3,a)') '! Multi-structure equality restriction ',i,' cannot contain one variable that is already &
defined as a dependant variable in its structure!'
				write(*,*) 'Please correct your data - press ENTER to resume'
			endif
			read(*,*)
			stop
		endif
	enddo
enddo
!

! Les variables INdépendantes sélectionnées ne peuvent être des variables dépendantes des sous-structures !!!
! (sinon conflit possible) => TEST si cest ok
! Théoriquement, ça pourrait être le cas mais c'est plus difficile à mettre en oeuvre
do i=1,negalt_multisection
	do j=1,ntot(mega_global(i,6))
		if ((mega_global(i,3).eq.mega(j,1,mega_global(i,6))).and.(mega_global(i,4).eq.mega(j,2,mega_global(i,6)))) then
		!   num ref restri multi = num ref restri local      and  num pan multi = num pan local
			if (langue.eq.1) then
				write(*,'(a,i3,a)') '! Restriction d''egalites multi-structure ',i,' ne peut contenir une variable qui est deja &
definie comme variable dependante dans sa structure !'
				write(*,*) 'Veuillez corriger vos donnees - appuyer sur ENTER pour poursuivre'
			elseif (langue.eq.2) then
				write(*,'(a,i3,a)') '! Multi-structure equality restriction ',i,' cannot contain one variable that is already &
defined as a dependant variable in its structure!'
				write(*,*) 'Please correct your data - press ENTER to resume'
			endif
			read(*,*)
			stop
		endif
	enddo
enddo
!


! Modification des numéros de panel (par bateau => devient pour toute la structure)
do i=1,negalt_multisection
    index_shift=0
	do j=1,mega_global(i,5)-1
		index_shift = index_shift + neto_vector(j)
	enddo
	mega_global(i,2) = mega_global(i,2) + index_shift
	
	index_shift=0
	do j=1,mega_global(i,6)-1
		index_shift = index_shift + neto_vector(j)
	enddo
	mega_global(i,4) = mega_global(i,4) + index_shift
enddo


! ====================================================================================================
!     vérifications des données
! ====================================================================================================
it=0
ixi=0

do i=1,negalt_multisection
   if(mega_global(i,2).eq.mega_global(i,4))then
      if(mega_global(i,1).eq.mega_global(i,3))then
         write(*,*) 'error 1 ds les donnees des contraintes d''egalite MULTI-STRUCTURE'
         write(*,*) 'Restriction egalite n°',i   
   		 write(*,*) 'stop'
		 read(*,*)
         stop
   	 endif	
   endif

   if(mega_global(i,2).lt.it)then
		write(*,*) 'error 2 : contraintes d''egalite MULTI-STRUCTURE mal classees'
		write(*,*) 'Restriction egalite n°',i	
		write(*,*) 'stop'
		read(*,*)
		stop
   endif

   if(mega_global(i,2).eq.it)then
		if(mega_global(i,1).le.ixi)then
			write(*,*) 'error 3: contraintes d''égalite MULTI-STRUCTURE mal classees'
			write(iu_31(iboat),*) 'error 3: contraintes d''egalite mal classees'
			write(*,*) 'Restriction egalite n°',i
			write(*,*) 'stop'
			read(*,*)
			stop
		endif
	endif
	
	it =mega_global(i,2)
	ixi=mega_global(i,1)
enddo


rewind(6800)

return

!
! erreur de lecture du fichier de donnees (unite 6700) ..........................
  903 if(langue.eq.1)write(*,*)'Fichier des restrictions d''egalites manquant ou mal defini!'
      if(langue.eq.2)write(*,*)'Equality restrictions file missing or bad!'
      stop 1

end
