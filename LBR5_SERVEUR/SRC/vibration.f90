subroutine vibration()
!------------------------------------------------------------------------------
!							Module Vibration
!
!	Ce module va donner les fréquences propres de chaque panneau sélectionné
!	par l'utilisateur. Il peut s'agir :
!
!	- d'un panneau LBR-5;
!	- d'une partie d'un panneau LBR-5;
!	- d'un ensemble de panneau LBR-5;
!
!------------------------------------------------------------------------------
use param_section, MODE_=>MODE			! Pour avoir les infos panneaux
				! Pour avoir le nom du fichier sol2
		! Pour avoir les infos relatives aux conditions de bord

implicit double precision(a-h,o-z)
double precision list_elmt_vib(100,3)	!1ere col.: numéro de l'élément de vibration
										!2ème col.: nbr de panneaux dans cet élément
										!3ème col.: longueur de cet élément
double precision temp_(100,3)

character*3 mode
double precision epaiss_pond,longueur,max
integer*4 found,sens,ep_ponderee

type def_elt_vib
	integer*4 id							!identifiant de l'élément
	integer*4 nbrpan						!nombre de panneaux
	integer*4 List(1000)					!Liste contenant les panneaux
	! TODO allouer la vraie taille
	double precision list_pos_raid(1000,2)  !1ere col.: position du raidisseur
	double precision list_pos_cadre(1000,2) !2ème col.: type de raidisseur
	double precision nbr_type				!nbr types raid.diff. (normalement entier)
	double precision list_type(1000,4)		!haut.ame, ep.ame, larg.sem.,ep.sem.
	double precision long					!longueur de l'élément
	double precision larg					!largeur de l'élément
	integer*4 nbr_raid						!nombre de raidisseurs sur l'élément
	integer*4 nbr_cadre						!nombre de cadres sur l'élément
	double precision freq_propre(3)			!OUTPUT = frequences propres résultant du calcul de vibration
	integer*4 cb							!num.de condition de bord pour vibration (voir rapport Adrian Contantinescou)
	character*16 cb_g						!cond.de bord à gauche
	character*16 cb_d						!cond.de bord à droite
	character*16 cb_av						!cond.de bord à l'avant (X=0 si sens non inversé)
	character*16 cb_ar						!cond.de bord à l'arrière (X=long si sens non inversé)
	integer*4 rotation						!=0 normal; =1 => on doit inverser cadres et raidisseurs
	integer*4 icd_g							!condition de bord gauche du panneau (avant rotation éventuelle)
	integer*4 icd_d							!condition de bord droite du panneau (avant rotation éventuelle)
	integer*4 cas_sym						!Si différent de 0 => symétrie => on doit étudier le panneau dans son entiereté
	double precision delta					!=épaisseur moyenne pondérée
end type

type(def_elt_vib), allocatable, save :: elt_vib(:)

call get_section_header_vector()                       
call allocate_param_section()
call allocate_panel_data()
call get_panel_data_global()


!	Détermination du nombre d'éléments à étudier et du nombre de panneau(x) par élément
nbr_elmt_vib = 0
do nel=1,neto
	if ((panneau(nel).elmt_vib).ne.0) then
		!On regarde si on l'a déjà
		found = 0
		do i=1,nbr_elmt_vib
			if (list_elmt_vib(i,1).eq.(panneau(nel).elmt_vib)) then
				found=1
				list_elmt_vib(i,2) = list_elmt_vib(i,2) + 1
			endif
		enddo
		if (found.eq.0) then
			nbr_elmt_vib = nbr_elmt_vib + 1
			list_elmt_vib(nbr_elmt_vib,1) = panneau(nel).elmt_vib
			list_elmt_vib(nbr_elmt_vib,2) = 1
			list_elmt_vib(nbr_elmt_vib,3) = panneau(nel).long_vib
		endif
	endif
enddo

allocate (elt_vib(nbr_elmt_vib))
! Initialisation
do i=1,nbr_elmt_vib
	call annuli(elt_vib(i).List,100)
	call annuld(elt_vib(i).list_pos_raid,1000*2)
	call annuld(elt_vib(i).list_pos_cadre,1000*2)
	call annuld(elt_vib(i).list_type,1000*4)
	call annuld(elt_vib(i).freq_propre,3)
enddo

! Tri de list_elmt_vib pour traiter d'abord le pan de vibr 1, puis le 2 etc
! (facultatif)
do i=1,nbr_elmt_vib
    ! Recherche du plus grand
	max=0
    do j=1,nbr_elmt_vib
		if (list_elmt_vib(j,1).gt.max) then 
			max = list_elmt_vib(j,1)
			ind_max = j
		endif
	enddo
	temp_(nbr_elmt_vib-i+1,1) = list_elmt_vib(ind_max,1)
	temp_(nbr_elmt_vib-i+1,2) = list_elmt_vib(ind_max,2)
	temp_(nbr_elmt_vib-i+1,3) = list_elmt_vib(ind_max,3)
	list_elmt_vib(ind_max,1) = 0
enddo
do i=1,nbr_elmt_vib
	list_elmt_vib(i,1) = temp_(i,1)
	list_elmt_vib(i,2) = temp_(i,2)
	list_elmt_vib(i,3) = temp_(i,3)
enddo
!

do i=1,nbr_elmt_vib
	elt_vib(i).id = list_elmt_vib(i,1)
	elt_vib(i).nbrpan = list_elmt_vib(i,2)
	elt_vib(i).larg = list_elmt_vib(i,3)
	do nel=1,neto
		if ((panneau(nel).elmt_vib).eq.(elt_vib(i).id)) then
			call add_int(elt_vib(i).List,nel)
		endif
	enddo
enddo

! Tri de la liste des panneaux par élément pour qu'ils soient dans l'ordre réel successif
do i=1,nbr_elmt_vib
	do j=1,elt_vib(i).nbrpan !on le trie npan fois !
	!!! Tri de gauche à droite !!! (si vert. = de bas en haut)
		call trie_liste(elt_vib(i).List(1:elt_vib(i).nbrpan),elt_vib(i).nbrpan)
	enddo
enddo


! Calcul de la longueur de l'élément et du nbr raidisseurs/cadres (AVEC SECONDAIRES)
do i=1,nbr_elmt_vib
	elt_vib(i).long = 0.d00
	elt_vib(i).nbr_raid = 0
	elt_vib(i).nbr_cadre = 0
	debut = 0.d00	!
	elt_vib(i).nbr_type = 0	!nombre de type de raidisseurs différents par élément
	do j=1,elt_vib(i).nbrpan
		hauteur = abs(panneau(elt_vib(i).List(j)).hight) !hauteur du panneau
		elt_vib(i).long = elt_vib(i).long + abs(panneau(elt_vib(i).List(j)).hight)

		ecartement = panneau(elt_vib(i).List(j)).entr	 !entredistance des raid
		
		if (panneau(elt_vib(i).List(j)).hxr.gt.0.001) then
	
			mode = panneau(elt_vib(i).List(j)).mode			 ! mode EE1 ou EE2
			if ((mode.eq.'EE1').or.(mode.eq.'ee1')) then
				nbr_raid = nint(hauteur/ecartement) - 1		!nint = arrondi à l'entier le plus proche
			else
				nbr_raid = nint(hauteur/ecartement)
			endif
		
			if (z(elt_vib(i).List(j),1).eq.z(elt_vib(i).List(j),1)) then !alors pan vertical
				if (z(elt_vib(i).List(j),3).lt.z(elt_vib(i).List(j),4)) then !sens LBR5 = sens vibration
					sens = 0
				else
					sens = 1
				endif
			else
				if (z(elt_vib(i).List(j),1).lt.z(elt_vib(i).List(j),2)) then !sens LBR5 = sens vibration
					sens = 0
				else
					sens = 1
				endif
			endif

			elt_vib(i).nbr_type = elt_vib(i).nbr_type + 1
			call insert_pos(elt_vib(i).list_pos_raid,mode,ecartement,nbr_raid,debut,	&
						elt_vib(i).nbr_type,sens,hauteur)
			elt_vib(i).list_type(elt_vib(i).nbr_type,1) = panneau(elt_vib(i).List(j)).hxr
			elt_vib(i).list_type(elt_vib(i).nbr_type,2) = panneau(elt_vib(i).List(j)).dxr
			elt_vib(i).list_type(elt_vib(i).nbr_type,3) = panneau(elt_vib(i).List(j)).wxr
			elt_vib(i).list_type(elt_vib(i).nbr_type,4) = panneau(elt_vib(i).List(j)).txr
	
			elt_vib(i).nbr_raid = elt_vib(i).nbr_raid + nbr_raid
		endif

		if (((panneau(elt_vib(i).List(j)).entr2).ge.(1.0d-5))		&
			.and.((panneau(elt_vib(i).List(j)).hxr2).gt.0.001)) then !ALORS RAIDISSEURS SECONDAIRES
			ecartement = panneau(elt_vib(i).List(j)).entr2	 !entredistance des raid secondaires
			nbr_raid = hauteur/ecartement !
			elt_vib(i).nbr_type = elt_vib(i).nbr_type + 1
			call insert_pos(elt_vib(i).list_pos_raid,'EE2',ecartement,nbr_raid,	&
							debut,elt_vib(i).nbr_type,sens,hauteur)
			elt_vib(i).list_type(elt_vib(i).nbr_type,1) = panneau(elt_vib(i).List(j)).hxr2
			elt_vib(i).list_type(elt_vib(i).nbr_type,2) = panneau(elt_vib(i).List(j)).dxr2
			elt_vib(i).list_type(elt_vib(i).nbr_type,3) = panneau(elt_vib(i).List(j)).wxr2
			elt_vib(i).list_type(elt_vib(i).nbr_type,4) = panneau(elt_vib(i).List(j)).txr2

			elt_vib(i).nbr_raid = elt_vib(i).nbr_raid + nbr_raid
		endif

		if ((panneau(elt_vib(i).List(j)).mt).gt.0) then		!ALORS TRAVERSES
			do k=1,panneau(elt_vib(i).List(j)).mt
				elt_vib(i).nbr_type = elt_vib(i).nbr_type + 1
				if (sens.eq.0) then
					call insert_double(elt_vib(i).list_pos_raid,					&
							(debut+panneau(elt_vib(i).List(j)).abtr2(k)),			&
							elt_vib(i).nbr_type)
				else
					call insert_double(elt_vib(i).list_pos_raid,					&
							(debut+hauteur-panneau(elt_vib(i).List(j)).abtr2(k)),	&
							elt_vib(i).nbr_type)
				endif
				elt_vib(i).list_type(elt_vib(i).nbr_type,1) = panneau(elt_vib(i).List(j)).hxtr(k)
				elt_vib(i).list_type(elt_vib(i).nbr_type,2) = panneau(elt_vib(i).List(j)).dxtr(k)
				elt_vib(i).list_type(elt_vib(i).nbr_type,3) = panneau(elt_vib(i).List(j)).wxtr(k)
				elt_vib(i).list_type(elt_vib(i).nbr_type,4) = panneau(elt_vib(i).List(j)).txtr(k)
			enddo
			elt_vib(i).nbr_raid = elt_vib(i).nbr_raid + panneau(elt_vib(i).List(j)).mt
		endif

		debut = debut + hauteur
	enddo
	
	nbr_cadre = elt_vib(i).larg/panneau(elt_vib(i).List(1)).epsa
	
	elt_vib(i).nbr_type = elt_vib(i).nbr_type + 1
	call insert_pos(elt_vib(i).list_pos_cadre,'EE2',panneau(elt_vib(i).List(1)).epsa,	&
					nbr_cadre,0.d00,elt_vib(i).nbr_type,sens,elt_vib(i).larg) !ici 'sens' pas important
	elt_vib(i).list_type(elt_vib(i).nbr_type,1) = panneau(elt_vib(i).List(1)).hya
	elt_vib(i).list_type(elt_vib(i).nbr_type,2) = panneau(elt_vib(i).List(1)).dya
	elt_vib(i).list_type(elt_vib(i).nbr_type,3) = panneau(elt_vib(i).List(1)).wya
	elt_vib(i).list_type(elt_vib(i).nbr_type,4) = panneau(elt_vib(i).List(1)).tya

	elt_vib(i).nbr_cadre = elt_vib(i).nbr_cadre + nbr_cadre

	if (((panneau(elt_vib(i).List(1)).epsa2).ge.(1.0d-5))	&
		.and.((panneau(elt_vib(i).List(1)).hya2).gt.0.001))then !ALORS CADRES SECONDAIRES
		nbr_cadre = elt_vib(i).larg/panneau(elt_vib(i).List(1)).epsa2
		elt_vib(i).nbr_type = elt_vib(i).nbr_type + 1
		call insert_pos(elt_vib(i).list_pos_cadre,'EE2',panneau(elt_vib(i).List(1)).epsa2, &
						nbr_cadre,0.d00,elt_vib(i).nbr_type,sens,elt_vib(i).larg) !ici 'sens' pas important
		elt_vib(i).list_type(elt_vib(i).nbr_type,1) = panneau(elt_vib(i).List(1)).hya2
		elt_vib(i).list_type(elt_vib(i).nbr_type,2) = panneau(elt_vib(i).List(1)).dya2
		elt_vib(i).list_type(elt_vib(i).nbr_type,3) = panneau(elt_vib(i).List(1)).wya2
		elt_vib(i).list_type(elt_vib(i).nbr_type,4) = panneau(elt_vib(i).List(1)).tya2

		elt_vib(i).nbr_cadre = elt_vib(i).nbr_cadre + nbr_cadre
	endif
enddo

! Détermination des conditions limites
do i=1,nbr_elmt_vib
	icd_g = 0		! pas supprimer !
	icd_g = 0
	!On cherche si le 1er panneau de l'élément a un panneau qui arrive sur lui
	found=0
	do k=1,neto
		do kk=1,10
			if (noh(k,kk).eq.elt_vib(i).List(1)) found = 1
		enddo
	enddo

	do j=1,ncondi
		if (neto.eq.1) then	!Cas particulier : le 1er chiffre de nno n'est plus le numéro de panneau
			icd_g = nno9(1,2)
			icd_d = nno9(2,2)
		else
			if (nno9(j,1).eq.elt_vib(i).List(1)) then !on regarde si il y a une CL sur le 1er panneau de l'élément
				if (found.eq.0) then !=>1er elmt n'a pas de panneau qui arrive sur lui
					icd_g = nno9(j,2) !condition limite du premier panneau de l'élément
				endif
			endif
			if (nno9(j,1).eq.elt_vib(i).List(elt_vib(i).nbrpan)) then
				if (noh(elt_vib(i).List(elt_vib(i).nbrpan),1).eq.0) then !=>dernier elmt n'a pas de voisins
					icd_d = nno9(j,2) !condition limite du dernier panneau de l'élément
				endif
			endif
		endif
	enddo
	if (icd_g.eq.0) icd_g = 2 ! 2=>simply supported
	if (icd_d.eq.0) icd_d = 2 ! 2=>simply supported

	elt_vib(i).icd_g = icd_g ! sauvegarde de la CL pour plus tard tester si y a une symétrie ou non
	elt_vib(i).icd_d = icd_d

	icd_av = panneau(elt_vib(i).List(1)).cl_av_vib ! 2=>simply supported, 6=>clamped
	icd_ar = panneau(elt_vib(i).List(1)).cl_ar_vib ! 2=>simply supported, 6=>clamped

	elt_vib(i).rotation = 0
	call fill_bc(elt_vib(i).cb,elt_vib(i).cb_av,elt_vib(i).cb_ar,				&
					elt_vib(i).cb_g,elt_vib(i).cb_d,icd_av,icd_ar,icd_g,icd_d,	&
					elt_vib(i).list_pos_raid(1:elt_vib(i).nbr_raid,:),			&
					elt_vib(i).nbr_raid,										&
					elt_vib(i).list_pos_cadre(1:elt_vib(i).nbr_cadre,:),		&
					elt_vib(i).nbr_cadre,				&
					elt_vib(i).long,elt_vib(i).larg,elt_vib(i).rotation)
enddo

! Vérification de la cohérence des données
do i=1,nbr_elmt_vib
    ep_ponderee = 0 !Si les pan.ont des ép.diff.=> on pondère l'épaisser (var.devient true)
	do j=2,elt_vib(i).nbrpan
		! 1) Verif si les panneaux d'un même élément ont le même matériau
		if (e(indMateriau(elt_vib(i).List(1))).ne.e(indMateriau(elt_vib(i).List(j)))) then
			write(*,'(a,i3,a)') 'Attention ! Module vibration : les panneaux de l''element',i,' n''ont pas tous le meme materiau !'
			write(*,'(a,i3)') 'Le materiau choisi dans le calul correspondra a celui du panneau',elt_vib(i).List(1)
			read(*,*)
		endif
		! 2) Verif si les panneaux d'un même élément ont la même long_vib
		if (panneau(elt_vib(i).List(1)).long_vib.ne.panneau(elt_vib(i).List(j)).long_vib) then
			write(*,'(a,i3,a)') 'Attention ! Module vibration : les panneaux de l''element',i,' n''ont pas tous la meme longueur_vibration !'
			write(*,'(a,i3)') 'La longueur_vibration choisie dans le calul correspondra a celle du panneau',elt_vib(i).List(1)
			read(*,*)
		endif
		! 3) Verif si les panneaux d'un même élément ont le même écartement de cadre
		if (panneau(elt_vib(i).List(1)).epsa.ne.panneau(elt_vib(i).List(j)).epsa) then
			write(*,'(a,i3,a)') 'Attention ! Module vibration : les panneaux de l''element',i,' n''ont pas tous le meme écartement des cadres !'
			write(*,'(a,i3)') 'L''ecartement des cadres choisi dans le calul correspondra a celui du panneau',elt_vib(i).List(1)
			read(*,*)
		endif
		! 4) Verif si les panneaux d'un même élément ont la même hauteur d'âme de cadre
		if (panneau(elt_vib(i).List(1)).hya.ne.panneau(elt_vib(i).List(j)).hya) then
			write(*,'(a,i3,a)') 'Attention ! Module vibration : les panneaux de l''element',i,' n''ont pas tous la meme hauteur d''ame des cadres !'
			write(*,'(a,i3)') 'La hauteur d''ame des cadres choisie dans le calul correspondra a celle du panneau',elt_vib(i).List(1)
			read(*,*)
		endif
		! 5) Verif si les panneaux d'un même élément ont la même épaisseur d'âme de cadre
		if (panneau(elt_vib(i).List(1)).dya.ne.panneau(elt_vib(i).List(j)).dya) then
			write(*,'(a,i3,a)') 'Attention ! Module vibration : les panneaux de l''element',i,' n''ont pas tous la meme epaisseur d''ame des cadres !'
			write(*,'(a,i3)') 'L''epaisseur d''ame des cadres choisie dans le calul correspondra a celle du panneau',elt_vib(i).List(1)
			read(*,*)
		endif
		! 6) Verif si les panneaux d'un même élément ont la même largeur de semelle de cadre
		if (panneau(elt_vib(i).List(1)).wya.ne.panneau(elt_vib(i).List(j)).wya) then
			write(*,'(a,i3,a)') 'Attention ! Module vibration : les panneaux de l''element',i,' n''ont pas tous la meme largeur de semelle des cadres !'
			write(*,'(a,i3)') 'La largeur de semelle des cadres choisie dans le calul correspondra a celle du panneau',elt_vib(i).List(1)
			read(*,*)
		endif
		! 7) Verif si les panneaux d'un même élément ont la même épaisseur de semelle de cadre
		if (panneau(elt_vib(i).List(1)).tya.ne.panneau(elt_vib(i).List(j)).tya) then
			write(*,'(a,i3,a)') 'Attention ! Module vibration : les panneaux de l''element',i,' n''ont pas tous la meme epaisseur de semelle des cadres !'
			write(*,'(a,i3)') 'L''epaisseur de semelle des cadres choisie dans le calul correspondra a celle du panneau',elt_vib(i).List(1)
			read(*,*)
		endif
		! 8) Verif si les panneaux d'un même élément ont le même écartement de cadre secondaire
		if (panneau(elt_vib(i).List(1)).epsa2.ne.panneau(elt_vib(i).List(j)).epsa2) then
			write(*,'(a,i3,a)') 'Attention ! Module vibration : les panneaux de l''element',i,' n''ont pas tous le meme écartement des cadres secondaire !'
			write(*,'(a,i3)') 'L''ecartement des cadres secondaires choisi dans le calul correspondra a celui du panneau',elt_vib(i).List(1)
			read(*,*)
		endif
		! 9) Verif si les panneaux d'un même élément ont la même hauteur d'âme de cadre secondaire
		if (panneau(elt_vib(i).List(1)).hya2.ne.panneau(elt_vib(i).List(j)).hya2) then
			write(*,'(a,i3,a)') 'Attention ! Module vibration : les panneaux de l''element',i,' n''ont pas tous la meme hauteur d''ame des cadres secondaire !'
			write(*,'(a,i3)') 'La hauteur d''ame des cadres secondaires choisie dans le calul correspondra a celle du panneau',elt_vib(i).List(1)
			read(*,*)
		endif
		! 10) Verif si les panneaux d'un même élément ont la même épaisseur d'âme de cadre secondaire
		if (panneau(elt_vib(i).List(1)).dya2.ne.panneau(elt_vib(i).List(j)).dya2) then
			write(*,'(a,i3,a)') 'Attention ! Module vibration : les panneaux de l''element ',i,' n''ont pas tous la meme epaisseur d''ame des cadres secondaire !'
			write(*,'(a,i3)') 'L''epaisseur d''ame des cadres secondaires choisie dans le calul correspondra a celle du panneau',elt_vib(i).List(1)
			read(*,*)
		endif
		! 11) Verif si les panneaux d'un même élément ont la même largeur de semelle de cadre secondaire
		if (panneau(elt_vib(i).List(1)).wya2.ne.panneau(elt_vib(i).List(j)).wya2) then
			write(*,'(a,i3,a)') 'Attention ! Module vibration : les panneaux de l''element',i,' n''ont pas tous la meme largeur de semelle des cadres secondaire !'
			write(*,'(a,i3)') 'La largeur de semelle des cadres secondaires choisie dans le calul correspondra a celle du panneau',elt_vib(i).List(1)
			read(*,*)
		endif
		! 12) Verif si les panneaux d'un même élément ont la même épaisseur de semelle de cadre
		if (panneau(elt_vib(i).List(1)).tya.ne.panneau(elt_vib(i).List(j)).tya) then
			write(*,'(a,i3,a)') 'Attention ! Module vibration : les panneaux de l''element',i,' n''ont pas tous la meme epaisseur de semelle des cadres !'
			write(*,'(a,i3)') 'L''epaisseur de semelle des cadres choisie dans le calul correspondra a celle du panneau',elt_vib(i).List(1)
			read(*,*)
		endif
		! 13) Verif si les panneaux d'un même élément sont alignés
		!if (dabs(panneau(elt_vib(i).List(1)).phil-panneau(elt_vib(i).List(j)).phil).gt.1e-10) then
		if (dabs(dabs(panneau(elt_vib(i).List(1)).teta)-dabs(panneau(elt_vib(i).List(j)).teta)).gt.1e-10) then
			write(*,'(3(a,i3),a)') 'Attention ! Module vibration : les panneaux ',elt_vib(i).List(1),' et ',elt_vib(i).List(j),' de l''element ',i,' ne sont pas alignes !'
			write(*,'(a)') 'Les resultats du calcul de vibration seront donc non significatifs !'
			read(*,*)
		endif
		! 14) Verif si les panneaux sont bien contigus
		found = 0
		!do k=1,10
		!	if (noh(elt_vib(i).List(j-1),k).eq.elt_vib(i).List(j)) found = 1
		!	if (noh(elt_vib(i).List(j),k).eq.elt_vib(i).List(j-1)) found = 1
		!enddo
		if (((dabs(z(elt_vib(i).List(j-1),1) - z(elt_vib(i).List(j),1)).lt.0.001)		&
		   .and.(dabs(z(elt_vib(i).List(j-1),2) - z(elt_vib(i).List(j),2)).lt.0.001))	&
		.or.((dabs(z(elt_vib(i).List(j-1),1) - z(elt_vib(i).List(j),3)).lt.0.001)		&
		   .and.(dabs(z(elt_vib(i).List(j-1),2) - z(elt_vib(i).List(j),4)).lt.0.001))	&	
		.or.((dabs(z(elt_vib(i).List(j-1),3) - z(elt_vib(i).List(j),1)).lt.0.001)		&
		   .and.(dabs(z(elt_vib(i).List(j-1),4) - z(elt_vib(i).List(j),2)).lt.0.001))	&
		.or.((dabs(z(elt_vib(i).List(j-1),3) - z(elt_vib(i).List(j),3)).lt.0.001)		&
		   .and.(dabs(z(elt_vib(i).List(j-1),4) - z(elt_vib(i).List(j),4)).lt.0.001))) then
			found = 1
		endif
		if (found.eq.0) then
			write(*,'(3(a,i3),a)') 'Attention ! Module vibration : les panneaux ',elt_vib(i).List(1),' et ',elt_vib(i).List(j),' de l''element ',i,' ne sont pas contigus !'
			write(*,'(a)') 'Les resultats du calcul de vibration seront donc non significatifs !'
			read(*,*)
		endif
		! 15) Verif si les panneaux d'un même élément ont la même épaisseur
		if (panneau(elt_vib(i).List(1)).delta.ne.panneau(elt_vib(i).List(j)).delta) then
			write(*,'(a,i3,a)') 'Attention ! Module vibration : les panneaux de l''element',i,' n''ont pas tous la meme epaisseur !'
			write(*,'(a,i3)') 'L''epaisseur choisie dans le calul correspondra a l''epaisseur ponderee des panneaux'
			!read(*,*)
			ep_ponderee = 1
		endif
		! 16) Verif si les panneaux d'un même élément ont la même première condition limite
		if (panneau(elt_vib(i).List(1)).cl_av_vib.ne.panneau(elt_vib(i).List(j)).cl_av_vib) then
			write(*,'(a,i3,a)') 'Attention ! Module vibration : les panneaux de l''element',i,' n''ont pas tous la meme 1ere condition limite !'
			write(*,'(a,i3)') 'La 1ere condition limite choisie dans le calul correspondra a celle du panneau',elt_vib(i).List(1)
			read(*,*)
		endif
		! 17) Verif si les panneaux d'un même élément ont la même deuxième condition limite
		if (panneau(elt_vib(i).List(1)).cl_ar_vib.ne.panneau(elt_vib(i).List(j)).cl_ar_vib) then
			write(*,'(a,i3,a)') 'Attention ! Module vibration : les panneaux de l''element',i,' n''ont pas tous la meme 2eme condition limite !'
			write(*,'(a,i3)') 'La 2eme condition limite choisie dans le calul correspondra a celle du panneau',elt_vib(i).List(1)
			read(*,*)
		endif
	enddo
	! 18) Verif si 2 raidisseurs ne se trouvent pas au même endroit (une traverse et un raidisseur normal par exemple)
	do j=1,(elt_vib(i).nbr_raid-1)
		if (elt_vib(i).list_pos_raid(j,1).eq.elt_vib(i).list_pos_raid(j+1,1)) then
			write(*,'(a)') 'Attention ! Module vibration : deux raidisseurs se trouvent au meme endroit : traverse, raid. ou raid.compl. !'
			write(*,'(a)') 'Risque eleve de problemes dans le calcul de vibration !'
			read(*,*)
		endif
	enddo
	! 19) Verif qu'il n'y a pas de raidisseur aux extrémités ! (tant qu'Adrian n'a pas résolu le problème)
	found = 0
	if (elt_vib(i).list_pos_raid(1,1).lt.1e-10) found = 1
	if ((elt_vib(i).rotation.eq.0).or.(elt_vib(i).rotation.eq.180)) then
		if (elt_vib(i).list_pos_raid(elt_vib(i).nbr_raid,1).ge.elt_vib(i).long) found = 1
	else
		if (elt_vib(i).list_pos_cadre(elt_vib(i).nbr_cadre,1).ge.elt_vib(i).larg) found = 1
	endif
	if (found.eq.1) then
		write(*,'(a)') 'Attention ! Module vibration : 1 raidisseur se trouve a l''extremite du panneau. Non autorise dans cette version!'
		write(*,'(a,i3)') 'Element de vibration : ',elt_vib(i).id
		read(*,*)
	endif

	! Pondération de l'épaisseur si elle est différente pour chaque panneau
	if (ep_ponderee.eq.1) then
	    epaiss_pond = 0
		longueur = 0
		do j=1,elt_vib(i).nbrpan
			epaiss_pond = epaiss_pond + dabs(panneau(elt_vib(i).List(j)).hight)*panneau(elt_vib(i).List(j)).delta
			longueur = longueur + dabs(panneau(elt_vib(i).List(j)).hight)
		enddo
		epaiss_pond = epaiss_pond/longueur
		elt_vib(i).delta = epaiss_pond

	else
		elt_vib(i).delta = panneau(elt_vib(i).List(1)).delta !sinon on prend celle du 1er panneau (=même que les autres)
	endif
	!
enddo



! Création du fichier Input et calcul de vibration pour chaque élément
do i=1,nbr_elmt_vib
	if ((elt_vib(i).rotation.eq.0).or.(elt_vib(i).rotation.eq.180)) then
		call cree_input_file(elt_vib(i).long,elt_vib(i).larg,elt_vib(i).delta,				&
			elt_vib(i).nbr_raid,elt_vib(i).nbr_cadre,spec(indMateriau(elt_vib(i).List(1))),	&
			e(indMateriau(elt_vib(i).List(1))),eta(indMateriau(elt_vib(i).List(1))),		&
			elt_vib(i).cb,elt_vib(i).list_pos_raid,elt_vib(i).list_pos_cadre,				&
			elt_vib(i).nbr_type,elt_vib(i).list_type)
	else !alors on inverse les cadres et les raidisseurs
		call cree_input_file(elt_vib(i).larg,elt_vib(i).long,elt_vib(i).delta,				&
			elt_vib(i).nbr_cadre,elt_vib(i).nbr_raid,spec(indMateriau(elt_vib(i).List(1))),	&
			e(indMateriau(elt_vib(i).List(1))),eta(indMateriau(elt_vib(i).List(1))),		&
			elt_vib(i).cb,elt_vib(i).list_pos_cadre,elt_vib(i).list_pos_raid,				&
			elt_vib(i).nbr_type,elt_vib(i).list_type)
	endif

	! Si l'élément de vibration a à son extrêmité un axe de symétrie
	! => on modifie le fichier Input en doublant le nombre d'éléments
	elt_vib(i).cas_sym = 5 ! 5 => pas de symétrie
	if (elt_vib(i).rotation.eq.0) then
		if (elt_vib(i).icd_g.eq.7) elt_vib(i).cas_sym = 1
		if (elt_vib(i).icd_d.eq.7) elt_vib(i).cas_sym = 2
	elseif (elt_vib(i).rotation.eq.90) then
		if (elt_vib(i).icd_g.eq.7) elt_vib(i).cas_sym = 3 !4
		if (elt_vib(i).icd_d.eq.7) elt_vib(i).cas_sym = 4 !3
	elseif (elt_vib(i).rotation.eq.180) then
		if (elt_vib(i).icd_g.eq.7) elt_vib(i).cas_sym = 2
		if (elt_vib(i).icd_d.eq.7) elt_vib(i).cas_sym = 1
	elseif (elt_vib(i).rotation.eq.270) then
		if (elt_vib(i).icd_g.eq.7) elt_vib(i).cas_sym = 4 !3
		if (elt_vib(i).icd_d.eq.7) elt_vib(i).cas_sym = 3 !4
	endif
	
	call vibration_mod_input_file(elt_vib(i).cas_sym)

	write(*,'(a,i3)') 'Lancement du calcul de vibration de l''element :', elt_vib(i).id
	write(*,'(a)') '****************************************************'
	write(*,'(a,9(i3))') 'Panneau(x) LBR5 :',(elt_vib(i).List(j),j=1,elt_vib(i).nbrpan)	
	call vibration_module_sans_dll(elt_vib(i).freq_propre(1))
enddo

! Impression des résultats de calcul dans le fichier Sol2
write(iu_12(iboat),*)
write(iu_12(iboat),'(a)') 'RESULTAT DU CALCUL DE VIBRATION'
write(iu_12(iboat),'(a)') '*******************************'
write(iu_12(iboat),'(a,i3)') 'Nombre d''element(s) etudie(s) = ',nbr_elmt_vib
write(iu_12(iboat),'(a)') 'Convention de signe : l''element est suppose etre horizontal avec comme sens de parcours "de la gauche vers la droite".'
write(iu_12(iboat),'(a)') 'Si le panneau LBR5 qui constitue l''element va dans l''autre sens, la position des raidisseurs longi est inversee.'
write(iu_12(iboat),'(a)') 'L''axe de reference se trouve au point arriere gauche.'
write(iu_12(iboat),'(a)') 'Ci-apres, le terme gauche et droite est donc utilise avec cette hypothese.'
write(iu_12(iboat),'(a)') 'Rem.: une rotation autour de z dans le sens trigonometrique peut etre effectuee pour respecter les conditions limites'
write(iu_12(iboat),*)
write(iu_12(iboat),*)
write(iu_12(iboat),'(a)') '                        Arriere'
write(iu_12(iboat),'(a)') '           _____________________________________'
write(iu_12(iboat),'(a)') '          /                                    /'
write(iu_12(iboat),'(a)') '         /                                    /'
write(iu_12(iboat),'(a)') '        /                                    /'
write(iu_12(iboat),'(a)') ' Gauche/                                    /Droite'
write(iu_12(iboat),'(a)') '      /                                    /'
write(iu_12(iboat),'(a)') '     /               Avant                /'
write(iu_12(iboat),'(a)') '    /____________________________________/'
write(iu_12(iboat),'(a)') '            ---> panneau Vibration'
write(iu_12(iboat),'(a)') ''
write(iu_12(iboat),'(a)') '    <             longueur            >'
write(iu_12(iboat),'(a)') ''

do i=1,nbr_elmt_vib
	write(iu_12(iboat),*)
	write(iu_12(iboat),'(a,i3)') 'Element n°',i
	write(iu_12(iboat),'(a)')    '-------------'
	write(iu_12(iboat),'(a)') 'Liste des panneaux : '
	write(iu_12(iboat),'(i3)') (elt_vib(i).List(j),j=1,elt_vib(i).nbrpan)	
	write(iu_12(iboat),'(2(a,d10.3))') 'Longueur de l''element = ',elt_vib(i).long,'; Largeur = ',elt_vib(i).larg
	if ((elt_vib(i).rotation.eq.0).or.(elt_vib(i).rotation.eq.180)) then !TODO A vérifier
		write(iu_12(iboat),'(a,i3)') 'Nbr de raidisseurs selon la longueur = ',elt_vib(i).nbr_cadre
		write(iu_12(iboat),'(a,i3)') 'Nbr de raidisseurs selon la largeur  = ',elt_vib(i).nbr_raid
	else
		write(iu_12(iboat),'(a,i3)') 'Nbr de raidisseurs selon la longueur = ',elt_vib(i).nbr_raid
		write(iu_12(iboat),'(a,i3)') 'Nbr de raidisseurs selon la largeur  = ',elt_vib(i).nbr_cadre
	endif
	if (elt_vib(i).rotation.ne.0) then
		write(iu_12(iboat),'(a,i3,a)') '!!! Attention !!! Une rotation de ',elt_vib(i).rotation,	&
								' degres a ete effectuee pour satisfaire les conditions limites'
	endif
	write(iu_12(iboat),'(8(a))') 'Bord gauche : ',elt_vib(i).cb_g,' || Bord droit : ',	&
								elt_vib(i).cb_d,' || Bord avant : ',elt_vib(i).cb_av,	&
								' || Bord arriere : ',elt_vib(i).cb_ar
	write(iu_12(iboat),'(a,i3)') '=> Condition de bord utilisee : ',elt_vib(i).cb
	write(iu_12(iboat),*)
	if (elt_vib(i).cas_sym.ne.5) then
		write(iu_12(iboat),'(a)') 'Attention ! Element de vibration sur un axe de symetrie !'
		write(iu_12(iboat),'(a)') 'On etudie le panneau ENTIER !'
	endif
	write(iu_12(iboat),'(a)') 'RESULTATS DU CALCUL :'
	write(iu_12(iboat),'(3(a,d10.3))') 'Freq.(1)',elt_vib(i).freq_propre(1),	&
										' Freq.(2)',elt_vib(i).freq_propre(2),	&
										' Freq.(3)',elt_vib(i).freq_propre(3)
enddo


deallocate (elt_vib)

call deallocate_panel_data()
call deallocate_param_section()	
call deallocate_section_header()
call deallocate_param_materiau()
call rewind_all()

return
end

!*******************************************************
subroutine add_int(liste,element)
implicit double precision(a-h,o-z)
integer*4 liste(1000), element

i=1
do while (liste(i).ne.0)
	i = i + 1
enddo
liste(i) = element
return
end
!*******************************************************
subroutine insert_row(liste,xmax,ymax,row)
!insert une ligne vide dans la table liste (2 dimensions) à la ligne row
implicit double precision(a-h,o-z)
integer*4 xmax,ymax,row
double precision liste(xmax,ymax),temp_(xmax,ymax)

i=row
do while ((liste(i,1).ne.0).and.(liste(i,2).ne.0))
	temp_(i,1) = liste(i,1)
	temp_(i,2) = liste(i,2)
	i = i + 1
enddo
rowmax = i

liste(row,1)=0.d00
liste(row,2)=0.d00
do i=row,rowmax-1
	liste(i+1,1) = temp_(i,1)
	liste(i+1,2) = temp_(i,2)
enddo

return
end
!*******************************************************
subroutine insert_double(liste,element1,element2)
!insert l'element1 dans la liste juste après le + grand (col1)
!insert à la même ligne l'element2
implicit double precision(a-h,o-z)
double precision liste(1000,2), element1,element2

i=1
do while ((liste(i,1).lt.element1).and.(liste(i,1).gt.1e-6))
	i = i + 1
enddo
call insert_row(liste,1000,2,i)
liste(i,1) = element1
liste(i,2) = element2

return
end
!*******************************************************
subroutine trie_liste(liste,max)
use param_section
implicit double precision(a-h,o-z)
integer*4 liste(max)


found=0
i = 1
if (dabs(z(liste(1),1)-z(liste(1),2)).lt.000001) then !coord. X noeud départ du pan liste(1) = coord X noeud arrivee
! PANNEAU est vertical !
	do while ((found.eq.0).and.(i.lt.max))
		if (dmin1(z(liste(i),3),z(liste(i),4)).gt.dmin1(z(liste(i+1),3),z(liste(i+1),4))) then
			call permute(liste,max,i,i+1)
			found = 1
		endif
		i = i+1
	enddo
else
! PANNEAU n'est pas vertical !
	do while ((found.eq.0).and.(i.lt.max))
		if (dmin1(z(liste(i),1),z(liste(i),2)).gt.dmin1(z(liste(i+1),1),z(liste(i+1),2))) then
			call permute(liste,max,i,i+1)
			found = 1
		endif
		i = i+1
	enddo
endif

!found=0
!i = 1
!do while ((found.eq.0).and.(i.le.max))
	! Recherche de suivant
	! suivant = élément qui doit suivre l'élément liste(i)
!	suivant = 0
!	do j=1,10
!		do k=1,max
!			if (noh(liste(i),j).eq.liste(k)) then
!				suivant = liste(k)
!				indk = k
!			endif
!		enddo
!	enddo
!
!	if (suivant.eq.0) then
!		if (indk.ne.max) then
!			call permute(liste,max,i,max)
!			found = 1 !=> on stop
!		else
!			i = i+1 !i était déjà bien mis => on a fini
!		endif
!	else
!		if (liste(i).ne.liste(indk-1)) then
!			call permute(liste,max,i,suivant-1) !on met le pan i juste avant son suivant
!			found = 1 !=> on stop
!		else
!			i = i+1 !i était déjà bien mis => on passe au suivant
!		endif
!	endif
!
!enddo

return
end
!*******************************************************
subroutine permute(liste,max,i,j)
implicit double precision(a-h,o-z)
integer*4 liste(max),temp_
temp_ = liste(j)
liste(j) = liste(i)
liste(i) = temp_
return
end
!*******************************************************
subroutine insert_pos(liste,mode_,ecartement,nbr,debut,itype,sens,longueur)
! ajoute  à la liste la position des nouveaux raid/cadre en fct de l'écartement spécifié
implicit double precision(a-h,o-z)
character*3 mode_
double precision liste(1000,2) !TODO a changer
double precision itype,longueur
integer*4 sens

do i=1,nbr
	if ((mode_.eq.'EE1').or.(mode_.eq.'ee1')) then
		if (sens.eq.0) then		!sens LBR5 = sens panneau vibration
			call insert_double(liste,(debut + i*ecartement),itype)
		else
			call insert_double(liste,(debut + longueur - i*ecartement),itype)
		endif
	else
		if (sens.eq.0) then		!sens LBR5 = sens panneau vibration
			call insert_double(liste,(debut + ecartement/2.d00 + (i-1)*ecartement),itype)
		else
			call insert_double(liste,(debut + longueur - ecartement/2.d00 - (i-1)*ecartement),itype)
		endif
	endif
enddo

return
end
!*******************************************************
subroutine cree_input_file(long,larg,delta,nbr_cadre,nbr_raid,spec,e,eta,bc,	&
							list_pos_cadre,list_pos_raid,nbr_type,list_type)

implicit double precision(a-h,o-z)
double precision list_pos_raid(1000,2)	!1ere col.: position du raidisseur
double precision list_pos_cadre(1000,2)	!2ème col.: type de raidisseur
double precision list_type(1000,4)		!haut.ame, ep.ame, larg.sem.,ep.sem.
double precision long					!longueur de l'élément
double precision larg					!largeur de l'élément
double precision nbr_type				!nbre de raidisseurs de types différents
integer*4 bc							!boundary condition

open(910,file='input_data_full_dp_temp.dat')

!Ecriture des parametres generaux
	write(910,'(3(d10.3,3x),i3,3x,i3,3(3x,d10.3),3x,i3)') long,larg,delta,nbr_cadre,	&
															nbr_raid,spec/10,e,eta,bc
															! /10 pour respect convention
!Ecriture des cadres
do i=1,nbr_cadre
	itype = list_pos_cadre(i,2)
	write(910,'(d11.3,3x,i3,3x,a)') list_pos_cadre(i,1),itype,'I'
enddo
!Ecriture des raidisseurs
do i=1,nbr_raid
	itype = list_pos_raid(i,2)
	write(910,'(d11.3,3x,i3,3x,a)') list_pos_raid(i,1),itype,'I'
enddo
!Ecriture des types de raidisseurs/cadres
do i=1,nbr_type !!!
	write(910,'(i3,3x,4(d10.3,3x))') i,list_type(i,1),list_type(i,2),list_type(i,3),list_type(i,4)
enddo

close(910)

return
end
!*******************************************************
subroutine fill_bc(cb,cb_av,cb_ar,cb_g,cb_d,icd_av,icd_ar,icd_g,icd_d,			&
					list_pos_raid,nbr_raid,list_pos_cadre,nbr_cadre,long,larg,rotation)
!Détermine les conditions limites dans le panneau
implicit double precision(a-h,o-z)
integer*4 cb, icd_g,icd_d,icd_av,icd_ar,nbr_raid,nbr_cadre,rotation
double precision list_pos_raid(nbr_raid,2),list_pos_cadre(nbr_cadre,2),long,larg
character*16 cb_g,cb_d,cb_av,cb_ar

! Détermination des conditions limites
! cb = 1 => encastré partout
! cb = 2 => encastré partout sauf à l'arrière
! cb = 4 => encastré à droite, simplement supporté ailleurs
! cb = 5 => simplement supporté partout
! cb = 6 => encastré à droite et à gauche, simplement supporté à l'avant et arrière
! cb = 7 => encastré à l'avant et arrière, simplement supporté à droite et à gauche

if (icd_av.eq.6) then 
	cb_av = 'clamped'
elseif ((icd_av.eq.2).or.(icd_av.eq.7).or.(icd_av.eq.11).or.(icd_av.eq.12)) then
	cb_av = 'simply supported'
else
	write(*,*) 'Attention ! Condition limite du bord avant non autorisee !!!'
	write(*,*) 'L''appui a ete modifie en simply supported'
	cb_av = 'simply supported'
endif

if (icd_ar.eq.6) then 
	cb_ar = 'clamped'
elseif ((icd_ar.eq.2).or.(icd_ar.eq.7).or.(icd_ar.eq.11).or.(icd_ar.eq.12)) then
	cb_ar = 'simply supported'
else
	write(*,*) 'Attention ! Condition limite du bord arriere non autorisee !!!'
	write(*,*) 'L''appui a ete modifie en simply supported'
	cb_ar = 'simply supported'
endif

if (icd_g.eq.6) then
	cb_g = 'clamped'
elseif ((icd_g.eq.2).or.(icd_g.eq.7).or.(icd_g.eq.11).or.(icd_g.eq.12)) then
	cb_g = 'simply supported'
else
	write(*,*) 'Attention ! Condition limite du bord gauche non autorisee !!!'
	write(*,*) 'L''appui a ete modifie en simply supported'
	cb_g = 'simply supported'
endif

if (icd_d.eq.6) then
	cb_d = 'clamped'
elseif ((icd_d.eq.2).or.(icd_d.eq.7).or.(icd_d.eq.11).or.(icd_d.eq.12)) then
	cb_d = 'simply supported'
else
	write(*,*) 'Attention ! Condition limite du bord droit non autorisee !!!'
	write(*,*) 'L''appui a ete modifie en simply supported'
	cb_d = 'simply supported'
endif

!Détermination du cb pour le calcul de vibration

!4 BORDS ENCASTRES
if ((cb_av.eq.'clamped').and.(cb_d.eq.'clamped').and.	&
	(cb_ar.eq.'clamped').and.(cb_g.eq.'clamped')) then
	cb = 1
endif

!3 BORDS ENCASTRES
if ((cb_av.eq.'simply supported').and.(cb_d.eq.'clamped').and.	&
	(cb_ar.eq.'clamped').and.(cb_g.eq.'clamped')) then
	! L'avant devient l'arrière => on inverse l'ordre des cadres
	call inverse_sens(list_pos_cadre,nbr_cadre,larg)
	cb = 2
	rotation = 180
endif
if ((cb_av.eq.'clamped').and.(cb_d.eq.'simply supported').and.	&
	(cb_ar.eq.'clamped').and.(cb_g.eq.'clamped')) then
	! le côté droit devient l'arrière => on inverse l'ordre des raidisseurs
	! ET on doit intervertir les cadres et raidisseurs
	call inverse_sens(list_pos_raid,nbr_raid,long)
	cb = 2
	rotation = 90
endif
if ((cb_av.eq.'clamped').and.(cb_d.eq.'clamped').and.	&
	(cb_ar.eq.'simply supported').and.(cb_g.eq.'clamped')) then
	cb = 2
endif
if ((cb_av.eq.'clamped').and.(cb_d.eq.'clamped').and.	&
	(cb_ar.eq.'clamped').and.(cb_g.eq.'simply supported')) then
	! le côté gauche devient l'arrière
	! On doit intervertir cadres et raidisseurs
	cb = 2
	rotation = 270
endif

!2 BORDS ENCASTRES
if ((cb_av.eq.'clamped').and.(cb_d.eq.'clamped').and.	&
	(cb_ar.eq.'simply supported').and.(cb_g.eq.'simply supported')) then
	cb = 3
endif
if ((cb_av.eq.'simply supported').and.(cb_d.eq.'clamped').and.	&
	(cb_ar.eq.'simply supported').and.(cb_g.eq.'clamped')) then
	cb = 6
endif
if ((cb_av.eq.'clamped').and.(cb_d.eq.'simply supported').and.	&
	(cb_ar.eq.'clamped').and.(cb_g.eq.'simply supported')) then
	cb = 7
endif
if ((cb_av.eq.'simply supported').and.(cb_d.eq.'simply supported').and.	&
	(cb_ar.eq.'clamped').and.(cb_g.eq.'clamped')) then
	! le côté avant devient l'arrière, le côté gauche devient le droit
	! => on doit inverser l'ordre des raidisseurs
	! => on doit inverser l'ordre des cadres
	call inverse_sens(list_pos_cadre,nbr_cadre,larg)
	call inverse_sens(list_pos_raid,nbr_raid,long)
	cb = 3
	rotation = 180
endif
if ((cb_av.eq.'clamped').and.(cb_d.eq.'simply supported').and.	&
	(cb_ar.eq.'simply supported').and.(cb_g.eq.'clamped')) then
	! le côté droit devient l'arrière => on inverse l'ordre des raidisseurs
	! ET on doit intervertir les cadres et raidisseurs
	call inverse_sens(list_pos_raid,nbr_raid,long)
	cb = 3
	rotation = 90
endif
if ((cb_av.eq.'simply supported').and.(cb_d.eq.'clamped').and.	&
	(cb_ar.eq.'clamped').and.(cb_g.eq.'simply supported')) then
	! le côté gauche devient l'arrière
	! On doit intervertir cadres et raidisseurs
	cb = 3
	rotation = 270
endif

!1 BORD ENCASTRE
if ((cb_av.eq.'clamped').and.(cb_d.eq.'simply supported').and.	&
	(cb_ar.eq.'simply supported').and.(cb_g.eq.'simply supported')) then
	! le côté droit devient l'arrière => on inverse l'ordre des raidisseurs
	! ET on doit intervertir les cadres et raidisseurs
	call inverse_sens(list_pos_raid,nbr_raid,long)
	cb = 4
	rotation = 90
endif
if ((cb_av.eq.'simply supported').and.(cb_d.eq.'clamped').and.	&
	(cb_ar.eq.'simply supported').and.(cb_g.eq.'simply supported')) then
	cb = 4
endif
if ((cb_av.eq.'simply supported').and.(cb_d.eq.'simply supported').and.	&
	(cb_ar.eq.'clamped').and.(cb_g.eq.'simply supported')) then
	! le côté gauche devient l'arrière
	! On doit intervertir cadres et raidisseurs
	cb = 4
	rotation = 270
endif
if ((cb_av.eq.'simply supported').and.(cb_d.eq.'simply supported').and.	&
	(cb_ar.eq.'simply supported').and.(cb_g.eq.'clamped')) then
	! L'avant devient l'arrière => on inverse l'ordre des cadres
	call inverse_sens(list_pos_cadre,nbr_cadre,larg)
	cb = 4
	rotation = 180
endif

!0 BORD ENCASTRE
if ((cb_av.eq.'simply supported').and.(cb_d.eq.'simply supported').and.	&
	(cb_ar.eq.'simply supported').and.(cb_g.eq.'simply supported')) then
	cb = 5
endif

if (cb.eq.0) then
	write(*,*) 'Erreur dans la determination des conditions limites - Module Vibration'
	read(*,*)
	stop
endif

return
end

!*******************************************************
subroutine inverse_sens(liste,nmax,longueur)
!inverse la position des raidisseurs dans la liste
! (pos 1,2,5 (m) sur une long de 6m devient pos 1,4,5(m)
implicit double precision(a-h,o-z)
double precision liste(nmax,2),temp_(nmax,2),longueur
integer*4 nmax

do i=1,nmax
	temp_(nmax-i+1,1) = longueur - liste(i,1)
	temp_(nmax-i+1,2) = liste(i,2)
enddo

do i=1,nmax
	liste(i,1) = temp_(i,1)
	liste(i,2) = temp_(i,2)
enddo

return
end

