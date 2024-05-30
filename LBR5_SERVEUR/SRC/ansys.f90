subroutine ansys()
! **************************************************************************************************
!
! Création d'un fichier texte permettant d'automatiquement faire un calcul éléments finis ANSYS
!
! Les éléments seront créés par ensemble de panneaux : les panneaux LBR5 ayant les mêmes écartements
! des cadres seront créés d'une seule traite. C'est dû à la modélisation des cadres qui doit être 
! faite de façon à représenter les cadres par une seule surface.
!
! **************************************************************************************************

use param_section, TEMP_=>TEMP

character*30 nom_ANSYS
integer*4 nel,i,j,k,l,found,pan1,pan2,nbr_delete
integer*4 num_bloc									! nombre de blocs
integer*4, allocatable, save :: pan_place(:)		! =0 => pan pas encore placé dans une structure bloc
integer*4 liste_carac(300,3)						! répertorie la liste des différents epsa, ép.cadre et matériau	!TODO allouer dynamiquement
integer*4 hh,aa,num_pt_cadre
integer*4 APanWithoutFr(200)						! répertorie le numéro de surface qui représente le panneau sans cadre
integer*4 KPanWithoutFr(4,200)						! répertorie le numéro des points qui représentent le panneau sans cadre
integer*4 resultat
double precision xa_vol,ya_vol,xb_vol,yb_vol
integer*4 K1,K2,K3,K4,VV !,ATEMP
double precision DIST,delta
double precision xa_new,ya_new,xb_new,yb_new
integer*4 list_AA(10000)

character*200 temp

! Une structure bloc peut être dessinée entièrement dans ANSYS :
! les cadres sont équidistants et avec le même matériau (semelle peut être différente)
type bloc 
	integer*4 num_Elmt								! nombre de panneaux LBR-5 qui constituent le bloc
	integer*4 num_surface							! nombre de surface qui constituent le bloc
	integer*4 num_sections							! nombre des section (=nombre de frame à dessiner)
	integer*4 APlate(200)							! répertorie le numéro de surface qui représente la plate dans la nomenclature ANSYS
	integer*4 AFrame(200)							! répertorie le numéro de surface qui représente la frame dans la nomenclature ANSYS
	integer*4 KPlate(4,200)							! répertorie le numéro des points qui représentent la plate dans la nomenclature ANSYS
	integer*4 liste(200)							! liste répertoriant les panneaux LBR-5 de ce bloc (MAX 200 : pas dynamique, TODO A CHANGER)
	double precision epsa							! ecart entre les cadres
	double precision tya							! épaisseur de l'âme des cadres
	double precision liste_coord(10,2,200)			! pour chaque panneau, liste des 4 coord qui seront utilisées pour déterminer la surface
	integer*4 num_coord(200)
	! (à la base y en a 4 mais ça peut être augmenté ou diminué)
	double precision liste_coord_sem(2,2,200)		! pour chaque panneau, liste des 2 coord qui seront utilisées pour déterminer la semelle
	integer*4 ind_Mat								! indice du matériau de ce bloc
	integer*4 couple_actif(200,200,2)				! (2,4,1)=1 => le point c du panneau 2 est à l'intérieur du "cadre" du panneau 4
	integer*4 point_actif(200,2)					! (5,2)=1 => le point d du panneau 5 est à l'intérieur d'au moins un cadre d'un panneau adjacent
	!integer*4 nbr_point								! nombre de point qui définissent la surface
end type

type(bloc), allocatable, save :: liste_bloc(:)


! Allocation et initialisation des variables
allocate (pan_place(neto))
pan_place(:) = 0

num_bloc = 0		! nombre de panneau LBR5 qui ont un delta different, ou même delta mais épaiss. ou matériau différent
do nel=1,neto
	
	if ((dabs(panneau(nel).hight).gt.0.10).and.(itype(nel).ne.5).and.	&
			(panneau(nel).hya.gt.0.10)) then	!alors panneau pas encore placé dans bloc

		found = 0		! indique si le panneau a un epsa déjà répertorié dans liste_carac (avec même ép. et même mat.)
		do i=1,num_bloc
			if ((liste_carac(i,1).eq.floor(panneau(nel).epsa*1000)).and.	&
			(liste_carac(i,2).eq.floor(panneau(nel).tya*1000)).and. &
			(liste_carac(i,3).eq.indMateriau(nel))) then
				found = 1	!floor pour arrondir (vers le bas)
			endif
		enddo

		if (found.eq.0) then
			num_bloc = num_bloc + 1
			liste_carac(num_bloc,1) = floor(panneau(nel).epsa*1000)
			liste_carac(num_bloc,2) = floor(panneau(nel).tya*1000)
			liste_carac(num_bloc,3)= indMateriau(nel)
		endif
	endif
enddo

allocate (liste_bloc(num_bloc))		! certains panneaux ne sont peut-être pas "liés" -> A traiter par après
liste_bloc(:).num_Elmt = 0
liste_bloc(:).num_surface = 0
liste_bloc(:).num_sections = 0
do i=1,200		! TODO à allouer dynamiquement normalement
	liste_bloc(:).liste(i)=0
enddo
liste_bloc(:).epsa = 0.0d00
liste_bloc(:).tya = 0.0d00
do j=1,200
	do i=1,10
		liste_bloc(:).liste_coord(i,1,j) = 0.00
		liste_bloc(:).liste_coord(i,2,j) = 0.00
	enddo
	do i=1,2
		liste_bloc(:).liste_coord_sem(i,1,j) = 0.00
		liste_bloc(:).liste_coord_sem(i,2,j) = 0.00
	enddo
	liste_bloc(:).num_coord(j) = 0
enddo
liste_bloc(:).ind_Mat = 0
do j=1,200
	do i=1,200
		do k=1,2
			liste_bloc(:).couple_actif(j,i,k) = 0
		enddo
	enddo
	do k=1,2
		liste_bloc(:).point_actif(j,k) = 0
	enddo
	liste_bloc(:).APlate(j) = 0
	liste_bloc(:).AFrame(j) = 0
	do i=1,4
		liste_bloc(:).KPlate(i,j) = 0
	enddo
enddo

APanWithoutFr(:) = 0
KPanWithoutFr(:,:) = 0
VV = 0

! ********************************************************
! Préparation des données avant d'écrire le fichier ANSYS
! ********************************************************

! On va placer chaque panneau LBR5 dans une structure bloc

num_bloc = 0
do nel = 1,neto
	if ((dabs(panneau(nel).hight).gt.0.10).and.(itype(nel).ne.5).and.	&
			(panneau(nel).hya.gt.0.10)) then	!alors panneau pas encore placé dans bloc
		
		found = 0
		do i = 1,num_bloc
			
			if ((liste_bloc(i).epsa.eq.floor(panneau(nel).epsa*1000)).and.	&
				(liste_bloc(i).tya.eq.floor(panneau(nel).tya*1000)).and.		&
				(liste_bloc(i).ind_Mat.eq.indMateriau(nel))) then
				found = 1
				j = i
			endif
		enddo

		if (found.eq.0) then	! -> new bloc
			num_bloc = num_bloc + 1
			j = num_bloc
			liste_bloc(j).epsa = floor(panneau(nel).epsa*1000)
			liste_bloc(j).num_sections = floor(1000.*width/liste_bloc(j).epsa)
			liste_bloc(j).tya = floor(panneau(nel).tya*1000)
			liste_bloc(j).ind_Mat = indMateriau(nel)
		endif

		liste_bloc(j).num_Elmt = liste_bloc(j).num_Elmt + 1		! Un panneau en plus dans ce bloc
		liste_bloc(j).liste(liste_bloc(j).num_Elmt) = nel
		
		!calcul_coord(nel,aX,aY,bX,bY,cX,cY,dX,dY)
		call calcul_coord(nel,liste_bloc(j).liste_coord(1,1,nel),liste_bloc(j).liste_coord(1,2,nel),	&
				liste_bloc(j).liste_coord(2,1,nel),liste_bloc(j).liste_coord(2,2,nel),liste_bloc(j).liste_coord(3,1,nel),	&
				liste_bloc(j).liste_coord(3,2,nel),liste_bloc(j).liste_coord(4,1,nel),liste_bloc(j).liste_coord(4,2,nel))
		
		liste_bloc(j).num_coord(nel) = 4
	endif
enddo


! Pour chaque bloc, on va déterminer les intersections entre les panneaux contigus
do i=1,num_bloc
	do nel=1,liste_bloc(i).num_Elmt	!nombre de panneau du bloc
		!Recherche si dans les panneaux qui restent un appartient au panneau actuel
		
		pan1=liste_bloc(i).liste(nel)
		
		do j=1,liste_bloc(i).num_elmt-nel
			!pan1=liste_bloc(i).liste(nel)
			!if (nel.ne.j) then
				pan2=liste_bloc(i).liste(nel+j)
				found = 0
				
				call test_point_commun(pan1,pan2,liste_bloc(i).liste_coord,found)
				!do k=1,10	!boucle sur les 10 panneaux qui suivent
				!	if ((panneau(pan1).noh9(k).eq.pan2).or.panneau(pan2).noh9(k).eq.pan1) found = 1
				!enddo

				if ((found.eq.1)) then !.and.(liste_bloc(i).num_coord(pan1).eq.4).and.(liste_bloc(i).num_coord(pan2).eq.4)) then	!alors calcul des intersections entre ces 2 panneaux

				! correction des points du pan1
					call correction_coord(pan1,pan2,liste_bloc(i).liste_coord,liste_bloc(i).liste_coord_sem,	&
										liste_bloc(i).couple_actif,liste_bloc(i).point_actif,					&
										liste_bloc(i).num_coord)
		!			if ((liste_bloc(i).num_coord(pan1).eq.4).and.(liste_bloc(i).num_coord(pan2).eq.4)) then
						! correction des points du pan2
						call correction_coord(pan2,pan1,liste_bloc(i).liste_coord,liste_bloc(i).liste_coord_sem,	&
										liste_bloc(i).couple_actif,liste_bloc(i).point_actif,					&
										liste_bloc(i).num_coord)
						! (y a plus économe en temps mais c'est plus clair point de vue programmation)
		!			endif

				elseif ((found.eq.1).and.(liste_bloc(i).num_coord(pan1).eq.4).and.(liste_bloc(i).num_coord(pan2).eq.3)) then	!alors calcul des intersections entre ces 2 panneaux
					! si = 3 => faire une autre méthode !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
					! (ou même chose en prenant coord initiales et non plus celles modifiées ?)



				endif
			!endif
		enddo
	enddo
enddo

! Pour chaque bloc, il faut maintenant déterminer l'ordre de parcours de tous les points qui le composent
!do i=1,num_bloc


!enddo


! ********************************************************
! Création du fichier ANSYS
! ********************************************************

nom_ANSYS = section_files(iboat)
nom_ANSYS = nom_ANSYS(1:len_trim(section_files(iboat))-3)//'lgw'

open (5555,file=nom_ANSYS) ! nom_de_fichier.lgw = format ANSYS


!write (5555,'(a)') '/BATCH'
!write (5555,'(a)') '/input,menust,tmp,'''',,,,,,,,,,,,,,,,1'
write (5555,'(a)') 'WPSTYLE,,,,,,,,0'
write (5555,'(a)') '/FILNAME,NAME_OF_FILE,0'	!TODO change
write (5555,'(a)') '/TITLE,TITLE'				!TODO change
write (5555,'(a)') '/UNITS, user, 1000, 1, 1, 1, 1, 1, 1, 1'

! Définition des paramètres
write (5555,'(a)') '!PARAMETERS'

write(5555,'(a)')
! Définition des key points
write(5555,'(a)') '/PREP7  '	! indispensable avant de commencer à la définition des points géométriques
write(5555,'(a)') '!ISOMETRIC VIEW'
write(5555,'(a)') '/VIEW,1,1,1,1'

write(5555,'(a)')
write(5555,'(a)') '!KEY POINTS'


! Ecriture des points pour chaque bloc
HH=0
do i=1,num_bloc
	write (5555,'(a,i3)') '!Bloc',i
	do j=1,liste_bloc(i).num_Elmt
		nel = liste_bloc(i).liste(j)
		write (5555,'(a,i4)') '!Panel',nel
		do k=1,liste_bloc(i).num_coord(nel)
			HH = HH+1
			write (5555,'(a,i4,3(a,f8.4))') 'K,',HH,',',liste_bloc(i).liste_coord(k,1,nel),',',	&
					liste_bloc(i).liste_coord(k,2,nel),',',liste_bloc(i).epsa/(2.00*1000.)
		enddo

	enddo
enddo


write(5555,'(a)')
write(5555,'(a)')  '!DEFINITION OF SURFACES'

HH=0 !272 si commence à 6!!!
AA=0 !indice des surfaces
do i=1,num_bloc
	write (5555,'(a,i3,2(a,f8.1),a,i2)') '!Bloc',i,' Epsa',liste_bloc(i).epsa,' Thickness (tya)',liste_bloc(i).tya,' Material',liste_bloc(i).ind_Mat
	
	l = 0 ! = nombre de surface réelle à tracer (un panneau pourrait avoir moins de 3 points)
	do j=1,liste_bloc(i).num_Elmt
		nel = liste_bloc(i).liste(j)
		write (5555,'(a,i4)') '!Panel',nel
		temp = 'A'
		do k=1,liste_bloc(i).num_coord(nel)
			HH = HH + 1
			write(temp((len_trim(temp)+1):200),'(a,i4)') ',',HH
			!temp = temp(1:len_trim(temp))//','//HH//CHAR(0)
		enddo
		if (liste_bloc(i).num_coord(nel).gt.2) then ! alors on a une surface
			l = l+1
			write(temp((len_trim(temp)+1):200),'(a,i5)') ' !SURFACE A N°:',AA+1
			write (5555,'(a)') temp
			! ASEL,S,LOC,Z,liste_bloc(i).epsa/2
			! ASEL,R,LOC,X,liste_bloc(i).liste_coord(1,1,nel),liste_bloc(i).liste_coord(2,1,nel)
			! ASEL,R,LOC,Y,liste_bloc(i).liste_coord(1,1,nel),liste_bloc(i).liste_coord(2,1,nel)
			AA=AA+1
			liste_bloc(i).AFrame(nel) = AA
			liste_bloc(i).num_surface = liste_bloc(i).num_surface + 1
		endif
		
		! Si les surfaces sont superposées (et qu'on a au moins 2 surfaces) alors :
		! On les fusionnne avec la commande AADD
		if (l.gt.1) then
			if (j.eq.liste_bloc(i).num_Elmt) then
			!	temp = 'ASEL,S,AREA,,'
			!	write(temp((len_trim(temp)+1):200),'(i3,a,i3)') (AA-liste_bloc(i).num_surface+1),',',AA
			endif
			!temp = 'ASEL,S,AREA,,1,7' !pr sélectionner toutes les surfaces de num 1 à 7
		!	write(5555,'(2(a,f10.6))') 'ASEL,S,LOC,Z,',liste_bloc(i).epsa/(2.00*1000.) !-0.001,',',liste_bloc(i).epsa/2.00+0.001
			!write(5555,'(a)') 'AADD,ALL'
		endif
	enddo
	write(5555,'(a)')
	!write(5555,'(a)') '!FUSION OF AREAS OF EACH BLOC'
	!write(5555,'(2(a,f8.2))') 'ASEL,S,LOC,Z,',liste_bloc(i).epsa/2.00 !-0.001,',',liste_bloc(i).epsa/2.00+0.001
	!write(5555,'(a)') 'AADD,ALL'
	!write(5555,'(a)')
enddo

!AA=0
!do i=1,num_bloc
!	if (liste_bloc(i).num_surface.gt.1) then
!		temp = 'ASEL,S,AREA,,'
!		write(temp((len_trim(temp)+1):200),'(i3,a,i3)') (AA+1),',',(AA+liste_bloc(i).num_surface)
!		write(5555,'(a)') temp
!		write(5555,'(a)') 'AADD,ALL'
!	endif
!	AA = AA + liste_bloc(i).num_surface
!enddo


! Création des tôles et des lignes pour les raidisseurs
write (5555,'(a)') '!CREATION OF THE PLATES'
num_pt_cadre = HH
l = 0
nbr_delete = 0
do i=1,num_bloc

	do j=1,liste_bloc(i).num_Elmt
		nel = liste_bloc(i).liste(j)
		do k=1,2
			HH = HH + 1
			write (5555,'(a,i4,3(a,f8.4))') 'K,',HH,',',liste_bloc(i).liste_coord(k,1,nel),',',	&
					liste_bloc(i).liste_coord(k,2,nel),',',0.00			
		enddo
		temp = 'A'
		l = l+1
		write(temp((len_trim(temp)+1):200),'(a,i4)') ',',HH-num_pt_cadre-1+ (l-1)*2 - nbr_delete
		write(temp((len_trim(temp)+1):200),'(a,i4)') ',',HH-num_pt_cadre  + (l-1)*2 - nbr_delete
		write(temp((len_trim(temp)+1):200),'(a,i4)') ',',HH
		write(temp((len_trim(temp)+1):200),'(a,i4)') ',',HH-1
		write(temp((len_trim(temp)+1):200),'(a,i5)') ' !SURFACE A N°:',AA+1
		write (5555,'(a)') temp
		if (liste_bloc(i).num_coord(nel).eq.3) nbr_delete = nbr_delete + 1
		AA = AA+1
		liste_bloc(i).KPlate(1,nel) = HH-num_pt_cadre-1+ (l-1)*2 - nbr_delete
		liste_bloc(i).KPlate(2,nel) = HH-num_pt_cadre  + (l-1)*2 - nbr_delete
		liste_bloc(i).KPlate(3,nel) = HH
		liste_bloc(i).KPlate(4,nel) = HH-1
		liste_bloc(i).APlate(nel) = AA
	enddo
enddo
write (5555,'(a)')

! Dessin des panneaux sans frame
write (5555,'(a)') '!PANELS WITHOUT ANY FRAME'
do nel=1,neto
	
	if ((dabs(panneau(nel).hight).gt.0.10).and.(itype(nel).ne.5).and.	&
			(panneau(nel).hya.lt.0.10)) then	!alors panneau sans frame
		write (5555,'(a,i3)') '!Panel without frame',nel
		HH = HH + 1
		write (5555,'(a,i4,3(a,f8.4))') 'K,',HH,',',z(nel,1),',',z(nel,3),',',0.000
		HH = HH + 1
		write (5555,'(a,i4,3(a,f8.4))') 'K,',HH,',',z(nel,2),',',z(nel,4),',',0.000
		HH = HH + 1
		write (5555,'(a,i4,3(a,f8.4))') 'K,',HH,',',z(nel,1),',',z(nel,3),',',width
		HH = HH + 1
		write (5555,'(a,i4,3(a,f8.4))') 'K,',HH,',',z(nel,2),',',z(nel,4),',',width
		temp = 'A'
		write(temp((len_trim(temp)+1):200),'(a,i4)') ',',HH-3
		write(temp((len_trim(temp)+1):200),'(a,i4)') ',',HH-2
		write(temp((len_trim(temp)+1):200),'(a,i4)') ',',HH
		write(temp((len_trim(temp)+1):200),'(a,i4)') ',',HH-1
		write(temp((len_trim(temp)+1):200),'(a,i5)') ' !SURFACE A N°:',AA+1
		write (5555,'(a)') temp
		AA = AA+1
		KPanWithoutFr(1,nel) = HH-3
		KPanWithoutFr(2,nel) = HH-2
		KPanWithoutFr(3,nel) = HH
		KPanWithoutFr(4,nel) = HH-1
		APanWithoutFr(nel) = AA
	endif
enddo
write (5555,'(a)')

list_AA(:) = 0
do i=1,AA
	list_AA(i) = 1
enddo



i=0
!if (i.Eq.1) then

! Suppression des surfaces qui traversent les panneaux longitudinaux
write (5555,'(a)') '!DELETE SURFACES THAT GO TROUGH LONGITUDINAL PANELS'
do i=1, num_bloc
	do j=1,liste_bloc(i).num_Elmt
		pan1 = liste_bloc(i).liste(j)
		do pan2=1,neto
			if ((pan2.ne.pan1).and.(dabs(panneau(pan2).hight).gt.0.10).and.(itype(pan2).ne.5)) then
				!ASBA,1,2,,DELETE,KEEP  !coupe le pan 1 par le 2, et le 1 est supprimé et renommé en 3 et 4
	!!!			temp = 'ASBA,'
				found = 0
				if (panneau(pan2).hya.lt.0.10) then !alors pan2 est un panel sans frame
	!!!				write(temp((len_trim(temp)+1):200),'(2(i5,a))') liste_bloc(i).AFrame(Pan1),',',APanWithoutFr(Pan2),',,KEEP,KEEP'
					K1 = KPanWithoutFr(1,Pan2)
					K2 = KPanWithoutFr(2,Pan2)
					K3 = KPanWithoutFr(3,Pan2)
					K4 = KPanWithoutFr(4,Pan2)

					pan2_Xa = z(pan2,1)
					pan2_Ya = z(pan2,3)
					pan2_Xb = z(pan2,2)
					pan2_Yb = z(pan2,4)
					!pan2_Xc = liste_coord(3,1,pan1) ! prendre un point sur la perp. à ab passant par b
					!pan2_Yc = liste_coord(3,2,pan1)
					!pan2_Xd = liste_coord(4,1,pan1)
					!pan2_Yd = liste_coord(4,2,pan1) ! prendre un point sur la perp. à ab passant par a


					!ATEMP = APanWithoutFr(Pan2)
					DIST = width
					found = 1
				else
					! Recherche de pan2 dans un bloc
					do k=1,num_bloc
						do l=1,liste_bloc(k).num_Elmt
							if (liste_bloc(k).liste(l).eq.pan2) then
	!!!							write(temp((len_trim(temp)+1):200),'(2(i5,a))') liste_bloc(i).AFrame(Pan1),',',liste_bloc(k).APlate(Pan2),',,KEEP,KEEP'
								K1 = liste_bloc(k).KPlate(1,Pan2)
								K2 = liste_bloc(k).KPlate(2,Pan2)
								K3 = liste_bloc(k).KPlate(3,Pan2)
								K4 = liste_bloc(k).KPlate(4,Pan2)
							
								pan2_Xa = liste_bloc(k).liste_coord(1,1,pan2)
								pan2_Ya = liste_bloc(k).liste_coord(1,2,pan2)
								pan2_Xb = liste_bloc(k).liste_coord(2,1,pan2)
								pan2_Yb = liste_bloc(k).liste_coord(2,2,pan2)
								pan2_Xc = liste_bloc(k).liste_coord(3,1,pan2)
								pan2_Yc = liste_bloc(k).liste_coord(3,2,pan2)
								pan2_Xd = liste_bloc(k).liste_coord(4,1,pan2)
								pan2_Yd = liste_bloc(k).liste_coord(4,2,pan2)

								!ATEMP = liste_bloc(k).APlate(Pan2)
								DIST = liste_bloc(k).epsa/(2.00*1000.)
								found=1
							endif
						enddo
					enddo
				endif
				if (found.eq.0) then
					write(*,*) 'Error in subroutine ANSYS! Panel',pan2,' not found in any bloc definition!'
					write(*,*) 'Program will stop - press return to resume'
					read(*,*)
					stop
				else
					!!! prendre les bonnes coord de pan2
					
					call is_Frame_intersecte_Panel(pan1,pan2,liste_bloc(i).liste_coord,liste_bloc(i).num_coord,	&
													resultat,xa_vol,ya_vol,xb_vol,yb_vol,xa_new,ya_new,xb_new,yb_new)
					!Si les 2 surfaces se coupaient bien alors
					if (resultat.eq.1) then

						! crée un "volume" ayant pour base APlate(Pan2) et extrudé dans la direction opposée à frame(Pan1)
						! (AA devient alors AA + 6 (ou 5))
						
						! Création des points manquants du volume
						! (On recrée aussi la surface initiale sinon le DELETE la supprime)
						! (elle doit d'ailleurs être plus "large")

! à modifier donc en ne prenant pas une largeur au hasard !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
						delta = 2.00

			!			call calcul_intersection(pan2_Xa,pan2_Ya,pan2_Xd,pan2_Yd,z(pan1,1),z(pan1,3),z(pan1,2),z(pan1,4),parallele,Xa_new,Yi) !ad pan2 avec ab pan 1
			!			call calcul_coeff_droite(pan2_Xa,pan2_Ya,pan2_Xd,pan2_Yd,a,b,verticale)
						if (verticale.eq.0) then
			!				Ya_new =  a*Xa_new + b
						else
			!				Ya_new = pan2_Ya + delta ! valeur arbitraire
						endif
						
			!			call calcul_intersection(pan2_Xb,pan2_Yb,pan2_Xc,pan2_Yc,z(pan1,1),z(pan1,3),z(pan1,2),z(pan1,4),parallele,Xb_new,Yi) !bc pan2 avec ab pan 1
			!			call calcul_coeff_droite(pan2_Xb,pan2_Yb,pan2_Xc,pan2_Yc,a,b,verticale)
						if (verticale.eq.0) then
			!				Yb_new =  a*Xb_new + b
						else
			!				Yb_new = pan2_Yb + delta ! valeur arbitraire
						endif
						
						!call extrapole_segment(z(pan2,1),z(pan2,3),z(pan2,2),z(pan2,4),delta,Xa_new,Ya_new,Xb_new,Yb_new)

						HH = HH + 1
						
						!TODO Ya_new pas bon : à vérifier
						
						write (5555,'(a,i4,3(a,f8.4))') 'K,',HH,',',xa_new,',',ya_new,',',DIST
						HH = HH + 1
						write (5555,'(a,i4,3(a,f8.4))') 'K,',HH,',',xb_new,',',yb_new,',',DIST
						HH = HH + 1
						write (5555,'(a,i4,3(a,f8.4))') 'K,',HH,',',xb_new,',',yb_new,',',0.000
						HH = HH + 1
						write (5555,'(a,i4,3(a,f8.4))') 'K,',HH,',',xa_new,',',ya_new,',',0.000
						
						!call extrapole_segment(xa_vol,ya_vol,xb_vol,yb_vol,delta,Xa_new,Ya_new,Xb_new,Yb_new)

		!				call calcul_intersection(xa_vol,ya_vol,pan2_Xd,pan2_Yd,z(pan1,1),z(pan1,3),z(pan1,2),z(pan1,4),parallele,Xa_new,Yi) !ad pan2 avec ab pan 1
!						call calcul_coeff_droite(xa_vol,ya_vol,pan2_Xd,pan2_Yd,a,b,verticale)
						if (verticale.eq.0) then
!							Ya_new =  a*Xa_new + b
						else
!							Ya_new = pan2_Ya + delta ! valeur arbitraire
						endif
						
		!				call calcul_intersection(xb_vol,pan2_Yb,pan2_Xc,pan2_Yc,z(pan1,1),z(pan1,3),z(pan1,2),z(pan1,4),parallele,Xb_new,Yi) !bc pan2 avec ab pan 1
		!				call calcul_coeff_droite(xb_vol,pan2_Yb,pan2_Xc,pan2_Yc,a,b,verticale)
						if (verticale.eq.0) then
		!					Yb_new =  a*Xb_new + b
						else
		!					Yb_new = pan2_Yb + delta ! valeur arbitraire
						endif


						HH = HH + 1
						write (5555,'(a,i4,3(a,f8.4))') 'K,',HH,',',Xa_vol,',',Ya_vol,',',DIST
						HH = HH + 1
						write (5555,'(a,i4,3(a,f8.4))') 'K,',HH,',',Xb_vol,',',Yb_vol,',',DIST
						HH = HH + 1
						write (5555,'(a,i4,3(a,f8.4))') 'K,',HH,',',Xb_vol,',',Yb_vol,',',0.000
						HH = HH + 1
						write (5555,'(a,i4,3(a,f8.4))') 'K,',HH,',',Xa_vol,',',Ya_vol,',',0.000

						temp = 'V,'
						!write(temp((len_trim(temp)+1):200),'(8(i5,a),i3)') K1,',',K2,',',K3,',',K4,',',HH-3,',',HH-2,',',HH-1,',',HH,' ! Vol of the panel ',Pan2
						write(temp((len_trim(temp)+1):200),'(8(i5,a),i3)') HH-7,',',HH-6,',',HH-5,',',HH-4,',',HH-3,',',HH-2,',',HH-1,',',HH,' ! Vol of the panel ',Pan2
						VV = VV + 1
						
						do l=1,6
							k=1
							do while (List_AA(k).ne.0)
								k = k+1
							enddo
							List_AA(k) = 2 ! indique que c'est la surface k fait partie du vol. Elle sera supprimée
						enddo
						!AA = AA + 6 ! TODO check
						HH = HH + 2 !!! A vérifier si c'est toujours 2
						write (5555,'(a)') temp
						
						! Ecrire condition : si intersection entre bloc et surface => commande ASBV
						! else => commande supprime Volume (en prenant alors des blocs plus "larges)

						! Ou: si Il existe panneau à côté du pan2 sur la même horizontale => on garde ainsi
						! sinon on prend une largeur plus grande !!!

						temp = 'ASBV,'
						write(temp((len_trim(temp)+1):200),'(2(i5,a))') liste_bloc(i).AFrame(Pan1),',',1,',,DELETE,DELETE'
						write (5555,'(a)') temp
					
						k=1
						do while (List_AA(k).ne.0)
							k = k+1
						enddo
						!AA = AA + 1 ! car une nouvelle surface est créée
						List_AA(liste_bloc(i).AFrame(Pan1)) = 2 !c'est l'ancien AFrame(Pan1) -> cette surface va disparaitre
						List_AA(k) = 1
						liste_bloc(i).AFrame(Pan1) = k

						! On supprime toutes les surfaces inutiles (ancien Frame(Pan1) + volume)
						k=1
						do while (List_AA(k).ne.0)
							if (List_AA(k).eq.2) List_AA(k)=0
							k = k+1
						enddo

						!!!   ! AA change ? !TODO check

						! ASBV, n°A,n°V,,DELETE,DELETE

	!!!					write(temp((len_trim(temp)+1):200),'(a,i3,a,i3)') ' !Panels:',pan1,' and ',pan2
	!!!					write (5555,'(a)') temp
	!!!					AA = AA+2
	!!!					liste_bloc(i).AFrame(Pan1) = AA !  or AA-1 !!!!!! ADELE,4,,,1
	!!!					temp = 'ADELE,'
	!!!					write(temp((len_trim(temp)+1):200),'(i5,a)') AA-1, ',,,1'
						
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! il faut modifier les coord des blocs concernés !!!

	!!!					write (5555,'(a)') temp
					endif

					! On doit déterminer laquelle de ces 2 surfaces est à conserver => on regarde les 4 premières lignes
					! de chaque avec la ligne de seg1 de pan 1 et si au moins une est la même => c'est la surface à garder
!					write (5555,'(a)') '*SET, FOUND, 0'
					!write (5555,'(a)') '*DO,K,1,4,1'
!					write (5555,'(a,i4,a)') '*GET,L1,AREA,',AA-1,',LOOP,LINE,1'
!					write (5555,'(a,i4,a)') '*GET,L2,AREA,',AA-1,',LOOP,LINE,2'
!					write (5555,'(a,i4,a)') '*GET,L3,AREA,',AA-1,',LOOP,LINE,3'
!					write (5555,'(a,i4,a)') '*GET,L4,AREA,',AA-1,',LOOP,LINE,4'
!					write (5555,'(a,i4,a)') '*GET,L5,AREA,',AA,',LOOP,LINE,1'
!					write (5555,'(a,i4,a)') '*GET,L6,AREA,',AA,',LOOP,LINE,2'
!					write (5555,'(a,i4,a)') '*GET,L7,AREA,',AA,',LOOP,LINE,3'
!					write (5555,'(a,i4,a)') '*GET,L8,AREA,',AA,',LOOP,LINE,4'
!					write (5555,'(a,i4,a)') '*GET,REF,AREA,',liste_bloc(i).AFrame(Pan1),',LOOP,LINE,1' !TODO CHECK !!!
!					write (5555,'(a)') 
!					write (5555,'(a)') '*IF,L1,EQ,REF,THEN'
!					write (5555,'(a)') '*ELSEIF,L2,EQ,REF,THEN'
!					write (5555,'(a)') '*ELSEIF,L3,EQ,REF,THEN'
!					write (5555,'(a)') '*ELSEIF,L4,EQ,REF,THEN'
!					write (5555,'(a)') '*ELSEIF,L5,EQ,REF,THEN'
!					write (5555,'(a)') '*ELSEIF,L6,EQ,REF,THEN'
!					write (5555,'(a)') '*ELSEIF,L7,EQ,REF,THEN'
!					write (5555,'(a)') '*ELSEIF,L8,EQ,REF,THEN'
!					write (5555,'(a)') '*ENDIF'
!					!write (5555,'(a)') '*ENDDO'


					!temp = 'ADELE,
				endif
							
			endif
		enddo
	enddo
enddo
write (5555,'(a)')


!endif !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



! On va 'fusionner' toutes les  surfaces qui se surposent ou se touchent au sein d'un même bloc
! Fusionner à faire dans le bon ordre !!!!!!
write  (5555,'(a)') '! Fusion of surfaces'


do i=1, num_bloc
	if (liste_bloc(i).num_Elmt.gt.1) then
		temp = 'AADD'
		do j=1,liste_bloc(i).num_Elmt
			pan1 = liste_bloc(i).liste(j) 
		!do pan2=1,neto

			write(temp((len_trim(temp)+1):200),'(a,i4)') ',',liste_bloc(i).AFrame(Pan1)

		!enddo
		enddo
		write(temp((len_trim(temp)+1):200),'(a,i2)') ' ! Bloc n°',i
		!write (5555,'(a)') temp
	endif
enddo



write  (5555,'(a)')


! Dessin des épontilles
write (5555,'(a)') '!BEAMS'
do nel=1,neto
	
	if (itype(nel).eq.5) then
		write (5555,'(a,i3)') '!Beam ',nel
		HH = HH + 1
		write (5555,'(a,i4,3(a,f8.4))') 'K,',HH,',',z(nel,1),',',z(nel,3),',',panneau(nel).epsa/2.00
		HH = HH + 1
		write (5555,'(a,i4,3(a,f8.4))') 'K,',HH,',',z(nel,2),',',z(nel,4),',',panneau(nel).epsa/2.00
		temp = 'L'
		write(temp((len_trim(temp)+1):200),'(a,i4)') ',',HH-1
		write(temp((len_trim(temp)+1):200),'(a,i4)') ',',HH
		write (5555,'(a)') temp
	endif
enddo
write (5555,'(a)')


! Copie de chaque surface sur la longueur
write (5555,'(a)') '!COPY OF EACH SURFACE OVER Z'
do i=1,num_bloc
	do j=1,liste_bloc(i).num_Elmt
		nel = liste_bloc(i).liste(j)
		write (5555,'(a,i4)') 'ASEL,S,AREA,,',liste_bloc(i).AFrame(nel)
		write (5555,'(a,i4,a,f8.4)')  'AGEN,',liste_bloc(i).num_sections,',all,,,,,',liste_bloc(i).epsa/1000.
		write (5555,'(a,i4)') 'ASEL,S,AREA,,',liste_bloc(i).APlate(nel)
		write (5555,'(a,i4,a,f8.4)')  'AGEN,',liste_bloc(i).num_sections*2+1,',all,,,,,',liste_bloc(i).epsa/2000.
	enddo
enddo
write (5555,'(a)')

! Copie de chaque beam sur la longueur
write (5555,'(a)') '!COPY OF EACH BEAM OVER Z'
l = 0
do nel=1,neto
	if (itype(nel).eq.5) then
		l = l+1 ! on ne peut pas utiliser les numéros de ligne car il y en a déjà qui ont été générées automatiquement
		write (5555,'(a,f8.4)') 'LSEL,S,LOC,Z,',panneau(nel).epsa/2.00
		write (5555,'(2(a,f8.4))') 'LSEL,R,LOC,X,',DMIN1(z(nel,1),z(nel,3)),',',DMAX1(z(nel,1),z(nel,3))
		write (5555,'(2(a,f8.4))') 'LSEL,R,LOC,X,',DMIN1(z(nel,2),z(nel,4)),',',DMAX1(z(nel,2),z(nel,4))
		write (5555,'(a,i4,a,f8.4)')  'LGEN,',floor(width/panneau(nel).epsa),',all,,,,,',panneau(nel).epsa
	endif
enddo
write (5555,'(a)')



! Suppression des noeuds rededondants
write (5555,'(a)') '!MERGE AND COMPRESS OF COINCIDENT ELEMENTS'
write (5555,'(a)') 'KSEL,ALL'
write (5555,'(a)') 'NUMMRG, KP, , , , , '
write (5555,'(a)') 'NUMCMP,KP'
write(5555,'(a)')

! On "assemble" toutes les surface entre elles
write (5555,'(a)') '! ''ASSEMBLE'' ALL SURFACES'
!write (5555,'(a)') 'ASEL,ALL'
!write (5555,'(a)') 'AGLUE,ALL'
write (5555,'(a)')


write(5555,'(a)') 'LSEL,ALL'

! Affichage de toutes les surfaces
write(5555,'(a)') '!DISPLAY OF ALL SURFACES'
write(5555,'(a)') 'ASEL,ALL'
write(5555,'(a)') 'APLOT,ALL'

close (5555)


! Deallocation des vecteurs
deallocate (pan_place)
deallocate (liste_bloc)

return
end


subroutine calcul_coord(nel,aX,aY,bX,bY,cX,cY,dX,dY)
! ***********************************************************************
!
! Pour un panneau nel donné, cette sous-routine retourne les coordonnées
! des 4 coins du panneau (! sens du panneau et côté du cadre)
!
!
!	d			   c
!	---------------
!  |               |
!  |			   |
!   ------->-------
!  a			   b
!
! ***********************************************************************
use param_section,pi1_=>pi1 ! TODO check si c'est pas le même

double precision aX,aY,bX,bY,cX,cY,dX,dY
double precision pi1
integer*4 nel

aX = z(nel,1)
aY = z(nel,3)
bX = z(nel,2)
bY = z(nel,4)

pi1=pi/180.d00

if (panneau(nel).ksa.eq.1) then
	dX = z(nel,1) + panneau(nel).hya*DSIN(pi1*panneau(nel).ang)	!TODO check les 4 coordonnées
	dY = z(nel,3) + panneau(nel).hya*DCOS(pi1*panneau(nel).ang)
	cX = z(nel,2) + panneau(nel).hya*DSIN(pi1*panneau(nel).ang)
	cY = z(nel,4) + panneau(nel).hya*DCOS(pi1*panneau(nel).ang)
elseif (panneau(nel).ksa.eq.2) then
	dX = z(nel,1) - panneau(nel).hya*DSIN(pi1*panneau(nel).ang)	!TODO check les 4 coordonnées
	dY = z(nel,3) - panneau(nel).hya*DCOS(pi1*panneau(nel).ang)
	cX = z(nel,2) - panneau(nel).hya*DSIN(pi1*panneau(nel).ang)
	cY = z(nel,4) - panneau(nel).hya*DCOS(pi1*panneau(nel).ang)
endif

return
end

subroutine calcul_intersection(Xa,Ya,Xb,Yb,Xc,Yc,Xd,Yd,parallele,Xi,Yi)
! ***********************************************************************
!
! Calcul de l'intersection d'une droite AB avec une droite CD
! où A est défini par Xa, Ya
! où B est défini par Xb, Yb
! où C est défini par Xc, Yc
! où D est défini par Xd, Yd
!
! Le résultats est donné dans Xi,Yi
! parallele = 1 => les droites sont paralleles
!
! ***********************************************************************

double precision Xa,Ya,Xb,Yb,Xc,Yc,Xd,Yd,Xi, Yi
double precision a1,b1,a2,b2
double precision eps
integer*4 parallele	!retourne 1 si les droites sont parallèles
integer*4 verticale

eps = 0.0001

! Y = aX + b = équation d'une droite

! Pour la droite 1 :
call calcul_coeff_droite(Xa,Ya,Xb,Yb,a1,b1,verticale)

!if (dabs(Xa-Xb).gt.eps) then ! alors droite 1 n'est pas verticale
!	a1 = (Ya-Yb)/(Xa-Xb)
!	b1 = Ya - a1*Xa
!else ! alors droite 1 est verticale
!	a1=9999999999.00
!	b1 = Xa
!endif

! Pour la droite 2 :
call calcul_coeff_droite(Xc,Yc,Xd,Yd,a2,b2,verticale)

!if (dabs(Xc-Xd).gt.eps) then ! alors droite 2 n'est pas verticale
!	a2 = (Yc-Yd)/(Xc-Xd)
!	b2 = Yc - a2*Xc
!else
!	a2=9999999999.00
!	b2 = Xc
!endif ! alors droite 2 est verticale

if ((dabs(a1-a2).lt.eps)) then !.or.((dabs(Xa-Xb).gt.eps).and.(dabs(Xc-Xd).gt.eps))) then ! alors droite //
	parallele = 1 ! les droites sont parallèles
else
	parallele = 0
	if (dabs(Xa-Xb).lt.eps) then
		Xi = Xa
		!if (dabs(a2).lt.eps) then !droite horizontale
		!	Xi = b1
		!else
			!Xi = (Yi - b2)/a2
			Yi = a2*Xi + b2
		!endif
	elseif (dabs(Xc-Xd).lt.eps) then
		Xi = Xc !Yi = Xc
		!if (dabs(a1).lt.eps) then !droite horizontale
		!	Xi = b2
		!else
			!Xi = (Yi - b1)/a1
			Yi = a1*Xi + b1
		!endif
	else
		Xi = (b1-b2)/(a2-a1)
		Yi = a1*Xi + b1
	endif
endif


!if ((((Yb-Ya)/(Xb-Xa))-((Yd-Yc)/(Xd-Xc))).ne.0) then
!	Xi=(((Yb-Ya)/(Xb-Xa))*Xa+Ya-((Yd-Yc)/(Xd-Xc))*Xc-Yc) / (((Yb-Ya)/(Xb-Xa))-((Yd-Yc)/(Xd-Xc)))
!	Yi=((Yb-Ya)/(Xb-Xa))*((((Yb-Ya)/(Xb-Xa))*Xa+Ya-((Yd-Yc)/(Xd-Xc))*Xc-Yc) / (((Yb-Ya)/(Xb-Xa))-((Yd-Yc)/(Xd-Xc)))-Xa)+Ya
!else
!	parallele = 1 ! les droites sont parallèles
!endif


return
end

subroutine calcul_distance_2points(Xa,Ya,Xb,Yb,distance)
! *******************************************************************************
!
! Calcul la distance entre 2 points A et B
!
! *******************************************************************************

double precision Xa,Ya,Xb,Yb,distance

distance = dsqrt((Xa-Xb)**2 + (Ya-Yb)**2)

return
end

subroutine correction_coord(pan1,pan2,liste_coord,liste_coord_sem,couple_actif,point_actif,num_coord)
! *******************************************************************************
!
! Teste l'intersection entre le seg2 de pan 1 et les 2 segments de pan2
! L'intersection avec le seg1 de pan2 est le point X1
! L'intersection avec le seg2 de pan2 est le point X2
!
! Si un point du seg2 du pan 1 (pt1) est compris dans le "cadre" de pan2 :
!	- on regarde le point X1 et X2 le plus proche (=pt3) de l'autre pt du seg2 du pan 1 (pt2)
!   - on modifie les coordonnées du pt1 et on met celle du pt3
!
! Si aucun n'est compris dans le "cadre" de pan2 :
!   - Si X2 appartient à cd :
!		- on prend le point
!   - Si X2 n'appartient pas à cd :
!		- on prend le min de pt1-X2 pt2-X2 ce qui nous donne notre pt à modifier et sa valeur
!   - on modifie les coordonnées de ce pt
!
! Si les 2 sont compris dedans : error (TODO : en fait, il suffirait de ne pas tracer ce cadre)
!
! *******************************************************************************

integer*4 pan1,pan2,parallele
integer*4 i
double precision liste_coord(10,2,200)		! pour chaque panneau, liste des 4 coord qui seront utilisées pour déterminer la surface
double precision liste_coord_sem(2,2,200)	! pour chaque panneau, liste des 2 coord qui seront utilisées pour déterminer la semelle
integer*4 num_coord(200)
integer*4 couple_actif(200,200,2)				! (2,4,2)=1 => le point c du panneau 2 est à l'intérieur du "cadre" du panneau 4
integer*4 point_actif(200,2)


double precision Xi,Yi,point1_X,point1_Y,point2_X,point2_Y,point_intX,point_intY,distance
double precision pan1_Xa,pan1_Ya,pan1_Xb,pan1_Yb,pan1_Xc,pan1_Yc,pan1_Xd,pan1_Yd

double precision pan2_Xa,pan2_Ya,pan2_Xb,pan2_Yb
double precision pan2_Xc,pan2_Yc,pan2_Xd,pan2_Yd

double precision xy(1:2,1:200),temp
integer*4 inpoly,ab

double precision eps

eps = 0.000001


pan1_Xa = liste_coord(1,1,pan1)
pan1_Ya = liste_coord(1,2,pan1)
pan1_Xb = liste_coord(2,1,pan1)
pan1_Yb = liste_coord(2,2,pan1)

pan1_Xc = liste_coord(3,1,pan1)
pan1_Yc = liste_coord(3,2,pan1)
pan1_Xd = liste_coord(4,1,pan1)
pan1_Yd = liste_coord(4,2,pan1)

call calcul_coord(pan2,pan2_Xa,pan2_Ya,pan2_Xb,pan2_Yb,pan2_Xc,pan2_Yc,pan2_Xd,pan2_Yd)
! (car il vaut mieux utiliser les coord initiales plutôt que les modifiées)

!pan2_Xa = liste_coord(1,1,pan2) !segment 1
!pan2_Ya = liste_coord(1,2,pan2)
!pan2_Xb = liste_coord(2,1,pan2)
!pan2_Yb = liste_coord(2,2,pan2)

!pan2_Xc = liste_coord(3,1,pan2) !segment2
!pan2_Yc = liste_coord(3,2,pan2)
!pan2_Xd = liste_coord(4,1,pan2)
!pan2_Yd = liste_coord(4,2,pan2)

! Recherche du point commun entre pan1 et pan2

if ((dabs(pan1_Xa-pan2_Xa).lt.eps).and.(dabs(pan1_Ya-pan2_Ya).lt.eps).or.	&
	(dabs(pan1_Xa-pan2_Xb).lt.eps).and.(dabs(pan1_Ya-pan2_Yb).lt.eps))	then
	! Alors c'est le point a qui est commun
	ab=1
else
	! Alors c'est le point b qui est commun
	ab=2
	pan1_Xa = liste_coord(2,1,pan1)
	pan1_Ya = liste_coord(2,2,pan1)
	pan1_Xb = liste_coord(1,1,pan1)
	pan1_Yb = liste_coord(1,2,pan1)

	pan1_Xc = liste_coord(4,1,pan1)
	pan1_Yc = liste_coord(4,2,pan1)
	pan1_Xd = liste_coord(3,1,pan1)
	pan1_Yd = liste_coord(3,2,pan1)
endif


methode = 1

if (methode.eq.1) then

call calcul_intersection(pan1_Xb,pan1_Yb,pan1_Xd,pan1_Yd,pan2_Xa,pan2_Ya,pan2_Xb,pan2_Yb,parallele,Xi,Yi) !!!
i = 0

if (parallele.eq.0) then
	call point_appartient_segment(Xi,Yi,pan2_Xa,pan2_Ya,pan2_Xb,pan2_Yb,i) !!!

	if (i.eq.1) then !Xi,Yi appartient au seg2 du pan 2 à l'intérieur du segment
		i=0
		call point_appartient_segment(Xi,Yi,pan1_Xb,pan1_Yb,pan1_Xd,pan1_Yd,i)
	endif

    !!!call calcul_intersection(pan1_Xa,pan1_Ya,pan1_Xd,pan1_Yd,pan2_Xa,pan2_Ya,pan2_Xb,pan2_Yb,parallele,Xi,Yi) !!!
    call calcul_intersection(pan1_Xa,pan1_Ya,pan1_Xd,pan1_Yd,pan2_Xa,pan2_Ya,pan2_Xb,pan2_Yb,parallele,Xi,Yi) !!!
	!call calcul_intersection(pan1_Xa,pan1_Ya,pan1_Xd,pan1_Yd,pan2_Xc,pan2_Yc,pan2_Xd,pan2_Yd,parallele,Xi,Yi)
	j = 0

	if (parallele.eq.0) then
		call point_appartient_segment(Xi,Yi,pan2_Xc,pan2_Yc,pan2_Xd,pan2_Yd,j) !déjà vérifié en fait

		if (j.eq.1) then !Xi,Yi appartient au seg2 du pan 2 à l'intérieur du segment
			j=0
			call point_appartient_segment(Xi,Yi,pan1_Xa,pan1_Ya,pan1_Xd,pan1_Yd,j)
		endif
	endif

	if (j.eq.0) then !on regarde qd même en 1 point juste à côté de a, car il existe un cas particulier ou tester les 2
					 ! pts précédents n'est pas suffisant, il faut tester aussi les points entre. Avec rien que ce point,
					 ! ça devrait être ok (même si pas une solution parfaite)
		temp = 0.1
		call calcul_point_intermediaire(pan1_Xa,pan1_Ya,pan1_Xb,pan1_Yb,point_intX,point_intY,temp)
		call calcul_intersection(point_intX,point_intY,pan1_Xd,pan1_Yd,pan2_Xa,pan2_Ya,pan2_Xb,pan2_Yb,parallele,Xi,Yi) !!!
		if (parallele.eq.0) then
			j = 0
			call point_appartient_segment(Xi,Yi,pan2_Xa,pan2_Ya,pan2_Xb,pan2_Yb,j) !déjà vérifié en fait   !!!

			if (j.eq.1) then !Xi,Yi appartient au seg1 du pan 2 à l'intérieur du segment
				j=0
				!call point_appartient_segment(Xi,Yi,pan1_Xa,pan1_Ya,pan1_Xd,pan1_Yd,j)
				call point_appartient_segment(Xi,Yi,point_intX,point_intY,pan1_Xd,pan1_Yd,j)
			endif
		endif
	endif

endif

if ((i.eq.1).or.(j.eq.1)) then ! bd ou ad coupe seg2 pan2 à l'intérieur des 2 segments
	! Le point d de pan1 est à "supprimer"
	
	i = 0
	call calcul_intersection(pan1_Xc,pan1_Yc,pan1_Xd,pan1_Yd,pan2_Xa,pan2_Ya,pan2_Xb,pan2_Yb,parallele,Xi,Yi)
	
	if (parallele.eq.0) then
		call point_appartient_segment(Xi,Yi,pan2_Xa,pan2_Ya,pan2_Xb,pan2_Yb,i)
		if (i.eq.1) then
			! Xi,Yi est le nouveau point à prendre en considération dans le trajet
			if (ab.eq.1) then
				liste_coord(4,1,pan1) = Xi
				liste_coord(4,2,pan1) = Yi
			else
				liste_coord(3,1,pan1) = Xi
				liste_coord(3,2,pan1) = Yi
			endif		
		endif
	endif

	if (i.eq.0) then
		call calcul_intersection(pan1_Xc,pan1_Yc,pan1_Xb,pan1_Yb,pan2_Xa,pan2_Ya,pan2_Xb,pan2_Yb,parallele,Xi,Yi)
		
		if (parallele.eq.0) then
			call point_appartient_segment(Xi,Yi,pan2_Xa,pan2_Ya,pan2_Xb,pan2_Yb,i)
	!!!		if (i.eq.1) then
				! Xi,Yi est le nouveau point à prendre en considération dans le trajet
				! + on doit "supprimer" également le point c de pan 1
				num_coord(pan1) = 3
				if (ab.eq.1) then		!TODO même chose en fait ?
					liste_coord(3,1,pan1) = Xi
					liste_coord(3,2,pan1) = Yi
					liste_coord(4,1,pan1) = 0.00	!peut-être même pas nécessaire
					liste_coord(4,2,pan1) = 0.00
				else
					liste_coord(3,1,pan1) = Xi
					liste_coord(3,2,pan1) = Yi
					liste_coord(4,1,pan1) = 0.00	!peut-être même pas nécessaire
					liste_coord(4,2,pan1) = 0.00
				endif
	!!!		endif
		endif
	endif
	

endif

elseif (methode.eq.2) then	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Calcul des intersections du second segment du pan1

call calcul_intersection(pan1_Xc,pan1_Yc,pan1_Xd,pan1_Yd,pan2_Xa,pan2_Ya,pan2_Xb,pan2_Yb,parallele,Xi,Yi)

if (parallele.eq.0) then	!alors on peut garder l'intersection
	point1_X = Xi
	point1_Y = Yi
endif

pan2_Xc = liste_coord(3,1,pan2)
pan2_Yc = liste_coord(3,2,pan2)
pan2_Xd = liste_coord(4,1,pan2)
pan2_Yd = liste_coord(4,2,pan2)
				
call calcul_intersection(pan1_Xc,pan1_Yc,pan1_Xd,pan1_Yb,pan2_Xc,pan2_Yc,pan2_Xd,pan2_Yd,parallele,Xi,Yi)

if (parallele.eq.0) then	!alors on peut garder l'intersection
	point2_X = Xi
	point2_Y = Yi
endif

if (parallele.eq.0) then	!sinon pas de sens de chercher intersection, etc.

	! On regarde quels sont les points "actifs" du segment. Un point n'est pas actif s'il appartient
	! au rectangle du panneau lié (défini donc par les cadres). En effet, dans ce cas, ce point n'est
	! pas à être considéré dans le calcul ANSYS
	! !!! On peut être actif vis-à-vis d'un panneau mais pas d'un autre. On est "actif" par couple de
	!     panneaux !!!
	xy(1,1) = pan2_Xa
	xy(2,1) = pan2_Ya
	xy(1,2) = pan2_Xb
	xy(2,2) = pan2_Yb
	xy(1,3) = pan2_Xc
	xy(2,3) = pan2_Yc
	xy(1,4) = pan2_Xd
	xy(2,4) = pan2_Yd

	call ptinpoly(pan1_Xc,pan1_Yc,9999.d00,9999.d00,4,xy,inpoly)
	couple_actif(pan1,pan2,1) = inpoly
	point_actif(pan1,1) = inpoly
	call ptinpoly(pan1_Xd,pan1_Yd,9999.d00,9999.d00,4,xy,inpoly)
	couple_actif(pan1,pan2,2) = inpoly
	point_actif(pan1,2) = inpoly


	! On regarde la distance des pts du seg1 du panneau 1 par rapport aux 2 pts d'intersection
	if ((couple_actif(pan1,pan2,1).eq.1).and.(couple_actif(pan1,pan2,2).eq.4)) then
		write(*,*) 'Error transformation of the ANSYS file!!! Panel ',pan2,' too small, or frames of panel ',pan1,' too high'
		write(*,*) 'Please check your data - press RETURN to resume'
		read(*,*)
		stop
	elseif (couple_actif(pan1,pan2,1).eq.1) then !alors on doit changer les coord du point c
		! On prend le point X1, X2 le plus proche du point d

		call find_minimum_point(point1_X,point1_Y,point2_X,point2_Y,liste_coord(4,1,pan1),liste_coord(4,2,pan1),ll)

		if (ll.eq.1) then
			liste_coord(3,1,pan1) =  point1_X
			liste_coord(3,2,pan1) =  point1_Y
		else
			liste_coord(3,1,pan1) =  point2_X
			liste_coord(3,2,pan1) =  point2_Y
		endif

	elseif (couple_actif(pan1,pan2,2).eq.1) then !alors on doit changer les coord du point d
		! On prend le point X1, X2 le plus proche du point c

		call find_minimum_point(point1_X,point1_Y,point2_X,point2_Y,liste_coord(3,1,pan1),liste_coord(3,2,pan1),ll)

		if (ll.eq.1) then
			liste_coord(4,1,pan1) =  point1_X
			liste_coord(4,2,pan1) =  point1_Y
		else
			liste_coord(4,1,pan1) =  point2_X
			liste_coord(4,2,pan1) =  point2_Y
		endif
	else
		! alors on regarde quel est la plus petite distance entre les 2 points du seg et les 2 intersection
		! (le couple détermine le point à changer, et la valeur de ces coordonnées)
		distance_min = 9999999.00
								
		call calcul_distance_2points(pan1_Xc,pan1_Yc,point1_X,point1_Y,distance)
		if (distance.lt.distance_min) then
			distance = distance_min
			ll=1 !point c doit a priori être modifié
			k=1  !on prend le point X1 a priori
		endif
									
		call calcul_distance_2points(pan1_Xd,pan1_Yd,point1_X,point1_Y,distance)
		if (distance.lt.distance_min) then
			distance = distance_min
			ll=2 !point d doit être modifié
			k=1  !on prend le point X1 a priori
		endif
	
		call calcul_distance_2points(pan1_Xc,pan1_Yc,point2_X,point2_Y,distance)
		if (distance.lt.distance_min) then
			distance = distance_min
			ll=1 !point c doit a priori être modifié
			k=2  !on prend le point X2 a priori
		endif

		call calcul_distance_2points(pan1_Xd,pan1_Yd,point2_X,point2_Y,distance)
		if (distance.lt.distance_min) then
			distance = distance_min
			ll=1 !point d doit être modifié
			k=2  !on prend le point X2
		endif

		if (ll.eq.1) then
			if (k.eq.1) then
				liste_coord(3,1,pan2) =  point1_X
				liste_coord(3,2,pan2) =  point1_Y
			else
				!liste_coord(3,1,pan2) =  point2_X !NE RIEN FAIRE POUR L'INSTANT (très compliqué en fait)
				!liste_coord(3,2,pan2) =  point2_Y
			endif
		else
			if (k.eq.1) then
				liste_coord(4,1,pan2) =  point1_X
				liste_coord(4,2,pan2) =  point1_Y
			else
				!liste_coord(4,1,pan2) =  point2_X
				!liste_coord(4,2,pan2) =  point2_Y
			endif
		endif
	endif

endif


endif !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

return
end


subroutine find_minimum_point(pt1X,pt1Y,pt2X,pt2Y,x,y,i)
! *******************************************************************************
!
! Retourne le point le plus proche du point (X,Y)
! Output i : = 1 => c'est le pt1 le plus proche
!			 = 2 => c'est le pt2 le plus proche
!
! *******************************************************************************

double precision pt1X,pt1Y,pt2X,pt2Y,x,y
double precision distance,distance_min
integer*4 i

distance_min = 9999999.00
i = 0
						
call calcul_distance_2points(pt1X,pt1Y,x,y,distance)
if (distance.lt.distance_min) then
	distance = distance_min
	i=1
endif
									
call calcul_distance_2points(pt2X,pt2Y,x,y,distance)
if (distance.lt.distance_min) then
	distance = distance_min
	i=2
endif

return
end

subroutine point_appartient_segment(x,y,X1,Y1,X2,Y2,i)
! *******************************************************************************
!
! Indique si le point (x,y) appartient au segment défini par 2 points (X1,Y1) et (X2,Y2)
! (coins non compris)
! Output i : = 1 => oui
!			 = 0 => non
!
! *******************************************************************************

double precision x,y,X1,Y1,X2,Y2,eps,a,b
integer*4 i,verticale

i = 0
eps = 0.00001

! On regarde si le point appartient à la droite définie par les 2 points


! Y = aX + b = équation d'une droite
! Calcul des coefficients de la droite :
call calcul_coeff_droite(X1,Y1,X2,Y2,a,b,verticale)

if (dabs(X1-X2).gt.eps) then ! alors droite n'est pas verticale
	if (dabs((y-(a*x+b))).lt.eps) then	! alors le point est sur la droite
		if (dabs(Y1-Y2).gt.eps) then ! alors droite pas horizontale
			if ((x.gt.dmin1(X1,X2)+eps).and.(x.lt.dmax1(X1,X2)-eps).and.(y.gt.dmin1(Y1,Y2)+eps).and.(y.lt.dmax1(Y1,Y2)-eps)) then
				i = 1	! le point appartient bien au segment de droite
			endif
		else
			if ((x.gt.dmin1(X1,X2)+eps).and.(x.lt.dmax1(X1,X2)-eps).and.(dabs(y-Y1).lt.eps)) then
				i = 1	! le point appartient bien au segment de droite
			endif
		endif
	endif
else	! alors droite verticale
	if ((dabs(x-X1).lt.eps).and.(y.gt.(dmin1(Y1,Y2)+eps)).and.(y.lt.(dmax1(Y1,Y2)-eps))) then
		i = 1	! le point appartient bien au segment de droite
	endif
endif

return
end

subroutine calcul_point_intermediaire(X1,Y1,X2,Y2,point_intX,point_intY,ratio)
! *******************************************************************************
!
! Cette sous-routine retourne le point (point_intX,point_intY) qui se trouve sur
! le segment reliant (X1,Y1) et (X2,Y2) et se trouvant au ratio du tronçon
! (soit très proche du point (X1,Y1))
!
!
! *******************************************************************************

double precision X1,Y1,X2,Y2,point_intX,point_intY,a,b
double precision eps,ratio
integer*4 verticale
eps = 0.0001

call calcul_coeff_droite(X1,Y1,X2,Y2,a,b,verticale)

! Calcul du point recherché
if (dabs(X1-X2).gt.eps) then ! alors droite n'est pas verticale
	if (X2.gt.X1) point_intX = X1 + dabs(X2-X1)*ratio
	if (X2.le.X1) point_intX = X1 - dabs(X2-X1)*ratio
else ! alors droite 1 est verticale
	point_intX = X1
endif

point_intY = a*point_intX + b

return
end

subroutine test_point_commun(pan1,pan2,liste_coord,commun)
! *******************************************************************************
!
! Cette sous-routine teste si 2 panneaux pan1 et pan2 ont au moins un "coin" en commun
! (on ne peut utiliser NOH car si 2 panneaux arrivent au même point, cela ne marche pas,
!  NOH ne répertoriant que les panneaux qui suivent - partent)
!
! Output : commun (=0 => pas de point en commun)
!
! *******************************************************************************

integer*4 pan1,pan2,commun
double precision liste_coord(10,2,200)

double precision pan1_Xa,pan1_Ya,pan1_Xb,pan1_Yb
double precision pan2_Xa,pan2_Ya,pan2_Xb,pan2_Yb

double precision eps

eps = 0.000001

pan1_Xa = liste_coord(1,1,pan1)
pan1_Ya = liste_coord(1,2,pan1)
pan1_Xb = liste_coord(2,1,pan1)
pan1_Yb = liste_coord(2,2,pan1)


pan2_Xa = liste_coord(1,1,pan2) !segment 1
pan2_Ya = liste_coord(1,2,pan2)
pan2_Xb = liste_coord(2,1,pan2)
pan2_Yb = liste_coord(2,2,pan2)


! Recherche du point commun entre pan1 et pan2

if ((dabs(pan1_Xa-pan2_Xa).lt.eps).and.(dabs(pan1_Ya-pan2_Ya).lt.eps).or.	&
	(dabs(pan1_Xa-pan2_Xb).lt.eps).and.(dabs(pan1_Ya-pan2_Yb).lt.eps).or.	&
	(dabs(pan1_Xb-pan2_Xb).lt.eps).and.(dabs(pan1_Yb-pan2_Yb).lt.eps).or.	&
	(dabs(pan1_Xb-pan2_Xb).lt.eps).and.(dabs(pan1_Yb-pan2_Yb).lt.eps))	then
	! Alors on a un point en commun
	commun=1
endif

return
end

subroutine is_Frame_intersecte_Panel(pan1,pan2,liste_coord,num_coord,resultat,xa_vol,ya_vol,xb_vol,yb_vol,xa_new,ya_new,xb_new,yb_new)
! *******************************************************************************
!
! Cette sous-routine indique si la frame (pan1, dont les coord sont dans liste_coord)
! traverse le panneau Panel (pan2). La frame peut déjà avoir été modifiée avant, donc
! être de forme triangulaire plutôt que rectangulaire
! 
! On trace 10 // aux courbes ad et bc et on regarde si ça intersecte le seg1 du pan2
! (ce n'est donc pas une méthode fiable à 100% !!!)
!
! Resultat : 0 => pas d'intersection; 1 => il y a une intersection
!
! Si intersection : alors on trace le volume de base Pan2 (extrusion dans une direction
!                   opposée à pan 1). On donne donc les coordonnées xa_vol,ya_vol,xb_vol,yb_vol
!
! *******************************************************************************
use param_section, ONLY : z,panneau

integer*4 pan1,pan2,resultat,parallele,i,j
double precision liste_coord(10,2,200),Xi,Yi,ratio,delta
double precision point_intXa,point_intYa,point_intXd,point_intYd,point_intXb,point_intYb
double precision pan1_Xa,pan1_Ya,pan1_Xb,pan1_Yb,pan1_Xd,pan1_Yd,pan1_Xc,pan1_Yc,pan2_Xa,pan2_Ya,pan2_Xb,pan2_Yb
integer*4 num_coord(200)
double precision a,b,a1,b1,xa_vol,ya_vol,xb_vol,yb_vol,eps,val_ref
double precision xa_new,ya_new,xb_new,yb_new
integer*4 verticale,num_parallele
eps = 0.0001

pan1_Xa = liste_coord(1,1,pan1)
pan1_Ya = liste_coord(1,2,pan1)
pan1_Xb = liste_coord(2,1,pan1)
pan1_Yb = liste_coord(2,2,pan1)
pan1_Xc = liste_coord(3,1,pan1)
pan1_Yc = liste_coord(3,2,pan1)
pan1_Xd = liste_coord(4,1,pan1)
pan1_Yd = liste_coord(4,2,pan1)


pan2_Xa = z(pan2,1) !segment 1
pan2_Ya = z(pan2,3)
pan2_Xb = z(pan2,2)
pan2_Yb = z(pan2,4)

resultat = 0

num_parallele = 100 ! nombre de // à AD et BC testées (+ gd, + précision augmente)
!!!!!!!!!!!!!!!!!!!!!!call calcul_point_intermediaire(pan1_Xa,pan1_Ya,pan1_Xd,pan1_Yd,point_intX,point_intY,1/10.00)
do i=1,num_parallele+1
	if (resultat.eq.0) then

		if (i.le.num_parallele-1) then ! Cas des lignes // intermédiaires
			ratio = 1.0*i/num_parallele
			call calcul_point_intermediaire(pan1_Xa,pan1_Ya,pan1_Xb,pan1_Yb,point_intXa,point_intYa,ratio) ! point proche de a sur le seg ab
		elseif (i.eq.num_parallele) then ! Cas test de la ligne ad
			point_intXa = pan1_Xa
			point_intYa = pan1_Ya
		elseif (i.eq.num_parallele+1) then ! Cas test de la ligne bc
			point_intXa = pan1_Xb
			point_intYa = pan1_Yb
		endif

		j = 0
		if (num_coord(pan1).eq.4) then
			if (i.le.num_parallele-1) then ! Cas des lignes // intermédiaires
				call calcul_point_intermediaire(pan1_Xd,pan1_Yd,pan1_Xc,pan1_Yc,point_intXd,point_intYd,ratio) ! point proche de d sur le seg dc
			elseif (i.eq.num_parallele) then ! Cas test de la ligne ad
				point_intXd = pan1_Xd
				point_intYd = pan1_Yd
			elseif (i.eq.num_parallele+1) then ! Cas test de la ligne bc
				point_intXd = pan1_Xc
				point_intYd = pan1_Yc
			endif

			call calcul_intersection(point_intXa,point_intYa,point_intXd,point_intYd,pan2_Xa,pan2_Ya,pan2_Xb,pan2_Yb,parallele,Xi,Yi) !!!
			if (parallele.eq.0) then
				call point_appartient_segment(Xi,Yi,point_intXa,point_intYa,point_intXd,point_intYd,j) !!!
			endif
		elseif (num_coord(pan1).eq.3) then
			if (i.le.num_parallele-1) then ! Cas des lignes // intermédiaires
				call calcul_point_intermediaire(pan1_Xb,pan1_Yb,pan1_Xc,pan1_Yc,point_intXb,point_intYb,ratio) ! point proche de b sur le seg bc
			else ! Cas test de la ligne ad et Cas test de la ligne bc
				point_intXc = pan1_Xc
				point_intYc = pan1_Yc
			endif

			call calcul_intersection(point_intXa,point_intYa,point_intXb,point_intYb,pan2_Xa,pan2_Ya,pan2_Xb,pan2_Yb,parallele,Xi,Yi) !!!
			if (parallele.eq.0) then
				call point_appartient_segment(Xi,Yi,point_intXa,point_intYa,point_intXb,point_intYb,j) !!!	
			endif
		else
			write(*,*) 'Error in subroutine ANSYS! Problem in the panel ',pan1
			write(*,*) 'Wrong number of segments for the representation of the frames'
			write(*,*) 'Please check your data - press return to resume'
			read(*,*)
			stop
		endif

		if (j.eq.1) then
			j = 0
			call point_appartient_segment(Xi,Yi,pan2_Xa,pan2_Ya,pan2_Xb,pan2_Yb,j) ! point appartient au seg1 pan2	
			
			if (j.eq.1) then
				! Si Seg1 pan2 sur la même droite que ab ou bc, alors c'est un cas particulier, le segment 2 du Pan 2 n'est pas
				! vraiment à l'intérieur du frame du panel 1 => on ne doit rien modifier
				call calcul_coeff_droite(pan2_Xa,pan2_Ya,pan2_Xb,pan2_Yb,a,b,verticale) ! Calcul les coefficient d'une droite définie par 2 points (y = aX+b)
				call calcul_coeff_droite(pan1_Xa,pan1_Ya,pan1_Xc,pan1_Yc,a1,b1,verticale)
				if ((dabs(a-a1).lt.eps).and.(dabs(b-b1).lt.eps)) j = 0
				call calcul_coeff_droite(pan1_Xb,pan1_Yb,pan1_Xc,pan1_Yc,a1,b1,verticale)
				if ((dabs(a-a1).lt.eps).and.(dabs(b-b1).lt.eps)) j = 0
			endif

			if (j.eq.1) then
				resultat = 1

		!!!		call calcul_intersection(pan2_Xa,pan2_Ya,pan2_Xb,pan2_Yb,pan1_Xa,pan1_Ya,pan1_Xd,pan1_Yd,parallele,Xa_new,Ya_new) !ab pan2 avec ad pan 1
		!!!		call calcul_intersection(pan2_Xa,pan2_Ya,pan2_Xb,pan2_Yb,pan1_Xb,pan1_Yb,pan1_Xc,pan1_Yc,parallele,Xb_new,Yb_new) !ab pan2 avec bc pan 1
			!			call calcul_coeff_droite(pan2_Xa,pan2_Ya,pan2_Xd,pan2_Yd,a,b,verticale)
						if (verticale.eq.0) then
			!				Ya_new =  a*Xa_new + b
						else
			!				Ya_new = pan2_Ya + delta ! valeur arbitraire
						endif
				
				delta = 1.00
				call extrapole_segment(pan2_Xa,pan2_Ya,pan2_Xb,pan2_Yb,delta,Xa_new,Ya_new,Xb_new,Yb_new,pan2)

				!xa_new = pan2_Xa
				!ya_new = pan2_Ya
				!xb_new = pan2_Xb
				!yb_new = pan2_Yb

				! Détermination des points qui vont servir à tracer le volume permettant de "couper" les frames qui "dépassent"
				val_ref = 1.000
				delta = DSIN(panneau(pan2).ang*3.14/180.)*val_ref ! val_ref = valeur standard ! doit pas être trop petite
				!call calcul_coeff_droite(pan2_Xa,pan2_Ya,pan2_Xb,pan2_Yb,a,b,verticale) ! Calcul les coefficients d'une droite définie par 2 points (y = aX+b)
				if (panneau(pan2).ksa.eq.1) then	!TODO check if 1 or 2 + ! sens du panneau2 !!!
					xa_vol = xa_new - delta !xa_vol = pan2_Xa + delta
					xb_vol = xb_new - delta
					if (dabs(a).lt.eps) then !droite seg1 pan 2 horizontale
						ya_vol = ya_new - val_ref
						yb_vol = yb_new - val_ref
					else
						ya_vol = -xa_vol/a + ya_new+xa_new/a ! xa_vol/a + b
						yb_vol = -xb_vol/a + yb_new+xb_new/a ! xb_vol/a + b
					endif
				else
					xa_vol = xa_new + delta
					xb_vol = xb_new + delta
					if (dabs(a).lt.eps) then !droite horizontale
						ya_vol = ya_new + val_ref
						yb_vol = yb_new + val_ref
					else
						ya_vol = -xa_vol/a + ya_new+xa_new/a ! -xa_vol/a + b
						yb_vol = -xb_vol/a + yb_new+xb_new/a !-xb_vol/a + b
					endif
				endif
			endif
		endif
	endif
enddo


return
end


subroutine calcul_coeff_droite(X1,Y1,X2,Y2,a,b,verticale)
! *************************************************************************************************
!
! Calcul des coefficients a et b d'une droite définie par 2 points
! (y = aX + b)
!
! *************************************************************************************************

double precision X1,Y1,X2,Y2,a,b
integer*4 verticale

double precision eps
eps = 0.0001
verticale = 0
! Calcul des coefficients de la droite
if (dabs(X1-X2).gt.eps) then ! alors droite n'est pas verticale
	a = (Y1-Y2)/(X1-X2)
	b = Y1 - a*X1
	verticale = 0
else ! alors droite 1 est verticale
	a=9999999999.00
	b = X1
	verticale = 1
endif


return
end

subroutine extrapole_segment(Xa,Ya,Xb,Yb,delta,Xa_new,Ya_new,Xb_new,Yb_new,pan1)
! *******************************************************************************************************
!
! Soit un segment défini par 2 points a et B. Cette sous-routine crée un segment plus grand englobant 
! celui-ci et l'agrandissant d'un facteur lié à delta donné en input.
! Si le point A (ou B) a un panneau de même pente juste à côté => on n'extrapole pas
! Sinon on effectue l'extrapolation
!
! *******************************************************************************************************
use param_section

double precision Xa,Ya,Xb,Yb,delta,Xa_new,Ya_new,Xb_new,Yb_new
double precision a1,b1,a2,b2,alpha,distance,eps
integer*4 pan1,pan2,verticale,founda,foundb

eps = 0.000001

call calcul_coeff_droite(Xa,Ya,Xb,Yb,a1,b1,verticale)

call calcul_distance_2points(Xa,Ya,Xb,Yb,distance)

alpha = DACOS(dabs(Xa-Xb)/distance)

! On regarde s'il y a au moins un panneau qui passe par Xa,Ya et qui a le même coefficient directeur

founda = 0
do pan2=1,neto
	if ((pan1.ne.pan2).and.(dabs(panneau(pan2).hight).gt.0.10)) then
		if ((dabs(Xa-z(pan2,1)).lt.eps).and.(dabs(Ya-z(pan2,3)).lt.eps).or.	&
			(dabs(Xa-z(pan2,2)).lt.eps).and.(dabs(Ya-z(pan2,4)).lt.eps))	then
			! Alors on a un point en commun
			
			call calcul_coeff_droite(z(pan2,1),z(pan2,3),z(pan2,2),z(pan2,4),a2,b2,verticale)
			if (dabs(a1-a2).lt.eps) founda=1
		endif
	endif
enddo

foundb = 0
do pan2=1,neto
	if ((pan1.ne.pan2).and.(dabs(panneau(pan2).hight).gt.0.10)) then
		if ((dabs(Xb-z(pan2,1)).lt.eps).and.(dabs(Yb-z(pan2,3)).lt.eps).or.	&
			(dabs(Xb-z(pan2,2)).lt.eps).and.(dabs(Yb-z(pan2,4)).lt.eps))	then
			! Alors on a un point en commun
			
			call calcul_coeff_droite(z(pan2,1),z(pan2,3),z(pan2,2),z(pan2,4),a2,b2,verticale)
			if (dabs(a1-a2).lt.eps) foundb=1
		endif
	endif
enddo

if (Xa.gt.Xb) then
	if (founda.eq.1) then
		Xa_new = Xa
	else
		Xa_new = Xa + delta*DCOS(alpha)
	endif
	
	if (foundb.eq.1) then
		Xb_new = Xb
	else
		Xb_new = Xb - delta*DCOS(alpha)
	endif
else
	if (founda.eq.1) then
		Xa_new = Xa
	else
		Xa_new = Xa - delta*DCOS(alpha)
	endif

	if (foundb.eq.1) then
		Xb_new = Xb
	else
		Xb_new = Xb + delta*DCOS(alpha)
	endif
endif

if (verticale.eq.0) then ! droite non verticale
	Ya_new = a1*Xa_new + b1
	Yb_new = a1*Xb_new + b1
else
	if (Ya.gt.Yb) then
		Ya_new = Ya + delta
		Yb_new = Yb - delta
	else
		Ya_new = Ya - delta
		Yb_new = Yb + delta
	endif
endif

return
end


