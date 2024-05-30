subroutine write_LotStat()

use param_section, temp_=>temp			! Pour avoir les infos panneaux

implicit double precision(a-h,o-z)
integer*4 temp(100),nbr_lot,found,hor

type struct_lot
	double precision thick					!épaisseur du panneau le + gd
	integer*4 famille
	double precision delta_lisse
	double precision poids_lot				! pds pont + cloisons longi en dessous
	double precision long_lot
	
	integer*4 id
	double precision poids_pont
	double precision delta_cadre
	!NO
	character*3 double_fond					!YES OR NO
	character*4 grade						!A ou AH36
	double precision largueur_lot			!Somme des largeurs des panneaux
	double precision avg_thick

	integer*4 list_pan(100) !liste des panneaux qui le constituent
	integer*4 nbr_pan		!nbr de pan.
	double precision long_bigpan ! long du plus gd panneau
end type

type(struct_lot), allocatable, save :: lot(:)

open(9444,file='LotStatistique.csv')

! Compte le nombre de lot
temp(:) = 0
nbr_lot = 0
do i=1,neto
	if (panneau(i).lot.ne.0) then
		found = 0
		do j=1,100
			if (panneau(i).lot.eq.temp(j)) found=1
		enddo
		if (found.eq.0) then
			nbr_lot = nbr_lot + 1
			temp(nbr_lot) = panneau(i).lot
		endif
	endif
enddo

! Initialisation de lot
allocate (lot(nbr_lot))
do i=1,nbr_lot
	lot(i).thick = 0.d00
	lot(i).famille = 0
	lot(i).delta_lisse = 0.d00
	lot(i).poids_lot = 0.d00
	lot(i).long_lot = 0.d00
	lot(i).id = 0
	lot(i).poids_pont = 0.d00
	lot(i).delta_cadre = 0.d00
	lot(i).double_fond = ''
	lot(i).grade = ''
	lot(i).largueur_lot = 0.d00
	lot(i).avg_thick = 0.d00

	do j=1,100
		lot(i).list_pan(j)=0
	enddo
	lot(i).nbr_pan = 0
	lot(i).long_bigpan = 0.d00
enddo

! Remplissage des informations de chaque lot

do i=1,nbr_lot
	lot(i).id = temp(i)
	lot(i).long_lot = width

	do j=1,neto
		if (panneau(j).lot.eq.lot(i).id) then
			lot(i).nbr_pan = lot(i).nbr_pan + 1
			lot(i).list_pan(lot(i).nbr_pan) = j

			!On regarde si le panneau est hor ou pas
			hor = 1 ! 1=> horizontal, 0=> vert
			!angle = dabs(panneau(j).angle)
			!if ((angle.lt.1).or.((angle.gt.179).and.(angle.lt.181)) ) then
			if (panneau(j).fami.eq.0) hor=0
			!

			if ((hor.eq.1).and.(lot(i).long_bigpan.lt.dabs(panneau(j).hight))) then
				lot(i).long_bigpan = dabs(panneau(j).hight)
				lot(i).thick = panneau(j).delta
			endif
			if (hor.eq.1) lot(i).famille = panneau(j).fami
			if (hor.eq.1) lot(i).delta_lisse = panneau(j).epsr
			
			!Calcul du poids du panneau
			poidspan = 0.d00
			delta = panneau(j).delta
			hya = panneau(j).hya
			dya = panneau(j).dya
			wya = panneau(j).wya
			tya = panneau(j).tya
			epsa = panneau(j).epsa
			hxr = panneau(j).hxr
			dxr = panneau(j).dxr
			wxr = panneau(j).wxr
			txr = panneau(j).txr
			epsr = panneau(j).epsr
			phil = dabs(panneau(j).phil) !!!
			q = panneau(j).q
			epais = panneau(j).epais
			call poidspanneau(poidspan,j,epsa,epsr,delta,hya,dya,wya,	&
					tya,hxr,dxr,wxr,txr,phil,q,epais)
			!Fin du calcul du poids du panneau

			lot(i).poids_lot = lot(i).poids_lot + poidspan
			if (hor.eq.1) lot(i).poids_pont = lot(i).poids_pont + poidspan
			if (hor.eq.1) lot(i).delta_cadre = panneau(j).epsa
			if (hor.eq.1) then
				if (indMateriau(j).eq.1) then
					lot(i).grade = 'A'
				else
					lot(i).grade = 'AH36'
				endif
			endif

			if (hor.eq.1) lot(i).largueur_lot = lot(i).largueur_lot + dabs(panneau(j).hight)
			if (hor.eq.1) lot(i).avg_thick = lot(i).avg_thick + panneau(j).delta*dabs(panneau(j).hight)
		endif
	enddo
	lot(i).avg_thick = lot(i).avg_thick / lot(i).largueur_lot
	lot(i).poids_pont = lot(i).poids_pont/(9.81*1000) ! pour avoir en tonnes
	lot(i).poids_lot = lot(i).poids_lot/(9.81*1000) ! pour avoir en tonnes
enddo
!

!Ecriture des résultats
write(9444,'(a)') 'THICKNESS;FAMILLE;DELTA LISSE;POIDS DU LOT;LONGUEUR LOT;POIDS LOT/LONGUEUR LOT;POIDS PONT;DELTA CADRE;SPECIAL;DOUBLE FOND;GRADE;LARGEUR LOT;AVERAGE THICKNESS;'
do i=1,nbr_lot
	write(9444,'(f6.4,a,i3,a,f6.3,a,f6.2,a,f9.3,a,f7.2,a,i3,a,f6.2,a,f5.3,5(a),f7.3,a,f8.5,a)') lot(i).thick,';',lot(i).famille,';',lot(i).delta_lisse,';',lot(i).poids_lot,';',lot(i).long_lot,';',lot(i).poids_lot/lot(i).long_lot,';',lot(i).id,';',lot(i).poids_pont,';',lot(i).delta_cadre,'; NO; NO;',lot(i).double_fond,';',lot(i).grade,';',lot(i).largueur_lot,';',lot(i).avg_thick,';'
enddo
!

deallocate (lot)

close (9444)
stop

return
end
