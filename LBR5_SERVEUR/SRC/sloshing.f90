subroutine sloshing(nel)

use param_section, ONLY : panneau,neto,iboat,langue,nfile,nvar,nxit,itera,iter1

implicit double precision (a-h,o-z)

!***********************************************************************
!     Subroutine sloshing
!     *******************
!     Cette subroutine calcule les 3 restrictions de sloshing
!     
!	  cj(xi) < cjmax  et les sensibilités = dcj(n) = d(cj)/dxi.
!
!     Restriction 1 : épaisseur minimale de bordé
!	  Restriction 2 : net shear sectional area
!	  Restriction 3 : net section modulus
!
!	  Voir le lien suivant pour le calcul des restrictions 2 et 3 :
!	  http://www1.veristar.com/Veristar/bvrules/B_4_s3_3_4.htm#ACTR.06.C748758001BDDBFF
!
!	  Rem.: La restriction 3 requiert le calcul de Aa = net sectional area of the 
!	  ----  attached plating. Il est donc nécessaire de connaître la largeur
!           collaborante. Actuellement, elle vaut : entr. A changer éventuellement.
!
!     Création     : 07/04/09 (F.Bair)
!     Modifications:
!            - 27/04/09 (F.Bair)
!			 - 19/05/09 (F.Bair) Modifications des bornes : maintenant
!						elles sont calculées en fonction de p_sloshing
!						et non plus données en entrée.
!			 - 10/06/09 (F.Bair) Correction bug : prise en compte de ENTR
!						au lieu de EPSR
!
!	  Rem. : cette sous-routine est lancée après GEOM où les dimensions txr peuvent
!			 avoir été modifiées !!! Ces modifs peuvent entraîner des erreurs de
!			 dérivées (de l'ordre de 10%) !
!
!***********************************************************************

character*80 vnom
dimension dcj(9)
integer*4 found,found_restri
double precision s,l,ca,cr,Ry,dca_s,dca_l,dt_s,dt_l,dw_s,dw_l,dAsh_s,dAsh_l
double precision s_min,l_min,delta_max,epsa_min,hxr_max,dxr_max,wxr_max,entr_min,entr_max
double precision ca_min,slosh_min,Aa_max,w_max
double precision beta_b,beta_s
double precision lambda_b,lambda_s,temp

nbrxi=nvar(nel,iboat)

! Détermination des dimensions de la maille (s=plus petite dimension, l =plus gde dimension)
if (panneau(nel).epsa.lt.panneau(nel).entr) then
	l = panneau(nel).entr !doivent être donné en mètre
	s = panneau(nel).epsa
else
	l = panneau(nel).epsa
	s = panneau(nel).entr
endif

! Détermination des valeurs extrêmes des variables intervenant (pour vérification)
delta_max = panneau(nel).delta
epsa_min = panneau(nel).epsa
hxr_max = panneau(nel).hxr
dxr_max = panneau(nel).dxr
wxr_max = panneau(nel).wxr
entr_min = panneau(nel).entr
entr_max = panneau(nel).entr

do k=1,nbrxi
	if (nxit(k,nel,iboat).eq.1) then
		delta_max = panneau(nel).dvmax(1)
	elseif (nxit(k,nel,iboat).eq.5) then
		epsa_min = panneau(nel).dvmin(5)
	elseif (nxit(k,nel,iboat).eq.6) then
		hxr_max = panneau(nel).dvmax(6)
	elseif (nxit(k,nel,iboat).eq.7) then
		dxr_max = panneau(nel).dvmax(7)
	elseif (nxit(k,nel,iboat).eq.8) then
		wxr_max = panneau(nel).dvmax(8)
	elseif (nxit(k,nel,iboat).eq.9) then
		entr_min = panneau(nel).dvmin(9)
		entr_max = panneau(nel).dvmax(9)
	endif
enddo

if (epsa_min.lt.entr_min) then
	l_min = entr_min !doivent être donné en mètre
	s_min = epsa_min
else
	l_min = epsa_min
	s_min = entr_min
endif

Ry = 235.d00 ! TODO CHECK !!! Tjs ça ? Ou lire valeur ?
!

!
if (nel.eq.1) then
	found_restri = 0
    do i=1,neto
		if (panneau(i).nsloshm.gt.0) found_restri=1
	enddo
	if (found_restri.eq.1.and.itera.eq.0) open(9100,file='Sloshing.dat') ! TODO Normalement, mettre iboat pour le multi !!!
endif
!

if (nel.eq.1) then
	if (found_restri.eq.1) write(9100,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
	if (itera.eq.0) then
		if (found_restri.eq.1) write(9100,*) 'ITERATION NUMERO : ',itera
	else
		if (found_restri.eq.1) write(9100,*) 'ITERATION NUMERO : ',(itera-1)
	endif
	if (found_restri.eq.1) write(9100,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
endif

if (panneau(nel).nsloshm.gt.0) then
	if (found_restri.eq.1) write(9100,*) '****************************************************'
	if (found_restri.eq.1) write(9100,*) 'PANNEAU NUMERO : ',nel
endif

do i=1,panneau(nel).nsloshm
	
	dcj(:) = 0.d00

	if (panneau(nel).nslosh(i).eq.1) then ! Restriction sur l'épaisseur minimale du panneau
		vnom = 'dslosh-d<0       '
		! Calcul de la valeur de la borne! (panneau(nel).slosh(i))
		ca = 1.21*dsqrt(1+0.33*(s/l)**2)-0.69*s/l ! <1 ??????????????
		if (ca.gt.1) then
			ca = 1.d00
		endif
		cr = 1	! Radius of curvature

		! Test avec valeurs extrêmes autorisées des variables de conception
		ca_min = 1.21*dsqrt(1+0.33*(s_min/l_min)**2)-0.69*s_min/l_min !
		if (ca_min.gt.1) then
			ca_min = 1.d00
		endif
		slosh_min = 14.9*ca_min*cr*s_min*dsqrt(1.1*1.02*(1.0*25+1.05*panneau(nel).press_slosh)/Ry)
		if (slosh_min.gt.delta_max*1000.d00) then
			if (langue.eq.1) then
				write(*,*) 'Vu les bornes des variables, il est impossible de satisfaire'
				write(*,*) 'la restriction de sloshing',i,' du panneau',nel
			elseif (langue.eq.2) then

			endif
			read(*,*)
			stop
		endif
		!

		panneau(nel).slosh(i) = 14.9*ca*cr*s*dsqrt(1.1*1.02*(1.0*25+1.05*panneau(nel).press_slosh)/Ry)
		!
		! Calcul des dérivées de la borne
		dca_s = 1.21*0.5/dsqrt(1+0.33*(s/l)**2)*0.33*2*s/l**2 - 0.69/l !dér de ca p.r. à s
		dca_l = 1.21*0.5/dsqrt(1+0.33*(s/l)**2)*0.33*2*(-1)*(s/l)**2 + 0.69*s/l**2 ! dér. de ca p.r. à l
		if (ca.ge.1) then
			dca_s = 0.d00
			dca_l = 0.d00
		endif
		dt_s = panneau(nel).slosh(i)/(s*ca)*(s*dca_s+ca) !dér de panneau(nel).slosh(i) p.r. à s
		dt_l = panneau(nel).slosh(i)/ca*dca_l !dér de panneau(nel).slosh(i) p.r. à l
		!

	    write(9100,*) 'Restriction sur l''epaisseur minimale du panneau'
		write(9100,*) panneau(nel).slosh(i),'< ?',panneau(nel).delta*1000.d00

		cj = panneau(nel).slosh(i) - panneau(nel).delta*1000.d00
		cjm = 0.d00
		do ik=1,nbrxi
			j=nxit(ik,nel,iboat)
			if (j.eq.1)  then
				dcj(ik) = -1000.d00
			elseif (j.eq.5) then
				if (panneau(nel).epsa.lt.panneau(nel).entr) then
					dcj(ik) = dt_s
				else
					dcj(ik) = dt_l
				endif
			elseif (j.eq.9) then
				!prise en compte de  d(entr)/d(epsr) = (entr/epsr)**2
				temp=(panneau(nel).entr/panneau(nel).epsr)**2
				if (panneau(nel).epsa.lt.panneau(nel).entr) then
					dcj(ik) = dt_l * temp
				else
					dcj(ik) = dt_s * temp
				endif
			else
				dcj(ik) = 0.d00
			endif
			!l=l+1
		enddo

		! Test si les bornes des variables sont compatibles et si variable delta sélectionnée
        !if (dvmax(1,nel).lt.panneau(nel).slosh(i)) then
		!	if (langue.eq.1) write(*,*) 'Erreur dans la restriction de sloshing ! Borne max de l''epaisseur trop petite pour pouvoir satisfaire la contrainte ! Panneau :',nel
		!	if (langue.eq.2) write(*,*) 'Error sloshing contraint! Max value of thickness too low to satisfy the constraint! Panel:',nel
		!	read(*,*)
		!	stop
		!endif
		found = 0
		do k=1,nbrxi
			if (nxit(k,nel,iboat).eq.1.or.nxit(k,nel,iboat).eq.5.or.nxit(k,nel,iboat).eq.9) found = 1
		enddo
		if (found.eq.0) then
			if (langue.eq.1) write(*,*) 'Erreur sloshing : variable de conception adequate non selectionnee ! Panneau :',nel
			if (langue.eq.2) write(*,*) 'Error sloshing: good design variable not selected! Panel:',nel
			read(*,*)
			stop
		endif

		! Sauvegarde des données
		call save_sloshing(cj,cjm,dcj,vnom,nbrxi)

	elseif (panneau(nel).nslosh(i).eq.2) then ! Restriction sur l'inertie minimale des raidisseurs
		! Calcul de la valeur de la borne! (panneau(nel).slosh(i))
		lambda_s = 1.d00
		beta_s = 1.d00 !if no bracket
		temp = 10.*1.1*1.02*lambda_s*beta_s*(1.0*25+1.05*panneau(nel).press_slosh)/Ry
		
		! Test avec valeurs extrêmes autorisées des variables de conception
		slosh_min = temp*(1-s_min/(2.*l_min))*s_min*l_min
		if (slosh_min.gt.hxr_max*dxr_max*100**2) then
			if (langue.eq.1) then
				write(*,*) 'Vu les bornes des variables, il est impossible de satisfaire'
				write(*,*) 'la restriction de sloshing',i,' du panneau',nel
			elseif (langue.eq.2) then

			endif
			read(*,*)
			stop
		endif
		!
		
		panneau(nel).slosh(i) = temp*(1-s/(2.*l))*s*l !=Ash
		!
		! Calcul des dérivées de la borne
		dAsh_s = temp*(-s/2.d00 + (1-s/(2.*l))*l)
		dAsh_l = temp*(s**2/(2.*l) + (1-s/(2.*l))*s)

		! Ash = NET SHEAR SECTIONAL AREA (in cm²)
		! VOIR http://www1.veristar.com/Veristar/bvrules/B_4_s3_3_4.htm#ACTR.06.C748758001BDDBFF
		Ash = panneau(nel).hxr*panneau(nel).dxr*100**2 !*100**2 pour avoir en cm2
		vnom ='i_slosh-inertie<0'

		write(9100,*) 'Restriction sur l''inertie minimale des raidisseurs'
		write(9100,*) panneau(nel).slosh(i),'< ?',Ash

		cj = panneau(nel).slosh(i) - Ash
		cjm = 0.d00
		do ik=1,nbrxi
			j=nxit(ik,nel,iboat)
			if (j.eq.5)  then
				if (panneau(nel).epsa.lt.panneau(nel).entr) then
					dcj(ik) = dAsh_s
				else
					dcj(ik) = dAsh_l
				endif
			elseif (j.eq.6)  then
				dcj(ik) = -panneau(nel).dxr*100**2 !*100**2 pour avoir en cm2
			elseif (j.eq.7)  then
				dcj(ik) = -panneau(nel).hxr*100**2 !*100**2 pour avoir en cm2
			elseif (j.eq.9)  then
				!prise en compte de  d(entr)/d(epsr) = (entr/epsr)**2
				temp=(panneau(nel).entr/panneau(nel).epsr)**2
				if (panneau(nel).epsa.lt.panneau(nel).entr) then
					dcj(ik) = dAsh_l * temp
				else
					dcj(ik) = dAsh_s * temp
				endif
			else
				dcj(ik) = 0.d00
			endif
			!l=l+1
		enddo

		! Test si au moins une var de conception sélectionnée
	    found = 0
		do k=1,nbrxi
			if (nxit(k,nel,iboat).eq.6.or.nxit(k,nel,iboat).eq.5.or.nxit(k,nel,iboat).eq.7.or.nxit(k,nel,iboat).eq.9) found = 1
		enddo
		if (found.eq.0) then
			if (langue.eq.1) write(*,*) 'Erreur sloshing : variable de conception adequate non selectionnee ! Panneau :',nel
			if (langue.eq.2) write(*,*) 'Error sloshing: good design variable not selected! Panel:',nel
			read(*,*)
			stop
		endif

		! Sauvegarde des données
		call save_sloshing(cj,cjm,dcj,vnom,nbrxi)

	elseif (panneau(nel).nslosh(i).eq.3) then ! Restrictions sur la section minimale des raidisseurs
		! Calcul de la valeur de la borne! (panneau(nel).slosh(i))
		!temp2 = 0.2*(1.0*(psd-psu)+1.05*)
		lambda_b = 1.d00
		beta_b = 1.d00 ! if no bracket
		temp = 1.1*1.02*lambda_b*beta_b*(1.0*25+1.05*panneau(nel).press_slosh)/(12*Ry)
		
		! Test avec valeurs extrêmes autorisées des variables de conception
		slosh_min = temp*(1.0-s_min/(2.*l_min))*s_min*l_min**2*1000.
		Aa_max = delta_max*entr_max ! Net sectional area (in mm²), of the attached plating	! TODO A VERIFIER
		Aa_max = Aa_max*1000**2
		w_max = hxr_max*panneau(nel).txr*wxr_max/1000.d00	&
			+ dxr_max*(hxr_max**2)/6000.d00*	&
			(1 + (Aa_max-panneau(nel).txr*wxr_max)/(Aa_max+(dxr_max*hxr_max)/2.d00))
		w_max = w_max*1000000000.d00
		if (slosh_min.gt.w_max) then
			if (langue.eq.1) then
				write(*,*) 'Vu les bornes des variables, il est impossible de satisfaire'
				write(*,*) 'la restriction de sloshing',i,' du panneau',nel
			elseif (langue.eq.2) then

			endif
			read(*,*)
			stop
		endif
		!
		
		panneau(nel).slosh(i) = temp*(1.0-s/(2.*l))*s*l**2*1000. ! = w
		!
		! Calcul des dérivées de la borne
		dw_s = temp*(-s/(2.*l) + (1-s/(2.*l)))*l**2*1000.
		dw_l = temp*(s/(2.*l) + (1-s/(2.*l))*2.)*s*l*1000.
		!
		
		! w = NET SECTION MODULUS (in cm³)
		! VOIR http://www1.veristar.com/Veristar/bvrules/B_4_s3_3_4.htm#ACTR.06.C748758001BDDBFF
		Aa = panneau(nel).delta*panneau(nel).entr ! Net sectional area (in mm²), of the attached plating	! TODO A VERIFIER
		Aa = Aa*1000**2 !pour avoir en mm², car tout est défini en m
		! (ici, on le laisse en m², car après on modifie w pour passer de m² à cm²
		w = panneau(nel).hxr*panneau(nel).txr*panneau(nel).wxr/1000.d00	&
			+ panneau(nel).dxr*(panneau(nel).hxr**2)/6000.d00*	&
			(1 + (Aa-panneau(nel).txr*panneau(nel).wxr)/(Aa+(panneau(nel).dxr*panneau(nel).hxr)/2.d00))
		w = w*1000000000.d00 ! pour passer en cm³
		vnom ='s_slosh-section<0'

		write(9100,*) 'Restrictions sur la section minimale des raidisseurs'
		write(9100,*) panneau(nel).slosh(i),'< ?',w

		cj = panneau(nel).slosh(i) - w
		cjm = 0.d00
		do ik=1,nbrxi
			j=nxit(ik,nel,iboat)
			if (j.eq.1) then ! delta
				dAa = panneau(nel).entr*1000**2
				dcj(ik) = -1.d00*( panneau(nel).dxr*(panneau(nel).hxr**2)/6000.d00*	&
						  + dAa*(panneau(nel).dxr*panneau(nel).hxr/2.d00 + panneau(nel).txr*panneau(nel).wxr)	&
						  / (Aa + panneau(nel).dxr*panneau(nel).hxr/2.d00)**2)
				dcj(ik) = dcj(ik)*1000000000.d00 ! pour passer en cm³
			elseif (j.eq.5)  then ! hxr
				if (panneau(nel).epsa.lt.panneau(nel).entr) then
					dcj(ik) = dw_s
				else
					dcj(ik) = dw_l
				endif
			elseif (j.eq.6)  then ! hxr
				dcj(ik) = -1.d00*( panneau(nel).txr*panneau(nel).wxr/1000.d00	&
						  + 2*panneau(nel).dxr*panneau(nel).hxr/6000.d00*(1.d00 + (Aa-panneau(nel).txr*panneau(nel).wxr)/(Aa+panneau(nel).dxr*panneau(nel).hxr/2.d00))	&
						  + panneau(nel).dxr*panneau(nel).hxr**2/6000.d00*(-1.d00*(Aa-panneau(nel).txr*panneau(nel).wxr)/((Aa+panneau(nel).dxr*panneau(nel).hxr/2.d00)**2)*panneau(nel).dxr/2.d00))
				dcj(ik) = dcj(ik)*1000000000.d00 ! pour passer en cm³
			elseif (j.eq.7) then ! dxr
				dcj(ik) = -1.d00*( panneau(nel).hxr**2/6000.d00*(1.d00 + (Aa-panneau(nel).txr*panneau(nel).wxr)/(Aa+panneau(nel).dxr*panneau(nel).hxr/2.d00))	&
						  + panneau(nel).dxr*panneau(nel).hxr**2/6000.d00*(-1.d00*(Aa-panneau(nel).txr*panneau(nel).wxr)/((Aa+panneau(nel).dxr*panneau(nel).hxr/2.d00)**2)*panneau(nel).hxr/2.d00))
				dcj(ik) = dcj(ik)*1000000000.d00 ! pour passer en cm³
			elseif (j.eq.8) then ! wxr
				dcj(ik) = -1.d00*( panneau(nel).hxr*panneau(nel).txr/1000.d00	&
						  + panneau(nel).dxr*panneau(nel).hxr**2/6000.d00*(-panneau(nel).txr/(Aa+(panneau(nel).dxr*panneau(nel).hxr)/2.d00)))
				dcj(ik) = dcj(ik)*1000000000.d00 ! pour passer en cm³
			elseif (j.eq.9) then ! entr
				!prise en compte de  d(entr)/d(epsr) = (entr/epsr)**2
				temp=(panneau(nel).entr/panneau(nel).epsr)**2

				dAa = panneau(nel).delta*1000**2
				dcj(ik) = -1.d00*( panneau(nel).dxr*(panneau(nel).hxr**2)/6000.d00*	&
						  + dAa*(panneau(nel).dxr*panneau(nel).hxr/2.d00 + panneau(nel).txr*panneau(nel).wxr)	&
						  / (Aa + panneau(nel).dxr*panneau(nel).hxr/2.d00)**2)
				dcj(ik) = dcj(ik)*1000000000.d00 ! pour passer en cm³
				if (panneau(nel).epsa.lt.panneau(nel).entr) then
					dcj(ik) = (dw_l + dcj(ik))*temp
				else
					dcj(ik) = (dw_s + dcj(ik))*temp
				endif
			else
				dcj(ik) = 0.d00
			endif
			!dcj(ik) = dcj(ik)*1000.d00 ! pour passer en cm³
			!l=l+1
		enddo

		! Vérification de validation d'application des formules
		if ((Aa.lt.(panneau(nel).txr*panneau(nel).wxr)).or.		&
			(panneau(nel).hxr/panneau(nel).delta.lt.10).or.		&
			(panneau(nel).hxr/panneau(nel).txr.lt.10)) then
			if (langue.eq.1) then
			    write(*,*) 'Attention panneau ',nel
				write(*,*) 'Attention ! Conditions d''applications des formules de calcul de sloshing (NET SECTION MODULUS) non verifiees.'
				write(*,*) 'Les restrictions calculees ne sont donc sans doute pas exactes.'
			elseif (langue.eq.2) then
				write(*,*) 'Attention pannel ',nel
				write(*,*) 'Attention! Formulae used in sloshing evaluation (NET SECTION MODULUS) are not applicable.'
				write(*,*) 'Constraints calculated could be wrong!'
			endif
		endif
		!

		! Test si au moins une var de conception sélectionnée
	    found = 0
		do k=1,nbrxi
			if (nxit(k,nel,iboat).eq.1.or.nxit(k,nel,iboat).eq.5.or.nxit(k,nel,iboat).eq.6.or.nxit(k,nel,iboat).eq.7.or.nxit(k,nel,iboat).eq.9) found = 1
		enddo
		if (found.eq.0) then
			if (langue.eq.1) write(*,*) 'Erreur sloshing : variable de conception adequate non selectionnee! Panneau :',nel
			if (langue.eq.2) write(*,*) 'Error sloshing: good design variable not selected! Panel:',nel
			read(*,*)
			stop
		endif
		!

		! Sauvegarde des données
		call save_sloshing(cj,cjm,dcj,vnom,nbrxi)

	else
		if(langue.eq.1) write(*,*) 'Mauvais parametre dans la definition des contraintes de sloshing ! Panneau :',nel
		if(langue.eq.2) write(*,*) 'Bad parameter when defining sloshing constraint! Panel:',nel
	endif

enddo

if (nel.eq.neto.and.iter1.eq.itera.and.iboat.eq.nfile) then
    found_restri = 0
    do i=1,neto
		if (panneau(i).nsloshm.gt.0) found_restri=1
	enddo
	if (found_restri.eq.1) close(9100)
endif


return
end

!**********************************************
subroutine save_sloshing(cj,cjm,dcj,vnom,nbrxi)
use param_section, ONLY : ncont,kcontr,iboat,cijopt,cjopt,cjmopt,vnom2

implicit double precision (a-h,o-z)
character*80 vnom
dimension dcj(9)

kcontr = kcontr+1
do i=1,nbrxi
	cijopt(i+ncont,kcontr,iboat)=dcj(i)
enddo
cjopt(kcontr,iboat) = cj
cjmopt(kcontr,iboat) = cjm
vnom2(kcontr,iboat) = vnom
!ncont = ncont + nbrxi

!write(iu_26(iboat),*) ntot
!write(iu_26(iboat),*) (dcj(i),i=1,nbrxi)
!write(iu_26(iboat),*) cj
!write(iu_26(iboat),*) cjm
!write(iu_26(iboat),'(a)') vnom

return
end
!**********************************************
