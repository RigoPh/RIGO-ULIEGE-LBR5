subroutine allocate_panel_data()

use param_section

implicit double precision (a-h,o-z)

! Allocation des panneaux et initialisation de ses composants dynamiques !!!!:-)
allocate (panneau(nmax))

allocate (title_cha_full(nsolmax,nmax)) !tous les cas de charge

allocate (title_loadcase(nsolmax,nmax))

allocate (mode(nmax))

allocate (xmode(nmax))
allocate (denom(nmax))

!allocate (kse   (nmax))
allocate (ipa   (nmax))
allocate (ivaria(nmax))

allocate (xi(nsol,nmax))
allocate (xf(nsol,nmax))
allocate (xxi(nsol,nmax))
allocate (xxf(nsol,nmax))

!allocate (part(nmax))

allocate (cha(100,3,nsolm,nmax))
allocate (cha_full(100,3,nsolm,nmax))
allocate (dcha(100,nsolm,9,nmax))
allocate (npt(nsol,nmax))
allocate (npt_full(nsolmax,nmax))


iy_max=3
ix_max=5


!!! TODO améliorer l'allocation pour ne pas gaspiller l'espace mémoire
! Stiffener
allocate (sx_raid_sem(nsol_fatigue,nmax,ix_max,iy_max))		!fat_new
allocate (sx_loc_raid_sem(nsol_fatigue,nmax)) ! Contrainte locale uniquement ou combinaison ?

! Plate
allocate (sx_plaque_top(nsol_fatigue,nmax,ix_max,iy_max))		!fat_new
allocate (sy_plaque_top(nsol_fatigue,nmax,ix_max,iy_max))		!fat_new
allocate (sxy_plaque_top(nsol_fatigue,nmax,ix_max,iy_max))		!fat_new
allocate (sx_plaque_bott(nsol_fatigue,nmax,ix_max,iy_max))		!fat_new
allocate (sy_plaque_bott(nsol_fatigue,nmax,ix_max,iy_max))		!fat_new
allocate (sxy_plaque_bott(nsol_fatigue,nmax,ix_max,iy_max))		!fat_new

! Frame
	!sx_plaque_top
allocate (sy_cadre_jab(nsol_fatigue,nmax,ix_max,iy_max))		!fat_new
allocate (sxy_cadre_jab(nsol_fatigue,nmax,ix_max,iy_max))		!fat_new

allocate (sy_cadre_sem(nsol_fatigue,nmax,ix_max,iy_max))		!fat_new



!Stiffeners

allocate (kc_Stif(nsol_fatigue,nmax,ix_max,iy_max))
allocate (NOM_STRESS_Stif(nsol_fatigue,nmax,ix_max,iy_max))
allocate (HS_STRESS_Stif(nsol_fatigue,nmax,ix_max,iy_max))
allocate (NS_STRESS_Stif(nsol_fatigue,nmax,ix_max,iy_max))
allocate (Damage_Stif(nsol_fatigue,nmax,ix_max,iy_max))
allocate (Damage_Stif_full(nmax,ix_max,iy_max))
allocate (Damage_Stif_ball(nmax,ix_max,iy_max))
allocate (Damage_Stif_total(nmax,ix_max,iy_max))


! Plate

allocate (kc_Plate(nsol_fatigue,nmax,ix_max,iy_max))
allocate (NOM_STRESS_Plate(nsol_fatigue,nmax,ix_max,iy_max))
allocate (NOM_STRESS_Plate_top(nsol_fatigue,nmax,ix_max,iy_max))
allocate (NOM_STRESS_Plate_bott(nsol_fatigue,nmax,ix_max,iy_max))
allocate (HS_STRESS_Plate(nsol_fatigue,nmax,ix_max,iy_max))
allocate (NS_STRESS_Plate(nsol_fatigue,nmax,ix_max,iy_max))
allocate (Damage_Plate(nsol_fatigue,nmax,ix_max,iy_max))
allocate (Damage_Plate_full(nmax,ix_max,iy_max))
allocate (Damage_Plate_ball(nmax,ix_max,iy_max))
allocate (Damage_Plate_total(nmax,ix_max,iy_max))


!Intersection Plate/frame--------------------------------------------------------------

allocate (kc_Plate_Frame(nsol_fatigue,nmax,ix_max,iy_max))
allocate (NOM_STRESS_Plate_Frame(nsol_fatigue,nmax,ix_max,iy_max))
allocate (NOM_STRESS_Plate_Frame_top(nsol_fatigue,nmax,ix_max,iy_max))
allocate (NOM_STRESS_Plate_Frame_bott(nsol_fatigue,nmax,ix_max,iy_max))
allocate (HS_STRESS_Plate_Frame(nsol_fatigue,nmax,ix_max,iy_max))
allocate (NS_STRESS_Plate_Frame(nsol_fatigue,nmax,ix_max,iy_max))
allocate (Damage_Plate_Frame(nsol_fatigue,nmax,ix_max,iy_max))
allocate (Damage_Plate_Frame_full(nmax,ix_max,iy_max))
allocate (Damage_Plate_Frame_ball(nmax,ix_max,iy_max))
allocate (Damage_Plate_Frame_total(nmax,ix_max,iy_max))

! Frame

allocate (kc_Frame(nsol_fatigue,nmax,ix_max,iy_max))
allocate (NOM_STRESS_Frame(nsol_fatigue,nmax,ix_max,iy_max))
allocate (HS_STRESS_Frame(nsol_fatigue,nmax,ix_max,iy_max))
allocate (NS_STRESS_Frame(nsol_fatigue,nmax,ix_max,iy_max))
allocate (Damage_Frame(nsol_fatigue,nmax,ix_max,iy_max))
allocate (Damage_Frame_full(nmax,ix_max,iy_max))
allocate (Damage_Frame_ball(nmax,ix_max,iy_max))
allocate (Damage_Frame_total(nmax,ix_max,iy_max))



!Girders--------------------------------------------------------------


!	allocate (kc_Girder(nsol_fatigue,nmax,ix_max,iy_max))
!	allocate (NOM_STRESS_Girder(nsol_fatigue,nmax,ix_max,iy_max))
!	allocate (HS_STRESS_Girder(nsol_fatigue,nmax,ix_max,iy_max))
!	allocate (NS_STRESS_Girder(nsol_fatigue,nmax,ix_max,iy_max))
!	allocate (Damage_Girder(nsol_fatigue,nmax,ix_max,iy_max))
!	allocate (Damage_Girder_full(nmax,ix_max,iy_max))
!	allocate (Damage_Girder_ball(nmax,ix_max,iy_max))
!	allocate (Damage_Girder_total(nmax,ix_max,iy_max))


!Pillars--------------------------------------------------------------

!	allocate (kc_Pillar(nsol_fatigue,nmax,ix_max,iy_max))
!	allocate (NOM_STRESS_Pillar(nsol_fatigue,nmax,ix_max,iy_max))
!	allocate (NOM_STRESS_Pillar1(nsol_fatigue,nmax,ix_max,iy_max))
!	allocate (NOM_STRESS_Pillar2(nsol_fatigue,nmax,ix_max,iy_max))
!	allocate (HS_STRESS_Pillar(nsol_fatigue,nmax,ix_max,iy_max))
!	allocate (NS_STRESS_Pillar(nsol_fatigue,nmax,ix_max,iy_max))
!	allocate (Damage_Pillar(nsol_fatigue,nmax,ix_max,iy_max))
!	allocate (Damage_Pillar_full(nmax,ix_max,iy_max))
!	allocate (Damage_Pillar_ball(nmax,ix_max,iy_max))
!	allocate (Damage_Pillar_total(nmax,ix_max,iy_max))













!allocate (m1   (nsolm,nmax))

panneau(:).types = ''
panneau(:).section = ''

panneau(:).delta = 0.d00
panneau(:).hya = 0.d00
panneau(:).dya = 0.d00
panneau(:).wya = 0.d00
panneau(:).tya = 0.d00
panneau(:).epsa = 0.d00
panneau(:).hxr = 0.d00
panneau(:).dxr = 0.d00
panneau(:).wxr = 0.d00
panneau(:).txr = 0.d00
panneau(:).epsr = 0.d00
panneau(:).entr = 0.d00
panneau(:).kse = 0.d00

panneau(:).part = 0.d00

panneau(:).wya_fatigue = 0.d00
panneau(:).tya_fatigue = 0.d00
panneau(:).wxr_fatigue = 0.d00
panneau(:).txr_fatigue = 0.d00

panneau(:).itype_longit = 0
panneau(:).itype_transv = 0
panneau(:).itype_transv_second = 0
panneau(:).itype_longit_second = 0
panneau(:).name_type_transv = ''
panneau(:).name_type_transv_second = ''
panneau(:).name_type_longit = ''
panneau(:).name_type_longit_second = ''

panneau(:).hya2 = 0.d00
panneau(:).dya2 = 0.d00
panneau(:).wya2 = 0.d00
panneau(:).tya2 = 0.d00
panneau(:).epsa2 = 0.d00
panneau(:).hxr2 = 0.d00
panneau(:).dxr2 = 0.d00
panneau(:).wxr2 = 0.d00
panneau(:).txr2 = 0.d00
panneau(:).epsr2 = 0.d00
panneau(:).entr2 = 0.d00
panneau(:).ksr2 = 0	
panneau(:).ksa2 = 0
do i=1,9
	panneau(:).dvmin(i) = 0.d00
	panneau(:).dvmax(i) = 0.d00
enddo
panneau(:).heff = 0
do i=1,3
	panneau(:).corro(i) = 0.d00
enddo
do i=1,10
	panneau(:).noh9(i) = 0
enddo
do i=1,nmax
! Par défaut, à la création de la structure panneau, il crée une adresse
! au pointeur (un peu comme il peut attribuer à un champ entier la valeur -14534654
! => ON DOIT d'abord faire NULLIFY
	NULLIFY (panneau(i)%xi_full)
	NULLIFY (panneau(i)%xf_full)
	allocate (panneau(i)%xi_full(nsolmax))
	allocate (panneau(i)%xf_full(nsolmax))
	do j=1,nsolmax
		panneau(i).xi_full(j) = 0.d00
		panneau(i).xf_full(j) = 0.d00
	enddo
enddo
panneau(:).m2 = 0
panneau(:).im1 = 0
panneau(:).m1cont = 0
do i=1,nmax
	NULLIFY (panneau(i)%m1)
	allocate (panneau(i)%m1(50))		!!! A changer
	do j=1,50							!!! A changer
		panneau(i).m1(j) = 0
	enddo
enddo
panneau(:).ibm2 = 0
panneau(:).isema = 0
panneau(:).isemr = 0
panneau(:).inda = 0
panneau(:).indr = 0
do i=1,40 !TODO faudra mettre dynamique
	panneau(:).lm2(i) = 0
enddo

panneau(:).delt = 0.d00
panneau(:).delt2 = 0.d00
panneau(:).xneu = 0.d00
panneau(:).yneu = 0.d00
panneau(:).d1 = 0.d00
panneau(:).d2 = 0.d00
panneau(:).omega = 0.d00
panneau(:).omega2 = 0.d00
do i=1,74
	panneau(:).const(i) = 0.d00
enddo
do i=1,6
	do j=1,9
		panneau(:).const2(i,j) = 0.d00
	enddo
enddo
do i=1,2
	do j=1,10
		panneau(:).const3(i,j) = 0.d00
	enddo
enddo
do i=1,nmax
	NULLIFY (panneau(i)%chamax)
	NULLIFY (panneau(i)%dchamax)
enddo
panneau(:).q = 0.d00
panneau(:).phil = 0.d00
panneau(:).epais = 0.d00
panneau(:).hight = 0.d00
panneau(:).mode = ''
panneau(:).teta = 0.d00
panneau(:).tetas = 0.d00
panneau(:).mt = 0
panneau(:).mmt = 0
panneau(:).ksr = 0
panneau(:).ksa = 0
panneau(:).kst = 0

do i=1,10
   panneau(:).hxtr(i)          = 0.d00
   panneau(:).dxtr(i)          = 0.d00
   panneau(:).wxtr(i)          = 0.d00
   panneau(:).txtr(i)          = 0.d00
   panneau(:).abtr(i)          = 0.d00
   panneau(:).abtr2(i)         = 0.d00
   panneau(:).ipts3(i)         = 0
enddo
panneau(:).sh = 0.d00
panneau(:).ang = 0.d00
panneau(:).angle = 0.d00
panneau(:).code = 0
panneau(:).fami = 0
panneau(:).lot = 0
panneau(:).rend = 0.d00

panneau(:).ploc = 0.d00

panneau(:).icha = 0
panneau(:).elmt_vib = 0
panneau(:).long_vib = 0.d00
panneau(:).cl_av_vib = 0
panneau(:).cl_ar_vib = 0

do i=1,nmax
	NULLIFY (panneau(i)%lcont)
!	NULLIFY (panneau(i)%ipts2)
	NULLIFY (panneau(i)%cjmax)
	NULLIFY (panneau(i)%inv)
	!allocate (panneau(i)%lcont(2,(m1tmax*nsolmax)))
	!allocate (panneau(i)%ipts2(iptmax))
	!allocate (panneau(i)%cjmax(m1tmax*nsolmax))
	!allocate (panneau(i)%inv(m1tmax*nsolmax))
	!do j=1,(m1tmax*nsolmax)
	!	panneau(i).lcont(1,j) = 0
	!	panneau(i).lcont(2,j) = 0
	!	panneau(i).cjmax(j) = 0.d00
	!	panneau(i).inv(j) = 0
	!enddo
	!do j=1,iptmax
	!	panneau(i).ipts2(j) = 0
	!enddo
enddo
panneau(:).ipts = 0
do i=1,4 ! TODO normalement IPTMAX (mais marche pas qd ipts2=pointeur ds la structure)
	panneau(:).ipts2(i) = 0
enddo

do i=1,nmax
	NULLIFY (panneau(i)%lcont4_init)
	NULLIFY (panneau(i)%lcont4)
	NULLIFY (panneau(i)%inv3_init)
	NULLIFY (panneau(i)%inv3)
	NULLIFY (panneau(i)%cjmax9_init)
	NULLIFY (panneau(i)%cjmax9)
enddo

panneau(:).nsloshm = 0
do i=1,3
	panneau(:).nslosh(i) = 0
	panneau(:).slosh(i) = 0.d00
enddo
panneau(:).press_slosh = 0.d00

panneau(:).aire = 0.d00
panneau(:).aiy = 0.d00
panneau(:).aix = 0.d00
panneau(:).sredy = 0.d00
panneau(:).sredx = 0.d00
panneau(:).tork = 0.d00

panneau(:).plat = 0.d00
panneau(:).fl = 0.d00
panneau(:).sig1 = 0.d00
panneau(:).sig2 = 0.d00
panneau(:).sig3 = 0.d00
panneau(:).sig4 = 0.d00
panneau(:).tau = 0.d00
panneau(:).wpl1 = 0.d00
panneau(:).wpl2 = 0.d00
panneau(:).vmaxsig = 0.d00
panneau(:).vmaxsigc = 0.d00
panneau(:).sigmag = 0.d00

panneau(:).kcor = 0.d00			!fatigue Corrosion factor					!fat_new

do i=1,nmax
	NULLIFY (panneau(i)%sigplaque)
	NULLIFY (panneau(i)%dsigplaque)
	NULLIFY (panneau(i)%sigxplaque)
	NULLIFY (panneau(i)%sigyplaque)
	NULLIFY (panneau(i)%phiplaque)
	NULLIFY (panneau(i)%sigvmtplaque)
	NULLIFY (panneau(i)%sigvmcplaque)
	NULLIFY (panneau(i)%indplaque)
	NULLIFY (panneau(i)%sigmx)
	NULLIFY (panneau(i)%sigmy)
	allocate (panneau(i)%sigplaque(iptmax))
	allocate (panneau(i)%dsigplaque(9*iptmax))
	allocate (panneau(i)%sigxplaque(iptmax))
	allocate (panneau(i)%sigyplaque(iptmax))
	allocate (panneau(i)%phiplaque(iptmax))
	allocate (panneau(i)%sigvmtplaque(iptmax))
	allocate (panneau(i)%sigvmcplaque(iptmax))
	allocate (panneau(i)%indplaque(iptmax))
	allocate (panneau(i)%sigmx(iptmax))
	allocate (panneau(i)%sigmy(iptmax))
	do j=1,iptmax
		panneau(i).sigplaque(j) = 0.d00
		panneau(i).dsigplaque(j) = 0.d00
		panneau(i).sigxplaque(j) = 0.d00
		panneau(i).sigyplaque(j) = 0.d00
		panneau(i).phiplaque(j) = 0.d00
		panneau(i).sigvmtplaque(j) = 0.d00
		panneau(i).sigvmcplaque(j) = 0.d00
		panneau(i).indplaque(j) = 0.d00
		panneau(i).sigmx(j) = 0.d00
		panneau(i).sigmy(j) = 0.d00
	enddo


	NULLIFY(panneau(i)%ix_Stif)
	NULLIFY(panneau(i)%iy_Stif)
	NULLIFY(panneau(i)%ES_Stif)
	NULLIFY(panneau(i)%KT_Stif)
	NULLIFY(panneau(i)%Kw_Stif)
	NULLIFY(panneau(i)%lambda_stif)
	NULLIFY(panneau(i)%teta_stif)
	NULLIFY(panneau(i)%BetaIf_stif)
	NULLIFY(panneau(i)%m_stif)
	NULLIFY(panneau(i)%c_stif)
	NULLIFY(panneau(i)%weib_fact_stif)

	nbr_fat_max=9		! Maximum Number of considered points for fatigue ASSESSEMENT
	ix_max=5		! Maximum Number of considered points for in X direction fatigue ASSESSEMENT	!fat_new
	iy_max=3		! Maximum Number of considered points for in Y direction fatigue ASSESSEMENT	!fat_new

!    nbr_fat_stif = 15           !!! TODO à changer
 !   nbr_fat_plate = 15          !!! TODO à changer
  !  nbr_fat_plate_frame = 15    !!! TODO à changer
   ! nbr_fat_frame = 15          !!! TODO à changer
!fat_new :nbr_fat_stif, nbr_fat_plate, nbr_fat_plate_frame, nbr_fat_frame ont été remplacés par 
!nbr_fat_max danas les allocations qui suivant

	allocate (panneau(i)%ix_Stif(nbr_fat_max))
	allocate (panneau(i)%iy_Stif(nbr_fat_max))
	allocate (panneau(i)%ES_Stif(nbr_fat_max))
	allocate (panneau(i)%KT_Stif(nbr_fat_max))
	allocate (panneau(i)%Kw_Stif(nbr_fat_max))
	allocate (panneau(i)%lambda_stif(nbr_fat_max))
	allocate (panneau(i)%teta_stif(nbr_fat_max))
	allocate (panneau(i)%BetaIf_stif(nbr_fat_max))
	allocate (panneau(i)%m_stif(nbr_fat_max))
	allocate (panneau(i)%c_stif(nbr_fat_max))
	allocate (panneau(i)%weib_fact_stif(nbr_fat_max))

	do j=1,nbr_fat_max
		panneau(i).ix_Stif(j) = 0
		panneau(i).iy_Stif(j) = 0
		panneau(i).ES_Stif(j) = 0.d00
		panneau(i).KT_Stif(j) = 0.d00
		panneau(i).Kw_Stif(j) = 0.d00
		panneau(i).lambda_stif(j) = 0.d00
		panneau(i).teta_stif(j) = 0.d00
		panneau(i).BetaIf_stif(j) = 0.d00
		panneau(i).m_stif(j) = 0.d00
		panneau(i).c_stif(j) = 0.d00
		panneau(i).weib_fact_stif(j) = 0.d00
	enddo

	NULLIFY(panneau(i)%ix_Plate)
	NULLIFY(panneau(i)%iy_Plate)
	!NULLIFY(panneau(i)%ES_Plate)
	NULLIFY(panneau(i)%KT_Plate)
	NULLIFY(panneau(i)%Kw_Plate)
	NULLIFY(panneau(i)%lambda_Plate)
	NULLIFY(panneau(i)%teta_Plate)
	NULLIFY(panneau(i)%BetaIf_Plate)
	NULLIFY(panneau(i)%m_Plate)
	NULLIFY(panneau(i)%c_Plate)
	NULLIFY(panneau(i)%weib_fact_Plate)

	allocate (panneau(i)%ix_Plate(nbr_fat_max))
	allocate (panneau(i)%iy_Plate(nbr_fat_max))
	!allocate (panneau(i)%ES_Plate(nbr_fat_max))
	allocate (panneau(i)%KT_Plate(nbr_fat_max))
	allocate (panneau(i)%Kw_Plate(nbr_fat_max))
	allocate (panneau(i)%lambda_Plate(nbr_fat_max))
	allocate (panneau(i)%teta_Plate(nbr_fat_max))
	allocate (panneau(i)%BetaIf_Plate(nbr_fat_max))
	allocate (panneau(i)%m_Plate(nbr_fat_max))
	allocate (panneau(i)%c_Plate(nbr_fat_max))
	allocate (panneau(i)%weib_fact_Plate(nbr_fat_max))

	do j=1,nbr_fat_max
		panneau(i).ix_Plate(j) = 0
		panneau(i).iy_Plate(j) = 0
		!panneau(i).ES_Plate(j) = 0.d00
		panneau(i).KT_Plate(j) = 0.d00
		panneau(i).Kw_Plate(j) = 0.d00
		panneau(i).lambda_Plate(j) = 0.d00
		panneau(i).teta_Plate(j) = 0.d00
		panneau(i).BetaIf_Plate(j) = 0.d00
		panneau(i).m_Plate(j) = 0.d00
		panneau(i).c_Plate(j) = 0.d00
		panneau(i).weib_fact_Plate(j) = 0.d00
	enddo

	NULLIFY(panneau(i)%ix_Plate_Frame)
	NULLIFY(panneau(i)%iy_Plate_Frame)
	!NULLIFY(panneau(i)%ES_Plate_Frame)
	NULLIFY(panneau(i)%KT_Plate_Frame)
	NULLIFY(panneau(i)%Kw_Plate_Frame)
	NULLIFY(panneau(i)%lambda_Plate_Frame)
	NULLIFY(panneau(i)%teta_Plate_Frame)
	NULLIFY(panneau(i)%BetaIf_Plate_Frame)
	NULLIFY(panneau(i)%m_Plate_Frame)
	NULLIFY(panneau(i)%c_Plate_Frame)
	NULLIFY(panneau(i)%weib_fact_Plate_Frame)

	allocate (panneau(i)%ix_Plate_Frame(nbr_fat_max))
	allocate (panneau(i)%iy_Plate_Frame(nbr_fat_max))
	!allocate (panneau(i)%ES_Plate_Frame(nbr_fat_max))
	allocate (panneau(i)%KT_Plate_Frame(nbr_fat_max))
	allocate (panneau(i)%Kw_Plate_Frame(nbr_fat_max))
	allocate (panneau(i)%lambda_Plate_Frame(nbr_fat_max))
	allocate (panneau(i)%teta_Plate_Frame(nbr_fat_max))
	allocate (panneau(i)%BetaIf_Plate_Frame(nbr_fat_max))
	allocate (panneau(i)%m_Plate_Frame(nbr_fat_max))
	allocate (panneau(i)%c_Plate_Frame(nbr_fat_max))
	allocate (panneau(i)%weib_fact_Plate_Frame(nbr_fat_max))

	do j=1,nbr_fat_max
		panneau(i)%ix_Plate_Frame(j) = 0
		panneau(i)%iy_Plate_Frame(j) = 0
		!panneau(i)%ES_Plate_Frame(j) = 0.d00
		panneau(i)%KT_Plate_Frame(j) = 0.d00
		panneau(i)%Kw_Plate_Frame(j) = 0.d00
		panneau(i)%lambda_Plate_Frame(j) = 0.d00
		panneau(i)%teta_Plate_Frame(j) = 0.d00
		panneau(i)%BetaIf_Plate_Frame(j) = 0.d00
		panneau(i)%m_Plate_Frame(j) = 0.d00
		panneau(i)%c_Plate_Frame(j) = 0.d00
		panneau(i)%weib_fact_Plate_Frame(j) = 0.d00
	enddo

	NULLIFY(panneau(i)%ix_Frame)
	NULLIFY(panneau(i)%iy_Frame)
	NULLIFY(panneau(i)%ES_Frame)
	NULLIFY(panneau(i)%KT_Frame)
	NULLIFY(panneau(i)%Kw_Frame)
	NULLIFY(panneau(i)%lambda_Frame)
	NULLIFY(panneau(i)%teta_Frame)
	NULLIFY(panneau(i)%BetaIf_Frame)
	NULLIFY(panneau(i)%m_Frame)
	NULLIFY(panneau(i)%c_Frame)
	NULLIFY(panneau(i)%weib_fact_Frame)

	allocate (panneau(i)%ix_Frame(nbr_fat_max))
	allocate (panneau(i)%iy_Frame(nbr_fat_max))
	allocate (panneau(i)%ES_Frame(nbr_fat_max))
	allocate (panneau(i)%KT_Frame(nbr_fat_max))
	allocate (panneau(i)%Kw_Frame(nbr_fat_max))
	allocate (panneau(i)%lambda_Frame(nbr_fat_max))
	allocate (panneau(i)%teta_Frame(nbr_fat_max))
	allocate (panneau(i)%BetaIf_Frame(nbr_fat_max))
	allocate (panneau(i)%m_Frame(nbr_fat_max))
	allocate (panneau(i)%c_Frame(nbr_fat_max))
	allocate (panneau(i)%weib_fact_Frame(nbr_fat_max))

	do j=1,nbr_fat_max
		panneau(i)%ix_Frame(j) = 0
		panneau(i)%iy_Frame(j) = 0
		panneau(i)%ES_Frame(j) = 0.d00
		panneau(i)%KT_Frame(j) = 0.d00
		panneau(i)%Kw_Frame(j) = 0.d00
		panneau(i)%lambda_Frame(j) = 0.d00
		panneau(i)%teta_Frame(j) = 0.d00
		panneau(i)%BetaIf_Frame(j) = 0.d00
		panneau(i)%m_Frame(j) = 0.d00
		panneau(i)%c_Frame(j) = 0.d00
		panneau(i)%weib_fact_Frame(j) = 0.d00
	enddo

enddo
!
panneau(:).nfatiguem = 0
panneau(:).cjmax_fatigue = 0
!do i=1,4	!fat_new
!	panneau(:).i_fatigue_stiffened_panel(i) = 0	!fat_new
!enddo	!fat_new
!panneau(:).i_fatigue_frame = 0	!fat_new
!panneau(:).i_fatigue_girder = 0	!fat_new

!panneau(:).number_points_considered = 0	!fat_new
!do i=1,9	!fat_new
!	panneau(:).ix_stiffened_pannel(i) = 0	!fat_new
!	panneau(:).iy_stiffened_pannel(i) = 0	!fat_new
!enddo	!fat_new

!panneau(:).weib_fact = 0	!fat_new

!panneau(:).h_weibull = 0.d00	!fat_new

!panneau(:).ptyr = 0.d00
!panneau(:).esr = 0.d00
!panneau(:).lbr = 0.d00
!panneau(:).wbr = 0.d00
!panneau(:).tbr = 0.d00
!panneau(:).ptya = 0.d00
!panneau(:).esa = 0.d00
!panneau(:).lba = 0.d00
!panneau(:).wba = 0.d00
!panneau(:).tba = 0.d00

!do i=1,10
!	panneau(:).ptyg(i) = 0.d00
!	panneau(:).esg(i) = 0.d00
!	panneau(:).lbg(i) = 0.d00
!	panneau(:).wbg(i) = 0.d00
!	panneau(:).tbg(i) = 0.d00
!enddo

!panneau(:).kt_hs_1r = 0.d00
!panneau(:).kt_hs_2r = 0.d00
!panneau(:).kt_hs_3r = 0.d00
!panneau(:).kt_hs_4r = 0.d00	    !HOT SPOT FACTOR (STIFFENERS)
!panneau(:).kt_hs_1a = 0.d00
!panneau(:).kt_hs_2a = 0.d00
!panneau(:).kt_hs_3a = 0.d00
!panneau(:).kt_hs_4a = 0.d00	    !HOT SPOT FACTOR (FRAME)

!do i=1,10
!	panneau(:).kt_hs_1g(i) = 0.d00
!	panneau(:).kt_hs_2g(i) = 0.d00
!	panneau(:).kt_hs_3g(i) = 0.d00
!	panneau(:).kt_hs_4g(i) = 0.d00	!HOT SPOT FACTOR (GIRDER)			
!enddo

!panneau(:).betaif = 0.d00
!panneau(:).kcor = 0.d00
                                     
!panneau(:).tm = 0.d00
!panneau(:).tc = 0.d00
!do i=1,4                                     
!	panneau(:).toe_angle(i) = 0.d00
!	panneau(:).lambda_weld(i) = 0.d00
!enddo
do i=1,9
	panneau(:).panel(i)=''
enddo

!panneau(:).nsy = 0.d00
!panneau(:).nsz = 0.d00
!panneau(:).as = 0.d00
!panneau(:).iy = 0.d00
!panneau(:).iz = 0.d00
!do i=1,10
!	panneau(:).ksi(i) = 0.d00
!enddo
!panneau(:).ntcyc = 0.d00

!panneau(:).kt_hs_1 = 0.d00
!panneau(:).kt_hs_2 = 0.d00
!panneau(:).kt_hs_3 = 0.d00
!panneau(:).kt_hs_4 = 0.d00

!do i=1,10
!	panneau(:).length_girder (i) = 0.d00
!	panneau(:).epstr(i) = 0.d00
!	panneau(:).girder_length(i) = 0.d00
!enddo

nel     =0
temp    =0.d00 !on peut le supprimer il me semble

iff     =0

mode(:)=''
xmode(:)=0.d00
denom(:)=0.d00

!kse(:)  =0
ipa(:)  =0
ivaria(:)=0

sh=0.d00

xi(:,:)  =0.d00
xf(:,:)  =0.d00
xxi(:,:) =0.d00
xxf(:,:) =0.d00

teta=0.d00

qphil    =0.d00

aaa=0.d00
bbb=0.d00
ccc=0.d00
ddd=0.d00
delto=0.d00

!part(:)=0.d00

npt(:,:)=0
npt_full(:,:)=0
cha(:,:,:,:)=0.d00
cha_full(:,:,:,:)=0.d00
dcha(:,:,:,:)=0.d00

!m1(:,:) =0
m1_read=0

is=0
icc=0
icn=0

poids9   =0.d00
dpoids(:)=0.d00

title_cha_full(:,:)=''

title_loadcase(:,:)=''

sx_raid_sem(:,:,:,:) = 0.d00
sx_loc_raid_sem(:,:) = 0.d00
sx_plaque_top(:,:,:,:) = 0.d00
sy_plaque_top(:,:,:,:) = 0.d00
sxy_plaque_top(:,:,:,:) = 0.d00
sx_plaque_bott(:,:,:,:) = 0.d00
sy_plaque_bott(:,:,:,:) = 0.d00
sxy_plaque_bott(:,:,:,:) = 0.d00
sy_cadre_jab(:,:,:,:) = 0.d00
sxy_cadre_jab(:,:,:,:) = 0.d00
sy_cadre_sem(:,:,:,:) = 0.d00





!Stiffeners

kc_Stif(:,:,:,:) = 0.d00
NOM_STRESS_Stif(:,:,:,:) = 0.d00
HS_STRESS_Stif(:,:,:,:) = 0.d00
NS_STRESS_Stif(:,:,:,:) = 0.d00
Damage_Stif(:,:,:,:) = 0.d00
Damage_Stif_full(:,:,:) = 0.d00
Damage_Stif_ball(:,:,:) = 0.d00
Damage_Stif_total(:,:,:) = 0.d00


! Plate

kc_Plate(:,:,:,:) = 0.d00
NOM_STRESS_Plate(:,:,:,:) = 0.d00
NOM_STRESS_Plate_top(:,:,:,:) = 0.d00
NOM_STRESS_Plate_bott(:,:,:,:) = 0.d00
HS_STRESS_Plate(:,:,:,:) = 0.d00
NS_STRESS_Plate(:,:,:,:) = 0.d00
Damage_Plate(:,:,:,:) = 0.d00
Damage_Plate_full(:,:,:) = 0.d00
Damage_Plate_ball(:,:,:) = 0.d00
Damage_Plate_total(:,:,:) = 0.d00


!Intersection Plate/frame--------------------------------------------------------------

kc_Plate_Frame(:,:,:,:) = 0.d00
NOM_STRESS_Plate_Frame(:,:,:,:) = 0.d00
NOM_STRESS_Plate_Frame_top(:,:,:,:) = 0.d00
NOM_STRESS_Plate_Frame_bott(:,:,:,:) = 0.d00
HS_STRESS_Plate_Frame(:,:,:,:) = 0.d00
NS_STRESS_Plate_Frame(:,:,:,:) = 0.d00
Damage_Plate_Frame(:,:,:,:) = 0.d00
Damage_Plate_Frame_full(:,:,:) = 0.d00
Damage_Plate_Frame_ball(:,:,:) = 0.d00
Damage_Plate_Frame_total(:,:,:) = 0.d00

! Frame

kc_Frame(:,:,:,:) = 0.d00
NOM_STRESS_Frame(:,:,:,:) = 0.d00
HS_STRESS_Frame(:,:,:,:) = 0.d00
NS_STRESS_Frame(:,:,:,:) = 0.d00
Damage_Frame(:,:,:,:) = 0.d00
Damage_Frame_full(:,:,:) = 0.d00
Damage_Frame_ball(:,:,:) = 0.d00
Damage_Frame_total(:,:,:) = 0.d00



!Girders--------------------------------------------------------------


!	kc_Girder(:,:,:,:) = 0.d00
!	NOM_STRESS_Girder(:,:,:,:) = 0.d00
!	HS_STRESS_Girder(:,:,:,:) = 0.d00
!	NS_STRESS_Girder(:,:,:,:) = 0.d00
!	Damage_Girder(:,:,:,:) = 0.d00
!	Damage_Girder_full(:,:,:) = 0.d00
!	Damage_Girder_ball(:,:,:) = 0.d00
!	Damage_Girder_total(:,:,:) = 0.d00


!Pillars--------------------------------------------------------------

!	kc_Pillar(:,:,:,:) = 0.d00
!	NOM_STRESS_Pillar(:,:,:,:) = 0.d00
!	NOM_STRESS_Pillar1(:,:,:,:) = 0.d00
!	NOM_STRESS_Pillar2(:,:,:,:) = 0.d00
!	HS_STRESS_Pillar(:,:,:,:) = 0.d00
!	NS_STRESS_Pillar(:,:,:,:) = 0.d00
!	Damage_Pillar(:,:,:,:) = 0.d00
!	Damage_Pillar_full(:,:,:) = 0.d00
!	Damage_Pillar_ball(:,:,:) = 0.d00
!	Damage_Pillar_total(:,:,:) = 0.d00








return
end


