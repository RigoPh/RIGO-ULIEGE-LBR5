subroutine allocate_param_read_write_vector()		

use param_section
use PARAM_SECTION_VECTOR

implicit double precision (a-h,o-z)

! ====================================================================================================
! allocate 
! ====================================================================================================

!ALLOCATE(RP_ADD_RP%COEFF(MAX(SIZE(P1%COEFF), SIZE(P2%COEFF))))
allocate (panneau_vector(nmax_sup,nfile))
allocate (aono_vector(nmax_sup+1,2,nfile))
allocate (z_vector(nmax_sup,4,nfile))
allocate (noeud_vector(nmax_sup,2,nfile))

allocate (indMateriau_vector(nmax_sup,nfile))

allocate ( mode_vector(nmax_sup,nfile))
allocate (modes_vector(nmax_sup,nfile))
allocate (xmode_vector(nmax_sup,nfile))

allocate (kse_vector   (nmax_sup,nfile))
allocate (ipa_vector   (nmax_sup,nfile))
allocate (ivaria_vector(nmax_sup,nfile))

allocate (xi_vector(nsolmax,nmax_sup,nfile))
allocate (xf_vector(nsolmax,nmax_sup,nfile))

!allocate (part_vector  (nmax_sup,nfile))

allocate (m1tabl_vector (nmax_sup,nsolmax,nfile))
!allocate (m1cont_vector (nmax_sup,nfile))
allocate (m2cont_vector (nmax_sup,nfile))
allocate (m4cont_vector (nmax_sup,nfile))
allocate (m5cont_vector (nmax_sup,nfile))

allocate (noh_vector  (nmax_sup,10,nfile))

allocate (tfa_vector (nmax_sup,nfile))
allocate (tfr_vector (nmax_sup,nfile))

allocate (ypt9_vector(iptmax_sup,nmax_sup,nfile))
allocate (ypts_vector(iptmax_sup,nmax_sup,nfile))
allocate (abcd_vector(8,nfile))

allocate (npt_vector(nsolmax,nmax_sup,nfile))
allocate (npt_full_vector(nsolmax,nmax_sup,nfile))
allocate (cha_vector(100,3,nsolmax,nmax_sup,nfile)) !20 = nbr max de pas selon ox (à vérifier le nbr max)
allocate (cha_full_vector(100,3,nsolmax,nmax_sup,nfile))

allocate (title_cha_full_vector(nsolmax,nmax_sup,nfile))	!TODO mettre nsolmax_sup
allocate (title_m1_vector (nsolmax,nmax_sup,nfile))

allocate (title_loadcase_vector (nsolmax,nmax_sup,nfile))

allocate (bm1_vector(nsolmax,nfile))
allocate (bm2_vector(nsolmax,nfile))
allocate (bm3_vector(nsolmax,nfile))
allocate (bm4_vector(nsolmax,nfile))
allocate (sf1_vector(nsolmax,nfile))
allocate (sf3_vector(nsolmax,nfile))

allocate (bm11_vector(nsolmax,nfile))
allocate (bm31_vector(nsolmax,nfile))
allocate (sf11_vector(nsolmax,nfile))
allocate (sf31_vector(nsolmax,nfile))

!*******************************************************************************************
!===========================================================================================
!!Ajout des variables de travail relatives à la routine Effort Tranchant
!===========================================================================================

allocate (TEffortT_vector(nsolmax,nfile))
allocate (XEffortT_vector(nsolmax,nfile))
allocate (MEffortT_vector(nsolmax,nfile))
allocate (PEffortT_vector(nsolmax,nfile))
allocate (varoli_vector(nsolmax,nfile))
allocate (pointeur_vector(nmax_sup,nfile))
allocate (FFF_vector(nmax_sup,nfile))
allocate (aqoli_vector(nmax_sup,nfile))
allocate (bornestiffmin_vector(nmax_sup,nfile))
allocate (bornestiffmax_vector(nmax_sup,nfile))
!*******************************************************************************************
!===========================================================================================


!allocate (m1_vector   (nsolmax,nmax_sup,nfile))

allocate (itype_vector(nmax_sup,nfile))
allocate (isect_vector(nmax_sup,nfile))

!!! TODO améliorer l'allocation pour ne pas gaspiller l'espace mémoire

ix_max=5
iy_max=3

! Stiffener
allocate (sx_raid_sem_vector(nsol_fatigue,nmax_sup,1:ix_max,1:iy_max,nfile))		!fat_new
allocate (sx_loc_raid_sem_vector(nsol_fatigue,nmax_sup,nfile)) ! Contrainte locale uniquement ou combinaison ?

! Plate
allocate (sx_plaque_top_vector(nsol_fatigue,nmax_sup,1:ix_max,1:iy_max,nfile))		!fat_new
allocate (sy_plaque_top_vector(nsol_fatigue,nmax_sup,1:ix_max,1:iy_max,nfile))		!fat_new
allocate (sxy_plaque_top_vector(nsol_fatigue,nmax_sup,1:ix_max,1:iy_max,nfile))		!fat_new
allocate (sx_plaque_bott_vector(nsol_fatigue,nmax_sup,1:ix_max,1:iy_max,nfile))		!fat_new
allocate (sy_plaque_bott_vector(nsol_fatigue,nmax_sup,1:ix_max,1:iy_max,nfile))		!fat_new
allocate (sxy_plaque_bott_vector(nsol_fatigue,nmax_sup,1:ix_max,1:iy_max,nfile))		!fat_new

! Frame
	!sx_plaque_top
allocate (sy_cadre_jab_vector(nsol_fatigue,nmax_sup,1:ix_max,1:iy_max,nfile))		!fat_new
allocate (sxy_cadre_jab_vector(nsol_fatigue,nmax_sup,1:ix_max,1:iy_max,nfile))		!fat_new
allocate (sy_cadre_sem_vector(nsol_fatigue,nmax_sup,1:ix_max,1:iy_max,nfile))		!fat_new


panneau_vector(:,:).types = ''
panneau_vector(:,:).section = ''
panneau_vector(:,:).delta = 0.d00
panneau_vector(:,:).hya = 0.d00
panneau_vector(:,:).dya = 0.d00
panneau_vector(:,:).wya = 0.d00
panneau_vector(:,:).tya = 0.d00
panneau_vector(:,:).epsa = 0.d00
panneau_vector(:,:).hxr = 0.d00
panneau_vector(:,:).dxr = 0.d00
panneau_vector(:,:).wxr = 0.d00
panneau_vector(:,:).txr = 0.d00
panneau_vector(:,:).epsr = 0.d00
panneau_vector(:,:).entr = 0.d00
panneau_vector(:,:).kse = 0.d00
panneau_vector(:,:).part = 0.d00
panneau_vector(:,:).wya_fatigue = 0.d00
panneau_vector(:,:).tya_fatigue = 0.d00
panneau_vector(:,:).wxr_fatigue = 0.d00
panneau_vector(:,:).txr_fatigue = 0.d00
panneau_vector(:,:).itype_longit = 0
panneau_vector(:,:).itype_transv = 0
panneau_vector(:,:).itype_transv_second = 0
panneau_vector(:,:).itype_longit_second = 0
panneau_vector(:,:).name_type_transv = ''
panneau_vector(:,:).name_type_transv_second = ''
panneau_vector(:,:).name_type_longit = ''
panneau_vector(:,:).name_type_longit_second = ''
panneau_vector(:,:).hya2 = 0.d00
panneau_vector(:,:).dya2 = 0.d00
panneau_vector(:,:).wya2 = 0.d00
panneau_vector(:,:).tya2 = 0.d00
panneau_vector(:,:).epsa2 = 0.d00
panneau_vector(:,:).hxr2 = 0.d00
panneau_vector(:,:).dxr2 = 0.d00
panneau_vector(:,:).wxr2 = 0.d00
panneau_vector(:,:).txr2 = 0.d00
panneau_vector(:,:).epsr2 = 0.d00
panneau_vector(:,:).entr2 = 0.d00
panneau_vector(:,:).ksr2 = 0	
panneau_vector(:,:).ksa2 = 0
do i=1,9
	panneau_vector(:,:).dvmin(i) = 0.d00
	panneau_vector(:,:).dvmax(i) = 0.d00
enddo
panneau_vector(:,:).heff = 0
do i=1,3
	panneau_vector(:,:).corro(i) = 0.d00
enddo
do i=1,10
	panneau_vector(:,:).noh9(i) = 0
enddo
do j=1,nfile
	do i=1,nmax_sup
		NULLIFY (panneau_vector(i,j)%xi_full)
		NULLIFY (panneau_vector(i,j)%xf_full)
		allocate (panneau_vector(i,j)%xi_full(nsolmax))
		allocate (panneau_vector(i,j)%xf_full(nsolmax))
		do k=1,nsolmax
			panneau_vector(i,j).xi_full(k) = 0.d00
			panneau_vector(i,j).xf_full(k) = 0.d00
		enddo
	enddo
enddo
panneau_vector(:,:).m2 = 0
panneau_vector(:,:).im1 = 0
panneau_vector(:,:).m1cont = 0
do j=1,nfile
	do i=1,nmax_sup
		NULLIFY (panneau_vector(i,j)%m1)
		allocate (panneau_vector(i,j)%m1(50))	!!! A changer
		do k=1,50								!!! A changer
			panneau_vector(i,j).m1(k) = 0
		enddo
	enddo
enddo
panneau_vector(:,:).ibm2 = 0
panneau_vector(:,:).isema = 0
panneau_vector(:,:).isemr = 0
panneau_vector(:,:).inda = 0
panneau_vector(:,:).indr = 0
do i=1,40 !TODO faudra mettre dynamique
	panneau_vector(:,:).lm2(i) = 0
enddo
panneau_vector(:,:).delt = 0.d00
panneau_vector(:,:).delt2 = 0.d00
panneau_vector(:,:).xneu = 0.d00
panneau_vector(:,:).yneu = 0.d00
panneau_vector(:,:).d1 = 0.d00
panneau_vector(:,:).d2 = 0.d00
panneau_vector(:,:).omega = 0.d00
panneau_vector(:,:).omega2 = 0.d00
do i=1,74
	panneau_vector(:,:).const(i) = 0.d00
enddo
do i=1,6
	do j=1,9
		panneau_vector(:,:).const2(i,j) = 0.d00
	enddo
enddo
do i=1,2
	do j=1,10
		panneau_vector(:,:).const3(i,j) = 0.d00
	enddo
enddo
do i=1,nmax_sup
	do j=1,nfile
		NULLIFY (panneau_vector(i,j)%chamax)
		NULLIFY (panneau_vector(i,j)%dchamax)
	enddo
enddo
panneau_vector(:,:).q = 0.d00
panneau_vector(:,:).phil = 0.d00
panneau_vector(:,:).epais = 0.d00
panneau_vector(:,:).hight = 0.d00
panneau_vector(:,:).mode = ''
panneau_vector(:,:).teta = 0.d00
panneau_vector(:,:).tetas = 0.d00
panneau_vector(:,:).mt = 0
panneau_vector(:,:).mmt = 0
panneau_vector(:,:).ksr = 0
panneau_vector(:,:).ksa = 0
panneau_vector(:,:).kst = 0
do i=1,10
	panneau_vector(:,:).hxtr(i) = 0.d00
	panneau_vector(:,:).dxtr(i) = 0.d00
	panneau_vector(:,:).wxtr(i) = 0.d00
	panneau_vector(:,:).txtr(i) = 0.d00
	panneau_vector(:,:).abtr(i) = 0.d00
	panneau_vector(:,:).abtr2(i) = 0.d00
	panneau_vector(:,:).ipts3(i) = 0
enddo
panneau_vector(:,:).sh = 0.d00
panneau_vector(:,:).ang = 0.d00
panneau_vector(:,:).angle = 0.d00
panneau_vector(:,:).code = 0
panneau_vector(:,:).fami = 0
panneau_vector(:,:).lot = 0
panneau_vector(:,:).rend = 0.d00
panneau_vector(:,:).ploc = 0.d00
panneau_vector(:,:).icha = 0
panneau_vector(:,:).elmt_vib = 0
panneau_vector(:,:).long_vib = 0.d00
panneau_vector(:,:).cl_av_vib = 0
panneau_vector(:,:).cl_ar_vib	= 0

do j=1,nfile
	do i=1,nmax_sup
		NULLIFY (panneau_vector(i,j)%lcont)
!		NULLIFY (panneau_vector(i,j)%ipts2)
		NULLIFY (panneau_vector(i,j)%cjmax)
		NULLIFY (panneau_vector(i,j)%inv)
		!m1tmax = 20*nmax_sup ! TODO : faudra mettre tout vraiment dynamique un jour
		!allocate (panneau_vector(i,j)%lcont(2,(m1tmax*nsolmax)))
		!allocate (panneau_vector(i,j)%ipts2(iptmax))
		!allocate (panneau_vector(i,j)%cjmax((m1tmax*nsolmax)))
		!allocate (panneau_vector(i,j)%inv((m1tmax*nsolmax)))
		!do k=1,(m1tmax*nsolmax)
		!	panneau_vector(i,j).lcont(1,k) = 0
		!	panneau_vector(i,j).lcont(2,k) = 0
		!	panneau_vector(i,j).cjmax(k) = 0.d00
		!	panneau_vector(i,j).inv(k) = 0
		!enddo
		!do k=1,iptmax
		!	panneau_vector(i,j).ipts2(k) = 0
		!enddo
	enddo
enddo
panneau_vector(:,:).ipts = 0
do i=1,nmax_sup
	do j=1,nfile
		NULLIFY (panneau_vector(i,j)%lcont4_init)
		NULLIFY (panneau_vector(i,j)%lcont4)
		NULLIFY (panneau_vector(i,j)%inv3_init)
		NULLIFY (panneau_vector(i,j)%inv3)
		NULLIFY (panneau_vector(i,j)%cjmax9_init)
		NULLIFY (panneau_vector(i,j)%cjmax9)
	enddo
enddo
panneau_vector(:,:).nsloshm = 0
do i=1,3
	panneau_vector(:,:).nslosh(i) = 0
	panneau_vector(:,:).slosh(i) = 0.d00
enddo
panneau_vector(:,:).press_slosh = 0.d00
panneau_vector(:,:).aire = 0.d00
panneau_vector(:,:).aiy = 0.d00
panneau_vector(:,:).aix = 0.d00
panneau_vector(:,:).sredy = 0.d00
panneau_vector(:,:).sredx = 0.d00
panneau_vector(:,:).tork = 0.d00
panneau_vector(:,:).plat = 0.d00
panneau_vector(:,:).fl = 0.d00
panneau_vector(:,:).sig1 = 0.d00
panneau_vector(:,:).sig2 = 0.d00
panneau_vector(:,:).sig3 = 0.d00
panneau_vector(:,:).sig4 = 0.d00
panneau_vector(:,:).tau = 0.d00
panneau_vector(:,:).wpl1 = 0.d00
panneau_vector(:,:).wpl2 = 0.d00
panneau_vector(:,:).vmaxsig = 0.d00
panneau_vector(:,:).vmaxsigc = 0.d00
panneau_vector(:,:).sigmag = 0.d00
do j=1,nfile
	do i=1,nmax_sup
		NULLIFY (panneau_vector(i,j)%sigplaque)
		NULLIFY (panneau_vector(i,j)%dsigplaque)
		NULLIFY (panneau_vector(i,j)%sigxplaque)
		NULLIFY (panneau_vector(i,j)%sigyplaque)
		NULLIFY (panneau_vector(i,j)%phiplaque)
		NULLIFY (panneau_vector(i,j)%sigvmtplaque)
		NULLIFY (panneau_vector(i,j)%sigvmcplaque)
		NULLIFY (panneau_vector(i,j)%indplaque)
		NULLIFY (panneau_vector(i,j)%sigmx)
		NULLIFY (panneau_vector(i,j)%sigmy)
		allocate (panneau_vector(i,j)%sigplaque(iptmax))
		allocate (panneau_vector(i,j)%dsigplaque(9*iptmax))
		allocate (panneau_vector(i,j)%sigxplaque(iptmax))
		allocate (panneau_vector(i,j)%sigyplaque(iptmax))
		allocate (panneau_vector(i,j)%phiplaque(iptmax))
		allocate (panneau_vector(i,j)%sigvmtplaque(iptmax))
		allocate (panneau_vector(i,j)%sigvmcplaque(iptmax))
		allocate (panneau_vector(i,j)%indplaque(iptmax))
		allocate (panneau_vector(i,j)%sigmx(iptmax))
		allocate (panneau_vector(i,j)%sigmy(iptmax))
		do k=1,iptmax
			panneau_vector(i,j).sigplaque(k) = 0.d00
			panneau_vector(i,j).dsigplaque(k) = 0.d00
			panneau_vector(i,j).sigxplaque(k) = 0.d00
			panneau_vector(i,j).sigyplaque(k) = 0.d00
			panneau_vector(i,j).phiplaque(k) = 0.d00
			panneau_vector(i,j).sigvmtplaque(k) = 0.d00
			panneau_vector(i,j).sigvmcplaque(k) = 0.d00
			panneau_vector(i,j).indplaque(k) = 0.d00
			panneau_vector(i,j).sigmx(k) = 0.d00
			panneau_vector(i,j).sigmy(k) = 0.d00
		enddo
	enddo
enddo

!fat_new

!panneau_vector(:,:).h_weibull = 0.d00
!panneau_vector(:,:).ptyr = 0.d00
!panneau_vector(:,:).esr = 0.d00
!panneau_vector(:,:).lbr = 0.d00
!panneau_vector(:,:).wbr = 0.d00
!panneau_vector(:,:).tbr = 0.d00
!panneau_vector(:,:).ptya = 0.d00
!panneau_vector(:,:).esa = 0.d00
!panneau_vector(:,:).lba = 0.d00
!panneau_vector(:,:).wba = 0.d00
!panneau_vector(:,:).tba = 0.d00
!do i=1,10
!	panneau_vector(:,:).ptyg(i) = 0.d00
!	panneau_vector(:,:).esg(i) = 0.d00
!	panneau_vector(:,:).lbg(i) = 0.d00
!	panneau_vector(:,:).wbg(i) = 0.d00
!	panneau_vector(:,:).tbg(i) = 0.d00
!enddo
!panneau_vector(:,:).kt_hs_1r = 0.d00
!panneau_vector(:,:).kt_hs_2r = 0.d00
!panneau_vector(:,:).kt_hs_3r = 0.d00
!!panneau_vector(:,:).kt_hs_4r = 0.d00	    !HOT SPOT FACTOR (STIFFENERS)
!panneau_vector(:,:).kt_hs_1a = 0.d00
!panneau_vector(:,:).kt_hs_2a = 0.d00
!panneau_vector(:,:).kt_hs_3a = 0.d00
!panneau_vector(:,:).kt_hs_4a = 0.d00	    !HOT SPOT FACTOR (FRAME)
!do i=1,10
!	panneau_vector(:,:).kt_hs_1g(i) = 0.d00
!	panneau_vector(:,:).kt_hs_2g(i) = 0.d00
!	panneau_vector(:,:).kt_hs_3g(i) = 0.d00
!	panneau_vector(:,:).kt_hs_4g(i) = 0.d00	!HOT SPOT FACTOR (GIRDER)			
!enddo
!panneau_vector(:,:).betaif = 0.d00
!panneau_vector(:,:).kcor = 0.d00         
!panneau_vector(:,:).tm = 0.d00
!panneau_vector(:,:).tc = 0.d00
!do i=1,4                                     
!	panneau_vector(:,:).toe_angle(i) = 0.d00
!	panneau_vector(:,:).lambda_weld(i) = 0.d00
!enddo
do i=1,9
	panneau_vector(:,:).panel(i)=''
enddo
!panneau_vector(:,:).nsy = 0.d00
!panneau_vector(:,:).nsz = 0.d00
!panneau_vector(:,:).as = 0.d00
!panneau_vector(:,:).iy = 0.d00
!panneau_vector(:,:).iz = 0.d00
!do i=1,10
!	panneau_vector(:,:).ksi(i) = 0.d00
!enddo
!panneau_vector(:,:).ntcyc = 0.d00
!panneau_vector(:,:).kt_hs_1 = 0.d00
!panneau_vector(:,:).kt_hs_2 = 0.d00
!panneau_vector(:,:).kt_hs_3 = 0.d00
!panneau_vector(:,:).kt_hs_4 = 0.d00
!do i=1,10
!	panneau_vector(:,:).length_girder (i) = 0.d00
!	panneau_vector(:,:).epstr(i) = 0.d00
!	panneau_vector(:,:).girder_length(i) = 0.d00
!enddo

aono_vector(:,:,:) = 0.d00
z_vector(:,:,:) = 0.d00
noeud_vector(:,:,:) = 0.d00

indMateriau_vector(1:nmax_sup,1:nfile)  = 0.d00

mode_vector(1:nmax_sup,1:nfile)  = ''   
modes_vector(1:nmax_sup,1:nfile)  = ''   
xmode_vector(1:nmax_sup,1:nfile)  =0.d00

kse_vector   (1:nmax_sup,1:nfile)   =    0
ipa_vector   (1:nmax_sup,1:nfile)   =    0
ivaria_vector(1:nmax_sup,1:nfile)   =    0
                                  
xi_vector(1:nsolmax,1:nmax_sup,1:nfile)  =   0.d00
xf_vector(1:nsolmax,1:nmax_sup,1:nfile)  =   0.d00

!part_vector  (1:nmax_sup,1:nfile)   =    0.d00

m1tabl_vector(1:nmax_sup,1:nsolmax,1:nfile)     =  0
!m1cont_vector(1:nmax_sup,1:nfile)             =  0
m2cont_vector(1:nmax_sup,1:nfile)             =  0      
m4cont_vector(1:nmax_sup,1:nfile)             =  0
m5cont_vector(1:nmax_sup,1:nfile)             =  0

noh_vector   (1:nmax_sup,1:10,1:nfile)        =  0  

tfa_vector (1:nmax_sup,1:nfile)               =  0.d00
tfr_vector (1:nmax_sup,1:nfile)               =  0.d00
!part_vector(1:nmax_sup,1:nfile)               =  0.d00

ypt9_vector  (1:iptmax_sup,1:nmax_sup ,1:nfile)    =  0.d00
ypts_vector  (1:iptmax_sup,1:nmax_sup ,1:nfile)    =  0.d00
abcd_vector                         (8,1:nfile)    =  0.d00

npt_vector(:,:,:) = 0
npt_full_vector(:,:,:) = 0
cha_vector(:,:,:,:,:) = 0.d00
cha_full_vector(:,:,:,:,:) = 0.d00

title_cha_full_vector(:,1:nmax_sup,1:nfile) =   ''
title_m1_vector (:,1:nmax_sup,1:nfile)      =   ''

title_loadcase_vector (1:nsolm,1:nmax_sup,1:nfile)      =   ''
                                    
!m1_vector(:,1:nmax_sup,1:nfile) = 0.d00

itype_vector(1:nmax_sup,1:nfile) = 0
isect_vector(1:nmax_sup,1:nfile) = 0

sx_raid_sem_vector(:,:,:,:,:) = 0.d00
sx_loc_raid_sem_vector(:,:,:) = 0.d00
sx_plaque_top_vector(:,:,:,:,:) = 0.d00
sy_plaque_top_vector(:,:,:,:,:) = 0.d00
sxy_plaque_top_vector(:,:,:,:,:) = 0.d00
sx_plaque_bott_vector(:,:,:,:,:) = 0.d00
sy_plaque_bott_vector(:,:,:,:,:) = 0.d00
sxy_plaque_bott_vector(:,:,:,:,:) = 0.d00
sy_cadre_jab_vector(:,:,:,:,:) = 0.d00
sxy_cadre_jab_vector(:,:,:,:,:) = 0.d00
sy_cadre_sem_vector(:,:,:,:,:) = 0.d00
                     
end
