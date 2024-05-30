subroutine allocate_param_section()		


use param_section



implicit double precision (a-h,o-z)


nmax   = neto	!nbr max de panneau											
nemax  = nmax*8
nvmax  = nmax*9 !le nbre  de variables de conception                       (max = 9*n)					
m1max  =  30    !nbre max  restrictions structurelles par panneau          (pour 1 cas de charge )
m1tmax =m1max*nmax !nbre max  restrictions structurelles pour la structure    (pour un cas de charge)	
m2max  =  20    !nbre max  restrictions géométriques  par panneau									
m2tmax =m2max*nmax !nbre max  restrictions géométriques  pour la structure								
ngmax  = nmax*9 !nbre max  restrictions d'égalité                          (pour la structure    )
iptmax =   4   !nbre max de pts de calcul des sensibilités par panneau
mmmax  = m1tmax*10 + m2tmax	!7000	!mm=somme(m1t)+m2t  < 8000 (*10 car 10 cas de charge max)

! variables materiaux (dont notamment paramètres relatifs aux coûts)===============================


allocate (indMateriau(nmax))	!jan09

allocate (sxm(iptmax+2*neto+11))
sxm(:) = 0.d00

! valeur de pi =======================================================================================

pi=2.d00*acos(0.d00)
!
kcontr = 0
ncont = 0

! allocation des tableaux et matrices ================================================================

allocate (modes(nmax))
allocate (blocka(nemax,nemax))
allocate (solt(nemax,nemax))
allocate (sens1(9,iptmax,9,nmax))
allocate (sens2(16,iptmax,9,nmax))
allocate (sens3(21,iptmax,9,nmax))
allocate (sens4(9,iptmax,9,nmax))

allocate (a(nemax,nemax))
allocate (b(nemax,nemax+1))
allocate (dzsn(nemax,9*nmax,nsol))

dzsn(:,:,:)=0.d00

allocate (bm1(nsolm))
allocate (bm2(nsolm))
allocate (bm3(nsolm))
allocate (bm4(nsolm))
allocate (sf1(nsolm))
allocate (sf3(nsolm))	

allocate (bm11(nsolm))
allocate (bm31(nsolm))
allocate (sf11(nsolm))
allocate (sf31(nsolm))	

bm1(:) = 0.d00
bm2(:) = 0.d00
bm3(:) = 0.d00
bm4(:) = 0.d00
sf1(:) = 0.d00
sf3(:) = 0.d00

bm11(:) = 0.d00
bm31(:) = 0.d00
sf11(:) = 0.d00
sf31(:) = 0.d00

!*******************************************************************************************
!===========================================================================================
!!ajout des valeurs de travail
!===========================================================================================

allocate (TEffortT(nsolm))
allocate (MEffortT(nsolm))	
allocate (XEffortT(nsolm))	
allocate (PEffortT(nsolm))	
allocate (varoli(nsolm))	
allocate (pointeur(nmax))	
allocate (aqoli(nmax))
allocate (FFF(nmax))


TEffortT(:) = 0.d00
MEffortT(:) = 0.d00
XEffortT(:) = 0.d00
PEffortT(:) = 0.d00
varoli(:) = 0.d00
pointeur(:) = 0
FFF(:) = 0.d00
Noli=0
Nolir=0.d00
stepoliinf=0
stepolisup=0
aqoli(:)=0.d00
Poidsoli=0.d00

!===========================================================================================
!===========================================================================================
!*******************************************************************************************
allocate (ptri(nmax))
allocate (GMT(6))
ptri(:) = 0
allocate (tetas(nmax))
allocate (tetaq(nmax))
allocate (z(nmax,4))
allocate (noh (nmax,10))
allocate (noeud(nmax,2))
allocate (mcomp(2*nmax,2))
allocate (nno (nmax+1,2))
allocate (nno9(nmax+1,2)) !+1 car si un seul panneau => on peut avoir 2 cond limites

allocate (nsign(nmax))

allocate (m1tabl(nmax,nsolmax))
!allocate (m1cont(nmax))
allocate (m2cont(nmax))
allocate (m4cont(nmax))
allocate (m5cont(nmax))
allocate (ypts(iptmax,nmax))
allocate (ypt9(iptmax,nmax))

allocate (kli(nemax,2))

allocate (zsn(nemax,nsol))
allocate (abc(nmax))
allocate (asol(nmax))
allocate (bsol(nmax))
allocate (csol(nmax))
allocate (dsol(nmax))
allocate (amom(nmax))
allocate (bmom(nmax))
allocate (cmom(nmax))
allocate (dmom(nmax))

allocate (abcd(8))

allocate (coef(nmax,8,nsolmax))		

allocate (itype(nmax))
allocate (isect(nmax))

allocate (tfa(nmax))
allocate (tfr(nmax))

!allocate (izut(mmmax))

! variables ushull ===================================================================================

allocate (dsud(9,nmax))
allocate (dsub(9,nmax))
allocate (dsub1(9,nmax))
allocate (dsusu(9,nmax))
allocate (dsusl(9,nmax))
allocate (dh(9,nmax))
allocate (dg(9,nmax))
allocate (dad(9,nmax))
allocate (dab(9,nmax))
allocate (dab1(9,nmax))
allocate (das(9,nmax))
allocate (dult(9*nmax))
allocate (id(nmax))				
allocate (ib1(nmax))			
allocate (ib(nmax))				
allocate (is9(nmax))

! variables sensibcout ===============================================================================

allocate (vlarg(nmax))
allocate (vepaiss(nmax))
allocate (vhac(nmax))
allocate (veac(nmax))
allocate (vlsc(nmax))
allocate (vesc(nmax))
allocate (deltacsc(nmax))
allocate (vhar(nmax))
allocate (vear(nmax))
allocate (vlsr(nmax))
allocate (vesr(nmax))
allocate (deltar(nmax))
allocate (entrsc(nmax))
allocate (philsc(nmax))
allocate (qsc(nmax))
allocate (coutpan(nmax))
allocate (costpan(nmax))
allocate (coutmat(nmax))
allocate (vdiffc(nmax))

!    variables coord ================================================================================
allocate (aiyy(nmax))
allocate (aixx(nmax))
allocate (iyy2(nmax))
allocate (aiyy2(nmax))
allocate (aixx2(nmax))
allocate (aixxpart(nmax))
allocate (aono(neto+1,2))

!	variables inertia ================================================================================

allocate (dyneutnet(9,nmax))	
allocate (dyneutgro(9,nmax))	
allocate (dinet(9,nmax))			
allocate (digro(9,nmax))			
allocate (dyneutplnet(9,nmax))	
allocate (dyneutplgro(9,nmax))	
allocate (diplnet(9,nmax))			
allocate (diplgro(9,nmax))			
allocate (dsignet(2,9,nmax))		
allocate (dsiggro(2,9,nmax))		

!	variables shear ==================================================================================

allocate (taunet(nmax,3))			
allocate (taugro(nmax,3))			
allocate (dtaunet(nmax,3,nmax))	
allocate (dtaugro(nmax,3,nmax))	

!	variables buckdcn ================================================================================

!	variables buckdcn_07
allocate (ddbuckpl_07  (9,nmax))				!dcn07
allocate (ddbuckstif_07(9,nmax))			    !dcn07
allocate (dinpart_07   (9,nmax))				!dcn07

!	variables buckdcn_08
allocate (ddbuckpl_08  (9,nmax))				!dcn08
allocate (ddbuckstif_08(9,nmax))			    !dcn08
allocate (dinpart_08   (9,nmax))				!dcn08

! ancien common hyp    ===============================================================================

allocate (dvarh (33,9,38,nsol))
allocate (sensh (13,4,9 ,nsol))
allocate (hyp(2*mt_max*(8+nsolmax)))              ! bo1,sens,bo2 (ok pour 10 traverses et 10 cas de charges
allocate (ali(2*mt_max*(8+nsolmax)))

allocate (alix(mt_max,13))
allocate (aliz(mt_max,13))

allocate (dalix(  mt_max,13,9))
allocate (daliz(  mt_max,13,9))
allocate (dhyp (2*mt_max,8+nsolmax,9))
allocate (dish(1710,nsol))

dvarh(:,:,:,:)=0.d00
sensh(:,:,:,:)=0.d00
hyp  (:)      =0.d00
ali  (:)      =0.d00
alix (:,:)    =0.d00
aliz (:,:)    =0.d00

dalix(:,:,:)  =0.d00
daliz(:,:,:)  =0.d00
dhyp (:,:,:)  =0.d00
dish (:,:)    =0.d00


! initialisation des tableaux et matrices  ===========================================================

noeud (:,:)    =0
tetas (:)      =0.d00
tetaq (:)      =0.d00
        
m1tabl(:,:)    =0
sens2 (:,:,:,:)=0.d00

!izut  (:)      =0

!====================================================================
! fatigue 
!====================================================================


!!!!!!!!!!!TODO : pas allouer ça si pas nécessaire => mettre la bonne condition !!!
!ifred=0
!if (ifred.eq.0) then
!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!

return

!!!!!!!!!!!!!!!!!!


!if (nsol_fatigue.NE.0.) then	!New11		!pas allouer si pas DE CALCUL FATIGUE
if (IS_FATIGUE.NE.0.) then	!fat_new		!pas allouer si pas DE CALCUL FATIGUE


allocate (damage_stiffened_pannel(3,3,4))
allocate (damage_fatigue(nmax))

allocate (index_damage_stiffened_pannel(3))

allocate (ddamage_fatigue(nmax,9,nmax))

!	stiffened plate sty=1 

allocate (dfull_11(nmax,3,3))
allocate (dfull_12(nmax,3,3))
allocate (dfull_13(nmax,3,3))
allocate (dfull_14(nmax,3,3))

!	girder sty=2 and sty2=1

allocate (dfull_21_1(nmax,3,10))
allocate (dfull_22_1(nmax,3,10))
allocate (dfull_23_1(nmax,3,10))
allocate (dfull_24_1(nmax,3,10))

!	web frame  sty=2 sty2=2

allocate (dfull_21_2(nmax,3,2))
allocate (dfull_22_2(nmax,3,2))
allocate (dfull_23_2(nmax,3,2))
allocate (dfull_24_2(nmax,3,2))


!	stiffened plate sty=1 

allocate (dball_11(nmax,3,3))
allocate (dball_12(nmax,3,3))
allocate (dball_13(nmax,3,3))
allocate (dball_14(nmax,3,3))

!	girder sty=2 and sty2=1

allocate (dball_21_1(nmax,3,10))
allocate (dball_22_1(nmax,3,10))
allocate (dball_23_1(nmax,3,10))
allocate (dball_24_1(nmax,3,10))

!	web frame  sty=2 sty2=2

allocate (dball_21_2(nmax,3,2))
allocate (dball_22_2(nmax,3,2))
allocate (dball_23_2(nmax,3,2))
allocate (dball_24_2(nmax,3,2))

!	stiffened plate sty=1 

allocate (ddfull_11(nmax,3,3,9,nmax))
allocate (ddfull_12(nmax,3,3,9,nmax))
allocate (ddfull_13(nmax,3,3,9,nmax))
allocate (ddfull_14(nmax,3,3,9,nmax))

!	girder sty=2 and sty2=1
allocate (ddfull_21_1(nmax,3,10,9,nmax))
allocate (ddfull_22_1(nmax,3,10,9,nmax))
allocate (ddfull_23_1(nmax,3,10,9,nmax))
allocate (ddfull_24_1(nmax,3,10,9,nmax))

!	web frame  sty=2 sty2=2
allocate (ddfull_21_2(nmax,3,2,9,nmax))
allocate (ddfull_22_2(nmax,3,2,9,nmax))
allocate (ddfull_23_2(nmax,3,2,9,nmax))
allocate (ddfull_24_2(nmax,3,2,9,nmax))
  
!	stiffened plate sty=1 

allocate (ddball_11(nmax,3,3,9,nmax))
allocate (ddball_12(nmax,3,3,9,nmax))
allocate (ddball_13(nmax,3,3,9,nmax))
allocate (ddball_14(nmax,3,3,9,nmax))

!	girder sty=2 and sty2=1
allocate (ddball_21_1(nmax,3,10,9,nmax))
allocate (ddball_22_1(nmax,3,10,9,nmax))
allocate (ddball_23_1(nmax,3,10,9,nmax))
allocate (ddball_24_1(nmax,3,10,9,nmax))

!	web frame  sty=2 sty2=2

allocate (ddball_21_2(nmax,3,2,9,nmax))
allocate (ddball_22_2(nmax,3,2,9,nmax))
allocate (ddball_23_2(nmax,3,2,9,nmax))
allocate (ddball_24_2(nmax,3,2,9,nmax))

!	stiffened plate sty=1 

allocate (dtot_11(nmax,3,3))
allocate (dtot_12(nmax,3,3))
allocate (dtot_13(nmax,3,3))
allocate (dtot_14(nmax,3,3))

!	girder sty=2 and sty2=1

allocate (dtot_21_1(nmax,3,10))
allocate (dtot_22_1(nmax,3,10))
allocate (dtot_23_1(nmax,3,10))
allocate (dtot_24_1(nmax,3,10))

!	web frame  sty=2 sty2=2

allocate (dtot_21_2(nmax,3,2))
allocate (dtot_22_2(nmax,3,2))
allocate (dtot_23_2(nmax,3,2))
allocate (dtot_24_2(nmax,3,2))

!	stiffened plate sty=1 

allocate (ddtot_11(nmax,3,3,9,nmax))
allocate (ddtot_12(nmax,3,3,9,nmax))
allocate (ddtot_13(nmax,3,3,9,nmax))
allocate (ddtot_14(nmax,3,3,9,nmax))

!	girder sty=2 and sty2=1

allocate (ddtot_21_1(nmax,3,10,9,nmax))
allocate (ddtot_22_1(nmax,3,10,9,nmax))
allocate (ddtot_23_1(nmax,3,10,9,nmax))
allocate (ddtot_24_1(nmax,3,10,9,nmax))

!	web frame  sty=2 sty2=2


allocate (ddtot_21_2(nmax,3,2,9,nmax))
allocate (ddtot_22_2(nmax,3,2,9,nmax))
allocate (ddtot_23_2(nmax,3,2,9,nmax))
allocate (ddtot_24_2(nmax,3,2,9,nmax))

!====================================================================

allocate (dnx11(nmax,nsol_fatigue,3,3))
allocate (dny11(nmax,nsol_fatigue,3,3)) 
allocate (dqx11(nmax,nsol_fatigue,3,3)) 
allocate (dqy11(nmax,nsol_fatigue,3,3)) 
allocate (dmx11(nmax,nsol_fatigue,3,3)) 
allocate (dmy11(nmax,nsol_fatigue,3,3)) 
allocate (dq11 (nmax,nsol_fatigue,3,3)) 


allocate (ds_11_1(nmax,nsol_fatigue,3,3))
allocate (ds_12_1(nmax,nsol_fatigue,3,3))
allocate (ds_13_1(nmax,nsol_fatigue,3,3))
allocate (ds_14_1(nmax,nsol_fatigue,3,3))
            
!  hot spot stress range
allocate (dshs_11_1(nmax,nsol_fatigue,3,3))
allocate (dshs_12_1(nmax,nsol_fatigue,3,3))
allocate (dshs_13_1(nmax,nsol_fatigue,3,3))
allocate (dshs_14_1(nmax,nsol_fatigue,3,3))

!  notch stress range
allocate (dsln_11_1(nmax,nsol_fatigue,3,3))
allocate (dsln_12_1(nmax,nsol_fatigue,3,3))
allocate (dsln_13_1(nmax,nsol_fatigue,3,3))
allocate (dsln_14_1(nmax,nsol_fatigue,3,3))

!  damage
allocate (d_11_1(nmax,nsol_fatigue,3,3))
allocate (d_12_1(nmax,nsol_fatigue,3,3))
allocate (d_13_1(nmax,nsol_fatigue,3,3))
allocate (d_14_1(nmax,nsol_fatigue,3,3))


allocate (dd_11_1(nmax,nsol_fatigue,3,3,9,nmax))
allocate (dd_12_1(nmax,nsol_fatigue,3,3,9,nmax))
allocate (dd_13_1(nmax,nsol_fatigue,3,3,9,nmax))
allocate (dd_14_1(nmax,nsol_fatigue,3,3,9,nmax))


allocate (dnx21(nmax,nsol_fatigue,3,10))
allocate (dny21(nmax,nsol_fatigue,3,10))
allocate (dqx21(nmax,nsol_fatigue,3,10))
allocate (dqy21(nmax,nsol_fatigue,3,10))
allocate (dmx21(nmax,nsol_fatigue,3,10))
allocate (dmy21(nmax,nsol_fatigue,3,10))
allocate (dq21 (nmax,nsol_fatigue,3,10))
                                
!		nominal stress range
allocate (ds_21_1(nmax,nsol_fatigue,3,10))
allocate (ds_22_1(nmax,nsol_fatigue,3,10))
allocate (ds_23_1(nmax,nsol_fatigue,3,10))
allocate (ds_24_1(nmax,nsol_fatigue,3,10))

!		hot spot stress range
allocate (dshs_21_1(nmax,nsol_fatigue,3,10))
allocate (dshs_22_1(nmax,nsol_fatigue,3,10))
allocate (dshs_23_1(nmax,nsol_fatigue,3,10))
allocate (dshs_24_1(nmax,nsol_fatigue,3,10))

!		notch stress range
allocate (dsln_21_1(nmax,nsol_fatigue,3,10))
allocate (dsln_22_1(nmax,nsol_fatigue,3,10))
allocate (dsln_23_1(nmax,nsol_fatigue,3,10))
allocate (dsln_24_1(nmax,nsol_fatigue,3,10))

!		damage
allocate (d_21_1(nmax,nsol_fatigue,3,10))
allocate (d_22_1(nmax,nsol_fatigue,3,10))
allocate (d_23_1(nmax,nsol_fatigue,3,10))
allocate (d_24_1(nmax,nsol_fatigue,3,10))



allocate (dd_21_1(nmax,nsol_fatigue,3,10,9,nmax))
allocate (dd_22_1(nmax,nsol_fatigue,3,10,9,nmax))
allocate (dd_23_1(nmax,nsol_fatigue,3,10,9,nmax))
allocate (dd_24_1(nmax,nsol_fatigue,3,10,9,nmax))





allocate (dnx22(nmax,nsol_fatigue,3,2)) 
allocate (dny22(nmax,nsol_fatigue,3,2)) 
allocate (dqx22(nmax,nsol_fatigue,3,2)) 
allocate (dqy22(nmax,nsol_fatigue,3,2)) 
allocate (dmx22(nmax,nsol_fatigue,3,2)) 
allocate (dmy22(nmax,nsol_fatigue,3,2)) 
allocate (dq22 (nmax,nsol_fatigue,3,2)) 

!		nominal stress range
allocate (ds_21_2(nmax,nsol_fatigue,3,2))
allocate (ds_22_2(nmax,nsol_fatigue,3,2))
allocate (ds_23_2(nmax,nsol_fatigue,3,2))
allocate (ds_24_2(nmax,nsol_fatigue,3,2))

!		hot spot stress range
allocate (dshs_21_2(nmax,nsol_fatigue,3,2))
allocate (dshs_22_2(nmax,nsol_fatigue,3,2))
allocate (dshs_23_2(nmax,nsol_fatigue,3,2))
allocate (dshs_24_2(nmax,nsol_fatigue,3,2))

!		notch stress range
allocate (dsln_21_2(nmax,nsol_fatigue,3,2))
allocate (dsln_22_2(nmax,nsol_fatigue,3,2))
allocate (dsln_23_2(nmax,nsol_fatigue,3,2))
allocate (dsln_24_2(nmax,nsol_fatigue,3,2))

!		damage
allocate (d_21_2(nmax,nsol_fatigue,3,2))
allocate (d_22_2(nmax,nsol_fatigue,3,2))
allocate (d_23_2(nmax,nsol_fatigue,3,2))
allocate (d_24_2(nmax,nsol_fatigue,3,2))



allocate (dd_21_2(nmax,nsol_fatigue,3,2,9,nmax))
allocate (dd_22_2(nmax,nsol_fatigue,3,2,9,nmax))
allocate (dd_23_2(nmax,nsol_fatigue,3,2,9,nmax))
allocate (dd_24_2(nmax,nsol_fatigue,3,2,9,nmax))


endif


return 
end
