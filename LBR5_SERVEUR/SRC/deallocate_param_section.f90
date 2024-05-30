subroutine deallocate_param_section()	


use param_section


implicit double precision (a-h,o-z)

!call annuli(langue,1)
neto = 0
iana = 0

! ====================================================================================================
! valeurs des variables de l'ancien common dim1 
! ====================================================================================================

nmax=0
nemax=0
nvmax=0
m1max=0
m1tmax=0
m2max=0
m2tmax=0
!mmmax=0
ngmax=0
iptmax=0

! ====================================================================================================
! allocation des moments
! ====================================================================================================

deallocate (bm1)
deallocate (bm2)
deallocate (bm3)
deallocate (bm4)
deallocate (sf1)
deallocate (sf3)	

deallocate (bm11)
deallocate (bm31)
deallocate (sf11)
deallocate (sf31)	

!*******************************************************************************************
!===========================================================================================
!!ajout des valeurs de travail
!===========================================================================================

deallocate (TEffortT)
deallocate (MEffortT)	
deallocate (PEffortT)
deallocate (XEffortT)
deallocate (varoli)					
deallocate (pointeur)
deallocate (aqoli)
deallocate (FFF)
deallocate (ptri)
deallocate (GMT)
! ====================================================================================================
! allocation des tableaux et matrices
! ====================================================================================================e

! ====================================================================================================
! allocation des tableaux et matrices
! ====================================================================================================

deallocate (indMateriau)

deallocate (sxm)

deallocate (modes)
deallocate (blocka)
deallocate (solt)
deallocate (sens1)
deallocate (sens2)
deallocate (sens3)
deallocate (sens4)

deallocate (a)
deallocate (b)
deallocate (dzsn)
deallocate (tetas)
deallocate (tetaq)
deallocate (z)
deallocate (noh)
deallocate (noeud)
deallocate (mcomp)

deallocate (nno)
deallocate (nno9)

deallocate (nsign)

deallocate (ypts)
deallocate (ypt9)
!deallocate (m1cont)
deallocate (m2cont)
deallocate (m4cont)
deallocate (m5cont)
deallocate (kli)
deallocate (zsn)
deallocate (abc)
deallocate (asol)
deallocate (bsol)
deallocate (csol)
deallocate (dsol)
deallocate (amom)
deallocate (bmom)
deallocate (cmom)
deallocate (dmom)

deallocate (abcd)
deallocate (coef)
!deallocate (icha)
deallocate (itype)
deallocate (isect)
deallocate (tfa)
deallocate (tfr)
deallocate (m1tabl)
!deallocate (izut)
!deallocate (zut)
!deallocate (corro)
!deallocate (corroel)

! ====================================================================================================
! variables ushull 
! ====================================================================================================

deallocate (dsud)
deallocate (dsub)
deallocate (dsub1)
deallocate (dsusu)
deallocate (dsusl)
deallocate (dh)
deallocate (dg)
deallocate (dad)
deallocate (dab)
deallocate (dab1)
deallocate (das)
deallocate (dult)
deallocate (id)				
deallocate (ib1)			
deallocate (ib)				
deallocate (is9)			


! ====================================================================================================
! variables sensibcout 
! ====================================================================================================

deallocate (vlarg)
deallocate (vepaiss)
deallocate (vhac)
deallocate (veac)
deallocate (vlsc)
deallocate (vesc)
deallocate (deltacsc)
deallocate (vhar)
deallocate (vear)
deallocate (vlsr)
deallocate (vesr)
deallocate (deltar)
deallocate (entrsc)
deallocate (philsc)
deallocate (qsc)
deallocate (coutpan)
deallocate (costpan)
deallocate (coutmat)
!deallocate (dcorsc)
!deallocate (dcorsc2)
!deallocate (dcorsc3)
deallocate (vdiffc)

!    variables coord ================================================================================
deallocate (aiyy)
deallocate (aixx)
deallocate (iyy2)
deallocate (aiyy2)
deallocate (aixx2)
deallocate (aixxpart)
deallocate (aono)

! ====================================================================================================
!	variables inertia 
! ====================================================================================================

deallocate (dyneutnet)
deallocate (dyneutgro)
deallocate (dinet)
deallocate (digro)
deallocate (dyneutplnet)
deallocate (dyneutplgro)
deallocate (diplnet)
deallocate (diplgro)
deallocate (dsignet)
deallocate (dsiggro)

! ====================================================================================================
!	variables shear 
! ====================================================================================================

deallocate (taunet)
deallocate (taugro)
deallocate (dtaunet)
deallocate (dtaugro)

! ====================================================================================================
!	variables buckdcn 
! ====================================================================================================

deallocate (ddbuckpl_07)				!dcn07
deallocate (ddbuckstif_07)			    !dcn07
deallocate (dinpart_07)					!dcn07

!	variables buckdcn_08
deallocate (ddbuckpl_08)				!dcn08
deallocate (ddbuckstif_08)			    !dcn08
deallocate (dinpart_08)					!dcn08

!	variables dcn08
!deallocate (itype_transv)
!deallocate (itype_longit)
!deallocate (itype_transv_second)
!deallocate (itype_longit_second)

!deallocate (name_type_transv)
!deallocate (name_type_transv_second)
!deallocate (name_type_longit)
!deallocate (name_type_longit_second)


! ancien common hyp    ===============================================================================

deallocate (dvarh)
deallocate (sensh)

deallocate (hyp)           
deallocate (ali)

deallocate (alix)
deallocate (aliz)

deallocate (dalix)
deallocate (daliz)
deallocate (dhyp)
deallocate (dish)



! ====================================================================================================
!	variables fatigue
! ====================================================================================================

!!!!!!!!!!!TODO : pas allouer ça si pas nécessaire => mettre la bonne condition !!!
!ifred=0
!if (ifred.eq.0) then
!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!

return

!!!!!!!!!!!!!!!!!!!!!!!!!!

if (nsol_fatigue.NE.0.) then	!New11		!pas allouer si pas DE CALCUL FATIGUE

deallocate (damage_stiffened_pannel)
deallocate (damage_fatigue)
deallocate (index_damage_stiffened_pannel)
deallocate (ddamage_fatigue)

!	stiffened plate sty=1 

deallocate (dfull_11)
deallocate (dfull_12)
deallocate (dfull_13)
deallocate (dfull_14)

!	girder sty=2 and sty2=1

deallocate (dfull_21_1)
deallocate (dfull_22_1)
deallocate (dfull_23_1)
deallocate (dfull_24_1)

!	web frame  sty=2 sty2=2

deallocate (dfull_21_2)
deallocate (dfull_22_2)
deallocate (dfull_23_2)
deallocate (dfull_24_2)


!	stiffened plate sty=1 

deallocate (dball_11)
deallocate (dball_12)
deallocate (dball_13)
deallocate (dball_14)

!	girder sty=2 and sty2=1

deallocate (dball_21_1)
deallocate (dball_22_1)
deallocate (dball_23_1)
deallocate (dball_24_1)

!	web frame  sty=2 sty2=2

deallocate (dball_21_2)
deallocate (dball_22_2)
deallocate (dball_23_2)
deallocate (dball_24_2)

!	stiffened plate sty=1 

deallocate (ddfull_11)
deallocate (ddfull_12)
deallocate (ddfull_13)
deallocate (ddfull_14)

!	girder sty=2 and sty2=1
deallocate (ddfull_21_1)
deallocate (ddfull_22_1)
deallocate (ddfull_23_1)
deallocate (ddfull_24_1)

!	web frame  sty=2 sty2=2
deallocate (ddfull_21_2)
deallocate (ddfull_22_2)
deallocate (ddfull_23_2)
deallocate (ddfull_24_2)
  
!	stiffened plate sty=1 

deallocate (ddball_11)
deallocate (ddball_12)
deallocate (ddball_13)
deallocate (ddball_14)

!	girder sty=2 and sty2=1
deallocate (ddball_21_1)
deallocate (ddball_22_1)
deallocate (ddball_23_1)
deallocate (ddball_24_1)

!	web frame  sty=2 sty2=2

deallocate (ddball_21_2)
deallocate (ddball_22_2)
deallocate (ddball_23_2)
deallocate (ddball_24_2)

!	stiffened plate sty=1 

deallocate (dtot_11)
deallocate (dtot_12)
deallocate (dtot_13)
deallocate (dtot_14)

!	girder sty=2 and sty2=1

deallocate (dtot_21_1)
deallocate (dtot_22_1)
deallocate (dtot_23_1)
deallocate (dtot_24_1)

!	web frame  sty=2 sty2=2

deallocate (dtot_21_2)
deallocate (dtot_22_2)
deallocate (dtot_23_2)
deallocate (dtot_24_2)

!	stiffened plate sty=1 

deallocate (ddtot_11)
deallocate (ddtot_12)
deallocate (ddtot_13)
deallocate (ddtot_14)

!	girder sty=2 and sty2=1

deallocate (ddtot_21_1)
deallocate (ddtot_22_1)
deallocate (ddtot_23_1)
deallocate (ddtot_24_1)

!	web frame  sty=2 sty2=2


deallocate (ddtot_21_2)
deallocate (ddtot_22_2)
deallocate (ddtot_23_2)
deallocate (ddtot_24_2)

!====================================================================

deallocate (dnx11)
deallocate (dny11) 
deallocate (dqx11) 
deallocate (dqy11) 
deallocate (dmx11) 
deallocate (dmy11) 
deallocate (dq11 ) 


deallocate (ds_11_1)
deallocate (ds_12_1)
deallocate (ds_13_1)
deallocate (ds_14_1)
            
!  hot spot stress range
deallocate (dshs_11_1)
deallocate (dshs_12_1)
deallocate (dshs_13_1)
deallocate (dshs_14_1)

!  notch stress range
deallocate (dsln_11_1)
deallocate (dsln_12_1)
deallocate (dsln_13_1)
deallocate (dsln_14_1)

!  damage
deallocate (d_11_1)
deallocate (d_12_1)
deallocate (d_13_1)
deallocate (d_14_1)


deallocate (dd_11_1)
deallocate (dd_12_1)
deallocate (dd_13_1)
deallocate (dd_14_1)


deallocate (dnx21)
deallocate (dny21)
deallocate (dqx21)
deallocate (dqy21)
deallocate (dmx21)
deallocate (dmy21)
deallocate (dq21 )
                                
!		nominal stress range
deallocate (ds_21_1)
deallocate (ds_22_1)
deallocate (ds_23_1)
deallocate (ds_24_1)

!		hot spot stress range
deallocate (dshs_21_1)
deallocate (dshs_22_1)
deallocate (dshs_23_1)
deallocate (dshs_24_1)

!		notch stress range
deallocate (dsln_21_1)
deallocate (dsln_22_1)
deallocate (dsln_23_1)
deallocate (dsln_24_1)

!		damage
deallocate (d_21_1)
deallocate (d_22_1)
deallocate (d_23_1)
deallocate (d_24_1)



deallocate (dd_21_1)
deallocate (dd_22_1)
deallocate (dd_23_1)
deallocate (dd_24_1)





deallocate (dnx22) 
deallocate (dny22) 
deallocate (dqx22) 
deallocate (dqy22) 
deallocate (dmx22) 
deallocate (dmy22) 
deallocate (dq22 ) 

!		nominal stress range
deallocate (ds_21_2)
deallocate (ds_22_2)
deallocate (ds_23_2)
deallocate (ds_24_2)

!		hot spot stress range
deallocate (dshs_21_2)
deallocate (dshs_22_2)
deallocate (dshs_23_2)
deallocate (dshs_24_2)

!		notch stress range
deallocate (dsln_21_2)
deallocate (dsln_22_2)
deallocate (dsln_23_2)
deallocate (dsln_24_2)

!		damage
deallocate (d_21_2)
deallocate (d_22_2)
deallocate (d_23_2)
deallocate (d_24_2)



deallocate (dd_21_2)
deallocate (dd_22_2)
deallocate (dd_23_2)
deallocate (dd_24_2)

endif

return
end
