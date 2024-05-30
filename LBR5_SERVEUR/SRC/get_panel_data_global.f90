subroutine get_panel_data_global()

use param_section
use PARAM_SECTION_VECTOR

implicit double precision (a-h,o-z)

nsolm            =         nsolm_vector(iboat)
nsol_fatigue     =  nsol_fatigue_vector(iboat)
nsolm_fatigue     =  nsolm_fatigue_vector(iboat)
                                       
indMateriau(:) = indMateriau_vector(:,iboat)

panneau(:) = panneau_vector(:,iboat)

aono(:,:) = aono_vector(:,:,iboat)
z(:,:) = z_vector(:,:,iboat)
noeud(:,:) = noeud_vector(:,:,iboat)

mode(:)       =        mode_vector(:,iboat)    
modes(:)       =       modes_vector(:,iboat)    
xmode(:)       =       xmode_vector(:,iboat)    
                   
!kse   (:)       =      kse_vector   (:,iboat)                     
ipa   (:)       =      ipa_vector   (:,iboat)                     
ivaria(:)       =      ivaria_vector(:,iboat)                     
                                                                              
xi(:,:)     =     xi_vector(:,:,iboat)                 
xf(:,:)     =     xf_vector(:,:,iboat)                 

!part  (:)       =      part_vector  (:,iboat)      
                                                   
m1tabl (:,:) =     m1tabl_vector(:,:,iboat)           
!m1cont (:)         =     m1cont_vector(:,iboat)
m2cont (:)         =     m2cont_vector(:,iboat)
m4cont (:)         =     m4cont_vector(:,iboat)
m5cont (:)         =     m5cont_vector(:,iboat)

noh    (:,:)    =     noh_vector   (:,:,iboat)                
                                                                              
   tfa (:)         =       tfa_vector (:,iboat)                          
   tfr (:)         =       tfr_vector (:,iboat)                          
!   part(:)         =       part_vector(:,iboat)                          
                                                                              
  ypt9 (:,:)=     ypt9_vector(:,:,iboat)          
  ypts (:,:)=     ypts_vector(:,:,iboat)          

npt(:,:) = npt_vector(:,:,iboat)
npt_full(:,:) = npt_full_vector(:,:,iboat)
cha(:,:,:,:) = cha_vector(:,:,:,:,iboat)
cha_full(:,:,:,:) = cha_full_vector(:,:,:,:,iboat)

title_cha_full(:,:) = title_cha_full_vector(:,:,iboat)

title_loadcase(:,:) =     title_loadcase_vector (:,:,iboat)                
                                                                              
!m1(:,:) = m1_vector(:,:,iboat)

xk9 = xk_vector      (iboat)
yk9        =yk_vector      (iboat)
       xkgmin     =xkgmin_vector  (iboat)
       xkgmax     =xkgmax_vector  (iboat)
       modmin     =modmin_vector  (iboat)
       imin       =imin_vector    (iboat)
       wmax       =wmax_vector    (iboat)
       pricemax   =pricemax_vector(iboat)
       uhogm      =uhogm_vector   (iboat)
       usagm      =usagm_vector   (iboat)
       yred       =yred_vector    (iboat)

       bm1 (:) =bm1_vector(:,iboat)
       bm2 (:) =bm2_vector(:,iboat)
       bm3 (:) =bm3_vector(:,iboat)
       bm4 (:) =bm4_vector(:,iboat)
       sf1 (:) =sf1_vector(:,iboat)
       sf3 (:) =sf3_vector(:,iboat)	


       bm11(:) =bm11_vector(:,iboat)
       bm31(:) =bm31_vector(:,iboat)
       sf11(:) =sf11_vector(:,iboat)
       sf31(:) =sf31_vector(:,iboat)	

!*******************************************************************************************
!===========================================================================================
!!ajout des variables de travail relatives à la routine Effort tranchant
!===========================================================================================

TEffortT(:)		= TEffortT_vector(:,iboat)
XEffortT(:)		= XEffortT_vector(:,iboat)
MEffortT(:)		= MEffortT_vector(:,iboat)
PEffortT(:)		= PEffortT_vector(:,iboat)
varoli(:)	    = varoli_vector(:,iboat)
pointeur(:)	    = pointeur_vector(:,iboat)
aqoli(:)	    = aqoli_vector(:,iboat)
FFF(:)	        = FFF_vector(:,iboat)

!===========================================================================================
!===========================================================================================
!*******************************************************************************************



! ====================================================================================================

igrav         =igrav_vector(iboat)
inert         =inert_vector(iboat)
imod           =imod_vector(iboat)
imom           =imom_vector(iboat)
irestr       =irestr_vector(iboat)
iweight     =iweight_vector(iboat)
iprice       =iprice_vector(iboat)
ielt           =ielt_vector(iboat)
icost         =icost_vector(iboat)
ncondi       =ncondi_vector(iboat)

nno(:,:)=nno_vector(:,:,iboat)
nno9(:,:)=nno9_vector(:,:,iboat)

itype(:) = itype_vector(:,iboat)
isect(:) = isect_vector(:,iboat)

sx_raid_sem(:,:,:,:) = sx_raid_sem_vector(:,:,:,:,iboat)
sx_loc_raid_sem(:,:) = sx_loc_raid_sem_vector(:,:,iboat)
sx_plaque_top(:,:,:,:) = sx_plaque_top_vector(:,:,:,:,iboat)
sy_plaque_top(:,:,:,:) = sy_plaque_top_vector(:,:,:,:,iboat)
sxy_plaque_top(:,:,:,:) = sxy_plaque_top_vector(:,:,:,:,iboat)
sx_plaque_bott(:,:,:,:) = sx_plaque_bott_vector(:,:,:,:,iboat)
sy_plaque_bott(:,:,:,:) = sy_plaque_bott_vector(:,:,:,:,iboat)
sxy_plaque_bott(:,:,:,:) = sxy_plaque_bott_vector(:,:,:,:,iboat)
sy_cadre_jab(:,:,:,:) = sy_cadre_jab_vector(:,:,:,:,iboat)
sxy_cadre_jab(:,:,:,:) = sxy_cadre_jab_vector(:,:,:,:,iboat)
sy_cadre_sem(:,:,:,:) = sy_cadre_sem_vector(:,:,:,:,iboat)

! ====================================================================================================   

return
end
