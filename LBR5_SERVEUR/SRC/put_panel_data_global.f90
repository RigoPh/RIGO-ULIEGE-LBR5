subroutine put_panel_data_global()

use param_section
use PARAM_SECTION_VECTOR

implicit double precision (a-h,o-z)

panneau_vector(1:nmax,iboat) = panneau(1:nmax)

nsol_fatigue_vector(iboat)=nsol_fatigue    
nsolm_fatigue_vector(iboat)=nsolm_fatigue    

aono_vector(1:nmax+1,:,iboat) = aono(1:nmax+1,:)
z_vector(1:nmax,:,iboat) = z(1:nmax,:)
noeud_vector(1:nmax,:,iboat) = noeud(1:nmax,:)

nsolm_vector(iboat)      =  nsolm
                            
indMateriau_vector(1:nmax,iboat)   = indMateriau(1:nmax)   

mode_vector(1:nmax,iboat)   = mode(1:nmax)		!TODO a supprimer
mode_vector(1:nmax,iboat)   = panneau(1:nmax).mode
modes_vector(1:nmax,iboat)  = modes(1:nmax)   
xmode_vector(1:nmax,iboat)  = xmode(1:nmax)

!kse_vector   (1:nmax,iboat)   =    kse   (1:nmax)
ipa_vector   (1:nmax,iboat)   =    ipa   (1:nmax)
ivaria_vector(1:nmax,iboat)   =    ivaria(1:nmax)
                                  
xi_vector(1:nsol,1:nmax,iboat)  =   xi(1:nsol,1:nmax)
xf_vector(1:nsol,1:nmax,iboat)  =   xf(1:nsol,1:nmax)

!part_vector(1:nmax,iboat) = part  (1:nmax)                     

m1tabl_vector(1:nmax,1:nsolmax,iboat)  = m1tabl (1:nmax,1:nsolmax)
!m1cont_vector(1:nmax,iboat)         = m1cont (1:nmax)        
m2cont_vector(1:nmax,iboat)         = m2cont (1:nmax)
m4cont_vector(1:nmax,iboat)         = m4cont (1:nmax)
m5cont_vector(1:nmax,iboat)         = m5cont (1:nmax)

noh_vector   (1:nmax,1:10,iboat)    =noh     (1:nmax,1:10)  

tfa_vector (1:nmax,iboat)           =    tfa (1:nmax)
tfr_vector (1:nmax,iboat)           =    tfr (1:nmax)
!part_vector(1:nmax,iboat)           =    part(1:nmax)

ypt9_vector (1:iptmax,1:nmax,iboat)  =   ypt9 (1:iptmax,1:nmax)
ypts_vector (1:iptmax,1:nmax,iboat)  =   ypts (1:iptmax,1:nmax)

npt_vector(1:nsol,1:nmax,iboat) = npt(1:nsol,1:nmax)
npt_full_vector(1:nsolmax,1:nmax,iboat) = npt_full(1:nsolmax,1:nmax)
cha_vector(1:50,1:3,1:nsolm,1:nmax,iboat) = cha(1:50,1:3,1:nsolm,1:nmax)
cha_full_vector(1:50,1:3,1:nsolm,1:nmax,iboat) = cha_full(1:50,1:3,1:nsolm,1:nmax)

title_loadcase_vector (1:nsolm,1:nmax,iboat)     =   title_loadcase(1:nsolm,1:nmax)
                                    
!m1_vector   (1:nsolm,1:nmax,iboat)  =   m1   (1:nsolm,1:nmax)

title_cha_full_vector(1:nsolm,1:nmax,iboat) = title_cha_full (1:nsolm,1:nmax)

itype_vector(1:nmax,iboat) = itype(1:nmax)
isect_vector(1:nmax,iboat) = isect(1:nmax)

! ====================================================================================================

xk_vector      (iboat)              =   xk9        
yk_vector      (iboat)              =   yk9        
xkgmin_vector  (iboat)              =   xkgmin    
xkgmax_vector  (iboat)              =   xkgmax    
modmin_vector  (iboat)              =   modmin    
imin_vector    (iboat)              =   imin      
wmax_vector    (iboat)              =   wmax      
pricemax_vector(iboat)              =   pricemax  
uhogm_vector   (iboat)              =   uhogm
usagm_vector   (iboat)              =   usagm
yred_vector   (iboat)               =   yred

bm1_vector(1:nsolm,iboat)               =   bm1 (1:nsolm) 
bm2_vector(1:nsolm,iboat)               =   bm2 (1:nsolm) 
bm3_vector(1:nsolm,iboat)               =   bm3 (1:nsolm) 
bm4_vector(1:nsolm,iboat)               =   bm4 (1:nsolm) 
sf1_vector(1:nsolm,iboat)               =   sf1 (1:nsolm) 
sf3_vector(1:nsolm,iboat)               =   sf3 (1:nsolm) 


bm11_vector(1:nsolm,iboat)              =   bm11(1:nsolm)
bm31_vector(1:nsolm,iboat)              =   bm31(1:nsolm)
sf11_vector(1:nsolm,iboat)              =   sf11(1:nsolm)
sf31_vector(1:nsolm,iboat)	            =   sf31(1:nsolm)  

!*******************************************************************************************
!===========================================================================================
!!ajout des variables de travail relatives à l'Effort Tranchant 
!===========================================================================================

TEffortT_vector(1:nsolm,iboat)=TEffortT(1:nsolm)
XEffortT_vector(1:nsolm,iboat)=XEffortT(1:nsolm)
MEffortT_vector(1:nsolm,iboat)=MEffortT(1:nsolm)
PEffortT_vector(1:nsolm,iboat)=PEffortT(1:nsolm)
varoli_vector(1:nsolm,iboat)=varoli(1:nsolm)
pointeur_vector(1:nmax,iboat)=pointeur(1:nmax)
FFF_vector(1:nmax,iboat)= FFF(1:nmax)

!===========================================================================================
!===========================================================================================
!*******************************************************************************************



   igrav_vector(iboat)              =   igrav       
   inert_vector(iboat)              =   inert       
    imod_vector(iboat)              =   imod        
    imom_vector(iboat)              =   imom        
  irestr_vector(iboat)              =   irestr      
 iweight_vector(iboat)              =   iweight     
  iprice_vector(iboat)              =   iprice      
    ielt_vector(iboat)              =   ielt        
   icost_vector(iboat)              =   icost       
  ncondi_vector(iboat)              =   ncondi      


nno_vector(1:nmax+1,1:2,iboat)        =   nno(1:nmax+1,1:2)
nno9_vector(1:nmax+1,1:2,iboat)        =   nno9(1:nmax+1,1:2)           

! ====================================================================================================                     

return
end
