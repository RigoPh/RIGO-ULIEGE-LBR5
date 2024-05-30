subroutine loop()

use param_section

implicit double precision (a-h,o-z)

call initialize_loop()  ! ecritures diverses
!==============================================================================
! 8.3 initialisation de ifonct
!==============================================================================
ifonct(1)=1      
ifonct(2)=0                       
! ifonct(terme impair)=1 pour termes 1,3,7,..     
! ifonct(terme pair  )=0 pour termes 2,4,6,...    
! si cas de charges symétriques selon x           
! ifonct(terme impair)=1 pour termes 1,3,7,..    
! ifonct(terme pair  )=0 pour termes 2,4,6,...                
! si cas de charges non symétriques selon x                        
! ifonct =1 pour tous les termes.                                 
!==============================================================================
! 9.0 lecture des donnees relatives a chaque panneau et calcul des 
!     grandeurs caracteristiques   (subr. ent) 
!==============================================================================
! iff=1 si error dans ent  
iff  =0        
! icha  indice de charges variables selon ox
!ichag=0        
pot  =0.d00
ntot (iboat) =0
mtot (iboat) =0
m1tot(iboat) =0
m2tot(iboat) =0
m3tot(iboat) =0
m4tot(iboat) =0
spoids1       =0.d00
!cha(:,:,:)    =0.d00
!dcha(:,:,:)   =0.d00

!!!!!!!!!!!!!!!
fiopt(:,iboat) = 0.0d00
!xicou (:,iboat)      =0.0d00
!ximin (:,iboat)      =0.0d00
!ximax (:,iboat)      =0.0d00
! ega  (:,iboat)      =0.d00
!mega  (:,:,iboat)    =0
!  nvs (:,iboat)      =0
!negal (:,iboat)      =0
!negalt(iboat)        =0
!nxi2  (:,:,iboat)    =0      
!nxit  (:,:,iboat)    =0
!nvar  (:,iboat)      =0
!nxitr  (:,:,iboat)   =0
!nvarr  (:,iboat)     =0
!nxi2r  (:,:,iboat)   =0
ic_rest(:,iboat)     =0
ratio_rest(:,iboat)  =0.d00
!sopt (iboat)      =0.d00
xiopt (:,iboat)   =0.d00
cjopt (:,iboat)   =0.d00
cjmopt(:,iboat)	  =0.d00	
cijopt(:,:,iboat) =0.d00
dxopt(:,:,iboat)    =0.d00
!!!!!!!!!!!!!!!

!call read_panel_XML()
!call write_panel_XML()
               
call read_write_panel()   !previously ENTS


call coord_transform()
call init_objective()
call edge_moments()  
call equal_restrictions()
call ultimate_resistance()



!call ansys()

jd=0
if (jd.eq.1) call write_lotStat() !calcul spécial pr JD

if(nsol_fatigue.gt.0) then
!!!   call allocate_param_fatigue(neto,nsolm_fatigue,nsol_fatigue)
endif

if(iana.eq.2) then
   call beam_theory()
else
   call lbr4()
endif

if(nsol_fatigue.gt.0) then
!!!   call deallocate_param_fatigue()
endif

return
end
