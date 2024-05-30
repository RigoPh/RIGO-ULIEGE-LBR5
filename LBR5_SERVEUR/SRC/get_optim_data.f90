subroutine get_optim_data()

use param_section
use PARAM_SECTION_VECTOR

implicit double precision (a-h,o-z)


!lcont         (1:2,1:m1tmax)               =lcont_vector  (1:2,1:m1tmax,iboat)  
!lcont4        (1:nsolmax,1:nmax,1:m1max,1:2) =lcont4_vector (1:nsolmax,1:nmax,1:m1max,1:2,iboat) 

!ipts    (1:nmax)              =ipts_vector(1:nmax,iboat)
!ipts2   (1:iptmax,1:nmax)     =ipts2_vector(1:iptmax,1:nmax,iboat)
!ipts3   (1:10,1:nmax)         =ipts3_vector(1:10,1:nmax,iboat)
!cjmax   (1:m1tmax)                       =cjmax_vector  (1:m1tmax,iboat)
!cjmax3  (1:nsolmax,1:nmax,1:m1max)         =cjmax3_vector (1:nsolmax,1:nmax,1:m1max,iboat)

!inv     (1:m1tmax)                 =inv_vector    (1:m1tmax,iboat) 
!inv3    (1:nsolmax,1:nmax,1:m1max)   =inv3_vector   (1:nsolmax,1:nmax,1:m1max,iboat)

!lm2(1:ngmax,1:nmax) = lm2_vector(1:ngmax,1:nmax,iboat) 
!nxi2    (1:9,1:nmax)     =nxi2_vector   (1:9,1:nmax,iboat)

ypts    (1:iptmax,1:nmax)=   ypts_vector(1:iptmax,1:nmax,iboat)          

! heff (1:nmax)    =   heff_vector (1:nmax,iboat)                       
       
tfa(1:nmax)           =     tfa_vector(1:nmax,iboat)
tfr(1:nmax)           =     tfr_vector(1:nmax,iboat)

!dvmin(1:9,1:nmax)     =   dvmin_vector(1:9,1:nmax,iboat)                        
!dvmax(1:9,1:nmax)     =   dvmax_vector(1:9,1:nmax,iboat)                        

indMateriau(1:nmax) = indMateriau_vector(1:nmax,iboat)    

mode(1:nmax)          =        mode_vector(1:nmax,iboat)  ! TODO mode(nel) à supprimer
modes(1:nmax)         =       modes_vector(1:nmax,iboat)    
xmode(1:nmax)         =       xmode_vector(1:nmax,iboat)    
abcd (1:8)            =     abcd_vector(1:8,iboat)

return
end





























































































