subroutine allocate_param_restri_global_vector()		

use param_section
use PARAM_SECTION_VECTOR

implicit double precision (a-h,o-z)

! ====================================================================================================
! allocate 
! ====================================================================================================

allocate (xk_vector      (nfile))
allocate (yk_vector      (nfile))
allocate (xkgmin_vector  (nfile))
allocate (xkgmax_vector  (nfile))
allocate (modmin_vector  (nfile))
allocate (imin_vector    (nfile))
allocate (wmax_vector    (nfile))
allocate (pricemax_vector(nfile))
allocate (uhogm_vector   (nfile))
allocate (usagm_vector   (nfile))
allocate (yred_vector    (nfile))



allocate (igrav_vector   (nfile))
allocate (inert_vector   (nfile))
allocate (imod_vector    (nfile))
allocate (imom_vector    (nfile))
allocate (irestr_vector  (nfile))
allocate (iweight_vector (nfile))
allocate (iprice_vector  (nfile))
allocate (ielt_vector    (nfile))
allocate (icost_vector   (nfile))
allocate (ncondi_vector  (nfile))

allocate (nno_vector(nmax_sup+1,2,nfile))
allocate (nno9_vector(nmax_sup+1,2,nfile))                                                                                                                                                             

      xk_vector(:)=0.d00  
      yk_vector(:)=0.d00
  xkgmin_vector(:)=0.d00
  xkgmax_vector(:)=0.d00
  modmin_vector(:)=0.d00
    imin_vector(:)=0.d00                                              
    wmax_vector(:)=0.d00 
pricemax_vector(:)=0.d00
 
  igrav_vector(:)=0
  inert_vector(:)=0
   imod_vector(:)=0
   imom_vector(:)=0
 irestr_vector(:)=0
iweight_vector(:)=0
 iprice_vector(:)=0
   ielt_vector(:)=0
  icost_vector(:)=0
 ncondi_vector(:)=0

nno_vector(:,:,:)=0
nno9_vector(:,:,:)=0

       
end
