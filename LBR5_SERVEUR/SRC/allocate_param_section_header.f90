subroutine allocate_param_section_header()

use param_section
use PARAM_SECTION_VECTOR

implicit double precision (a-h,o-z)

! ====================================================================================================
! allocate use PARAM_SECTION_VECTOR
! ====================================================================================================

allocate  (nom_vector(nfile))
allocate  (header1_vector(nfile))
allocate  (header2_vector(nfile))

allocate  (iunit1_vector(nfile))
allocate  (iunit2_vector(nfile))
allocate  (iunit3_vector(nfile))
allocate  (iunit4_vector(nfile))
allocate  (iunit5_vector(nfile))   
allocate  (iunit6_vector(nfile))
allocate  (iunit7_vector(nfile))
allocate  (iunit8_vector(nfile))
allocate  (iunit9_vector(nfile))
!allocate (iunit10_vector(nfile))
!allocate (iunit11_vector(nfile))
allocate (iunit12_vector(nfile))

allocate (nnsol_vector(nsolmax,nfile))

allocate    (neto_vector(nfile))
allocate   (ngmax_vector(nfile))
allocate    (iana_vector(nfile))
allocate  (indaig_vector(nfile))
allocate  (indrai_vector(nfile))
allocate   (ibusc_vector(nfile))
allocate  (ipoids_vector(nfile))
allocate   (nsolm_vector(nfile))
allocate   (nsol_fatigue_vector(nfile))
allocate   (nsolm_fatigue_vector(nfile))

allocate    (part_full_vector(nfile))
allocate    (method_fatigue_vector(nfile))	
allocate    (tfl_vector(nfile))		

allocate    (nsol_vector(nfile))
allocate      (ne_vector(nfile))
allocate    (imax_vector(nfile))
allocate    (jlph_vector(nfile))
allocate   (jlph2_vector(nfile))
allocate  (iprint_vector(nfile))

allocate  ( depth_vector(nfile))
allocate  (length_vector(nfile))	
allocate   (width_vector(nfile))

allocate   (dis_vector(5,nfile))
allocate   (fam_vector(6,nfile))

allocate (casename_vector(nsolmax,nfile))
!allocate (is_loadcase_selected_vector(nsolmax,nfile))
allocate (is_loadcase_fatigue_vector(nsolmax,nfile))		!fat_new
!allocate (is_loadcase_fatigue_vector(nsol_fatigue,nfile))		!fat_new

allocate (NNSOL_BV_TYPE_vector(nsolmax,nfile))		!fat_new
!allocate (NNSOL_BV_TYPE_vector(nsol_fatigue,nfile))		!fat_new

allocate (is_selected_loadcase_fatigue_vector(nsolmax,nfile))		!fat_new
!allocate (is_selected_loadcase_fatigue_vector(nsol_fatigue,nfile))		!fat_new

allocate (tirant_vector(nsolmax,nfile))

! ====================================================================================================
! allocate param_cout_vector
! ====================================================================================================

allocate (icout_vector    (nfile))
allocate (iredress_vector (nfile))
allocate (refthick_input_vector (nfile))

allocate (poids_vector    (nfile))
allocate (spoids_vector   (nfile))

allocate (fmat11_vector   (nfile))
allocate (fmat22_vector   (nfile))
allocate (fmat33_vector   (nfile))
allocate (fsou11_vector   (nfile))
allocate (fsou22_vector   (nfile))
allocate (fsou33_vector   (nfile))
allocate (fmdo11_vector   (nfile))
allocate (fmdo22_vector   (nfile))
allocate (fmdo33_vector   (nfile))
allocate (fmdo44_vector   (nfile))
allocate (fmdo55_vector   (nfile))
allocate (fmdo66_vector   (nfile))
allocate (fmdo77_vector   (nfile))   

allocate (rend_global_vector(nfile))
!allocate (eqp_global_vector (nfile))

allocate (nbrMat_vector(nfile))

allocate (lcc2_vector(nfile))
allocate (lcc3_vector(nfile))
allocate (lcc4_vector(nfile))
allocate (lcc5_vector(nfile))
allocate (lightweight_vector(nfile))
allocate (lightweight_init_vector(nfile))
allocate (deadweight_vector(nfile))
allocate (deadweight_init_vector(nfile))
allocate (poidsLBR5_init_vector(nfile))
allocate (lccscenario_vector(nfile))

! ====================================================================================================
! allocate param_multiobj_vector
! ====================================================================================================

allocate (imulti_vector   (nfile))
allocate (w1_vector   (nfile))
allocate (w2_vector   (nfile))
allocate (w3_vector   (nfile))
allocate (rho_vector   (nfile))
allocate (fk_vector   (3,nfile))							

allocate (ipareto_vector     (nfile))
allocate (nsteppar_vector    (nfile))
allocate (iparcout_vector    (nfile))
allocate (iparpoids_vector   (nfile))
allocate (iparinertia_vector (nfile))

return
end
