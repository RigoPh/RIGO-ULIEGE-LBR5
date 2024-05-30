subroutine deallocate_param_section_header()

use PARAM_SECTION_VECTOR

deallocate  (nom_vector)
deallocate  (header1_vector)
deallocate  (header2_vector)

deallocate  (iunit1_vector)
deallocate  (iunit2_vector)
deallocate  (iunit3_vector)
deallocate  (iunit4_vector)
deallocate  (iunit5_vector)   
deallocate  (iunit6_vector)
deallocate  (iunit7_vector)
deallocate  (iunit8_vector)
deallocate  (iunit9_vector)

deallocate (iunit12_vector)

deallocate (nnsol_vector)

deallocate    (neto_vector)
deallocate   (ngmax_vector)
deallocate    (iana_vector)
deallocate  (indaig_vector)
deallocate  (indrai_vector)
deallocate   (ibusc_vector)
deallocate  (ipoids_vector)
deallocate   (nsolm_vector)
deallocate   (nsol_fatigue_vector)
deallocate   (nsolm_fatigue_vector)

deallocate    (part_full_vector)
deallocate    (method_fatigue_vector)	
deallocate    (tfl_vector)		

deallocate    (nsol_vector)
deallocate      (ne_vector)
deallocate    (imax_vector)
deallocate    (jlph_vector)
deallocate   (jlph2_vector)
deallocate  (iprint_vector)

deallocate  ( depth_vector)
deallocate  (length_vector)	
deallocate   (width_vector)

deallocate   (dis_vector)
deallocate   (fam_vector)

deallocate (casename_vector)
!deallocate (is_loadcase_selected_vector)
deallocate (is_loadcase_fatigue_vector)
deallocate (NNSOL_BV_TYPE_vector)			!fat_new
deallocate (is_selected_loadcase_fatigue_vector)
deallocate (tirant_vector)

! ====================================================================================================
! deallocate param_cout_vector
! ====================================================================================================


deallocate (icout_vector    )
deallocate (iredress_vector )
deallocate (refthick_input_vector)

deallocate (poids_vector    )
deallocate (spoids_vector   )

deallocate (fmat11_vector   )
deallocate (fmat22_vector   )
deallocate (fmat33_vector   )
deallocate (fsou11_vector   )
deallocate (fsou22_vector   )
deallocate (fsou33_vector   )
deallocate (fmdo11_vector   )
deallocate (fmdo22_vector   )
deallocate (fmdo33_vector   )
deallocate (fmdo44_vector   )
deallocate (fmdo55_vector   )
deallocate (fmdo66_vector   )
deallocate (fmdo77_vector   )   

deallocate (rend_global_vector)
!deallocate (eqp_global_vector )

deallocate (nbrMat_vector)

deallocate (lcc2_vector)
deallocate (lcc3_vector)
deallocate (lcc4_vector)
deallocate (lcc5_vector)
deallocate (lightweight_vector)
deallocate (lightweight_init_vector)
deallocate (deadweight_vector)
deallocate (deadweight_init_vector)
deallocate (poidsLBR5_init_vector)
deallocate (lccscenario_vector)

! ====================================================================================================
! deallocate param_multiobj_vector
! ====================================================================================================

deallocate (imulti_vector   )
deallocate (w1_vector   )
deallocate (w2_vector   )
deallocate (w3_vector   )
deallocate (rho_vector   )
deallocate (fk_vector)


deallocate (ipareto_vector     )
deallocate (nsteppar_vector    )
deallocate (iparcout_vector    )
deallocate (iparpoids_vector   )
deallocate (iparinertia_vector )

return
end
