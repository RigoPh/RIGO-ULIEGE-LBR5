subroutine deallocate_param_restri_global_vector()

use PARAM_SECTION_VECTOR

deallocate (xk_vector      )
deallocate (yk_vector      )
deallocate (xkgmin_vector  )
deallocate (xkgmax_vector  )
deallocate (modmin_vector  )
deallocate (imin_vector    )
deallocate (wmax_vector    )
deallocate (pricemax_vector)
deallocate (uhogm_vector   )
deallocate (usagm_vector   )
deallocate (yred_vector    )

if (allocated(igrav_vector)) deallocate (igrav_vector   )
if (allocated(inert_vector)) deallocate (inert_vector   )
if (allocated(imod_vector)) deallocate (imod_vector    )
if (allocated(imom_vector)) deallocate (imom_vector    )
if (allocated(irestr_vector)) deallocate (irestr_vector  )
if (allocated(iweight_vector)) deallocate (iweight_vector )
if (allocated(iprice_vector)) deallocate (iprice_vector  )
if (allocated(ielt_vector)) deallocate (ielt_vector    )
if (allocated(icost_vector)) deallocate (icost_vector   )
if (allocated(ncondi_vector)) deallocate (ncondi_vector  )

deallocate (nno_vector)
deallocate (nno9_vector)

return
end
