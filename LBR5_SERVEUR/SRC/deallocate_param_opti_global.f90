subroutine deallocate_param_opti_global()		


use param_section

!deallocate (index_begin)
!deallocate (index_end)

deallocate (qn_global)
deallocate (philn_global)
deallocate (modes_global)
deallocate (m1tabl_global)
deallocate (m2cont_global)
deallocate (m4cont_global)
deallocate (m5cont_global)

deallocate (igeom_index)
deallocate (igrav_index)
deallocate (inert_index)
deallocate ( imod_index)

deallocate (iweight_index)
deallocate (iprice_index)
deallocate (istruc_index)
deallocate (irestr_index)

deallocate (vnom2)
deallocate (vnom2_global)

deallocate (ig_conlin)
deallocate (jt_conlin)
!deallocate (ms_conlin)

deallocate (kcont)

deallocate (xicou)
deallocate (ximin)
deallocate (ximax)

deallocate (xicou_global)
deallocate (ximin_global)
deallocate (ximax_global)

deallocate (nr)
deallocate (ntot)
deallocate (mtot)
deallocate (m1tot)
deallocate (m2tot)
deallocate (m3tot)
deallocate (m4tot)
deallocate (m5tot)

deallocate (ega)
deallocate (mega)
deallocate (nvs)
deallocate (negal)
deallocate (negalt)

deallocate (nxi2)
deallocate (nxit)
deallocate (nvar)

deallocate (nvarr)
deallocate (nxitr)
deallocate (nxi2r)

deallocate (obj)
deallocate (obj2)
deallocate (jobj)
deallocate (objmulti)
deallocate (obj2multi)

deallocate (fiopt)			
deallocate (fiopt_global)

deallocate (xiopt)			
deallocate (cjopt)			
deallocate (cjmopt)		
deallocate (cijopt)

deallocate (ic_rest)
deallocate (ratio_rest)

deallocate (dxopt)
deallocate (z1opt)			
deallocate (z2opt)			
deallocate (z3opt)			

deallocate (sopt)

deallocate (fimulti)
deallocate (fimulti_global)
deallocate (derredress)

deallocate (shift_opt)

deallocate (xiopt_global)
deallocate (cjopt_global)			
deallocate (cjmopt_global)		
deallocate (cijopt_global)

deallocate (ic_rest_global)
deallocate (ratio_rest_global)

deallocate (dxopt_global)
deallocate (z1opt_global)
deallocate (z2opt_global)
deallocate (z3opt_global)

if (allocated(ega_global)) deallocate ( ega_global)
if (allocated(mega_global)) deallocate (mega_global)
deallocate (negal_global)
deallocate (  nvs_global)

deallocate (nxi2_global)
deallocate (nxi2r_global)
deallocate (nxit_global)
deallocate (nvar_global)

deallocate (nvarr_temp_global)
deallocate (nxitr_temp_global)

deallocate (nvarr_global)
deallocate (nxitr_global)

deallocate (bornestiffmin_global)
deallocate (bornestiffmax_global)
deallocate (bornestiff_global)
deallocate (bornestiffmode_global)
deallocate (bornestifflargeur_global)

return 
end
