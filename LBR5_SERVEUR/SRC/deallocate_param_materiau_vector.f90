subroutine deallocate_param_materiau_vector()

use PARAM_SECTION_VECTOR

deallocate (e_vector)
deallocate (eta_vector)
deallocate (sigy_vector)
deallocate (sigyadm_vector)
deallocate (coefk_vector)
deallocate (spec_vector)

deallocate (dref_vector)
deallocate (drefx_vector )
deallocate (drefy_vector )

deallocate (dref_b_vector )
deallocate (dref_c_vector )
deallocate (dref_l_vector )

deallocate  (c1_vector )
deallocate (dc1_vector )
deallocate  (c8_vector )
deallocate (dc8_vector )

deallocate (c2_vector )
deallocate (c3_vector )
deallocate (cout_vector )

deallocate (c_pb_vector)
deallocate (c_tc_vector)
deallocate (c_tl_vector)

deallocate (dw2_vector)
deallocate (dw3_vector)

deallocate (dw_b_vector)
deallocate (dw_c_vector)
deallocate (dw_l_vector)

deallocate (labour_cost_vector)

deallocate (p4_vector)
deallocate (dp4_vector )
deallocate (p5_vector)
deallocate (dp5_vector )
deallocate (p6_vector)
deallocate (p7_vector)

deallocate (p9x_vector)
deallocate (dp9x_vector)
deallocate (p9y_vector)
deallocate (dp9y_vector)

deallocate (p10_vector)
deallocate (dp10_vector)

deallocate (ber_vector)
deallocate (bet_vector)

deallocate (ialr_vector)
deallocate (ialt_vector)

return
end
