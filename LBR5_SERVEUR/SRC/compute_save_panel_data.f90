subroutine compute_save_panel_data(nel)

call compute_stiffness_coeff(nel)
call compute_panel_weight(nel)
call save_panel_data_for_loads(nel)
call cost(nel)
                 
return
end
