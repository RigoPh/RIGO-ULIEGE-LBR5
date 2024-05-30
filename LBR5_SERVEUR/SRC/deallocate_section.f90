subroutine deallocate_section()

implicit double precision (a-h,o-z)
 
! désallocation dynamique ============================================================================

!call deallocate_param_init()	
call deallocate_param_section()	
!call deallocate_param_opti_local()

return


end

