subroutine deallocate_param_multi()		

use param_section

implicit double precision (a-h,o-z)

! allocation des tableaux et matrices ================================================================


deallocate(section_files)
deallocate(key_gui)


! nombre maximum de panneaux fixés à 200
deallocate(iu_scratch_1)
deallocate(iu_scratch_2)
deallocate(iu_scratch_3)


deallocate(iu_10)
deallocate(iu_11)
deallocate(iu_12)
deallocate(iu_13)
deallocate(iu_14)
!deallocate(iu_15)
!deallocate(iu_16)
!deallocate(iu_17)
!deallocate(iu_18) 
deallocate(iu_19) 
deallocate(iu_20) 
deallocate(iu_21) 
!deallocate(iu_22) 
deallocate(iu_23) 
!deallocate(iu_24) 
!deallocate(iu_25) 
!deallocate(iu_26)    
deallocate(iu_27)    
!deallocate(iu_28)    
!deallocate(iu_29)    
deallocate(iu_30)    
deallocate(iu_31)    
!deallocate(iu_32)    
deallocate(iu_33)    
deallocate(iu_34)    
!deallocate(iu_35)    
deallocate(iu_36)    
deallocate(iu_37)    
deallocate(iu_38)    
deallocate(iu_39)    
deallocate(iu_40) 
!deallocate(iu_41) 
deallocate(iu_42) 
!deallocate(iu_43) 
!deallocate(iu_44)    
!deallocate(iu_45)    
!deallocate(iu_46)    
!deallocate(iu_47)    
!deallocate(iu_48)    
!deallocate(iu_49)    
!deallocate(iu_50) 
!deallocate(iu_51) 
!deallocate(iu_100)
deallocate(iu_redress) 


return 
end
