subroutine open_local()

use param_section
                           
! subr participation:forces de bord (abcd) 
! coeff. de participation                                                    
open(iu_19(iboat),status='scratch',form='unformatted')      
open(iu_21(iboat),status='scratch',form='unformatted')   
! subr bateau,bo1:forces de bord (abcd)
! subr. ent,bo1 (charge de type cha) 
! sauvetage pour itera=1 seulement   

return
end

