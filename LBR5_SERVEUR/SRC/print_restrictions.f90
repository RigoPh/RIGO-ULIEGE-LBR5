subroutine print_restrictions(nel)

use param_section

implicit double precision (a-h,o-z)

       if(iopti.ge.1) then
          ia=m1tot(iboat)+m2tot(iboat)
          if(ia.gt.mmmax) then
             write(*,*)'le nombre max de restrictions est d�pass� !!'
             write(*,*)'too much much constraints are considered  !!'
             write(*,*) m1tot(iboat),' restr. struct. / struct. constraints'
             write(*,*) m2tot(iboat),' restr. g�om. /geometrical constraints'
             write(*,*) 'change your input data (donn�es)'
             write(iu_31(iboat),*)'le nombre max de restrictions est d�pass� !!'
             write(iu_31(iboat),*)'too much much constraints are considered  !!'
             write(iu_31(iboat),*) m1tot(iboat),' restr. struct. / struct. constraints'
             write(iu_31(iboat),*) m2tot(iboat),' restr. g�om. /geometrical constraints'
             write(iu_31(iboat),*) 'change your input data (donn�es)'
             write(iu_14(iboat) ,*)'le nombre max de restrictions est d�pass� !!'		
             write(iu_14(iboat) ,*)'too much much constraints are considered  !!'				
             write(iu_14(iboat) ,*) m1tot(iboat),' restr. struct. / struct. constraints'			
             write(iu_14(iboat) ,*) m2tot(iboat),' restr. g�om. /geometrical constraints'	
             write(iu_14(iboat) ,*) 'change your input data (donn�es)'						!bug	
             write(*,*) 'error'
			 read(*,*)																
		    iff=1
          endif
       else
          panneau(nel).m1cont=0
       endif

return
end
