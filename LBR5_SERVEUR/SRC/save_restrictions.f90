subroutine save_restrictions(nel)

use param_section

implicit double precision (a-h,o-z)



!      save list of constraints in subr. ent to be re-used in subr. contr
!      if((iopti.ge.1).and.(nel.eq.neto)) then
       if((iopti.ge.1).and.(itera.eq.0).and.(nel.eq.neto)) then
          do is=1,nsol
             ij=nnsol(is)
             do ip=1,neto
                do i=1,m1tabl(ip,is)
                   !write(iu_32(iboat)) panneau(ip).lcont4(ij,i,1),panneau(ip).lcont4(ij,i,2),panneau(ip).inv3(ij,i),panneau(ip).cjmax9(ij,i)
                   if (idebug.eq.1) write(9998,*) panneau(ip).lcont4(ij,i,1),panneau(ip).lcont4(ij,i,2),panneau(ip).inv3(ij,i),panneau(ip).cjmax9(ij,i)
                enddo
             enddo
          enddo
       endif
       do i=1,nsolm
          do j=1,neto
             do k=1,m1max
!                cjmax3(i,j,k)=cjmax9(i,j,k)
             enddo
          enddo
       enddo


return
end
