subroutine bending(nel,bm,yn,dyn,in,din,sig,dsig)

use param_section

implicit double precision (a-h,o-z)

double precision in
	
dimension dyn(9,nmax),din(9,nmax)

dimension sig(2),dsig(2,9,nmax)								

! ====================================================================================================

!	contraintes normales d'ensemble
!	===============================
v1=z(nel,3)-yn
v2=z(nel,4)-yn

sig(1)=panneau(nel).part*bm*v1/in
sig(2)=panneau(nel).part*bm*v2/in


!	sensibilités des contraintes normales d'ensemble
!	================================================
if(iopti.ge.1) then
   v1=z(nel,3)-yn
   v2=z(nel,4)-yn
   
   do ipan=1,neto
      nbrxi=nvar(ipan,iboat)																	
	  if(nbrxi.ne.0) then
	     if(itype(ipan).ne.5) then
		    do l=1,nbrxi
		       ll=nxit(l,ipan,iboat)
			   dsig(1,ll,ipan)=panneau(nel).part*bm*((-1.)*dyn(ll,ipan)*in-     &
			                   v1*din(ll,ipan))/in**2
               dsig(2,ll,ipan)=panneau(nel).part*bm*((-1.)*dyn(ll,ipan)*in-     &
			                   v2*din(ll,ipan))/in**2
            enddo
         endif
	   endif
   enddo

endif

return
end
