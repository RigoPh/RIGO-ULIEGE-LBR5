subroutine bendyield(siga,dsiga,ntot_)

use param_section
	
implicit double precision (a-h,o-z)

dimension dsiga(ntot_)
	
! ====================================================================================================

call annuld(dsiga,ntot_)

fact=1.
if(dabs(siggro(1)).ge.dabs(siggro(2))) then

   siga=dabs(siggro(1))
   if(siga.ne.siggro(1)) fact=-1.
   l1=0
   do ipan=1,neto
      nbrxi=nvar(ipan,iboat)
	  do l=1,nbrxi
	     ll=nxit(l,ipan,iboat)
	     dsiga(l1+l)=fact*dsiggro(1,ll,ipan)
	  enddo
      l1=l1+nbrxi
   enddo

   
else

  siga=dabs(siggro(2))
  if(siga.ne.siggro(2)) fact=-1.
  l1=0
  do ipan=1,neto
     nbrxi=nvar(ipan,iboat)
	 do l=1,nbrxi
        ll=nxit(l,ipan,iboat)
	    dsiga(l1+l)=fact*dsiggro(2,ll,ipan)
	 enddo
     l1=l1+nbrxi
  enddo


endif	  

return
end
