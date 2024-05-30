      subroutine bendyield(nel,siga,dsiga)

      use sharedvar
	
	implicit real*8 (a-h,o-z)

	dimension dsiga(ntot)
	
c	*****************************************************************************************************

	call annuld(dsiga,ntot)
	
	fact=1.
	if(dabs(siggro(1)).ge.dabs(siggro(2))) then
	  siga=dabs(siggro(1))
	  if(siga.ne.siggro(1)) fact=-1.
	  l1=0
	  do ipan=1,neto
	    nbrxi=nvar(ipan)
		do l=1,nbrxi
	      ll=nxit(l,ipan)
		  dsiga(l1+l)=fact*dsiggro(1,ll,ipan)
		enddo
	    l1=l1+nbrxi
	  enddo
	else
	  siga=dabs(siggro(2))
	  if(siga.ne.siggro(2)) fact=-1.
	  l1=0
	  do ipan=1,neto
	    nbrxi=nvar(ipan)
		do l=1,nbrxi
	      ll=nxit(l,ipan)
		  dsiga(l1+l)=fact*dsiggro(2,ll,ipan)
		enddo
	    l1=l1+nbrxi
	  enddo
	endif	  

      return
      end
