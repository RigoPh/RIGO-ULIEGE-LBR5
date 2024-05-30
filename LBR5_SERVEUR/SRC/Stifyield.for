      subroutine stifyield(nel,coefk1,siga,dsiga,ntot_)		!r&d15

	

      use param_section
!      use param_cout
!      use param_opti_local
      

      implicit double precision (a-h,o-z)

	dimension dsiga(ntot_)
	
c	*****************************************************************************************************
	
c	coefk=175.0e+06/sigm1		!r&d15	
	
	call annuld(dsiga,ntot_)
	
	fact=1.
	if(dabs(signet(1)).ge.dabs(signet(2))) then
	  if(dabs(signet(1)).lt.(60.e6/coefk1)) then	!r&d15
	    siga=60.e6/coefk1+sigstif					!r&d15
	    l1=0
	    do ipan=1,neto
	      nbrxi=nvar(ipan,iboat)
		  do l=1,nbrxi
	        ll=nxit(l,ipan,iboat)
	        if(nel.eq.ipan) dsiga(l1+l)=dsigstif(ll)
		  enddo
	      l1=l1+nbrxi
	    enddo
	  else
	    siga=dabs(signet(1))+sigstif
	    if(signet(1).lt.0.) fact=-1.
	    l1=0
	    do ipan=1,neto
	      nbrxi=nvar(ipan,iboat)
		  do l=1,nbrxi
	        ll=nxit(l,ipan,iboat)
		    dsiga(l1+l)=fact*dsignet(1,ll,ipan)
	        if(nel.eq.ipan) dsiga(l1+l)=dsiga(l1+l)+dsigstif(ll)
		  enddo
	      l1=l1+nbrxi
	    enddo
	  endif
	else
	  if(dabs(signet(2)).lt.(60.e6/coefk1)) then	!r&d15
	    siga=60.e6/coefk1+sigstif					!r&d15
	    l1=0
	    do ipan=1,neto
	      nbrxi=nvar(ipan,iboat)
		  do l=1,nbrxi
	        ll=nxit(l,ipan,iboat)
	        if(nel.eq.ipan) dsiga(l1+l)=dsigstif(ll)
		  enddo
	      l1=l1+nbrxi
	    enddo
	  else
	    siga=dabs(signet(2))+sigstif
	    if(signet(2).lt.0.) fact=-1.
	    l1=0
	    do ipan=1,neto
	      nbrxi=nvar(ipan,iboat)
		  do l=1,nbrxi
	        ll=nxit(l,ipan,iboat)
		    dsiga(l1+l)=fact*dsignet(2,ll,ipan)
	        if(nel.eq.ipan) dsiga(l1+l)=dsiga(l1+l)+dsigstif(ll)
		  enddo
	      l1=l1+nbrxi
	    enddo
	  endif
	endif	  

      return
      end
