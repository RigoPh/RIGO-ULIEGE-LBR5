	subroutine bending(nel,bm,yn,dyn,in,din,sig,dsig)

	use sharedvar

	implicit real*8 (a-h,o-z)

	real*8 in
		
	dimension dyn(9,nmax),din(9,nmax)
	
	dimension sig(2),dsig(2,9,nmax)									!résultats

c*********************************************************************************

c	contraintes normales d'ensemble
c	-------------------------------
	v1=z(nel,3)-yn
      v2=z(nel,4)-yn

	sig(1)=part(nel)*bm*v1/in
	sig(2)=part(nel)*bm*v2/in
	

c	sensibilités des contraintes normales d'ensemble
c	------------------------------------------------
	if(iopti.ge.1) then

	  v1=z(nel,3)-yn
        v2=z(nel,4)-yn
		  
	  do 2 ipan=1,neto
	    nbrxi=nvar(ipan)																	
	    if(nbrxi.eq.0) goto 2
	    if(itype(ipan).ne.5) then
		  do l=1,nbrxi
		    ll=nxit(l,ipan)
	        dsig(1,ll,ipan)=part(nel)*bm*((-1.)*dyn(ll,ipan)*in-
     *					    v1*din(ll,ipan))/in**2
		    dsig(2,ll,ipan)=part(nel)*bm*((-1.)*dyn(ll,ipan)*in-
     *					    v2*din(ll,ipan))/in**2
	      enddo
	    endif
    2	  continue

      endif

      return
      end