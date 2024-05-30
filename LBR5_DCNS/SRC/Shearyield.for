      subroutine shearyield(nel,is,taua,dtaua)

	use sharedvar

      implicit real*8 (a-h,o-z)

	dimension dtaua(ntot)
	
c	*****************************************************************************************************

	call annuld(dtaua,ntot)
	
	fact=1.
	taumax=max(dabs(taugro(nel,1)),dabs(taugro(nel,2)),
     *	   dabs(taugro(nel,3)))
	if(dabs(taugro(nel,1)).eq.taumax) then
	  taua=dabs(sf1(is)*taugro(nel,1))
	  if(taua.ne.(sf1(is)*taugro(nel,1))) fact=-1.
	  l1=0
	  do ipan=1,neto
	    nbrxi=nvar(ipan)
		do l=1,nbrxi
	      ll=nxit(l,ipan)
		  if(ll.eq.1) then
		    dtaua(l1+l)=fact*sf1(is)*dtaugro(nel,1,ipan)
			goto 1
	      endif
		enddo
    1	    l1=l1+nbrxi
	  enddo
	elseif(dabs(taugro(nel,2)).eq.taumax) then
	  taua=dabs(sf1(is)*taugro(nel,2))
	  if(taua.ne.(sf1(is)*taugro(nel,2))) fact=-1.
	  l1=0
	  do ipan=1,neto
	    nbrxi=nvar(ipan)
		do l=1,nbrxi
	      ll=nxit(l,ipan)
		  if(ll.eq.1) then
		    dtaua(l1+l)=fact*sf1(is)*dtaugro(nel,2,ipan)
			goto 2
	      endif
		enddo
    2	    l1=l1+nbrxi
	  enddo
	elseif(dabs(taugro(nel,3)).eq.taumax) then
	  taua=dabs(sf1(is)*taugro(nel,3))
	  if(taua.ne.(sf1(is)*taugro(nel,3))) fact=-1.
	  l1=0
	  do ipan=1,neto
	    nbrxi=nvar(ipan)
		do l=1,nbrxi
	      ll=nxit(l,ipan)
		  if(ll.eq.1) then
		    dtaua(l1+l)=fact*sf1(is)*dtaugro(nel,3,ipan)
			goto 3
	      endif
		enddo
    3	    l1=l1+nbrxi
	  enddo
	endif	  

      return
      end