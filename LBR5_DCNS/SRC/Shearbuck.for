      subroutine shearbuck(nel,e1,sigy1,q,epsa,epsr,
     *				     delta,hxr,epsa2,is,
     *					 dtaupl,ddtaupl)

	use sharedvar

      implicit real*8 (a-h,o-z)

	real *8 kt

	dimension ddtaupl(ntot)
	
	dimension dkt(ntot),dtaue(ntot),dtaua(ntot)		
     
c	*****************************************************************************************************

c	elastic buckling of plates
c	--------------------------

	phil=-philn(nel)*pi/180.							!juil07
	if(modes(nel).eq.'EE2') then						!juil07
	  bucksp=epsr										!juil07
	  dbucksp=1.										!juil07
	else												!juil07
	  bucksp=(dabs(q*phil)*epsr)/(dabs(q*phil)+epsr)	!juil07
	  dbucksp=dabs(q*phil)**2/(dabs(q*phil)+epsr)**2	!juil07
	endif

	call annuld(dkt,ntot)
	call annuld(dtaue,ntot)
	call annuld(dtaua,ntot)
	
	fact=1.
	taumax=max(dabs(taunet(nel,1)),dabs(taunet(nel,2)),
     *	   dabs(taunet(nel,3)))
	if(dabs(taunet(nel,1)).eq.taumax) then
	  taua=dabs(sf1(is)*taunet(nel,1))
	  if(taua.ne.(sf1(is)*taunet(nel,1))) fact=-1.
	  l1=0
	  do ipan=1,neto
	    nbrxi=nvar(ipan)
		do l=1,nbrxi
	      ll=nxit(l,ipan)
		  if(ll.eq.1) then
		    dtaua(l1+l)=fact*sf1(is)*dtaunet(nel,1,ipan)
			goto 1
	      endif
		enddo
    1	    l1=l1+nbrxi
	  enddo
	elseif(dabs(taunet(nel,2)).eq.taumax) then
	  taua=dabs(sf1(is)*taunet(nel,2))
	  if(taua.ne.(sf1(is)*taunet(nel,2))) fact=-1.
	  l1=0
	  do ipan=1,neto
	    nbrxi=nvar(ipan)
		do l=1,nbrxi
	      ll=nxit(l,ipan)
		  if(ll.eq.1) then
		    dtaua(l1+l)=fact*sf1(is)*dtaunet(nel,2,ipan)
			goto 2
	      endif
		enddo
    2	    l1=l1+nbrxi
	  enddo
	elseif(dabs(taunet(nel,3)).eq.taumax) then
	  taua=dabs(sf1(is)*taunet(nel,3))
	  if(taua.ne.(sf1(is)*taunet(nel,3))) fact=-1.
	  l1=0
	  do ipan=1,neto
	    nbrxi=nvar(ipan)
		do l=1,nbrxi
	      ll=nxit(l,ipan)
		  if(ll.eq.1) then
		    dtaua(l1+l)=fact*sf1(is)*dtaunet(nel,3,ipan)
			goto 3
	      endif
		enddo
    3	    l1=l1+nbrxi
	  enddo
	endif	  

      if(hxr.gt.0.010) then												!plating with longitudinal stiffeners
	  if(epsa2.ge.(0.00001)) then
	    if(epsa2.lt.epsa) then
		  kt=5.34+4.*(bucksp/epsa2)**2				!juil07
	      l1=0
	      do ipan=1,neto
	        nbrxi=nvar(ipan)
	        do l=1,nbrxi
	          ll=nxit(l,ipan)
	          if(nel.eq.ipan) then
	            if(ll.eq.9) then
	              dkt(l1+l)=4.*2.*(bucksp/epsa2)*(dbucksp/epsa2)	!juil07
	            endif
	          endif
	        enddo
     	        l1=l1+nbrxi
            enddo
	      goto 4
	    endif
	  endif
	  kt=5.34+4.*(bucksp/epsa)**2
	  l1=0
	  do ipan=1,neto
	    nbrxi=nvar(ipan)
	    do l=1,nbrxi
	      ll=nxit(l,ipan)
	      if(nel.eq.ipan) then
		    if(ll.eq.5) then
	          dkt(l1+l)=4.*2.*(bucksp/epsa)*(-bucksp/epsa**2)		!juil07
	        elseif(ll.eq.9) then
	          dkt(l1+l)=4.*2.*(bucksp/epsa)*(dbucksp/epsa)			!juil07
	        endif
	      endif
	    enddo
     	    l1=l1+nbrxi
        enddo
    4	  taue=0.9*kt*e1*(delta/bucksp)**2		!juil07
	  l1=0
	  do ipan=1,neto
	    nbrxi=nvar(ipan)
	    do l=1,nbrxi
	      ll=nxit(l,ipan)
	      if(nel.eq.ipan) then
	        if(ll.eq.1) then
	          dtaue(l1+l)=0.9*e1*kt*2.*(delta/bucksp)*(1./bucksp)		!juil07
		    elseif(ll.eq.5) then
	          dtaue(l1+l)=0.9*e1*(dkt(l1+l)*(delta/bucksp)**2)			!juil07
	        elseif(ll.eq.9) then
	          dtaue(l1+l)=0.9*e1*(dkt(l1+l)*(delta/bucksp)**2+			!juil07
     *					  kt*2.*(delta/bucksp)*(-delta*dbucksp/			!juil07
     *					  bucksp**2))									!juil07
	        endif
	      endif
	    enddo
     	    l1=l1+nbrxi
        enddo
	else																!plating with transverse stiffeners
	  phil=-philn(nel)*pi/180.
	  if(epsa2.ge.(0.00001)) then
		if(epsa2.lt.epsa) then
	      if(epsa2.lt.dabs(q*phil)) then
		    kt=5.34+4.*(epsa2/dabs(q*phil))**2
	        taue=0.9*kt*e1*(delta/epsa2)**2
	        l1=0
	        do ipan=1,neto
	          nbrxi=nvar(ipan)
	          do l=1,nbrxi
	            ll=nxit(l,ipan)
	            if(nel.eq.ipan) then
	              if(ll.eq.1) then
	                dtaue(l1+l)=0.9*e1*kt*2.*(delta/epsa2)*(1./epsa2)
		          endif
	            endif
	          enddo
     	          l1=l1+nbrxi
              enddo
	      else
	        kt=5.34+4.*(dabs(q*phil)/epsa2)**2
			taue=0.9*kt*e1*(delta/dabs(q*phil))**2
			l1=0
	        do ipan=1,neto
	          nbrxi=nvar(ipan)
	          do l=1,nbrxi
	            ll=nxit(l,ipan)
	            if(nel.eq.ipan) then
	              if(ll.eq.1) then
	                dtaue(l1+l)=0.9*e1*kt*2.*(delta/dabs(q*phil))*
     *							(1./dabs(q*phil))
		          endif
	            endif
	          enddo
     	          l1=l1+nbrxi
              enddo
	      endif
	    else
	      if(epsa.lt.dabs(q*phil)) then
		    kt=5.34+4.*(epsa/dabs(q*phil))**2
	        taue=0.9*kt*e1*(delta/epsa)**2
	        l1=0
	        do ipan=1,neto
	          nbrxi=nvar(ipan)
	          do l=1,nbrxi
	            ll=nxit(l,ipan)
	            if(nel.eq.ipan) then
	              if(ll.eq.1) then
	                dtaue(l1+l)=0.9*e1*kt*2.*(delta/epsa)*(1./epsa)
	              elseif(ll.eq.5) then
	                dkt(l1+l)=4.*2.*(epsa/dabs(q*phil))
	                dtaue(l1+l)=0.9*e1*(dkt(l1+l)*(delta/epsa)**2+
     *					        kt*2.*(delta/epsa)*(-delta/epsa**2))
		          endif
	            endif
	          enddo
     	          l1=l1+nbrxi
              enddo
	      else
	        kt=5.34+4.*(dabs(q*phil)/epsa)**2
		    taue=0.9*kt*e1*(delta/dabs(q*phil))**2
		    l1=0
	        do ipan=1,neto
	          nbrxi=nvar(ipan)
	          do l=1,nbrxi
	            ll=nxit(l,ipan)
	            if(nel.eq.ipan) then
	              if(ll.eq.1) then
	                dtaue(l1+l)=0.9*e1*kt*2.*(delta/dabs(q*phil))*
     *						    (1./dabs(q*phil))
				  elseif(ll.eq.5) then
				    dkt(l1+l)=4.*2.*(dabs(q*phil)/epsa)*
     *					      (-dabs(q*phil)/epsa**2)
				    dtaue(l1+l)=0.9*e1*(dkt(l1+l)*(delta/
     *						    dabs(q*phil))**2)
		          endif
	            endif
	          enddo
     	          l1=l1+nbrxi
              enddo
	      endif
	    endif
	  else
    	    if(epsa.lt.dabs(q*phil)) then
		  kt=5.34+4.*(epsa/dabs(q*phil))**2
	      taue=0.9*kt*e1*(delta/epsa)**2
	      l1=0
	      do ipan=1,neto
	        nbrxi=nvar(ipan)
	        do l=1,nbrxi
	          ll=nxit(l,ipan)
	          if(nel.eq.ipan) then
	            if(ll.eq.1) then
	              dtaue(l1+l)=0.9*e1*kt*2.*(delta/epsa)*(1./epsa)
	            elseif(ll.eq.5) then
	              dkt(l1+l)=4.*2.*(epsa/dabs(q*phil))
	              dtaue(l1+l)=0.9*e1*(dkt(l1+l)*(delta/epsa)**2+
     *					      kt*2.*(delta/epsa)*(-delta/epsa**2))
		        endif
	          endif
	        enddo
     	        l1=l1+nbrxi
            enddo
	    else
	      kt=5.34+4.*(dabs(q*phil)/epsa)**2
		  taue=0.9*kt*e1*(delta/dabs(q*phil))**2
		  l1=0
	      do ipan=1,neto
	        nbrxi=nvar(ipan)
	        do l=1,nbrxi
	          ll=nxit(l,ipan)
	          if(nel.eq.ipan) then
	            if(ll.eq.1) then
	              dtaue(l1+l)=0.9*e1*kt*2.*(delta/dabs(q*phil))*
     *						  (1./dabs(q*phil))
				elseif(ll.eq.5) then
				  dkt(l1+l)=4.*2.*(dabs(q*phil)/epsa)*
     *					    (-dabs(q*phil)/epsa**2)
				  dtaue(l1+l)=0.9*e1*(dkt(l1+l)*(delta/
     *						  dabs(q*phil))**2)
		        endif
	          endif
	        enddo
     	        l1=l1+nbrxi
            enddo
	    endif
	  endif
	endif
	
	if(taue.le.((sigy1/sqrt(3.))/2.)) then
	  dtaupl=taua/taue
	  l1=0
	  do ipan=1,neto
	    nbrxi=nvar(ipan)
	    do l=1,nbrxi
	      ll=nxit(l,ipan)
	      ddtaupl(l1+l)=(dtaua(l1+l)*taue-taua*dtaue(l1+l))/taue**2
	    enddo
     	    l1=l1+nbrxi
        enddo
	else
	  dtaupl=taua/((sigy1/sqrt(3.))*(1.-0.25*(sigy1/sqrt(3.))/taue))
	  l1=0
	  do ipan=1,neto
	    nbrxi=nvar(ipan)
	    do l=1,nbrxi
	      ll=nxit(l,ipan)
	      ddtaupl(l1+l)=1./(sigy1/sqrt(3.))*(dtaua(l1+l)*
     *				    (1.-0.25*(sigy1/sqrt(3.))/taue)-taua*
     *				    (0.25*(sigy1/sqrt(3.))*dtaue(l1+l)/taue**2))
     *			        /(1.-0.25*(sigy1/sqrt(3.))/taue)**2
	    enddo
     	    l1=l1+nbrxi
        enddo
	endif

      return
      end
