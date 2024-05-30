      subroutine compbuck(nel,e1,sigy1,coefk1,epsa,epsr,					!r&d15
     *				    delta,hxr,dxr,wxr,txr,wya,q,					!juil07
     *				    epsa2,wya2,dsigpl,ddsigpl)

	use sharedvar

      implicit real*8 (a-h,o-z)

	real *8 m

	dimension ddsigpl(ntot)
	
	dimension dpsi(9,neto),dm(9,neto),dsigepl(9,neto)					!plates			
     
c	*****************************************************************************************************

c	coefk=175.0e+06/sigm1		!r&d15

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

	call annuld(dpsi,9*neto)
	call annuld(dm,9*neto)
	call annuld(dsigepl,9*neto)
	call annuld(ddsigpl,ntot)
	
	if((signet(1).ge.0.).and.(signet(2).ge.0.)) then							!panneau en traction
	  dsigpl=0.
	  return											
	else
	  if((-signet(1).ge.0.).and.(-signet(2).ge.0.)) then						!panneau totalement en compression
	    if((-signet(1)).le.(-signet(2))) then
	      siga=-signet(2)
	      psi=signet(1)/signet(2)
	    else
	      siga=-signet(1)
	      psi=signet(2)/signet(1)
	    endif
	    if(siga.lt.(30.0e+06/coefk1)) siga=30.0e+06/coefk1						!r&d15
		do ipan=1,neto
	      nbrxi=nvar(ipan)
	      do l=1,nbrxi
	        ll=nxit(l,ipan)
		    if((-signet(1)).le.(-signet(2))) then
      	      dpsi(ll,ipan)=(dsignet(1,ll,ipan)*signet(2)-
     *				        signet(1)*dsignet(2,ll,ipan))/signet(2)**2
		    else
      	      dpsi(ll,ipan)=(dsignet(2,ll,ipan)*signet(1)-
     *				        signet(2)*dsignet(1,ll,ipan))/signet(1)**2
			endif
		  enddo
	    enddo
	  else																!panneau partiellement en compression
	    if((-signet(1)).ge.(-signet(2))) then
	      siga=-signet(1)
	    else
	      siga=-signet(2)
	    endif
		if(siga.lt.(30.0e+06/coefk1)) siga=30.0e+06/coefk1						!r&d15
	    psi=0.
	    do ipan=1,neto
	      nbrxi=nvar(ipan)
	      do l=1,nbrxi
	        ll=nxit(l,ipan)
		    dpsi(ll,ipan)=0.
	      enddo
	    enddo
	  endif
	  if(hxr.gt.0.010) then												!plating with longitudinal stiffeners
          m=8.4/(psi+1.1)
	    sigepl=0.9*m*e1*(delta/bucksp)**2								!juil07
	  else																!plating with transverse stiffeners
		if(epsa2.ge.(0.00001)) then
		  phil=-philn(nel)*pi/180.
		  if(epsa2.lt.epsa) then		
	        if(wya2.lt.0.010) then										!flat bars
	          c=1.05
			else														!angles or t-sections
	          c=1.21
			endif
			if(epsa2.lt.dabs(q*phil)) then
			  m=c*(1+(epsa2/dabs(q*phil))**2)**2*2.1/(psi+1.1)
	          sigepl=0.9*m*e1*(delta/epsa2)**2
	        else
	          m=c*(1+(dabs(q*phil)/epsa2)**2)**2*2.1/(psi+1.1)
			  sigepl=0.9*m*e1*(delta/dabs(q*phil))**2
			endif
		  else
	        if(wya.lt.0.010) then										!flat bars
	          c=1.05
			else														!angles or t-sections
	          c=1.21
			endif
			if(epsa.lt.dabs(q*phil)) then
			  m=c*(1+(epsa/dabs(q*phil))**2)**2*2.1/(psi+1.1)
	          sigepl=0.9*m*e1*(delta/epsa)**2
	        else
	          m=c*(1+(dabs(q*phil)/epsa)**2)**2*2.1/(psi+1.1)
	          sigepl=0.9*m*e1*(delta/dabs(q*phil))**2
			endif
	      endif
		else
	      phil=-philn(nel)*pi/180.
		  c=1.3															!floors or deep girders (primary supporting members)
		  if(epsa.lt.dabs(q*phil)) then
			m=c*(1+(epsa/dabs(q*phil))**2)**2*2.1/(psi+1.1)
			sigepl=0.9*m*e1*(delta/epsa)**2
	      else
	        m=c*(1+(dabs(q*phil)/epsa)**2)**2*2.1/(psi+1.1)
			sigepl=0.9*m*e1*(delta/dabs(q*phil))**2
		  endif
		endif
	  endif
	  if(sigepl.le.sigy1/2.) then
	    sigc=sigepl
	  else
	    sigc=sigy1*(1.-0.25*sigy1/sigepl)
	  endif
	  dsigpl=siga/sigc
	  l1=0
	  do ipan=1,neto
	    nbrxi=nvar(ipan)
	    do l=1,nbrxi
	      ll=nxit(l,ipan)
	      if(hxr.gt.0.010) then											!plating with longitudinal stiffeners
		    dm(ll,ipan)=-8.4*dpsi(ll,ipan)/((psi+1.1)*(psi+1.1))
			if((nel.eq.ipan).and.(ll.eq.1)) then
     	          dsigepl(ll,ipan)=0.9*e1*(dm(ll,ipan)*
     *					       (delta/bucksp)**2+2*m*delta/bucksp**2)	!juil07
		    else
		      if((nel.eq.ipan).and.(ll.eq.9)) then
     			    dsigepl(ll,ipan)=0.9*e1*(dm(ll,ipan)*
     *					         (delta/bucksp)**2+2*m*(delta/bucksp)*
     *					         (-delta*dbucksp/bucksp**2))			!juil07
			  else
			    dsigepl(ll,ipan)=0.9*e1*(delta/bucksp)**2*				!juil07
     *						     dm(ll,ipan)
			  endif
		    endif
	      else															!plating with transverse stiffeners
	        if(epsa2.ge.(0.00001)) then
	          if(epsa2.lt.epsa) then		
			    if(epsa2.lt.dabs(q*phil)) then
			      dm(ll,ipan)=-2.1*c*(1+(epsa2/dabs(q*phil))**2)**2*
     *						  dpsi(ll,ipan)/(psi+1.1)**2
				  if((nel.eq.ipan).and.(ll.eq.1)) then
     	                dsigepl(ll,ipan)=0.9*e1*(dm(ll,ipan)*
     *					             (delta/epsa2)**2+
     *						         2*m*delta/epsa2**2)
				  else
	                dsigepl(ll,ipan)=0.9*e1*(delta/epsa2)**2*
     *						         dm(ll,ipan)
				  endif
	            else
	              dm(ll,ipan)=-2.1*c*(1+(dabs(q*phil)/epsa2)**2)**2*
     *						  dpsi(ll,ipan)/(psi+1.1)**2
				  if((nel.eq.ipan).and.(ll.eq.1)) then
     	                dsigepl(ll,ipan)=0.9*e1*(dm(ll,ipan)*
     *					             (delta/dabs(q*phil))**2+
     *								 2*m*delta/dabs(q*phil)**2)
				  else
	                dsigepl(ll,ipan)=0.9*e1*(delta/dabs(q*phil))**2*
     *						         dm(ll,ipan)
				  endif
			    endif
	          else
			    if(epsa.lt.dabs(q*phil)) then
			      if((nel.eq.ipan).and.(ll.eq.1)) then
     	                dm(ll,ipan)=-2.1*c*(1+(epsa/dabs(q*phil))**2)**2*
     *						    dpsi(ll,ipan)/(psi+1.1)**2
                      dsigepl(ll,ipan)=0.9*e1*(dm(ll,ipan)*
     *					            (delta/epsa)**2+
     *						        2*m*delta/epsa**2)
				  else
				    if((nel.eq.ipan).and.(ll.eq.5)) then
				      dm(ll,ipan)=(2*c*(1+(epsa/dabs(q*phil))**2)*2*
     *							  (epsa/dabs(q*phil))*1/dabs(q*phil))*
     *							  (2.1/(psi+1.1))+
     *							  (c*(1+(epsa/dabs(q*phil))**2)**2)*
     *							  (-2.1*dpsi(ll,ipan)/(psi+1.1)**2)
					  dsigepl(ll,ipan)=0.9*e1*(dm(ll,ipan)*
     *					         (delta/epsa)**2+2*m*(delta/epsa)*
     *					         (-delta/epsa**2))
	                else
				      dm(ll,ipan)=-2.1*c*(1+(epsa/dabs(q*phil))**2)**2
     *						      *dpsi(ll,ipan)/(psi+1.1)**2
					  dsigepl(ll,ipan)=0.9*e1*(delta/epsa)**2*
     *						           dm(ll,ipan)
					endif
				  endif
	            else
	              if((nel.eq.ipan).and.(ll.eq.1)) then
     	                dm(ll,ipan)=-2.1*c*(1+(dabs(q*phil)/epsa)**2)**2*
     *						    dpsi(ll,ipan)/(psi+1.1)**2
                      dsigepl(ll,ipan)=0.9*e1*(dm(ll,ipan)*
     *					            (delta/dabs(q*phil))**2+
     *						        2*m*delta/dabs(q*phil)**2)
				  else
				    if((nel.eq.ipan).and.(ll.eq.5)) then
				      dm(ll,ipan)=(2*c*(1+(dabs(q*phil)/epsa)**2)*2*
     *							  (dabs(q*phil)/epsa)*
     *							  (-dabs(q*phil)/epsa**2)*
     *						      (2.1/(psi+1.1))+
     *							  (c*(1+(epsa/dabs(q*phil))**2)**2))*
     *							  (-2.1*dpsi(ll,ipan)/(psi+1.1)**2)
					  dsigepl(ll,ipan)=0.9*e1*(dm(ll,ipan)*
     *					               (delta/dabs(q*phil))**2+
     *								   2*m*(delta/dabs(q*phil))*
     *					               (-delta/dabs(q*phil)**2))
	                else
				      dm(ll,ipan)=-2.1*c*(1+(dabs(q*phil)/epsa)**2)**2
     *						      *dpsi(ll,ipan)/(psi+1.1)**2
					  dsigepl(ll,ipan)=0.9*e1*(delta/dabs(q*phil))**2*
     *						           dm(ll,ipan)
					endif
				  endif
			    endif
	          endif
			else
	          if(epsa.lt.dabs(q*phil)) then
			    if((nel.eq.ipan).and.(ll.eq.1)) then
     	              dm(ll,ipan)=-2.1*c*(1+(epsa/dabs(q*phil))**2)**2*
     *					      dpsi(ll,ipan)/(psi+1.1)**2
                    dsigepl(ll,ipan)=0.9*e1*(dm(ll,ipan)*
     *				               (delta/epsa)**2+
     *						       2*m*delta/epsa**2)
				else
				  if((nel.eq.ipan).and.(ll.eq.5)) then
				    dm(ll,ipan)=(2*c*(1+(epsa/dabs(q*phil))**2)*2*
     *						    (epsa/dabs(q*phil))*1/dabs(q*phil))*
     *							(2.1/(psi+1.1))+
     *							(c*(1+(epsa/dabs(q*phil))**2)**2)*
     *							(-2.1*dpsi(ll,ipan)/(psi+1.1)**2)
					dsigepl(ll,ipan)=0.9*e1*(dm(ll,ipan)*
     *					             (delta/epsa)**2+2*m*(delta/epsa)*
     *					             (-delta/epsa**2))
	              else
				    dm(ll,ipan)=-2.1*c*(1+(epsa/dabs(q*phil))**2)**2*
     *					        dpsi(ll,ipan)/(psi+1.1)**2
					dsigepl(ll,ipan)=0.9*e1*(delta/epsa)**2*
     *					             dm(ll,ipan)
				  endif
				endif
	          else
	            if((nel.eq.ipan).and.(ll.eq.1)) then
     	              dm(ll,ipan)=-2.1*c*(1+(dabs(q*phil)/epsa)**2)**2*
     *					      dpsi(ll,ipan)/(psi+1.1)**2
				  dsigepl(ll,ipan)=0.9*e1*(dm(ll,ipan)*
     *				               (delta/dabs(q*phil))**2+
     *						       2*m*delta/dabs(q*phil)**2)
				else
				  if((nel.eq.ipan).and.(ll.eq.5)) then
				    dm(ll,ipan)=(2*c*(1+(dabs(q*phil)/epsa)**2)*2*
     *						    (dabs(q*phil)/epsa)*
     *							(-dabs(q*phil)/epsa**2)*
     *						    (2.1/(psi+1.1))+
     *							(c*(1+(epsa/dabs(q*phil))**2)**2))*
     *							(-2.1*dpsi(ll,ipan)/(psi+1.1)**2)
					dsigepl(ll,ipan)=0.9*e1*(dm(ll,ipan)*
     *				                 (delta/dabs(q*phil))**2+
     *								 2*m*(delta/dabs(q*phil))*
     *					             (-delta/dabs(q*phil)**2))
	              else
				    dm(ll,ipan)=-2.1*c*(1+(dabs(q*phil)/epsa)**2)**2*
     *					        dpsi(ll,ipan)/(psi+1.1)**2
					dsigepl(ll,ipan)=0.9*e1*(delta/dabs(q*phil))**2*
     *					             dm(ll,ipan)
			      endif
				endif
	          endif
			endif
		  endif
		  if(sigepl.le.sigy1/2.) then
	        if(siga.lt.(30.0e+06/coefk1)) then								!r&d15
	          ddsigpl(l1+l)=(-siga*dsigepl(ll,ipan))/sigc**2
	        else
			  if((-signet(1)).ge.(-signet(2))) then
	            ddsigpl(l1+l)=(-dsignet(1,ll,ipan)*sigc-
     *						   siga*dsigepl(ll,ipan))/sigc**2
	          else
	            ddsigpl(l1+l)=(-dsignet(2,ll,ipan)*sigc-
     *						   siga*dsigepl(ll,ipan))/sigc**2
			  endif
	        endif
	      else
	        if(siga.lt.(30.0e+06/coefk1)) then								!r&d15
	          ddsigpl(l1+l)=(-siga*sigy1*(0.25*sigy1*
     *						 dsigepl(ll,ipan)/sigepl**2))/sigc**2
			else
			  if((-signet(1)).ge.(-signet(2))) then
			    ddsigpl(l1+l)=(-dsignet(1,ll,ipan)*sigc-
     *						   siga*sigy1*(0.25*sigy1*
     *						   dsigepl(ll,ipan)/sigepl**2))/sigc**2
			  else
	            ddsigpl(l1+l)=(-dsignet(2,ll,ipan)*sigc-
     *						   siga*sigy1*(0.25*sigy1*
     *						   dsigepl(ll,ipan)/sigepl**2))/sigc**2
			  endif
			endif
		  endif
		enddo
		l1=l1+nbrxi
	  enddo
	endif

    1 format(i3,39x,f8.3,3x,f6.3,6x,'plate')
    2 format(i3,15x,f8.3,16x,f8.3,3x,f6.3,6x,'plate')

      return
      end
