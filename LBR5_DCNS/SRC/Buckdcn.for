      subroutine buckdcn(nel,e1,sigy1,delta,hxr,dxr,wxr,txr,epsr,
     *				   epsa,wya,q,eff,itera,ibuck,nbuck,is)			!juil07

	use sharedvar
      
	implicit real*8 (a-h,o-z)

	dimension eff(9690)

	real*8 m														!plates
	dimension dpsi(9,neto),dm(9,neto)		
      dimension dsigepl(9,neto)										
     
      real*8 ia,mstat									!longitudinals
	dimension daire(9),dmstat(9),daxen(9),dia(9)						
	dimension dsigestif(9,neto)

	real*8 ipanel
	dimension dsig1(9,neto),dsig2(9,neto)
	dimension dipanel(9),dspanel(9)

	
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

    	call annuld(dpsi,9*neto)
	call annuld(dm,9*neto)
	call annuld(dsigepl,9*neto)
	call annuld(ddbuckpl,9*neto)

c	if((sig1.ge.0.).and.(sig2.ge.0.)) then	
	if((eff(4540).ge.0.).and.(eff(4570).ge.0.)) then	!panneau en traction
	  dbuckpl=0.
	  goto 30											
	else
	  sig1=-eff(4540)
	  sig2=-eff(4570)
c	  sig1=-sig1
c	  sig2=-sig2
	  if((sig1.ge.0.).and.(sig2.ge.0.)) then			!panneau totalement en compression
	    if(sig1.le.sig2) then
		  siga=sig2
	      psi=sig1/sig2
	    else
	      siga=sig1
	      psi=sig2/sig1
	    endif
		do ipan=1,neto
	      nbrxi=nvar(ipan)
	      do l=1,nbrxi
	        ll=nxit(l,ipan)
	        dsig1(ll,ipan)=-sens3(1,1,ll,ipan)			!ipt=1 (y = 0)
		    dsig2(ll,ipan)=-sens3(1,3,ll,ipan)			!ipt=3 (y = b, largeur du panneau)
c	        dsig1(ll,ipan)=-dsig1(ll,ipan)			
c		    dsig2(ll,ipan)=-dsig2(ll,ipan)			
			if(sig1.le.sig2) then
      	      dpsi(ll,ipan)=(dsig1(ll,ipan)*sig2-sig1*dsig2(ll,ipan))/
     *				        (sig2*sig2)
		    else
      	      dpsi(ll,ipan)=(dsig2(ll,ipan)*sig1-sig2*dsig1(ll,ipan))/
     *				        (sig1*sig1)
			endif
		  enddo
	    enddo
	  else												!panneau partiellement en compression
		if(sig1.ge.sig2) then
		  siga=sig1
	    else
	      siga=sig2
	    endif
	    psi=0.
	    do ipan=1,neto
	      nbrxi=nvar(ipan)
	      do l=1,nbrxi
	        ll=nxit(l,ipan)
	        dsig1(ll,ipan)=-sens3(1,1,ll,ipan)			!ipt=1 (y = 0)
		    dsig2(ll,ipan)=-sens3(1,3,ll,ipan)			!ipt=3 (y = b, largeur du panneau)
c	        dsig1(ll,ipan)=-dsig1(ll,ipan)			
c		    dsig2(ll,ipan)=-dsig2(ll,ipan)			
		    dpsi(ll,ipan)=0.
	      enddo
	    enddo
	  endif
	  if(hxr.gt.0.010) then							!plating with longitudinal stiffeners
          m=8.4/(psi+1.1)
	    sigepl=0.9*m*e1*(delta/bucksp)**2			!juil07
	  else											!plating with transverse stiffeners
	    do ipan=1,neto
	      if(ipan.eq.nel) then
	        read(302) epsap,epsrp,deltap,hyap,dyap,wyap,tyap,		
     *		          hxrp,dxrp,wxrp,txrp,phil,q,epais,epsa2
		    if(epsa2.ge.(0.00001)) then
			  backspace(302)
			  read(302) epsap,epsrp,deltap,hyap,dyap,wyap,tyap,		
     *		            hxrp,dxrp,wxrp,txrp,phil,q,epais,epsa2,
     *				    hya2,dya2,wya2,tya2							!juil07
              endif
	        goto 40
	      else
	        read(302)
	      endif
	    enddo
   40	    rewind 302
          phil=-philn(nel)*pi/180.
		if(epsa2.ge.(0.00001)) then
	      if(epsa2.lt.epsa) then		
c	        if(wya2.lt.0.010) then		!flat bars
c	          c=1.05
c			else						!angles or t-sections
c	          c=1.21
c			endif
			c=1.0
			if(epsa2.lt.dabs(q*phil)) then
			  m=c*(1+(epsa2/dabs(q*phil))**2)**2*2.1/(psi+1.1)
	          sigepl=0.9*m*e1*(delta/epsa2)**2
	        else
	          m=c*(1+(dabs(q*phil)/epsa2)**2)**2*2.1/(psi+1.1)
			  sigepl=0.9*m*e1*(delta/dabs(q*phil))**2
			endif
		  else
c	        if(wya.lt.0.010) then		!flat bars
c	          c=1.05
c			else						!angles or t-sections
c	          c=1.21
c			endif
			c=1.0
			if(epsa.lt.dabs(q*phil)) then
			  m=c*(1+(epsa/dabs(q*phil))**2)**2*2.1/(psi+1.1)
	          sigepl=0.9*m*e1*(delta/epsa)**2
	        else
	          m=c*(1+(dabs(q*phil)/epsa)**2)**2*2.1/(psi+1.1)
	          sigepl=0.9*m*e1*(delta/dabs(q*phil))**2
			endif
	      endif
		else
c	      c=1.3							!floors or deep girders (primary supporting members)
		  c=1.0
		  if(epsa.lt.dabs(q*phil)) then
			m=c*(1+(epsa/dabs(q*phil))**2)**2*2.1/(psi+1.1)
			sigepl=0.9*m*e1*(delta/epsa)**2
	      else
	        m=c*(1+(dabs(q*phil)/epsa)**2)**2*2.1/(psi+1.1)
			sigepl=0.9*m*e1*(delta/dabs(q*phil))**2
		  endif
		endif
	  endif
	  sigc=sigepl
	  dbuckpl=siga/sigc
	  do ipan=1,neto
	    nbrxi=nvar(ipan)
	    do l=1,nbrxi
	      ll=nxit(l,ipan)
	      if(hxr.gt.0.010) then										!plating with longitudinal stiffeners
		    dm(ll,ipan)=-8.4*dpsi(ll,ipan)/((psi+1.1)*(psi+1.1))
			if((nel.eq.ipan).and.(ll.eq.1)) then
     	          dsigepl(ll,ipan)=0.9*e1*(dm(ll,ipan)*
     *					       (delta/bucksp)**2+2*m*delta/bucksp**2)	!juil07
		    else
		      if((nel.eq.ipan).and.(ll.eq.9)) then
     			    dsigepl(ll,ipan)=0.9*e1*(dm(ll,ipan)*
     *					         (delta/bucksp)**2+2*m*(delta/bucksp)*	!juil07
     *					         (-delta*dbucksp/bucksp**2))			!juil07
			  else
			    dsigepl(ll,ipan)=0.9*e1*(delta/bucksp)**2*				!juil07
     *						     dm(ll,ipan)
			  endif
		    endif
	      else														!plating with transverse stiffeners
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
				  continue
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
		  if(sig1.ge.sig2) then												!critère d'euler
	        ddbuckpl(ll,ipan)=(dsig1(ll,ipan)*sigc-
     *					      siga*dsigepl(ll,ipan))/sigc**2
	      else
	        ddbuckpl(ll,ipan)=(dsig2(ll,ipan)*sigc-
     *					      siga*dsigepl(ll,ipan))/sigc**2
		  endif
	    enddo
	  enddo
	  if(itera.ge.1) write(666,2) nel,1.e-6*sigepl,1.e-6*sigc,dbuckpl
	endif

    1 format(i3,39x,f8.3,3x,f6.3,6x,'plate')
    2 format(i3,15x,f8.3,16x,f8.3,3x,f6.3,6x,'plate')

   30 if(ibuck.eq.1) return


c	elastic buckling of longitudinals (column buckling)
c	---------------------------------
      !flabement d'une poutre en I avec une largeur colborante de 40*delta	
      if(ibuck.eq.2) then

	call annuld(daire,9)
	call annuld(dmstat,9)
	call annuld(daxen,9)
	call annuld(dia,9)
	call annuld(dsigestif,9*neto)
	call annuld(ddbuckstif,9*neto)
	
	if(dbuckpl.eq.0.) then	
c	if((eff(7855).ge.0).and.(eff(7885).ge.0)) then
	  dbuckstif=0.
	  return		
	else
c	  sig1=-eff(7855)
c	  sig2=-eff(7885)
	  if(sig1.ge.sig2) then
		siga=sig1
	  else
		siga=sig2
	  endif

	  aire=delta*epsr+hxr*dxr+wxr*txr
	  daire(1)=epsr
	  daire(6)=dxr
	  daire(7)=hxr
	  daire(8)=txr
	  daire(9)=delta

	  mstat=delta*epsr*delta/2.+hxr*dxr*(hxr/2.+delta)+
     *	    wxr*txr*(txr/2.+hxr+delta)
	  dmstat(1)=epsr*delta+hxr*dxr+wxr*txr
	  dmstat(6)=dxr*(hxr/2.+delta)+0.5*hxr*dxr+wxr*txr
	  dmstat(7)=hxr*(hxr/2.+delta)
	  dmstat(8)=txr*(txr/2.+hxr+delta)
	  dmstat(9)=delta*delta/2.

	  axen=mstat/aire
	  do i=1,9
	    daxen(i)=(dmstat(i)*aire-mstat*daire(i))/(aire*aire)
	  enddo

	  ia=epsr*delta**3/12.+delta*epsr*(delta/2.-axen)**2+
     *	 dxr*hxr**3/12.+hxr*dxr*(hxr/2.+delta-axen)**2+
     *	 wxr*txr**3/12.+wxr*txr*(txr/2.+hxr+delta-axen)**2
	  dia(1)=epsr*delta**2/4.+epsr*(delta/2.-axen)**2+
     *		 2.*delta*epsr*(delta/2.-axen)*(0.5-daxen(1))+
     *		 2.*hxr*dxr*(hxr/2.+delta-axen)*(1.-daxen(1))+
     *		 2.*wxr*txr*(txr/2.+hxr+delta-axen)*(1.-daxen(1))
	  dia(6)=2.*delta*epsr*(delta/2.-axen)*(-daxen(6))+
     *		 dxr*hxr**2/4.+dxr*(hxr/2.+delta-axen)**2+
     *		 2.*hxr*dxr*(hxr/2.+delta-axen)*(0.5-daxen(6))+
     *		 2.*wxr*txr*(txr/2.+hxr+delta-axen)*(1.-daxen(6))
	  dia(7)=2.*delta*epsr*(delta/2.-axen)*(-daxen(7))+
     *		 hxr**3/12.+hxr*(hxr/2.+delta-axen)**2+
     *		 2.*hxr*dxr*(hxr/2.+delta-axen)*(-daxen(7))+
     *		 2.*wxr*txr*(txr/2.+hxr+delta-axen)*(-daxen(7))
	  dia(8)=2.*delta*epsr*(delta/2.-axen)*(-daxen(8))+
     *		 2.*hxr*dxr*(hxr/2.+delta-axen)*(-daxen(8))+
     *		 txr**3/12.+txr*(txr/2.+hxr+delta-axen)**2+
     *		 wxr*txr*(txr/2.+hxr+delta-axen)*(-daxen(8))
	  dia(9)=delta**3/12.+delta*(delta/2.-axen)**2+
     *		 2.*delta*epsr*(delta/2.-axen)*(-daxen(9))+
     *		 2.*hxr*dxr*(hxr/2.+delta-axen)*(-daxen(9))+
     *		 2.*wxr*txr*(txr/2.+hxr+delta-axen)*(-daxen(9))

	  sigestif=10.*e1*ia/(aire*epsa*epsa)
	  do i=1,9
	    dsigestif(i,nel)=10.*e1/(epsa*epsa)*(dia(i)*aire-ia*daire(i))/
     *				     aire**2
	  enddo
	  dsigestif(5,nel)=-2.*10.*e1*ia/aire*(1./epsa**3)

	  beta=1.1
	  if(sigestif.le.sigy1/2.) then											!critère de johnson
	    sigc=sigestif/beta
	  else
	    sigc=sigy1*(1-0.25*sigy1/sigestif)/beta
	  endif



	  dbuckstif=siga/sigc
	  do ipan=1,neto
	    nbrxi=nvar(ipan)
		do l=1,nbrxi
	      ll=nxit(l,ipan)
		  if(sigestif.le.sigy1/2.) then
  		    if(sig1.ge.sig2) then
c	          dsig1(ll,ipan)=-sens2(10,1,ll,ipan)
	          ddbuckstif(ll,ipan)=(dsig1(ll,ipan)*sigc-
     *						     siga*dsigestif(ll,ipan)/beta)/sigc**2
	        else
c	          dsig2(ll,ipan)=-sens2(10,3,ll,ipan)
			  ddbuckstif(ll,ipan)=(dsig2(ll,ipan)*sigc-
     *						     siga*dsigestif(ll,ipan)/beta)/sigc**2
			endif
	      else
			if(sig1.ge.sig2) then
c			  dsig1(ll,ipan)=-sens2(10,1,ll,ipan)
			  ddbuckstif(ll,ipan)=(dsig1(ll,ipan)*sigc-
     *						      siga*sigy1*(0.25*sigy1*
     *						      dsigestif(ll,ipan)/sigestif**2)/
     *							  beta)/sigc**2
			else
c	          dsig2(ll,ipan)=-sens2(10,3,ll,ipan)
			  ddbuckstif(ll,ipan)=(dsig2(ll,ipan)*sigc-
     *							  siga*sigy1*(0.25*sigy1*
     *						      dsigestif(ll,ipan)/sigestif**2)/
     *							  beta)/sigc**2
			endif
		  endif
		enddo
	  enddo

	  if(itera.ge.1) then
	    write(666,4) nel,1.e-6*sigestif,1.e-6*sigc,dbuckstif
	  endif
      endif

      elseif(ibuck.eq.3) then

	call annuld(daire,9)
	call annuld(dmstat,9)
	call annuld(daxen,9)
	call annuld(dia,9)
	call annuld(dsigestif,9*neto)
	call annuld(ddbuckstif,9*neto)
	
	if(dbuckpl.eq.0.) then	
c	if((eff(7855).ge.0).and.(eff(7885).ge.0)) then
	  dbuckstif=0.
	  return		
	else
c	  sig1=-eff(7855)
c	  sig2=-eff(7885)
	  if(sig1.ge.sig2) then
		siga=sig1
	  else
		siga=sig2
	  endif

!	  aire=delta*epsr+hxr*dxr+wxr*txr
	  aire=delta*(40*delta)+hxr*dxr+wxr*txr
	  daire(1)=80*delta
	  daire(6)=dxr
	  daire(7)=hxr
	  daire(8)=txr
!	  daire(9)=delta

!	  mstat=delta*epsr*delta/2.+hxr*dxr*(hxr/2.+delta)+
!     *	    wxr*txr*(txr/2.+hxr+delta)

	  mstat=delta*(40*delta)*delta/2.+hxr*dxr*(hxr/2.+delta)+
     *	    wxr*txr*(txr/2.+hxr+delta)

!	  dmstat(1)=epsr*delta+hxr*dxr+wxr*txr
	  dmstat(1)=60*delta**2+hxr*dxr+wxr*txr
	  dmstat(6)=dxr*(hxr/2.+delta)+0.5*hxr*dxr+wxr*txr
	  dmstat(7)=hxr*(hxr/2.+delta)
	  dmstat(8)=txr*(txr/2.+hxr+delta)
!	  dmstat(9)=delta*delta/2.

	  axen=mstat/aire
	  do i=1,9
	    daxen(i)=(dmstat(i)*aire-mstat*daire(i))/(aire*aire)
	  enddo

!	  ia=epsr*delta**3/12.+delta*epsr*(delta/2.-axen)**2+
!     *	 dxr*hxr**3/12.+hxr*dxr*(hxr/2.+delta-axen)**2+
!     *	 wxr*txr**3/12.+wxr*txr*(txr/2.+hxr+delta-axen)**2



	  ia=(40*delta)*delta**3/12.+delta*(40*delta)*(delta/2.-axen)**2+
     *	 dxr*hxr**3/12.+hxr*dxr*(hxr/2.+delta-axen)**2+
     *	 wxr*txr**3/12.+wxr*txr*(txr/2.+hxr+delta-axen)**2


!	  dia(1)=epsr*delta**2/4.+epsr*(delta/2.-axen)**2+
!     *		 2.*delta*epsr*(delta/2.-axen)*(0.5-daxen(1))+
!     *		 2.*hxr*dxr*(hxr/2.+delta-axen)*(1.-daxen(1))+
!     *		 2.*wxr*txr*(txr/2.+hxr+delta-axen)*(1.-daxen(1))

	  dia(1)=40.*delta**3/3.+80.*delta*(delta/2.-axen)**2+
     *		 (80.*delta**2)*(delta/2.-axen)*(0.5-daxen(1))+
     *		 2.*hxr*dxr*(hxr/2.+delta-axen)*(1.-daxen(1))+
     *		 2.*wxr*txr*(txr/2.+hxr+delta-axen)*(1.-daxen(1))


	  dia(6)=2.*delta*(40*delta)*(delta/2.-axen)*(-daxen(6))+
     *		 dxr*hxr**2/4.+dxr*(hxr/2.+delta-axen)**2+
     *		 2.*hxr*dxr*(hxr/2.+delta-axen)*(0.5-daxen(6))+
     *		 2.*wxr*txr*(txr/2.+hxr+delta-axen)*(1.-daxen(6))
	  dia(7)=2.*delta*(40*delta)*(delta/2.-axen)*(-daxen(7))+
     *		 hxr**3/12.+hxr*(hxr/2.+delta-axen)**2+
     *		 2.*hxr*dxr*(hxr/2.+delta-axen)*(-daxen(7))+
     *		 2.*wxr*txr*(txr/2.+hxr+delta-axen)*(-daxen(7))
	  dia(8)=2.*delta*(40*delta)*(delta/2.-axen)*(-daxen(8))+
     *		 2.*hxr*dxr*(hxr/2.+delta-axen)*(-daxen(8))+
     *		 txr**3/12.+txr*(txr/2.+hxr+delta-axen)**2+
     *		 wxr*txr*(txr/2.+hxr+delta-axen)*(-daxen(8))
!	  dia(9)=delta**3/12.+delta*(delta/2.-axen)**2+
!     *		 2.*delta*epsr*(delta/2.-axen)*(-daxen(9))+
!     *		 2.*hxr*dxr*(hxr/2.+delta-axen)*(-daxen(9))+
!     *		 2.*wxr*txr*(txr/2.+hxr+delta-axen)*(-daxen(9))

	  sigestif=10.*e1*ia/(aire*epsa*epsa)


	  do i=1,9
	    dsigestif(i,nel)=10.*e1/(epsa*epsa)*(dia(i)*aire-ia*daire(i))/
     *				     aire**2
	  enddo
	  dsigestif(5,nel)=-2.*10.*e1*ia/aire*(1./epsa**3)

!	  beta=1.1
	  if(sigestif.le.sigy1/2.) then											!critère de johnson
	    sigc=sigestif
	  else
	    sigc=sigy1*(1-0.25*sigy1/sigestif)
	  endif





	  dbuckstif=siga/sigc
	  do ipan=1,neto
	    nbrxi=nvar(ipan)
		do l=1,nbrxi
	      ll=nxit(l,ipan)
		  if(sigestif.le.sigy1/2.) then
  		    if(sig1.ge.sig2) then
c	          dsig1(ll,ipan)=-sens2(10,1,ll,ipan)
	          ddbuckstif(ll,ipan)=(dsig1(ll,ipan)*sigc-
     *						     siga*dsigestif(ll,ipan))/sigc**2
	        else
c	          dsig2(ll,ipan)=-sens2(10,3,ll,ipan)
			  ddbuckstif(ll,ipan)=(dsig2(ll,ipan)*sigc-
     *						     siga*dsigestif(ll,ipan))/sigc**2
			endif
	      else
			if(sig1.ge.sig2) then
c			  dsig1(ll,ipan)=-sens2(10,1,ll,ipan)
			  ddbuckstif(ll,ipan)=(dsig1(ll,ipan)*sigc-
     *						      siga*sigy1*(0.25*sigy1*
     *						      dsigestif(ll,ipan)/sigestif**2))
     *							  /sigc**2
			else
c	          dsig2(ll,ipan)=-sens2(10,3,ll,ipan)
			  ddbuckstif(ll,ipan)=(dsig2(ll,ipan)*sigc-
     *							  siga*sigy1*(0.25*sigy1*
     *						      dsigestif(ll,ipan)/sigestif**2))
     *							  /sigc**2
			endif
		  endif
		enddo
	  enddo

	  if(itera.ge.1) then
	    write(666,4) nel,1.e-6*sigestif,1.e-6*sigc,dbuckstif
	  endif

	endif






	endif



    3 format(i3,39x,f8.3,3x,f6.3,6x,'stiffener')
    4 format(i3,4x,f8.3,3x,f8.3,6x,'stiffener')      


      return
      end
