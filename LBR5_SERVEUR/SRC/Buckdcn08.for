      subroutine buckdcn08(nel,e1,sigy1,delta,hxr,dxr,wxr,txr,epsr,
     *				   epsa,q,eff)			!juil07

      use param_section
      
	implicit double precision (a-h,o-z)

	dimension eff(9690)

	double precision m														!plates
!	dimension dpsi(9,neto),dm(9,neto)		
!      dimension dsigepl(9,neto)										
     
      double precision ia,mstat									!longitudinals
	dimension daire(9),dmstat(9),daxen(9),dia(9)						
!	dimension dsigestif(9,neto)

!	double precision ipanel
!	dimension dsig1(9,neto),dsig2(9,neto)
!	dimension dipanel(9),dspanel(9)

	double precision, allocatable, save :: dpsi(:,:)
	double precision, allocatable, save :: dm(:,:)
	double precision, allocatable, save :: dsigepl(:,:)
	double precision, allocatable, save :: dsigestif(:,:)
	double precision, allocatable, save :: dsig1(:,:)
	double precision, allocatable, save :: dsig2(:,:)

	allocate (dpsi(9,neto))
	allocate (dm(9,neto))
	allocate (dsigepl(9,neto))
	allocate (dsigestif(9,neto))
	allocate (dsig1(9,neto))
	allocate (dsig2(9,neto))

c	*****************************************************************************************************

c	working stress car problèmes de convergence avec contraintes calculées par lbr-4 (subr. bateau et participation = pas de calcul de sensibilités)
c	--------------

c	if(nbuck.eq.0) then
c	  call annuld(dinpart,9*neto)
c	  call annuld(dipanel,9)
c	  call annuld(dspanel,9)

c	  inpart=0.
c	  ometpart=0.
c	  do ipan=1,neto
c	    read(302) epsap,epsrp,deltap,hyap,dyap,wyap,tyap,		
c     *		      hxrp,dxrp,wxrp,txrp,phil,q,epais
c		if(itype(ipan).ne.5) then
c	      sph2=vsin(-philn(ipan)/2.,0.d00)
c            c1=vcos(tetas(ipan),0.d00)
c	      s1=vsin(tetas(ipan),0.d00)
c            s2=vsin(tetas(ipan),-philn(ipan)/2.)
c		  phil=-philn(ipan)*pi/180.
c		  d2=q*(s1-2.*sph2*s2/phil)
	    
c		  omega=dabs(delt(ipan)*q*phil)
c	      ometpart=ometpart+part(ipan)*omega
c	      yneu=z(ipan,3)+d2
c	      ipanel=((c1*q*phil)**2+(s1*part(ipan)*delt(ipan))**2)*
c     *             part(ipan)*delt(ipan)*q*dabs(phil)/12.
c	      inpart=inpart+ipanel+part(ipan)*omega*(yneu-yneutpart)**2
c	    endif
c	  enddo
c	  if(sym.eq.1) inpart=2*inpart				
c	  if(sym.eq.2) inpart=4*inpart
c	  rewind 302
	  
c	  do 20 ipan=1,neto
c	    nbrxi=nvar(ipan,iboat)																	
c		read(302) epsap,epsrp,deltap,hyap,dyap,wyap,tyap,		
c     *		      hxrp,dxrp,wxrp,txrp,phil,q,epais
c		if(nbrxi.eq.0) goto 20
c		if(itype(ipan).ne.5) then
c		  sph2=vsin(-philn(ipan)/2.,0.d00)
c            c1=vcos(tetas(ipan),0.d00)
c	      s1=vsin(tetas(ipan),0.d00)
c            s2=vsin(tetas(ipan),-philn(ipan)/2.)
c		  phil=-philn(ipan)*pi/180.
c		  d2=q*(s1-2.*sph2*s2/phil)
	    
c		  omega=dabs(delt(ipan)*q*phil)
c	      yneu=z(ipan,3)+d2

c	      dipanel(1)=q*dabs(phil)/12.*
c     *	             (part(ipan)*((c1*q*phil)**2+
c     *				 (s1*part(ipan)*delt(ipan))**2)+
c     *				 part(ipan)*delt(ipan)*
c     *				(2.*(s1*part(ipan)*delt(ipan))*s1*part(ipan)))			! xi=1 épaisseur borde
c		  dipanel(6)=q*dabs(phil)/12.*				
c     *				 (part(ipan)*dxrp/epsrp*((c1*q*phil)**2+
c     *				 (s1*part(ipan)*delt(ipan))**2)+		
c     *				 part(ipan)*delt(ipan)*
c     *		         (2.*(s1*part(ipan)*delt(ipan))*s1*
c     *				 part(ipan)*dxrp/epsrp))								! xi=6 hauteur ame raidisseur
c            dipanel(7)=q*dabs(phil)/12.*				
c     *				 (part(ipan)*hxrp/epsrp*((c1*q*phil)**2+		
c     *				 (s1*part(ipan)*delt(ipan))**2)+
c     *				 part(ipan)*delt(ipan)*
c     *				 (2.*(s1*part(ipan)*delt(ipan))*s1*
c     *				 part(ipan)*hxrp/epsrp))								! xi=7 epaiss ame raidisseur
c            dipanel(8)=q*dabs(phil)/12.*				
c     *				 (part(ipan)*txrp/epsrp*((c1*q*phil)**2		
c     *				 +(s1*part(ipan)*delt(ipan))**2)+
c     *				 part(ipan)*delt(ipan)*
c     *				 (2.*(s1*part(ipan)*delt(ipan))*s1*
c     *				 part(ipan)*txrp/epsrp))								! xi=8 larg sem raidisseur
c		  dipanel(9)=q*dabs(phil)/12.*				
c     *				 ((-1.)*part(ipan)*(hxrp*dxrp+wxrp*txrp)/
c     *				 (epsrp**2)*((c1*q*phil)**2+(s1*part(ipan)*		
c     *				 delt(ipan))**2)+part(ipan)*delt(ipan)*
c     *				 (2.*(s1*part(ipan)*delt(ipan))*s1*(-1.)*
c     *				 part(ipan)*(hxrp*dxrp+wxrp*txrp)/(epsrp**2)))			! xi=9 larg sem raidisseur
c		  dspanel(1)=dabs(q*phil)											! xi=1 epaisseur borde
c		  dspanel(6)=dxrp/epsrp*dabs(q*phil)								! xi=6 haut ame raid
c		  dspanel(7)=hxrp/epsrp*dabs(q*phil)								! xi=7 epaiss ame raid
c		  dspanel(8)=txrp/epsrp*dabs(q*phil)								! xi=8 larg sem raid
c		  dspanel(9)=-(hxrp*dxrp+wxrp*txrp)/(epsrp*epsrp)*dabs(q*phil)		! xi=9 entredistance raid (epsrp)

c		  do l=1,nbrxi
c	        ll=nxit(l,ipan,iboat)		
c		    if(isymx.eq.0) then
c			  dinpart(ll,ipan)=dipanel(ll)+part(ipan)*dspanel(ll)*
c     *			    (yneu-yneutpart)**2+
c     *			    part(ipan)*omega*(2.*(yneu-yneutpart)*
c     *			    (-1.)*part(ipan)*dspanel(ll)*
c     *			    (yneu-yneutpart)/ometpart)
c			  if(sym.eq.1) dinpart(ll,ipan)=2.*dinpart(ll,ipan)
c	          if(sym.eq.2) dinpart(ll,ipan)=4.*dinpart(ll,ipan)
c		    else
c	          dinpart(ll,ipan)=dipanel(ll)+part(ipan)*dspanel(ll)*
c     *				          ((yneu-yneutpart)**2)
c			  if(sym.eq.1) dinpart(ll,ipan)=2.*dinpart(ll,ipan)
c	          if(sym.eq.2) dinpart(ll,ipan)=4.*dinpart(ll,ipan)
c			endif
c	      enddo
c	    endif
c   20	  continue
c	  rewind 302	  
c	endif

c      v1=z(nel,3)-yneutpart
c      v2=z(nel,4)-yneutpart

c	sig1=part(nel)*bm1(is)*v1/inpart
c	sig2=part(nel)*bm1(is)*v2/inpart

c	call annuld(dspanel,9)
c	call annuld(dsig1,9*neto)
c	call annuld(dsig2,9*neto)
c	do 21 ipan=1,neto
c	  nbrxi=nvar(ipan,iboat)																	
c	  read(302) epsap,epsrp,deltap,hyap,dyap,wyap,tyap,		
c     *			hxrp,dxrp,wxrp,txrp,phil,q,epais
c	  if(nbrxi.eq.0) goto 21
c	  if(itype(ipan).ne.5) then
c	    sph2=vsin(-philn(ipan)/2.,0.d00)
c          c1=vcos(tetas(ipan),0.d00)
c	    s1=vsin(tetas(ipan),0.d00)
c          s2=vsin(tetas(ipan),-philn(ipan)/2.)
c		phil=-philn(ipan)*pi/180.
c		d2=q*(s1-2.*sph2*s2/phil)
	    
c	    yneu=z(ipan,3)+d2
c	    dspanel(1)=dabs(q*phil)											! xi=1 epaisseur borde
c      	dspanel(6)=dxrp/epsrp*dabs(q*phil)								! xi=6 haut ame raid
c		dspanel(7)=hxrp/epsrp*dabs(q*phil)								! xi=7 epaiss ame raid
c		dspanel(8)=txrp/epsrp*dabs(q*phil)								! xi=8 larg sem raid
c		dspanel(9)=-(hxrp*dxrp+wxrp*txrp)/(epsrp*epsrp)*dabs(q*phil)	! xi=9 entredistance raid (epsrp)

c		do l=1,nbrxi
c		  ll=nxit(l,ipan,iboat)
c		  if(isymx.eq.0) then
c	        dsig1(ll,ipan)=part(nel)*bm1(is)*
c     *					   ((-1.)*part(ipan)*dspanel(ll)*
c     *				       (yneu-yneutpart)/ometpart*inpart-
c     *					   v1*dinpart(ll,ipan))/inpart**2
c			dsig2(ll,ipan)=part(nel)*bm1(is)*
c     *					   ((-1.)*part(ipan)*dspanel(ll)*
c     *				       (yneu-yneutpart)/ometpart*inpart-
c     *					   v2*dinpart(ll,ipan))/inpart**2
c	      else
c	        dsig1(ll,ipan)=part(nel)*bm1(is)*
c     *					   v1*(-dinpart(ll,ipan)/inpart**2)
c		    dsig2(ll,ipan)=part(nel)*bm1(is)*
c     *					   v2*(-dinpart(ll,ipan)/inpart**2)
c	      endif
c	    enddo
c	  endif
c   21	continue
c	rewind 302


c	elastic buckling of plates
c	--------------------------

	phil=-panneau(nel).phil*pi/180.							!juil07
	if((modes(nel).eq.'EE2').or.(modes(nel).eq.'ee2')) then	!juil07
	  bucksp=epsr											!juil07
	  dbucksp=1.											!juil07
	else													!juil07
	  bucksp=(dabs(q*phil)*epsr)/(dabs(q*phil)+epsr)		!juil07
	  dbucksp=dabs(q*phil)**2/(dabs(q*phil)+epsr)**2		!juil07
	endif

    	call annuld(dpsi,9*neto)
	call annuld(dm,9*neto)
	call annuld(dsigepl,9*neto)
	call annuld(ddbuckpl_08,9*neto)

c	if((sig1.ge.0.).and.(sig2.ge.0.)) then	
	if((eff(4540).ge.0.).and.(eff(4570).ge.0.)) then	!panneau en traction
	  dbuckpl_08=0.
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
	      nbrxi=nvar(ipan,iboat)
	      do l=1,nbrxi
	        ll=nxit(l,ipan,iboat)
	        dsig1(ll,ipan)=-sens3(1,     1,ll,ipan)			!ipts =1      (y = 0)
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
	      nbrxi=nvar(ipan,iboat)
	      do l=1,nbrxi
	        ll=nxit(l,ipan,iboat)
	        dsig1(ll,ipan)=-sens3(1,     1,ll,ipan)			!ipts =1      (y = 0)
		    dsig2(ll,ipan)=-sens3(1,3,ll,ipan)			!ipts =iptmax (y = b, largeur du panneau)
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
	        !read(iu_25(iboat)) epsap,epsrp,deltap,hyap,dyap,wyap,tyap,		
     *		!          hxrp,dxrp,wxrp,txrp,phil,q,epais,epsa2
		    if(epsa2.ge.(0.00001)) then
			!  backspace(iu_25(iboat))
			!  read(iu_25(iboat)) epsap,epsrp,deltap,hyap,dyap,wyap,		
     *		!            tyap,hxrp,dxrp,wxrp,txrp,phil,q,epais,epsa2,
     *		!		    hya2,dya2,wya2,tya2							!juil07
              endif
	        goto 40
	      else
	        !read(iu_25(iboat))
	      endif
	    enddo
   40	    continue !rewind iu_25(iboat)

		epsa=panneau(nel).epsa
		epsa2=panneau(nel).epsa2
		q=panneau(nel).q
		hya2=panneau(nel).hya2
		dya2=panneau(nel).dya2
		wya2=panneau(nel).wya2
		tya2=panneau(nel).tya2

          phil=-dabs(panneau(nel).phil)*pi/180.
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
	  dbuckpl_08=siga/sigc
	  do ipan=1,neto
	    nbrxi=nvar(ipan,iboat)
	    do l=1,nbrxi
	      ll=nxit(l,ipan,iboat)
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
	        ddbuckpl_08(ll,ipan)=(dsig1(ll,ipan)*sigc-
     *					      siga*dsigepl(ll,ipan))/sigc**2
	      else
	        ddbuckpl_08(ll,ipan)=(dsig2(ll,ipan)*sigc-
     *					      siga*dsigepl(ll,ipan))/sigc**2
		  endif
	    enddo
	  enddo
	  if(itera.ge.1) write(iu_31(iboat),2) nel,1.e-6*sigepl,1.e-6*sigc,
     *dbuckpl_08
	endif

    1 format(i3,39x,f8.3,3x,f6.3,6x,'plate')
    2 format(i3,15x,f8.3,16x,f8.3,3x,f6.3,6x,'plate')

   30 continue


c	elastic buckling of longitudinals (column buckling)
c	---------------------------------
	
	call annuld(daire,9)
	call annuld(dmstat,9)
	call annuld(daxen,9)
	call annuld(dia,9)
	call annuld(dsigestif,9*neto)
	call annuld(ddbuckstif_08,9*neto)
	
	if(dbuckpl_08.eq.0.) then	
c	if((eff(7855).ge.0).and.(eff(7885).ge.0)) then
	  dbuckstif_08=0.
	  deallocate (dpsi)
	  deallocate (dm)
	  deallocate (dsigepl)
	  deallocate (dsigestif)
	  deallocate (dsig1)
	  deallocate (dsig2)
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
     *		 2.*wxr*txr*(txr/2.+hxr+delta-axen)*(-daxen(8))
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





	  dbuckstif_08=siga/sigc
	  do ipan=1,neto
	    nbrxi=nvar(ipan,iboat)
		do l=1,nbrxi
	      ll=nxit(l,ipan,iboat)
		  if(sigestif.le.sigy1/2.) then
  		    if(sig1.ge.sig2) then
c	          dsig1(ll,ipan)=-sens2(10,1,ll,ipan)
	          ddbuckstif_08(ll,ipan)=(dsig1(ll,ipan)*sigc-
     *						     siga*dsigestif(ll,ipan))/sigc**2
	        else
c	          dsig2(ll,ipan)=-sens2(10,3,ll,ipan)
			  ddbuckstif_08(ll,ipan)=(dsig2(ll,ipan)*sigc-
     *						     siga*dsigestif(ll,ipan))/sigc**2
			endif
	      else
			if(sig1.ge.sig2) then
c			  dsig1(ll,ipan)=-sens2(10,1,ll,ipan)
			  ddbuckstif_08(ll,ipan)=(dsig1(ll,ipan)*sigc-
     *						      siga*sigy1*(0.25*sigy1*
     *						      dsigestif(ll,ipan)/sigestif**2))
     *							  /sigc**2
			else
c	          dsig2(ll,ipan)=-sens2(10,3,ll,ipan)
			  ddbuckstif_08(ll,ipan)=(dsig2(ll,ipan)*sigc-
     *							  siga*sigy1*(0.25*sigy1*
     *						      dsigestif(ll,ipan)/sigestif**2))
     *							  /sigc**2
			endif
		  endif
		enddo
	  enddo

	  if(itera.ge.1) then
	    write(iu_31(iboat),4) nel,1.e-6*sigestif,1.e-6*sigc,
     *							dbuckstif_08
	  endif

	endif

	deallocate (dpsi)
	deallocate (dm)
	deallocate (dsigepl)
	deallocate (dsigestif)
	deallocate (dsig1)
	deallocate (dsig2)

    3 format(i3,39x,f8.3,3x,f6.3,6x,'stiffener')
    4 format(i3,4x,f8.3,3x,f8.3,6x,'stiffener')      


      return
      end
