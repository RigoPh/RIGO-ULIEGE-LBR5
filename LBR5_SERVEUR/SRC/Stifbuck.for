      subroutine stifbuck(nel,e1,sigy1,coefk1,						!r&d15
     *					epsa,epsr,delta,hxr,dxr,wxr,txr,wya,q,		!juil07
     *					epsa2,wya2,dsigr,ddsigr,ntot_)

	
      use param_section
!      use param_cout
!      use param_opti_local
      

      implicit double precision (a-h,o-z)

	double precision m,ia,it,ip,iw,mstat,kp,k

	integer fin

	dimension ddsigpl(9,neto),ddsigr(ntot_)
	
!	dimension dpsi(9,neto),dm(9,neto),dsigepl(9,neto)					!plates			
     
      dimension daire(9),dmstat(9),daxen(9),dia(9),dit(9),dip(9),diw(9)	!longitudinals
!	dimension dsigestif(9,neto),dkp(9,neto),dc(9,neto),dk(9,neto)

	double precision, allocatable, save :: dpsi(:,:)
	double precision, allocatable, save :: dm(:,:)
	double precision, allocatable, save :: dsigepl(:,:)

	double precision, allocatable, save :: dsigestif(:,:)
	double precision, allocatable, save :: dkp(:,:)
	double precision, allocatable, save :: dc(:,:)
	double precision, allocatable, save :: dk(:,:)

	allocate (dpsi(9,neto))
	allocate (dm(9,neto))
	allocate (dsigepl(9,neto))

	allocate (dsigestif(9,neto))
	allocate (dkp(9,neto))
	allocate (dc(9,neto))
	allocate (dk(9,neto))


c	*****************************************************************************************************

c	coefk=175.0e+06/sigm1		!r&d15

c	elastic buckling of plates
c	--------------------------

	phil=-panneau(nel).phil*pi/180.							!juil07
	if((modes(nel).eq.'EE2').or.(modes(nel).eq.'ee2')) then	!juil07
	  bucksp=epsr											!juil07
	  dbucksp=1.											!juil07
	else													!juil07
	  bucksp=(dabs(q*phil)*epsr)/(dabs(q*phil)+epsr)		!juil07
	  dbucksp=(dabs(q*phil))**2/(dabs(q*phil)+epsr)**2		!juil07
	endif

	call annuld(dpsi,9*neto)
	call annuld(dm,9*neto)
	call annuld(dsigepl,9*neto)
	call annuld(ddsigpl,9*neto)
	
	if((signet(1).ge.0.).and.(signet(2).ge.0.)) then							!panneau en traction
	  dsigpl=0.
	  goto 30											
	else
	  if((-signet(1).ge.0.).and.(-signet(2).ge.0.)) then						!panneau totalement en compression
	    if((-signet(1)).le.(-signet(2))) then
	      siga=-signet(2)
	      psi=signet(1)/signet(2)
	    else
	      siga=-signet(1)
	      psi=signet(2)/signet(1)
	    endif
	    if(siga.lt.(30.0e+06/coefk1)) siga=30.0e+06/coefk1			!r&d15
		do ipan=1,neto
	      nbrxi=nvar(ipan,iboat)
	      do l=1,nbrxi
	        ll=nxit(l,ipan,iboat)
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
		if(siga.lt.(30.0e+06/coefk1)) siga=30.0e+06/coefk1			!r&d15
	    psi=0.
	    do ipan=1,neto
	      nbrxi=nvar(ipan,iboat)
	      do l=1,nbrxi
	        ll=nxit(l,ipan,iboat)
		    dpsi(ll,ipan)=0.
	      enddo
	    enddo
	  endif
	  if(hxr.gt.0.010) then												!plating with longitudinal stiffeners
          m=8.4/(psi+1.1)
	    sigepl=0.9*m*e1*(delta/bucksp)**2								!juil07
	  else																!plating with transverse stiffeners
		if(epsa2.ge.(0.00001)) then
		  phil=-panneau(nel).phil*pi/180.
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
	      phil=-panneau(nel).phil*pi/180.
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
	  do ipan=1,neto
	    nbrxi=nvar(ipan,iboat)
	    do l=1,nbrxi
	      ll=nxit(l,ipan,iboat)
	      if(hxr.gt.0.010) then											!plating with longitudinal stiffeners
		    dm(ll,ipan)=-8.4*dpsi(ll,ipan)/((psi+1.1)*(psi+1.1))
			if((nel.eq.ipan).and.(ll.eq.1)) then
     	          dsigepl(ll,ipan)=0.9*e1*(dm(ll,ipan)*
     *					       (delta/bucksp)**2+2*m*delta/bucksp**2)		!juil07
		    else
		      if((nel.eq.ipan).and.(ll.eq.9)) then
     			    dsigepl(ll,ipan)=0.9*e1*(dm(ll,ipan)*
     *					         (delta/bucksp)**2+2*m*(delta/bucksp)*		!juil07
     *					         (-delta*dbucksp/bucksp**2))				!juil07
			  else
			    dsigepl(ll,ipan)=0.9*e1*(delta/bucksp)**2*					!juil07
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
	          ddsigpl(ll,ipan)=(-siga*dsigepl(ll,ipan))/sigc**2
	        else
			  if((-signet(1)).ge.(-signet(2))) then
	            ddsigpl(ll,ipan)=(-dsignet(1,ll,ipan)*sigc-
     *						     siga*dsigepl(ll,ipan))/sigc**2
	          else
	            ddsigpl(ll,ipan)=(-dsignet(2,ll,ipan)*sigc-
     *						     siga*dsigepl(ll,ipan))/sigc**2
			  endif
	        endif
	      else
	        if(siga.lt.(30.0e+06/coefk1)) then								!r&d15
	          ddsigpl(ll,ipan)=(-siga*sigy1*(0.25*sigy1*
     *						   dsigepl(ll,ipan)/sigepl**2))/sigc**2
			else
			  if((-signet(1)).ge.(-signet(2))) then
			    ddsigpl(ll,ipan)=(-dsignet(1,ll,ipan)*sigc-
     *						     siga*sigy1*(0.25*sigy1*
     *						     dsigepl(ll,ipan)/sigepl**2))/sigc**2
			  else
	            ddsigpl(ll,ipan)=(-dsignet(2,ll,ipan)*sigc-
     *						     siga*sigy1*(0.25*sigy1*
     *						     dsigepl(ll,ipan)/sigepl**2))/sigc**2
			  endif
			endif
		  endif
		enddo
	  enddo
	endif

    1 format(i3,39x,f8.3,3x,f6.3,6x,'plate')
    2 format(i3,15x,f8.3,16x,f8.3,3x,f6.3,6x,'plate')


c	elastic buckling of longitudinals
c	---------------------------------
	
   30	call annuld(dkp,9*neto)
	call annuld(dc,9*neto)
	call annuld(dk,9*neto)
	call annuld(dsigestif,9*neto)
	call annuld(ddsigr,ntot_)
	
	if((signet(1).ge.0).and.(signet(2).ge.0)) then
	  dsigr=0.
	  deallocate (dpsi)
	  deallocate (dm)
	  deallocate (dsigepl)

	  deallocate (dsigestif)
	  deallocate (dkp)
	  deallocate (dc)
	  deallocate (dk)
	  return		
	else
	  if((-signet(1)).ge.(-signet(2))) then
	    siga=-signet(1)
	  else
	    siga=-signet(2)
	  endif
	  if(siga.lt.(30.0e+06/coefk1)) siga=30.0e+06/coefk1			!r&d15


c	column buckling
c	---------------

	  aire=delta*epsr+hxr*dxr+wxr*txr

	  mstat=delta*epsr*delta/2.+hxr*dxr*(hxr/2.+delta)+
     *	    wxr*txr*(txr/2.+hxr+delta)

	  axen=mstat/aire

	  ia=epsr*delta**3/12.+delta*epsr*(delta/2.-axen)**2+
     *	 dxr*hxr**3/12.+hxr*dxr*(hxr/2.+delta-axen)**2+
     *	 wxr*txr**3/12.+wxr*txr*(txr/2.+hxr+delta-axen)**2

	  sigestifc=10.*e1*ia/(aire*epsa**2)

	  beta=1.1
	  if(sigestifc.le.sigy1/2.) then
	    sigcc=sigestifc/beta
	  else
	    sigcc=sigy1*(1-0.25*sigy1/sigestifc)/beta
	  endif

	  goto 10 

c	*************************************************************************************
   13	  call annuld(daire,9)
	  call annuld(dmstat,9)
	  call annuld(daxen,9)
	  call annuld(dia,9)
	  
	  daire(1)=epsr
	  daire(6)=dxr
	  daire(7)=hxr
	  daire(8)=txr
	  daire(9)=delta
	  
	  dmstat(1)=epsr*delta+hxr*dxr+wxr*txr
	  dmstat(6)=dxr*(hxr/2.+delta)+0.5*hxr*dxr+wxr*txr
	  dmstat(7)=hxr*(hxr/2.+delta)
	  dmstat(8)=txr*(txr/2.+hxr+delta)
	  dmstat(9)=delta**2/2.

	  do i=1,9
	    daxen(i)=(dmstat(i)*aire-mstat*daire(i))/aire**2
	  enddo

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
	  
	  do i=1,9
	    dsigestif(i,nel)=10.*e1/epsa**2*(dia(i)*aire-ia*daire(i))/
     *				     aire**2
	  enddo
	  dsigestif(5,nel)=-2.*10.*e1*ia/aire*(1./epsa**3)

	  goto 16
c	*************************************************************************************

c	torsional buckling
c	------------------

   10	  it=(1./3.)*(hxr*dxr**3+wxr*txr**3*(1.-0.63*txr/wxr))

	  ip=(hxr**3*dxr)/3.+hxr**2*wxr*txr

	  iw=txr*wxr**3*hxr**2/12.

	  kp=1.-siga/sigepl
	  if(kp.le.0.1) then
	    kp=0.1
	  endif

	  c=kp*e1*delta**3/(3*epsr*(1.+1.33*kp*hxr*delta**3/
     *    (epsr*dxr**3)))

	  k=c*epsa**4/(pi**4*e1*iw)

	  m=1.
	  fin=0
	  do while(fin.eq.0)
		if((k.gt.((m-1)**2*m**2)).and.(k.le.(m**2*(m+1)**2))) then
		  fin=1
		else 
	      m=m+1.
	    endif
	  enddo 
	  
	  sigestift=pi**2*e1*iw/(ip*epsa**2)*(m**2+k/m**2)+0.385*e1*it/ip

	  beta=1.1
	  if(sigestift.le.sigy1/2.) then
	    sigct=sigestift/beta
	  else
	    sigct=sigy1*(1-0.25*sigy1/sigestift)/beta
	  endif
	  
	  goto 11

c	*************************************************************************************
   14   call annuld(dit,9)
	  call annuld(dip,9)
	  call annuld(diw,9)

	  dit(6)=(1./3.)*dxr**3
	  dit(7)=hxr*dxr**2
	  dit(8)=(1./3.)*(txr**3*(1.-0.63*txr/wxr)+
     *	      wxr*txr**3*(0.63*txr/wxr**2))

	  dip(6)=dxr*hxr**2+2*hxr*wxr*txr
	  dip(7)=hxr**3/3.
	  dip(8)=txr*hxr**2

	  diw(6)=txr*wxr**3*hxr/6.
	  diw(8)=txr*wxr**2*hxr**2/4.

	  do ipan=1,neto
	    nbrxi=nvar(ipan,iboat)
	    do l=1,nbrxi
	      ll=nxit(l,ipan,iboat)
		  if(siga.lt.(30.0e+06/coefk1)) then						!r&d15
		    dkp(ll,ipan)=-(-siga*dsigepl(ll,ipan))/sigepl**2
	      else
	        if((-signet(1)).ge.(-signet(2))) then
			  dkp(ll,ipan)=-(-dsignet(1,ll,ipan)*sigepl-
     *			           siga*dsigepl(ll,ipan))/sigepl**2
	        else
			  dkp(ll,ipan)=-(-dsignet(2,ll,ipan)*sigepl-
     *			           siga*dsigepl(ll,ipan))/sigepl**2
			endif
		  endif
		  if(kp.eq.0.1) dkp(ll,ipan)=0.
		enddo
	  enddo

	  do ipan=1,neto
	    nbrxi=nvar(ipan,iboat)
	    do l=1,nbrxi
	      ll=nxit(l,ipan,iboat)
		  dc(ll,ipan)=(((e1*delta**3/(3*epsr))*dkp(ll,ipan))*(1.+
     *		          (1.33*hxr*delta**3/(epsr*dxr**3))*kp)-
     *				  ((e1*delta**3/(3*epsr))*kp)*((1.33*hxr*delta**3/
     *				  (epsr*dxr**3))*dkp(ll,ipan)))/(1.+
     *				  (1.33*hxr*delta**3/(epsr*dxr**3))*kp)**2				  	
		enddo
	  enddo
	  dc(1,nel)=(((e1*delta**3/(3*epsr))*dkp(1,nel)+
     *			(e1*delta**2/epsr)*kp)*
     *		    (1.+(1.33*hxr*delta**3/(epsr*dxr**3))*kp)-
     *			((e1*delta**3/(3*epsr))*kp)*((1.33*hxr*delta**3/
     *			(epsr*dxr**3))*dkp(1,nel)+(3.99*hxr*delta**2/
     *			(epsr*dxr**3))*kp))/(1.+
     *			(1.33*hxr*delta**3/(epsr*dxr**3))*kp)**2
	  dc(6,nel)=(((e1*delta**3/(3*epsr))*dkp(6,nel))*(1.+
     *		    (1.33*hxr*delta**3/(epsr*dxr**3))*kp)-
     *			((e1*delta**3/(3*epsr))*kp)*((1.33*hxr*delta**3/
     *			(epsr*dxr**3))*dkp(6,nel)+(1.33*delta**3/
     *			(epsr*dxr**3))*kp))/(1.+
     *			(1.33*hxr*delta**3/(epsr*dxr**3))*kp)**2
	  dc(7,nel)=(((e1*delta**3/(3*epsr))*dkp(7,nel))*(1.+
     *		    (1.33*hxr*delta**3/(epsr*dxr**3))*kp)-
     *			((e1*delta**3/(3*epsr))*kp)*((1.33*hxr*delta**3/
     *			(epsr*dxr**3))*dkp(7,nel)+(-3.99*hxr*delta**3*
     *			epsr*dxr**2/(epsr*dxr**3)**2)*kp))/(1.+
     *			(1.33*hxr*delta**3/(epsr*dxr**3))*kp)**2
	  dc(9,nel)=(((e1*delta**3/(3*epsr))*dkp(9,nel)+
     *			(-3*e1*delta**3/(3*epsr)**2)*kp)*(1.+
     *		    (1.33*hxr*delta**3/(epsr*dxr**3))*kp)-
     *			((e1*delta**3/(3*epsr))*kp)*((1.33*hxr*delta**3/
     *			(epsr*dxr**3))*dkp(9,nel)+(-1.33*hxr*delta**3*
     *			dxr**3/(epsr*dxr**3)**2)))/(1.+
     *			(1.33*hxr*delta**3/(epsr*dxr**3))*kp)**2

	  do ipan=1,neto
	    nbrxi=nvar(ipan,iboat)
	    do l=1,nbrxi
	      ll=nxit(l,ipan,iboat)
		  if(ipan.eq.nel) then
		    dk(ll,ipan)=epsa**4/(pi**4*e1)*(dc(ll,ipan)*iw-c*diw(ll))/
     *					iw**2
		  else
	        dk(ll,ipan)=epsa**4/(pi**4*e1)*dc(ll,ipan)/iw
	      endif
		enddo
	  enddo
	  dk(5,nel)=dk(5,nel)+4*epsa**3/(pi**4*e1)*c/iw

	  do ipan=1,neto
	  	nbrxi=nvar(ipan,iboat)
	    do l=1,nbrxi
	      ll=nxit(l,ipan,iboat)
	      if(ipan.eq.nel) then
		    dsigestif(ll,ipan)=pi**2*e1/epsa**2*
     *						   ((diw(ll)*ip-iw*dip(ll))/ip**2*
     *					       (m**2+k/m**2)+iw/ip*dk(ll,ipan)/m**2)+
     *						   0.385*e1*(dit(ll)*ip-it*dip(ll))/ip**2
		  else
	        dsigestif(ll,ipan)=pi**2*e1/epsa**2*iw/ip*dk(ll,ipan)/m**2
	      endif
	    enddo
	  enddo
	  dsigestif(5,nel)=dsigestif(5,nel)+-2*pi**2*e1/epsa**3*iw/ip*
     *				  (m**2+k/m**2)

	  goto 16
c	**********************************************************************************

c	web buckling
c	------------
   11	  sigestifl=3.8*e1*(dxr/hxr)**2
        beta=1.
        if(sigestifl.le.sigy1/2.) then
	    sigcl=sigestifl/beta
	  else
	    sigcl=sigy1*(1-0.25*sigy1/sigestifl)/beta
	  endif
	  goto 12

c	**********************************************************************************
   15   dsigestif(6,nel)=7.6*e1*(dxr/hxr)*(-dxr/hxr**2)
	  dsigestif(7,nel)=7.6*e1*(dxr/hxr)*(1./hxr)
	
	  goto 16
c	**********************************************************************************

   12	  sigc=min(sigcc,sigct,sigcl)
	  beta=1.1
	  if(sigcc.eq.sigc) then
	    sigestif=sigestifc
		goto 13
	  endif
	  if(sigct.eq.sigc) then
	    sigestif=sigestift
		goto 14
	  endif
	  if(sigcl.eq.sigc) then
	    sigestif=sigestifl
		beta=1.
		goto 15
	  endif
   

   16	  dsigr=siga/sigc
	  l1=0
	  do ipan=1,neto
	    nbrxi=nvar(ipan,iboat)
		do l=1,nbrxi
	      ll=nxit(l,ipan,iboat)
		  if(sigestif.le.sigy1/2.) then
			if(siga.lt.(30.0e+06/coefk1)) then						!r&d15
	          ddsigr(l1+l)=(-siga*dsigestif(ll,ipan)/beta)/
     *					   sigc**2
			else
			  if((-signet(1)).ge.(-signet(2))) then
			    ddsigr(l1+l)=(-dsignet(1,ll,ipan)*sigc-siga*
     *						 dsigestif(ll,ipan)/beta)/sigc**2
	          else
			    ddsigr(l1+l)=(-dsignet(2,ll,ipan)*sigc-siga*
     *						 dsigestif(ll,ipan)/beta)/sigc**2
			  endif
	        endif
	      else
	        if(siga.lt.(30.0e+06/coefk1)) then						!r&d15
	          ddsigr(l1+l)=(-siga*sigy1*(0.25*sigy1*
     *					   dsigestif(ll,ipan)/sigestif**2)/beta)/
     *					   sigc**2
			else
			  if((-signet(1)).ge.(-signet(2))) then
		        ddsigr(l1+l)=(-dsignet(1,ll,ipan)*sigc-siga*
     *						 sigy1*(0.25*sigy1*dsigestif(ll,ipan)/
     *						 sigestif**2)/beta)/sigc**2							   
			  else
			    ddsigr(l1+l)=(-dsignet(2,ll,ipan)*sigc-siga*
     *						 sigy1*(0.25*sigy1*dsigestif(ll,ipan)/
     *						 sigestif**2)/beta)/sigc**2							   
			  endif
			endif
		  endif
		enddo
		l1=l1+nbrxi
	  enddo

	endif

	deallocate (dpsi)
	deallocate (dm)
	deallocate (dsigepl)

	deallocate (dsigestif)
	deallocate (dkp)
	deallocate (dc)
	deallocate (dk)

    3 format(i3,39x,f8.3,3x,f6.3,6x,'stiffener')
    4 format(i3,4x,f8.3,3x,f8.3,3x,f8.3,5x,f8.3,3x,f6.3,6x,'stiffener')      


      return
      end
