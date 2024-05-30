	subroutine inertia(yn,dyn,in,din,icor,ishear)					

      

      use param_section
!      use param_cout
!      use param_opti_local
      
	
	implicit double precision (a-h,o-z)

	double precision in,inelt

	dimension dyn(9,neto)
	dimension din(9,neto)
!	dimension xi(nsol),xf(nsol)
	
	dimension dinelt(9),dsyelt(9)
	
c	*****************************************************************************************************

	in=0.
	air=0.
	sy=0.
	call annuld(dyn,9*neto)
	call annuld(din,9*neto)

c	position de l'axe neutre
c	------------------------
!	rewind iu_25(iboat)
!	rewind iu_23(iboat)
	do ipan=1,neto
	  !read(iu_25(iboat)) epsa,epsr,delta,hya,dya,wya,tya,		
     *  !          hxr,dxr,wxr,txr
!	  read(iu_23(iboat)) abtr,phil,mt,teta,xi,xf,hxtr,wxtr,q,
!     *           kse,kst,ksa,
!     *           txtr,dxtr
        epsa = panneau(ipan).epsa
	  epsr = panneau(ipan).epsr
	  delta = panneau(ipan).delta
	  hya = panneau(ipan).hya
	  dya = panneau(ipan).dya
	  wya = panneau(ipan).wya
	  tya = panneau(ipan).tya
	  hxr = panneau(ipan).hxr
	  dxr = panneau(ipan).dxr
	  wxr = panneau(ipan).wxr
	  txr = panneau(ipan).txr

	  phil = dabs(panneau(ipan).phil)
	  mt = panneau(ipan).mt
	  q = panneau(ipan).q

	  if(itype(ipan).ne.5) then
	    if(icor.ne.0) then
	      if(ishear.ne.1) then
c		    dxr=dxr+corro(ipan,3)
c		    txr=txr+corro(ipan,3)
			deltpan=panneau(ipan).delt+panneau(ipan).corro(1)+
     *			panneau(ipan).corro(3)*(hxr+wxr)/epsr
	        do i=1,mt
	          deltpan=deltpan+panneau(ipan).corro(3)*
     *				(panneau(ipan).hxtr(i) + 
     *				panneau(ipan).wxtr(i))/(phil*q*pi/180.)
			enddo
	      else
	        deltpan=delta + panneau(ipan).corro(1)
		  endif									
	    else
	      if(ishear.ne.1) then
		    deltpan=panneau(ipan).delt
		  else
		    deltpan=delta
		  endif	
		endif

c		if(icor.ne.0) then
c		  if(ishear.ne.1) then
c		    deltpan=delt(ipan)-(hxr*dxr+wxr*txr)/epsr-delta
c			dxr=dxr+corro(ipan,3)
c		    txr=txr+corro(ipan,3)
c		  endif
c	      delta=delta+corro(ipan,1)									
c		  if(ishear.ne.1) then
c		    deltpan=deltpan+(hxr*dxr+wxr*txr)/epsr+delta
c	      else
c	        deltpan=delta
c	      endif
c	    else
c	      if(ishear.ne.1) then
c		    deltpan=delt(ipan)
c		  else
c		    deltpan=delta
c		  endif	
c		endif				

		sph2=vsin(-panneau(ipan).phil/2.,0.d00)
 	    s1=vsin(panneau(ipan).tetas,0.d00)
          s2=vsin(panneau(ipan).tetas,-panneau(ipan).phil/2.)
		phil=-panneau(ipan).phil*pi/180.
		d2=q*(s1-2.*sph2*s2/phil)
	    
		omega=dabs(deltpan*q*phil)
	    air=air+panneau(ipan).part*omega
	    yneu=z(ipan,3)+d2
	    sy=sy+panneau(ipan).part*omega*yneu
	  endif
	enddo
	yn=sy/air

c	moment d'inertie
c	----------------
!	rewind iu_25(iboat)
!	rewind iu_23(iboat)
	do ipan=1,neto
!	  read(iu_25(iboat)) epsa,epsr,delta,hya,dya,wya,tya,		
!     *            hxr,dxr,wxr,txr
!	  read(iu_23(iboat)) abtr,phil,mt,teta,xi,xf,hxtr,wxtr,q,
!     *           kse,kst,ksa,
!     *           txtr,dxtr
        epsa = panneau(ipan).epsa
	  epsr = panneau(ipan).epsr
	  delta = panneau(ipan).delta
	  hya = panneau(ipan).hya
	  dya = panneau(ipan).dya
	  wya = panneau(ipan).wya
	  tya = panneau(ipan).tya
	  hxr = panneau(ipan).hxr
	  dxr = panneau(ipan).dxr
	  wxr = panneau(ipan).wxr
	  txr = panneau(ipan).txr

	  phil = dabs(panneau(ipan).phil)
	  mt = panneau(ipan).mt
	  q = panneau(ipan).q

	  if(itype(ipan).ne.5) then
	    if(icor.ne.0) then
	      if(ishear.ne.1) then
c		    dxr=dxr+corro(ipan,3)
c		    txr=txr+corro(ipan,3)
			deltpan=panneau(ipan).delt+panneau(ipan).corro(1)+
     *				panneau(ipan).corro(3)*(hxr+wxr)/epsr
	        do i=1,mt
	          deltpan=deltpan+panneau(ipan).corro(3)*
     *			(panneau(ipan).hxtr(i) + 
     *			panneau(ipan).wxtr(i))/(phil*q*pi/180.)
			enddo
	      else
	        deltpan=delta+panneau(ipan).corro(1)
		  endif									
	    else
	      if(ishear.ne.1) then
		    deltpan=panneau(ipan).delt
		  else
		    deltpan=delta
		  endif	
		endif

c		if(icor.ne.0) then
c		  if(ishear.ne.1) then
c		    deltpan=delt(ipan)-(hxr*dxr+wxr*txr)/epsr-delta
c			dxr=dxr+corro(ipan,3)
c		    txr=txr+corro(ipan,3)
c		  endif
c	      delta=delta+corro(ipan,1)									
c		  if(ishear.ne.1) then
c		    deltpan=deltpan+(hxr*dxr+wxr*txr)/epsr+delta
c	      else
c	        deltpan=delta
c	      endif
c	    else
c	      if(ishear.ne.1) then
c		    deltpan=delt(ipan)
c		  else
c		    deltpan=delta
c		  endif	
c		endif					

		sph2=vsin(-panneau(ipan).phil/2.,0.d00)
          c19=vcos(panneau(ipan).tetas,0.d00)
	    s1=vsin(panneau(ipan).tetas,0.d00)
          s2=vsin(panneau(ipan).tetas,-panneau(ipan).phil/2.)
		phil=-panneau(ipan).phil*pi/180.
		d2=q*(s1-2.*sph2*s2/phil)
	    
		omega=dabs(deltpan*q*phil)
	    yneu=z(ipan,3)+d2
	    inelt=((c19*q*phil)**2+(s1*panneau(ipan).part*deltpan)**2)*
     *           panneau(ipan).part*deltpan*q*dabs(phil)/12.
	    in=in+inelt+panneau(ipan).part*omega*(yneu-yn)**2
	  endif
	enddo 
	if(sym.eq.1) in=2*in				
	if(sym.eq.2) in=4*in

!	rewind iu_25(iboat)
!	rewind iu_23(iboat)

	if(iopti.ge.1) then

c	sensibilités pour la position de l'axe neutre	
c	---------------------------------------------
	  do 1 ipan=1,neto
	    nbrxi=nvar(ipan,iboat)																	
!	    read(iu_25(iboat)) epsa,epsr,delta,hya,dya,wya,tya,		
!     *              hxr,dxr,wxr,txr,phil,q

		epsa = panneau(ipan).epsa
	    epsr = panneau(ipan).epsr
	    delta = panneau(ipan).delta
	    hya = panneau(ipan).hya
	    dya = panneau(ipan).dya
	    wya = panneau(ipan).wya
	    tya = panneau(ipan).tya
	    hxr = panneau(ipan).hxr
	    dxr = panneau(ipan).dxr
	    wxr = panneau(ipan).wxr
	    txr = panneau(ipan).txr

	    phil = dabs(panneau(ipan).phil) !TODO revérifier ici signe de phi
		q = panneau(ipan).q

          if(nbrxi.eq.0) goto 1
	    if(itype(ipan).ne.5) then
	      if(icor.ne.0) then
c		    delta=delta+corro(ipan,1)									
	        dxr=dxr+panneau(ipan).corro(3)
		    txr=txr+panneau(ipan).corro(3)
		  endif										
		
		  sph2=vsin(-panneau(ipan).phil/2.,0.d00)
            c19=vcos(panneau(ipan).tetas,0.d00)
	      s1=vsin(panneau(ipan).tetas,0.d00)
            s2=vsin(panneau(ipan).tetas,-panneau(ipan).phil/2.)
		  phil=-panneau(ipan).phil*pi/180.
		  d2=q*(s1-2.*sph2*s2/phil)
	    
	      yneu=z(ipan,3)+d2

		  dsyelt(1)=dabs(q*phil)									! xi=1 epaisseur borde
		  dsyelt(6)=dxr/epsr*dabs(q*phil)							! xi=6 haut ame raid
		  dsyelt(7)=hxr/epsr*dabs(q*phil)							! xi=7 epaiss ame raid
		  dsyelt(8)=txr/epsr*dabs(q*phil)							! xi=8 larg sem raid
		  dsyelt(9)=-(hxr*dxr+wxr*txr)/(epsr*epsr)*dabs(q*phil)		! xi=9 entredistance raid (epsr)

		  do l=1,nbrxi
	        ll=nxit(l,ipan,iboat)		
		    dyn(ll,ipan)=panneau(ipan).part*dsyelt(ll)*
     *					 (yneu-yn)/air
	      enddo
	    endif
    1	  continue

!	  rewind iu_25(iboat)

c	sensibilités pour le moment d'inertie	
c	-------------------------------------
	  do 2 ipan=1,neto
	    nbrxi=nvar(ipan,iboat)																	
	    if(nbrxi.eq.0) goto 2
	    do nel=1,neto
!	      read(iu_25(iboat)) epsa,epsr,delta,hya,dya,wya,tya,		
!     *                hxr,dxr,wxr,txr
!		  read(iu_23(iboat)) abtr,phil,mt,teta,xi,xf,hxtr,wxtr,q,
!     *               kse,kst,ksa,
!     *			   txtr,dxtr
		  epsa = panneau(ipan).epsa
		  epsr = panneau(ipan).epsr
		  delta = panneau(ipan).delta
		  hya = panneau(ipan).hya
		  dya = panneau(ipan).dya
		  wya = panneau(ipan).wya
		  tya = panneau(ipan).tya
		  hxr = panneau(ipan).hxr
		  dxr = panneau(ipan).dxr
		  wxr = panneau(ipan).wxr
	      txr = panneau(ipan).txr

		  phil = dabs(panneau(nel).phil)
	      mt = panneau(nel).mt
		  q = panneau(nel).q

     	      if(itype(nel).ne.5) then
c			if(icor.ne.0) then
c			  if(ishear.ne.1) then
c				dxr=dxr+corro(nel,3)
c				txr=txr+corro(nel,3)
c				deltpan=delt(nel)+corro(nel,1)+corro(nel,3)*
c     *				    (hxr+wxr)/epsr
c				do i=1,mt
c				  deltpan=deltpan+corro(nel,3)*(hxtr(i)+wxtr(i))/
c     *					  (phil*q*pi/180.)
c				enddo
c			  else
c				deltpan=delta+corro(nel,1)
c			  endif									
c			else
c			  if(ishear.ne.1) then
c			    deltpan=delt(nel)
c			  else
c		        deltpan=delta
c			  endif	
c			endif

		    if(icor.ne.0) then
		      if(ishear.ne.1) then
		        deltpan=panneau(nel).delt-(hxr*dxr+wxr*txr)/epsr-delta
			    dxr=dxr+panneau(nel).corro(3)
		        txr=txr+panneau(nel).corro(3)
		      endif
	          delta=delta+panneau(nel).corro(1)
		      if(ishear.ne.1) then
		        deltpan=deltpan+(hxr*dxr+wxr*txr)/epsr+delta
	          else
	            deltpan=delta
	          endif
	        else
	          if(ishear.ne.1) then
		        deltpan=panneau(nel).delt
		      else
 	          deltpan=delta
		      endif	
		    endif				
		
		    sph2=vsin(-panneau(nel).phil/2.,0.d00)
              c19=vcos(panneau(nel).tetas,0.d00)
	        s1=vsin(panneau(nel).tetas,0.d00)
              s2=vsin(panneau(nel).tetas,-panneau(nel).phil/2.)
		    phil=-panneau(nel).phil*pi/180.
		    d2=q*(s1-2.*sph2*s2/phil)
	    
		    omega=dabs(deltpan*q*phil)
	        yneu=z(nel,3)+d2

		    call annuld(dinelt,9)
		    call annuld(dsyelt,9)
		    if(nel.eq.ipan) then
		      dinelt(1)=q*dabs(phil)/12.*
     *	                (panneau(nel).part*((c19*q*phil)**2+
     *	  	  	        (s1*panneau(nel).part*deltpan)**2)+
     *			        panneau(nel).part*deltpan*
     *			        (2.*(s1*panneau(nel).part*deltpan)*s1*
     *					panneau(nel).part))			! xi=1 épaisseur borde
		      dinelt(6)=q*dabs(phil)/12.*				
     *	    	        (panneau(nel).part*dxr/epsr*((c19*q*phil)**2+
     *			        (s1*panneau(nel).part*deltpan)**2)+		
     *			        panneau(nel).part*deltpan*
     *		            (2.*(s1*panneau(nel).part*deltpan)*s1*
     *			        panneau(nel).part*dxr/epsr))								! xi=6 hauteur ame raidisseur
                dinelt(7)=q*dabs(phil)/12.*				
     *	  		        (panneau(nel).part*hxr/epsr*((c19*q*phil)**2+		
     *			        (s1*panneau(nel).part*deltpan)**2)+
     *			        panneau(nel).part*deltpan*
     *			        (2.*(s1*panneau(nel).part*deltpan)*s1*
     *			        panneau(nel).part*hxr/epsr))								! xi=7 epaiss ame raidisseur
                dinelt(8)=q*dabs(phil)/12.*				
     *	  		        (panneau(nel).part*txr/epsr*((c19*q*phil)**2		
     *			        +(s1*panneau(nel).part*deltpan)**2)+
     *			        panneau(nel).part*deltpan*
     *			        (2.*(s1*panneau(nel).part*deltpan)*s1*
     *			        panneau(nel).part*txr/epsr))								! xi=8 larg sem raidisseur
		      dinelt(9)=q*dabs(phil)/12.*				
     *	  		        ((-1.)*panneau(nel).part*(hxr*dxr+wxr*txr)/
     *			        (epsr**2)*((c19*q*phil)**2+(s1*
     *					panneau(nel).part*		
     *			        deltpan)**2)+panneau(nel).part*deltpan*
     *			        (2.*(s1*panneau(nel).part*deltpan)*s1*(-1.)*
     *			       panneau(nel).part*(hxr*dxr+wxr*txr)/(epsr**2)))				! xi=9 entredistance raidisseur
		      dsyelt(1)=dabs(q*phil)										! xi=1 epaisseur borde
		      dsyelt(6)=dxr/epsr*dabs(q*phil)								! xi=6 haut ame raid
		      dsyelt(7)=hxr/epsr*dabs(q*phil)								! xi=7 epaiss ame raid
		      dsyelt(8)=txr/epsr*dabs(q*phil)								! xi=8 larg sem raid
		      dsyelt(9)=-(hxr*dxr+wxr*txr)/(epsr*epsr)*dabs(q*phil)			! xi=9 entredistance raid (epsr)
	        endif

		    do l=1,nbrxi
	          ll=nxit(l,ipan,iboat)		
		      din(ll,ipan)=din(ll,ipan)+dinelt(ll)+panneau(nel).part*
     *					   dsyelt(ll)*(yneu-yn)**2+
     *					   panneau(nel).part*omega*(2.*(yneu-yn)*
     *					   (-1.)*dyn(ll,ipan))
	        enddo
	      endif
	    enddo
	    do l=1,nbrxi
	      ll=nxit(l,ipan,iboat)
	      if(sym.eq.1) din(ll,ipan)=2.*din(ll,ipan)
		  if(sym.eq.2) din(ll,ipan)=4.*din(ll,ipan)
	    enddo
!	    rewind iu_25(iboat)
!	    rewind iu_23(iboat)
    2	  continue

	endif

      return
      end
