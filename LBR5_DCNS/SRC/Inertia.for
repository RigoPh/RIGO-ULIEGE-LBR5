	subroutine inertia(yn,dyn,in,din,nsol,icor,ishear)					

      use sharedvar
	
	implicit real*8 (a-h,o-z)

	real*8 in,inelt

	dimension dyn(9,neto)
	dimension din(9,neto)
	dimension xi(nsol),xf(nsol)
	
	dimension dinelt(9),dsyelt(9)
	
c	*****************************************************************************************************

	in=0.
	air=0.
	sy=0.
	call annuld(dyn,9*neto)
	call annuld(din,9*neto)

c	position de l'axe neutre
c	------------------------
	rewind 302
	rewind 99
	do ipan=1,neto
	  read(302) epsa,epsr,delta,hya,dya,wya,tya,		
     *            hxr,dxr,wxr,txr
	  read(99) abtr,phil,mt,teta,xi,xf,hxtr,wxtr,q,kse,kst,ksa,
     *           txtr,dxtr
	  if(itype(ipan).ne.5) then
	    if(icor.ne.0) then
	      if(ishear.ne.1) then
c		    dxr=dxr+corro(ipan,3)
c		    txr=txr+corro(ipan,3)
			deltpan=delt(ipan)+corro(ipan,1)+corro(ipan,3)*
     *				(hxr+wxr)/epsr
	        do i=1,mt
	          deltpan=deltpan+corro(ipan,3)*(hxtr(i)+wxtr(i))/
     *				  (phil*q*pi/180.)
			enddo
	      else
	        deltpan=delta+corro(ipan,1)
		  endif									
	    else
	      if(ishear.ne.1) then
		    deltpan=delt(ipan)
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

		sph2=vsin(-philn(ipan)/2.,0.d00)
 	    s1=vsin(tetas(ipan),0.d00)
          s2=vsin(tetas(ipan),-philn(ipan)/2.)
		phil=-philn(ipan)*pi/180.
		d2=q*(s1-2.*sph2*s2/phil)
	    
		omega=dabs(deltpan*q*phil)
	    air=air+part(ipan)*omega
	    yneu=z(ipan,3)+d2
	    sy=sy+part(ipan)*omega*yneu
	  endif
	enddo
	yn=sy/air

c	moment d'inertie
c	----------------
	rewind 302
	rewind 99
	do ipan=1,neto
	  read(302) epsa,epsr,delta,hya,dya,wya,tya,		
     *            hxr,dxr,wxr,txr
	  read(99) abtr,phil,mt,teta,xi,xf,hxtr,wxtr,q,kse,kst,ksa,
     *           txtr,dxtr
	  if(itype(ipan).ne.5) then
	    if(icor.ne.0) then
	      if(ishear.ne.1) then
c		    dxr=dxr+corro(ipan,3)
c		    txr=txr+corro(ipan,3)
			deltpan=delt(ipan)+corro(ipan,1)+corro(ipan,3)*
     *				(hxr+wxr)/epsr
	        do i=1,mt
	          deltpan=deltpan+corro(ipan,3)*(hxtr(i)+wxtr(i))/
     *				  (phil*q*pi/180.)
			enddo
	      else
	        deltpan=delta+corro(ipan,1)
		  endif									
	    else
	      if(ishear.ne.1) then
		    deltpan=delt(ipan)
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

		sph2=vsin(-philn(ipan)/2.,0.d00)
          c19=vcos(tetas(ipan),0.d00)
	    s1=vsin(tetas(ipan),0.d00)
          s2=vsin(tetas(ipan),-philn(ipan)/2.)
		phil=-philn(ipan)*pi/180.
		d2=q*(s1-2.*sph2*s2/phil)
	    
		omega=dabs(deltpan*q*phil)
	    yneu=z(ipan,3)+d2
	    inelt=((c19*q*phil)**2+(s1*part(ipan)*deltpan)**2)*
     *           part(ipan)*deltpan*q*dabs(phil)/12.
	    in=in+inelt+part(ipan)*omega*(yneu-yn)**2
	  endif
	enddo 
	if(sym.eq.1) in=2*in				
	if(sym.eq.2) in=4*in

	rewind 302
	rewind 99

	if(iopti.ge.1) then

c	sensibilités pour la position de l'axe neutre	
c	---------------------------------------------
	  do 1 ipan=1,neto
	    nbrxi=nvar(ipan)																	
	    read(302) epsa,epsr,delta,hya,dya,wya,tya,		
     *              hxr,dxr,wxr,txr,phil,q
          if(nbrxi.eq.0) goto 1
	    if(itype(ipan).ne.5) then
	      if(icor.ne.0) then
c		    delta=delta+corro(ipan,1)									
	        dxr=dxr+corro(ipan,3)
		    txr=txr+corro(ipan,3)
		  endif										
		
		  sph2=vsin(-philn(ipan)/2.,0.d00)
            c19=vcos(tetas(ipan),0.d00)
	      s1=vsin(tetas(ipan),0.d00)
            s2=vsin(tetas(ipan),-philn(ipan)/2.)
		  phil=-philn(ipan)*pi/180.
		  d2=q*(s1-2.*sph2*s2/phil)
	    
	      yneu=z(ipan,3)+d2

		  dsyelt(1)=dabs(q*phil)									! xi=1 epaisseur borde
		  dsyelt(6)=dxr/epsr*dabs(q*phil)							! xi=6 haut ame raid
		  dsyelt(7)=hxr/epsr*dabs(q*phil)							! xi=7 epaiss ame raid
		  dsyelt(8)=txr/epsr*dabs(q*phil)							! xi=8 larg sem raid
		  dsyelt(9)=-(hxr*dxr+wxr*txr)/(epsr*epsr)*dabs(q*phil)		! xi=9 entredistance raid (epsr)

		  do l=1,nbrxi
	        ll=nxit(l,ipan)		
		    dyn(ll,ipan)=part(ipan)*dsyelt(ll)*
     *					 (yneu-yn)/air
	      enddo
	    endif
    1	  continue

	  rewind 302

c	sensibilités pour le moment d'inertie	
c	-------------------------------------
	  do 2 ipan=1,neto
	    nbrxi=nvar(ipan)																	
	    if(nbrxi.eq.0) goto 2
	    do nel=1,neto
	      read(302) epsa,epsr,delta,hya,dya,wya,tya,		
     *                hxr,dxr,wxr,txr
		  read(99) abtr,phil,mt,teta,xi,xf,hxtr,wxtr,q,kse,kst,ksa,
     *			   txtr,dxtr
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
		        deltpan=delt(nel)-(hxr*dxr+wxr*txr)/epsr-delta
			    dxr=dxr+corro(nel,3)
		        txr=txr+corro(nel,3)
		      endif
	          delta=delta+corro(nel,1)									
		      if(ishear.ne.1) then
		        deltpan=deltpan+(hxr*dxr+wxr*txr)/epsr+delta
	          else
	            deltpan=delta
	          endif
	        else
	          if(ishear.ne.1) then
		        deltpan=delt(nel)
		      else
 	          deltpan=delta
		      endif	
		    endif				
		
		    sph2=vsin(-philn(nel)/2.,0.d00)
              c19=vcos(tetas(nel),0.d00)
	        s1=vsin(tetas(nel),0.d00)
              s2=vsin(tetas(nel),-philn(nel)/2.)
		    phil=-philn(nel)*pi/180.
		    d2=q*(s1-2.*sph2*s2/phil)
	    
		    omega=dabs(deltpan*q*phil)
	        yneu=z(nel,3)+d2

		    call annuld(dinelt,9)
		    call annuld(dsyelt,9)
		    if(nel.eq.ipan) then
		      dinelt(1)=q*dabs(phil)/12.*
     *	                (part(nel)*((c19*q*phil)**2+
     *	  	  	        (s1*part(nel)*deltpan)**2)+
     *			        part(nel)*deltpan*
     *			        (2.*(s1*part(nel)*deltpan)*s1*part(nel)))			! xi=1 épaisseur borde
		      dinelt(6)=q*dabs(phil)/12.*				
     *	    	        (part(nel)*dxr/epsr*((c19*q*phil)**2+
     *			        (s1*part(nel)*deltpan)**2)+		
     *			        part(nel)*deltpan*
     *		            (2.*(s1*part(nel)*deltpan)*s1*
     *			        part(nel)*dxr/epsr))								! xi=6 hauteur ame raidisseur
                dinelt(7)=q*dabs(phil)/12.*				
     *	  		        (part(nel)*hxr/epsr*((c19*q*phil)**2+		
     *			        (s1*part(nel)*deltpan)**2)+
     *			        part(nel)*deltpan*
     *			        (2.*(s1*part(nel)*deltpan)*s1*
     *			        part(nel)*hxr/epsr))								! xi=7 epaiss ame raidisseur
                dinelt(8)=q*dabs(phil)/12.*				
     *	  		        (part(nel)*txr/epsr*((c19*q*phil)**2		
     *			        +(s1*part(nel)*deltpan)**2)+
     *			        part(nel)*deltpan*
     *			        (2.*(s1*part(nel)*deltpan)*s1*
     *			        part(nel)*txr/epsr))								! xi=8 larg sem raidisseur
		      dinelt(9)=q*dabs(phil)/12.*				
     *	  		        ((-1.)*part(nel)*(hxr*dxr+wxr*txr)/
     *			        (epsr**2)*((c19*q*phil)**2+(s1*part(nel)*		
     *			        deltpan)**2)+part(nel)*deltpan*
     *			        (2.*(s1*part(nel)*deltpan)*s1*(-1.)*
     *			        part(nel)*(hxr*dxr+wxr*txr)/(epsr**2)))				! xi=9 entredistance raidisseur
		      dsyelt(1)=dabs(q*phil)										! xi=1 epaisseur borde
		      dsyelt(6)=dxr/epsr*dabs(q*phil)								! xi=6 haut ame raid
		      dsyelt(7)=hxr/epsr*dabs(q*phil)								! xi=7 epaiss ame raid
		      dsyelt(8)=txr/epsr*dabs(q*phil)								! xi=8 larg sem raid
		      dsyelt(9)=-(hxr*dxr+wxr*txr)/(epsr*epsr)*dabs(q*phil)			! xi=9 entredistance raid (epsr)
	        endif

		    do l=1,nbrxi
	          ll=nxit(l,ipan)		
		      din(ll,ipan)=din(ll,ipan)+dinelt(ll)+part(nel)*
     *					   dsyelt(ll)*(yneu-yn)**2+
     *					   part(nel)*omega*(2.*(yneu-yn)*
     *					   (-1.)*dyn(ll,ipan))
	        enddo
	      endif
	    enddo
	    do l=1,nbrxi
	      ll=nxit(l,ipan)
	      if(sym.eq.1) din(ll,ipan)=2.*din(ll,ipan)
		  if(sym.eq.2) din(ll,ipan)=4.*din(ll,ipan)
	    enddo
	    rewind 302
	    rewind 99
    2	  continue

	endif

      return
      end
