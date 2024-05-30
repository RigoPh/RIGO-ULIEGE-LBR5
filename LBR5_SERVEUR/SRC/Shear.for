	subroutine shear(thicktau,yneuttau,dyneuttau,itau,ditau,tau,dtau)

	
      use param_section
      
	

	implicit double precision (a-h,o-z)
	double precision itau
		
	dimension thicktau(neto)
	dimension dyneuttau(9,neto),ditau(9,neto)
	dimension tau(neto,3),dtau(neto,3,neto)

	double precision, allocatable, save :: wq(:,:)
	double precision, allocatable, save :: tmean(:)
	integer*4, allocatable, save		:: list(:)
	double precision, allocatable, save :: fact(:)
	double precision, allocatable, save :: dwq(:,:,:)

!	allocate (tau(neto,3))
!	allocate (dtau(neto,3,neto))

	allocate (wq(neto,2))
	allocate (tmean(neto))
	allocate (list(ntn))
	allocate (fact(neto))
	allocate (dwq(neto,2,neto))

c*********************************************************************************
	xcondi = 0.d00
	
	tau(:,:)=0.d00
	dtau(:,:,:)=0.d00
	wq(:,:)=0.d00
	tmean(:)=0.d00
	list(:)=0
	fact=0.d00
	dwq(:,:,:)=0.d00


	pi1=pi/180.d00


c	détermination des valeurs nodales de warping pour les noeuds situés au-dessus de l'axe neutre 
c	---------------------------------------------------------------------------------------------
	np=0
	do 1 n=1,ntn
	  do nel=1,neto
	    if(n.eq.noeud(nel,1)) then
	      if(((z(nel,3)-yneuttau).ge.0.).and.
     *			(panneau(nel).part.ne.0.)) then
	        np=np+1
			list(np)=n
			goto 1
		  endif
	    endif
		if(n.eq.noeud(nel,2)) then
	      if(((z(nel,4)-yneuttau).ge.0.).and.
     *			(panneau(nel).part.ne.0.)) then
	        np=np+1
			list(np)=n
			goto 1
		  endif
	    endif
   	  enddo
    1 continue

	call warping(thicktau,yneuttau,dyneuttau,np,list,wq,dwq)


c	détermination des valeurs nodales de warping pour les noeuds situés en-dessous de l'axe neutre 
c	----------------------------------------------------------------------------------------------
	do i=1,np
	  list(i)=0
	enddo
	np=0
	do 2 n=1,ntn
	  do nel=1,neto
	    if(n.eq.noeud(nel,1)) then
	      if(((z(nel,3)-yneuttau).lt.0.).and.
     *					(panneau(nel).part.ne.0.)) then
	        np=np+1
			list(np)=n
			goto 2
		  endif
	    endif
		if(n.eq.noeud(nel,2)) then
	      if(((z(nel,4)-yneuttau).lt.0.).and.
     *					(panneau(nel).part.ne.0.)) then
	        np=np+1
			list(np)=n
			goto 2
		  endif
	    endif
   	  enddo
    2 continue

	call warping(thicktau,yneuttau,dyneuttau,np,list,wq,dwq)


c	détermination des contraintes de cisaillement (élément) en 3 points de l'axe local y 
c	------------------------------------------------------------------------------------
	do nel=1,neto
	  if((itype(nel).ne.5).and.(panneau(nel).part.ne.0.)) then

c		détermination du sens du flux de cisaillement
		y1=z(nel,3)-yneuttau
		y2=z(nel,4)-yneuttau
		fact(nel)=1.
		if((y1*y2).lt.0.) goto 3						!extrémités de l'élément de part et d'autre de l'axe neutre
		if(dabs(y1).lt.dabs(y2)) fact(nel)=-1.			!distance à l'axe neutre augmente suivant le sens de l'élément => sens de l'élément opposé à celui du flux de cisaillement
		if(dabs(y1-y2).lt.0.001) then					!élément horizontal
	      k=0
		  do ipan=1,neto								!flux de cisaillement provient du noeud avec condition de bord (libre ou axe symétrie) le plus proche 
		    if(dabs(z(ipan,3)-z(nel,3)).lt.0.001) then
	          if(nno(noeud(ipan,1),2).ne.0) then
				k=k+1
				if(k.ge.1) then
				  if(dabs(z(nel,1)-xcondi).gt.
     *				 dabs(z(nel,1)-z(ipan,1))) then
				    xcondi=z(ipan,1)
				  endif
				else
	              xcondi=z(ipan,1)
				endif
	          endif
			  if(nno(noeud(ipan,2),2).ne.0) then
			    k=k+1
				if(k.ge.1) then
				  if(dabs(z(nel,1)-xcondi).gt.
     *				 dabs(z(nel,1)-z(ipan,2))) then
				    xcondi=z(ipan,2)
				  endif
				else
	              xcondi=z(ipan,2)
				endif
	          endif
			endif
		  enddo
		  if(k.eq.0) xcondi=xneut						!si pas de condition de bord à la même ordonnée z, flux de cisaillement provient de l'axe neutre vertical
		  x1=z(nel,1)-xcondi
		  x2=z(nel,2)-xcondi
	      if(dabs(x1).gt.dabs(x2)) then
	        fact(nel)=-1.
	      else
	        fact(nel)=1.
	      endif
	    endif
   		if(y1.lt.0.) fact(nel)=-fact(nel)				!valeur opposée de fact pour les éléments situés sous l'axe neutre vu expression de tau(nel,i)

    3	    hight=dabs(panneau(nel).phil*panneau(nel).q*pi1)
		tmean(nel)=dabs(wq(nel,2)-wq(nel,1))/hight
		teta=-panneau(nel).angle*pi/180.
   		do i=1,3
	      s=(i-1)*hight/2.
		  if((y1*y2).lt.(-1e-10)) then						!extrémités de l'élément de part et d'autre de l'axe neutre
	        if((y1*(y1+s*sin(teta))).ge.0.) then
			  fact(nel)=1.
	          if(y1.lt.(-1e-10)) fact(nel)=-1.
			  breadth=dabs(y1/sin(teta))
			  tmean(nel)=dabs(0.-wq(nel,1))/breadth
	          tau(nel,i)=tmean(nel)+fact(nel)*(y1*(s-breadth/2.)+
     *			         sin(teta)/2.*(s**2-breadth**2/3.))
			else
	          fact(nel)=1.
	          if(y1.lt.(-1e-10)) fact(nel)=-1.
			  breadth=dabs(y2/sin(teta))
	          tmean(nel)=dabs(wq(nel,2)-0.)/breadth
	          ss=s-dabs(y1/sin(teta))
	          tau(nel,i)=tmean(nel)+fact(nel)*(0.*(ss-breadth/2.)+
     *			         sin(teta)/2.*(ss**2-breadth**2/3.))
			endif
		  else
		    tau(nel,i)=tmean(nel)+fact(nel)*(y1*(s-hight/2.)+
     *			       sin(teta)/2.*(s**2-hight**2/3.))
		  endif
		enddo
	  endif
	enddo


c	vérification de la conservation du flux de cisaillement à la jonction de 2 éléments horizontaux (modification éventuelle de fact)
c	-----------------------------------------------------------------------------------------------
	do nel=1,neto
	  if((itype(nel).ne.5).and.(panneau(nel).part.ne.0.)) then
	    y1=z(nel,3)-yneuttau
	    y2=z(nel,4)-yneuttau
	    if(dabs(y1-y2).lt.0.001) then
		  flw=0.
	      k=0
		  do ipan=1,neto
	        if((itype(ipan).ne.5).and.(ipan.ne.nel)) then
			  if(noeud(nel,1).eq.noeud(ipan,1)) then
	            flw=flw+tau(ipan,1)*panneau(ipan).part*thicktau(ipan)
				k=k+1
	          elseif(noeud(nel,1).eq.noeud(ipan,2)) then
	            flw=flw+tau(ipan,3)*panneau(ipan).part*thicktau(ipan)
	            k=k+1
	          endif
	        endif
	      enddo
		  if(k.eq.1) then
	        if(dabs(panneau(nel).part*thicktau(nel)*tau(nel,1)-flw).
     *		gt.0.001) then
	          fact(nel)=-fact(nel)
	          goto 4
	        endif
	      endif
	      flw=0.
	      k=0
		  do ipan=1,neto
	        if((itype(ipan).ne.5).and.(ipan.ne.nel)) then
			  if(noeud(nel,2).eq.noeud(ipan,1)) then
	            flw=flw+tau(ipan,1)*panneau(ipan).part*thicktau(ipan)
				k=k+1
	          elseif(noeud(nel,2).eq.noeud(ipan,2)) then
	            flw=flw+tau(ipan,3)*panneau(ipan).part*thicktau(ipan)
	            k=k+1
	          endif
	        endif
	      enddo
		  if(k.eq.1) then
	        if(dabs(panneau(nel).part*thicktau(nel)*tau(nel,3)-flw).
     *		gt.0.001) then
	          fact(nel)=-fact(nel)
	          goto 4
	        endif
	      endif
    4	      do i=1,3
	        hight=dabs(panneau(nel).phil*panneau(nel).q*pi1)
			teta=-panneau(nel).angle*pi/180.
		    s=(i-1)*hight/2
		    tau(nel,i)=tmean(nel)+fact(nel)*(y1*(s-hight/2.)+
     *			       sin(teta)/2.*(s**2-hight**2/3.))
		  enddo
		endif
	  endif
	enddo 

	do nel=1,neto
	  if((itype(nel).ne.5).and.(panneau(nel).part.ne.0)) then
	    do i=1,3
	      tau(nel,i)=panneau(nel).part*tau(nel,i)/itau
		enddo
	  endif
	enddo


c	détermination des sensibilités des contraintes de cisaillement par rapport aux variables de conception (ép. de tôle)
c	--------------------------------------------------------------------------------------------------------------------
	if(iopti.ge.1) then
	  do 5 ipan=1,neto
	    nbrxi=nvar(ipan,iboat)
	    if(nbrxi.eq.0) goto 5
	    do l=1,nbrxi
	      if(nxit(l,ipan,iboat).eq.1) goto 6
	    enddo
	    goto 5
    6	    do nel=1,neto
	      if((itype(nel).ne.5).and.(panneau(nel).part.ne.0.)) then
	        hight=dabs(panneau(nel).phil*panneau(nel).q*pi1)
		    if((wq(nel,2)-wq(nel,1)).ge.0.) then
			  dtmean=(dwq(nel,2,ipan)-dwq(nel,1,ipan))/hight
	        else
			  dtmean=(dwq(nel,1,ipan)-dwq(nel,2,ipan))/hight
	        endif
		    teta=-panneau(nel).angle*pi/180.
   		    y1=z(nel,3)-yneuttau
		    dy1=-dyneuttau(1,ipan)
		    y2=z(nel,4)-yneuttau
		    dy2=-dyneuttau(1,ipan)
		    do i=1,3
	          s=(i-1)*hight/2.
		      if((y1*y2).lt.0.) then						!extrémités de l'élément de part et d'autre de l'axe neutre
	            if((y1*(y1+s*sin(teta))).ge.0.) then
			      if(y1.ge.0.) then
				    breadth=y1/dabs(sin(teta))
				    dbreadth=dy1/dabs(sin(teta))
				  else
	                breadth=-y1/dabs(sin(teta))
				    dbreadth=-dy1/dabs(sin(teta))
				  endif
				  if(wq(nel,1).ge.0.) then
				    dtmean=(dwq(nel,1,ipan)*breadth-wq(nel,1)*
     *				       dbreadth)/breadth**2
				  else
				    dtmean=-(dwq(nel,1,ipan)*breadth-wq(nel,1)*
     *				       dbreadth)/breadth**2
				  endif
				  dtau(nel,i,ipan)=dtmean+fact(nel)*(dy1*
     *							   (s-breadth/2.)+y1*(s-dbreadth/2.)+
     *							   sin(teta)/2.*(s**2-2./3.*breadth*
     *							   dbreadth))
			    else
	              if(y2.ge.0.) then
				    breadth=y2/dabs(sin(teta))
				    dbreadth=dy2/dabs(sin(teta))
				  else
	                breadth=-y2/dabs(sin(teta))
				    dbreadth=-dy2/dabs(sin(teta))
				  endif
	              if(wq(nel,2).ge.0.) then
				    dtmean=(dwq(nel,2,ipan)*breadth-wq(nel,2)*
     *				       dbreadth)/breadth**2
				  else
				    dtmean=-(dwq(nel,2,ipan)*breadth-wq(nel,2)*
     *				       dbreadth)/breadth**2
				  endif
				  ss=s-dabs(y1/sin(teta))
				  if(y1.ge.0.) then
				    ss=s-y1/dabs(sin(teta))
				    dss=-dy1/dabs(sin(teta))
				  else
				    ss=s+y1/dabs(sin(teta))
				    dss=dy1/dabs(sin(teta))
				  endif
				  dtau(nel,i,ipan)=dtmean+fact(nel)*(sin(teta)/2.*
     *						       (2.*ss*dss-2./3.*breadth*dbreadth))
			    endif
		      else
			    dtau(nel,i,ipan)=dtmean+fact(nel)*dy1*(s-hight/2.)				   
		      endif
			  dtau(nel,i,ipan)=panneau(nel).part*(dtau(nel,i,ipan)
     *						   *itau-tau(nel,i)*itau/panneau(nel).part
     *						   *ditau(1,ipan))/itau**2
		    enddo
		  endif
	    enddo
    5	  continue
      endif	


	deallocate (wq)
	deallocate (tmean)
	deallocate (list)
	deallocate (fact)
	deallocate (dwq)

      return
      end


c	****************************************************************************
c	****************************************************************************

	subroutine warping(thicktau,yneuttau,dyneuttau,
     *				   np,list,wq,dwq)

	
	use param_section
      

	implicit double precision (a-h,o-z)

	dimension thicktau(neto)
	dimension wq(neto,2),dwq(neto,2,neto)
	dimension dyneuttau(9,neto)
	dimension list(np) !,arecip(np,np)

	double precision, allocatable, save :: aux(:,:)
	double precision, allocatable, save :: zb(:)
	double precision, allocatable, save :: arecip(:,:)

	allocate (aux(np,np))
	allocate (zb(np))
	allocate (arecip(np,np))

c*********************************************************************************
c*********************************************************************************

	call annuld(aux,np*np)
	call annuld(zb,np)
	call annuld(arecip,np*np)


	pi1=pi/180.


c	assemblage des matrices pour la résolution du système d'équations (dpi/dwq=0, pi=u-w: total potential energy of the system) 
c	---------------------------------------------------------------------------------------------------------------------------
	do i=1,np
	  do nel=1,neto

		if (panneau(nel).part.lt.0.1) then
			panneau(nel).part=0.001
		endif

		if(itype(nel).ne.5) then
	      hight=dabs(panneau(nel).phil*panneau(nel).q*pi1)
		  teta=-panneau(nel).angle*pi/180.
		  if(list(i).eq.noeud(nel,1)) then
	        fact=1.
			y1=z(nel,3)-yneuttau
			if(((z(nel,3)-yneuttau)*(z(nel,4)-yneuttau)).lt.(-1e-10)) 
     *			then	!extrémités de l'élément de part et d'autre de l'axe neutre
			  hight=dabs((z(nel,3)-yneuttau)/sin(teta))
			  goto 1
			endif
		    do j=1,np
			  if(noeud(nel,2).eq.list(j)) then
			    aux(i,j)=-panneau(nel).part*thicktau(nel)/hight
	          endif
			enddo
    1			aux(i,i)=aux(i,i)+panneau(nel).part*thicktau(nel)/hight
		    zb(i)=zb(i)+0.5*panneau(nel).part*thicktau(nel)*hight*
     *			  (y1+fact/3.*hight*sin(teta))			
		  elseif(list(i).eq.noeud(nel,2)) then
	        fact=2.
			y1=z(nel,3)-yneuttau
			if(((z(nel,3)-yneuttau)*(z(nel,4)-yneuttau)).lt.(-1e-10)) 
     *			then	!extrémités de l'élément de part et d'autre de l'axe neutre
	          hight=dabs((z(nel,4)-yneuttau)/sin(teta))
	          y1=0.
			  goto 2
			endif
		    do j=1,np
			  if(noeud(nel,1).eq.list(j)) then
			    aux(i,j)=-panneau(nel).part*thicktau(nel)/hight
	          endif
			enddo
    2			aux(i,i)=aux(i,i)+panneau(nel).part*thicktau(nel)/hight
		    zb(i)=zb(i)+0.5*panneau(nel).part*thicktau(nel)*hight*
     *		      (y1+fact/3.*hight*sin(teta))
		  endif
	    endif
	  enddo
	enddo


c	résolution du système d'équations (dpi/dwq=0, pi=u-w: total potential energy of the system) 
c	-------------------------------------------------------------------------------------------
	do i=1,np
	  do j=1,np
	    arecip(i,j)=aux(i,j)
	  enddo
	enddo

	call recip(np,np,arecip)

	do i=1,np
	  do nel=1,neto
	    do j=1,np
	      if(itype(nel).ne.5) then
	        if(list(i).eq.noeud(nel,1)) then
	          wq(nel,1)=wq(nel,1)+arecip(i,j)*zb(j)
			elseif(list(i).eq.noeud(nel,2)) then
	          wq(nel,2)=wq(nel,2)+arecip(i,j)*zb(j)
			endif
	      endif
		enddo
	  enddo
	enddo


c	sensibilités des valeurs nodales de warping wq par rapport aux variables de conception (ép. de tôle)
c	----------------------------------------------------------------------------------------------------
	if(iopti.ge.1) then
	  do 6 ipan=1,neto
	    nbrxi=nvar(ipan,iboat)
	    if(nbrxi.eq.0) goto 6
	    do l=1,nbrxi
	      if(nxit(l,ipan,iboat).eq.1) goto 7
	    enddo
	    goto 6
    7	    call annuld(aux,np*np)
	    call annuld(zb,np)
	    do i=1,np
	      do nel=1,neto
		    if(itype(nel).ne.5) then
	          hight=dabs(panneau(nel).phil*panneau(nel).q*pi1)
		      dhight=0.
		      teta=-panneau(nel).angle*pi/180.
			  if(nel.eq.ipan) then
			    dthicktau=1.
			  else
			    dthicktau=0.
			  endif
		      if(list(i).eq.noeud(nel,1)) then
	            fact=1.
			    y1=z(nel,3)-yneuttau
			    dy1=-dyneuttau(1,ipan)
			    if(((z(nel,3)-yneuttau)*(z(nel,4)-yneuttau)).lt.0.) 		!extrémités de l'élément de part et d'autre de l'axe neutre
     *		    then
				  if((z(nel,3)-yneuttau).ge.0.) then
				    hight=(z(nel,3)-yneuttau)/dabs(sin(teta))
				    dhight=-dyneuttau(1,ipan)/dabs(sin(teta))
				  else
	                hight=-(z(nel,3)-yneuttau)/dabs(sin(teta))
				    dhight=dyneuttau(1,ipan)/dabs(sin(teta))
				  endif
			      goto 3
			    endif
		        do j=1,np
			      if(noeud(nel,2).eq.list(j)) then
			        aux(i,j)=-panneau(nel).part*(dthicktau*hight
     *					     -thicktau(nel)*dhight)/hight**2
	              endif
			    enddo
    3			    aux(i,i)=aux(i,i)+panneau(nel).part*(dthicktau*hight-
     *			         thicktau(nel)*dhight)/hight**2
			    zb(i)=zb(i)+0.5*panneau(nel).part*(dthicktau*hight*
     *			      (y1+fact/3.*hight*sin(teta))+thicktau(nel)*
     *			      ((dhight*y1+fact/3.*hight*sin(teta))+hight*
     *			      (dy1+fact/3.*dhight*sin(teta))))	
		      elseif(list(i).eq.noeud(nel,2)) then
	            fact=2.
			    y1=z(nel,3)-yneuttau
	            dy1=-dyneuttau(1,ipan)
			    if(((z(nel,3)-yneuttau)*(z(nel,4)-yneuttau)).lt.0.) 		!extrémités de l'élément de part et d'autre de l'axe neutre
     *            then
				  if((z(nel,4)-yneuttau).ge.0.) then
				    hight=(z(nel,4)-yneuttau)/dabs(sin(teta))
				    dhight=-dyneuttau(1,ipan)/dabs(sin(teta))
				  else
	                hight=-(z(nel,4)-yneuttau)/dabs(sin(teta))
				    dhight=dyneuttau(1,ipan)/dabs(sin(teta))
				  endif
	              y1=0.
				  dy1=0.
			      goto 4
			    endif
		        do j=1,np
			      if(noeud(nel,1).eq.list(j)) then
			        aux(i,j)=-panneau(nel).part*(dthicktau*hight
     *					     -thicktau(nel)*dhight)/hight**2
	              endif
			    enddo
    4			    aux(i,i)=aux(i,i)+panneau(nel).part*(dthicktau*hight-
     *			         thicktau(nel)*dhight)/hight**2
			    zb(i)=zb(i)+0.5*panneau(nel).part*(dthicktau*hight*
     *			      (y1+fact/3.*hight*sin(teta))+thicktau(nel)*
     *			      (dhight*(y1+fact/3.*hight*sin(teta))+hight*
     *			      (dy1+fact/3.*dhight*sin(teta))))	
		      endif
	        endif
	      enddo
	    enddo
	    do i=1,np
	      do j=1,np
	        do nel=1,neto
	          if(itype(nel).ne.5) then
	            if(list(j).eq.noeud(nel,1)) then
	              warp=wq(nel,1)
			      goto 5
			    elseif(list(j).eq.noeud(nel,2)) then
	              warp=wq(nel,2)
			      goto 5
			    endif
			  endif
	        enddo
    5	        zb(i)=zb(i)-aux(i,j)*warp
	      enddo
	    enddo
	    do i=1,np
		  do nel=1,neto
	        do j=1,np
	          if(itype(nel).ne.5) then
	            if(list(i).eq.noeud(nel,1)) then
			      dwq(nel,1,ipan)=dwq(nel,1,ipan)+arecip(i,j)*zb(j)
	            elseif(list(i).eq.noeud(nel,2)) then
	              dwq(nel,2,ipan)=dwq(nel,2,ipan)+arecip(i,j)*zb(j)
	            endif
	          endif
		    enddo
	      enddo
	    enddo
    6	  continue
      endif

	deallocate (aux)
	deallocate (zb)
	deallocate (arecip)

      return
      end
