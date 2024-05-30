      subroutine objin1(obj,derivee,nn)			

      use sharedvar
	
	implicit real*8(a-h,o-z)
	real *8 ixx
	dimension ds(9)
	dimension derivee(nn)							

c ******************************************************************************
c     calcul de la fonction objectif inertie (obj) et
c     calcul des dérivées de la fonction objectif (derivee(i), i=1,nbrxi)
c
c     version du : février 2007                                créer 14-2-06
c
c ******************************************************************************

c les variables
c -------------
c	1	delta =  épaisseur du bordage
c	2	hya   =  hauteur de l'âme des aiguilles
c	3	dya   =  épaisseur de l'âme des aiguilles
c	4	wya   =  largeur des semelles des aiguilles
c	5	epsa  =  entredistance entre aiguilles
c	6	hxr   =  hauteur de l'âme des raidisseurs
c	7	dxr   =  épaisseur de l'âme des raidisseurs
c	8	wxr   =  largeur des semelles des raidisseurs
c	9	epsr  =  entredistance entre raidisseurs

c 	autres paramètres :
c	 	tya = épaisseur semelle aiguilles
c     	txr = épaisseur semelle raidisseurs 
c
c  les variables de conception pour epontille					
c  ------------------------------------------					
c	1	hya		=	hauteur demi âme ou diam ou côté ext.	
c	2	dya		=	épaisseur d'âme							
c	3	wya		=	largeur de semelle						
c	4	epais	=	épaisseur de paroi mince				
c	5	epsa	=	entredistance entre épontilles			
c

c ******************************************************************************

      ometpart=0.
	
	do nel=1,neto
	
	  read(302)epsa,epsr,delta,hya,dya,wya,tya,hxr,dxr,wxr,txr,phil,q,		!extension neto	!fev2007
     *           epais
     
        sph2=vsin(-philn(nel)/2.,0.d00)
        c19=vcos(tetas(nel),0.d00)
	  s1=vsin(tetas(nel),0.d00)
        s2=vsin(tetas(nel),-philn(nel)/2.)
	  phil=-philn(nel)*pi/180.
        d2=q*(s1-2.*sph2*s2/phil)
	  omega=dabs(delt(nel)*q*phil)
	  ometpart=ometpart+part(nel)*omega							!r&d13	!fev2007
	  yneu=z(nel,3)+d2
	  ixx=((c19*q*phil)**2+(s1*part(nel)*delt(nel))**2)*		!r&d13	!fev2007
     *      part(nel)*delt(nel)*q*dabs(phil)/12.					!r&d13	!fev2007	! ** cas des plaques
	  fct=ixx+part(nel)*omega*(yneu-yneutpart)**2				!r&d13	!fev2007

	  if(sym.eq.1) fct=2*fct				
	  if(sym.eq.2) fct=4*fct				
	
	  obj=obj+fct

	enddo
	
	write(666,*)
      write(666,*)' inertie autour axe neutre hor. (subr.objin1)'
      write(666,*)' --------------------------------------------'
      write(666,*)' inertie - inertia iyy = ',obj,' m**4'	

	rewind 302		!extension neto	!fev2007

c	calcul des sensibilites
	k=0
	do 1 nel=1,neto
	  
	  read(302)epsa,epsr,delta,hya,dya,wya,tya,hxr,dxr,wxr,txr,phil,q,		!extension neto	!fev2007
     *           epais
        
	  nbrxi=nvar(nel)
	  if (nbrxi.eq.0) goto 1
        if(itype(nel).ne.5) then
	    sph2=vsin(-philn(nel)/2.,0.d00)
	    c19=vcos(tetas(nel),0.d00)
	    s1=vsin(tetas(nel),0.d00)
	    s2=vsin(tetas(nel),-philn(1)/2.)
	    phil=-philn(nel)*pi/180.
	    d2=q*(s1-2.*sph2*s2/phil)
	    omega=dabs(delt(nel)*q*phil)
	    yneu=z(nel,3)+d2
	    dixx(1)=q*dabs(phil)/12.*										!r&d13	!fev2007
     *	        (part(nel)*((c19*q*phil)**2+							!r&d13	!fev2007
     *			(s1*part(nel)*delt(nel))**2)+							!r&d13	!fev2007
     *		    part(nel)*delt(nel)*									!r&d13	!fev2007
     *			(2.*(s1*part(nel)*delt(nel))*s1*part(nel)))				!r&d13	!fev2007	! xi=1 épaisseur borde
	    dixx(6)=q*dabs(phil)/12.*										!r&d13	!fev2007
     *	        (part(nel)*dxr/epsr*((c19*q*phil)**2+					!r&d13	!fev2007
     *			(s1*part(nel)*delt(nel))**2)+							!r&d13	!fev2007
     *		    part(nel)*delt(nel)*									!r&d13	!fev2007
     *			(2.*(s1*part(nel)*delt(nel))*s1*part(nel)*dxr/epsr))	!r&d13	!fev2007	! xi=6 hauteur ame raidisseur
          dixx(7)=q*dabs(phil)/12.*										!r&d13	!fev2007	
     *	        (part(nel)*hxr/epsr*((c19*q*phil)**2+					!r&d13	!fev2007
     *			(s1*part(nel)*delt(nel))**2)+							!r&d13	!fev2007
     *		    part(nel)*delt(nel)*									!r&d13	!fev2007
     *			(2.*(s1*part(nel)*delt(nel))*s1*part(nel)*hxr/epsr))	!r&d13	!fev2007	! xi=7 epaiss ame raidisseur
          dixx(8)=q*dabs(phil)/12.*										!r&d13	!fev2007				
     *	        (part(nel)*txr/epsr*((c19*q*phil)**2+					!r&d13	!fev2007
     *			(s1*part(nel)*delt(nel))**2)+							!r&d13	!fev2007
     *		    part(nel)*delt(nel)*									!r&d13	!fev2007
     *			(2.*(s1*part(nel)*delt(nel))*s1*part(nel)*txr/epsr))	!r&d13	!fev2007	! xi=8 larg sem raidisseur
          dixx(9)=q*dabs(phil)/12.*										!r&d13	!fev2007
     *	        ((-1.)*part(nel)*(hxr*dxr+wxr*txr)/(epsr**2)*			!r&d13	!fev2007
     *		    ((c19*q*phil)**2+(s1*part(nel)*delt(nel))**2)+			!r&d13	!fev2007
     *		    part(nel)*delt(nel)*(2.*(s1*part(nel)*delt(nel))*s1*	!r&d13	!fev2007
     *		    (-1.)*part(nel)*(hxr*dxr+wxr*txr)/(epsr**2)))			!r&d13	!fev2007	! xi=9 larg sem raidisseur
	    dixx(2)=dixx(6)/1.e3										! xi=2 hauteur ame cadre
	    dixx(3)=dixx(7)/1.e3										! xi=3 epaiss ame cadre
	    dixx(4)=dixx(8)/1.e3										! xi=4 largeur sem cadre
	    dixx(5)=dixx(9)/1.e3										! xi=5 entredistance cadre (epsa)
		ds(1)=dabs(q*phil)											! xi=1 epaisseur borde
	    ds(6)=dxr/epsr*dabs(q*phil)									! xi=6 haut ame raid
	    ds(7)=hxr/epsr*dabs(q*phil)									! xi=7 epaiss ame raid
	    ds(8)=txr/epsr*dabs(q*phil)									! xi=8 larg sem raid
	    ds(9)=-(hxr*dxr+wxr*txr)/(epsr*epsr)*dabs(q*phil)			! xi=9 entredistance raid (epsr)
	    ds(2)=ds(6)/1.e3											! xi=2 hauteur ame cadre
	    ds(3)=ds(7)/1.e3											! xi=3 epaiss ame cadre
	    ds(4)=ds(8)/1.e3											! xi=4 largeur sem cadre
	    ds(5)=ds(9)/1.e3											! xi=5 entredistance cadre (epsa)
		do i=1,nbrxi
	      j=nxit(i,nel)		! no de ref de chaque var. de conception
		  if(isymx.eq.0) then
	        derivee(k+i)=(-1.)*(dixx(j)+part(nel)*ds(j)*			!r&d13	!fev2007
     *					 ((yneu-yneutpart)**2)+						!r&d13	!fev2007	
     *					 part(nel)*omega*(2.*(yneu-yneutpart)*		!r&d13	!fev2007
     *					 (-1.)*part(nel)*ds(j)*(yneu-yneutpart)/	!r&d13	!fev2007
     *					 ometpart))									!r&d13	!fev2007
		  else
	        derivee(k+i)=(-1.)*(dixx(j)+part(nel)*ds(j)*
     *				     ((yneu-yneutpart)**2))
	      endif
		  if(sym.eq.1) derivee(k+i)=2.*derivee(k+i)
	      if(sym.eq.2) derivee(k+i)=4.*derivee(k+i)
	    enddo
	  else
	    do i=1,nbrxi
	      derivee(k+i)=1.e-10
	    enddo
	  endif
	  k=k+nbrxi
    1	continue
 	
	return
      end
