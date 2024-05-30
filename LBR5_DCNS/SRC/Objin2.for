      subroutine objin2(obj,xi,itt)
      
	use sharedvar
	
	implicit real*8(a-h,o-z)
	real *8 ixx
	dimension xi(9*neto)

c ******************************************************************************
c     calcul de la fonction objectif inertie (obj) et
c     calcul des dérivées de la fonction objectif (dfct(i), i=1,nbrxi)
c
c     version du : février 2006                                créer 14-2-06
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

      nc=0	
	omesypart=0.
	ometpart=0.

      do nel=1,neto
	  
	  nbrxi=nvar(nel)
	  
	  read(302)epsa,epsr,delta,hya,dya,wya,tya,hxr,dxr,wxr,txr,phil,q,		!extension neto	!fev2007
     *           epais
     
        delt(nel)=delt(nel)-(hxr*dxr+wxr*txr)/epsr-delta		
     
        do 123 i=1,nbrxi
	    goto(124,125,120,126,127,128,121,129,130),nxit(i,nel)
  124     delta=xi(nc+i)
          goto 123
  125     hya=xi(nc+i)
          goto 123
  120     dya=xi(nc+i)
          goto 123
  126     wya=xi(nc+i)
          goto 123
  127     epsa=xi(nc+i)
          goto 123
  128     hxr=xi(nc+i)
          goto 123
  121     dxr=xi(nc+i)
          goto 123
  129     wxr=xi(nc+i)
          goto 123
  130     epsr=xi(nc+i)
  123   continue

        nc=nc+nbrxi

	  delt(nel)=delt(nel)+(hxr*dxr+wxr*txr)/epsr+delta
  
        sph2=vsin(-philn(nel)/2.,0.d00)
        s1=vsin(tetas(nel),0.d00)
        s2=vsin(tetas(nel),-philn(nel)/2.)
        phil=-philn(nel)*pi/180.
        d2=q*(s1-2.*sph2*s2/phil)	
        omega=dabs(delt(nel)*q*phil)	
        ometpart=ometpart+part(nel)*omega				! section totale
        yneu=z(nel,3)+d2
	  omesypart=omesypart+part(nel)*omega*yneu		! moment statique vertical
	  if(isymx.ne.1) yneutpart=omesypart/ometpart	! axe neutre horizontale

	  if(itt.eq.1) then					!impression
          write (666,132)nel,'ep. bord','hâme cad','eâme cad',
     *       	   'lsem cad','tsem cad','epsa cad','hâme rai',
     *           'eâme rai','lsem rai','tsem rai','epsr rai'  
          write (666,131)delta,hya,dya,wya,tya,epsa,hxr,dxr,wxr,txr,epsr
	  endif

	enddo


	rewind 302		!extension neto	!fev2007
	
	
	do nel=1,neto

	  read(302)epsa,epsr,delta,hya,dya,wya,tya,hxr,dxr,wxr,txr,phil,q,		!extension neto	!fev2007
     *           epais
     
        c19=vcos(tetas(nel),0.d00)							!multi obj
	  sph1=vsin(-philn(nel),0.d00)
	  sph2=vsin(-philn(nel)/2.,0.d00)
        s1=vsin(tetas(nel),0.d00)
        s2=vsin(tetas(nel),-philn(nel)/2.)
	  phil=-philn(nel)*pi/180.
        d2=q*(s1-2.*sph2*s2/phil)
	  omega=dabs(delt(nel)*q*phil)
	  yneu=z(nel,3)+d2
	  ixx=((c19*q*phil)**2+(s1*part(nel)*delt(nel))**2)*
     *	  part(nel)*delt(nel)*q*dabs(phil)/12.						! ** cas des plaques
	  fct=ixx+part(nel)*omega*(yneu-yneutpart)**2

	  
	
	  if(sym.eq.1) fct=2*fct				
	  if(sym.eq.2) fct=4*fct				
	
	  obj=obj+fct
	
	enddo

      return

  131 format('variables_:',11(e11.4))
  132 format('panel n°',i2,2x,11(1x,a8,2x))

      stop

      end
