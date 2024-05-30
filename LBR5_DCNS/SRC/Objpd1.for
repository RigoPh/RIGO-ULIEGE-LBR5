      subroutine objpd1(nel,n,nn,nxi,nbrxi,obj,dfct,width,    !septembre 2006
     *                  iprint,spec1,itype1,isect1)			!f�vrier 2004
      use sharedvar
      implicit real*8(a-h,o-z)
      dimension dfct(n),nxi(nbrxi)
c      dimension xi(n),dfct(n),nxi(nbrxi)

c ******************************************************************************
c     calcul de la fonction objectif poids (obj) et
c     calcul des d�riv�es de la fonction objectif (dfct(i), i=1,nbrxi)
c
c     version du : nov 2001                                cr�er 29-4-94
c
c ******************************************************************************

c les variables
c -------------
c	1	delta =  �paisseur du bordage
c	2	hya   =  hauteur de l'�me des aiguilles
c	3	dya   =  �paisseur de l'�me des aiguilles
c	4	wya   =  largeur des semelles des aiguilles
c	5	epsa  =  entredistance entre aiguilles
c	6	hxr   =  hauteur de l'�me des raidisseurs
c	7	dxr   =  �paisseur de l'�me des raidisseurs
c	8	wxr   =  largeur des semelles des raidisseurs
c	9	epsr  =  entredistance entre raidisseurs

c 	autres param�tres :
c	 	tya = �paisseur semelle aiguilles
c     	txr = �paisseur semelle raidisseurs 
c
c  les variables de conception pour epontille					!f�vrier 2004
c  ------------------------------------------					
c	1	hya		=	hauteur demi �me ou diam ou c�t� ext.	
c	2	dya		=	�paisseur d'�me							
c	3	wya		=	largeur de semelle						
c	4	epais	=	�paisseur de paroi mince				
c	5	epsa	=	entredistance entre �pontilles			
c

c ******************************************************************************

	
	read(302)epsa,epsr,delta,hya,dya,wya,tya,hxr,dxr,wxr,txr,phil,q,	!extension neto	!fev2007
     *		epais									!f�vrier 2004

c     dcor = epaisseur de corrosion pour bord�
c	if(nel.eq.1) then
c	  rewind 57
c	  read(57,'(////,a1)') abidon
c	  read(57,*) ipan,dcor,dcor2,dcor3
c	else
c	  read(57,*) ipan,dcor,dcor2,dcor3
c	endif

	corroel=corro(nel,:)			!oct06
	dcor  = corroel(1)				!oct06			!corrosion
	dcor2 = corroel(2)				!oct06			!corrosion
	dcor3 = corroel(3)				!oct06			!corrosion


	if(isect1.eq.3) then				!�me double t entree=1/2 �me	!f�vrier 2004
	hyb=2*hya															!f�vrier 2004
	endif																!f�vrier 2004

	if(itype1.ne.5) then								!f�vrier 2004
	delta= delta+dcor
	else											!f�vrier 2004
	delta =0.000									!f�vrier 2004
	endif											!f�vrier 2004
c	dcor2   ! epaisseur de corrosion pour cadres
	dya  = dya +dcor2
	tya  = tya +dcor2
	epais = epais +dcor2							!f�vrier 2004
c	dcor3   ! epaisseur de corrosion pour lisses
	dxr  = dxr +dcor3
	txr  = txr +dcor3

c     fct = poids = bordage + ames + semelles  (du panneau nel)
c     obj = poids total (structure complete)

	dens = spec1/9.81
      temp=  phil * q * width * pi/180.
      temp=  temp * spec1  !si poids en n

	if(itype1.ne.5) then			  !plaque		!f�vrier 2004
      fct= delta 
     *  + (1.0+dw3)* ( dya * hya + tya* wya ) /epsa
     *  + (1.0+dw2)* ( dxr * hxr + txr* wxr ) /epsr
	else						    	! epontille						!f�vrier 2004
	  if(isect1.eq.3)then												!f�vrier 2004
      fct = delta +														!f�vrier 2004
c     *      (1.0+dw2)*(dxr*hxr+txr*wxr)/epsr +							!oct06
     *      (1.0+dw3)*(dya*hyb+2*tya*wya)/epsa    						!f�vrier 2004
	  elseif(isect1.eq.1)then											!f�vrier 2004
	fct = delta +														!f�vrier 2004
c     *      (1.0+dw2)*(dxr*hxr+txr*wxr)/epsr +							!oct06
     *      (1.0+dw3)*(pi*(hya*hya-(hya-2*epais)**2))						!f�vrier 2004
     *	  	/(4*epsa)													!f�vrier 2004
	  elseif(isect1.eq.2) then										!f�vrier 2004
      fct = delta +														!f�vrier 2004
c     *      (1.0+dw2)*(dxr*hxr+txr*wxr)/epsr +							!oct06
     *      (1.0+dw3)*(hya*hya-(hya-2*epais)**2)/epsa						!20.02.04
	  endif
	endif																!f�vrier 2004
      ppt= fct * temp ! poids du panneau
      obj= obj + ppt  ! poids des panneaux (cumule)
	      
	if ((iprint.ne.0).and.(nel.eq.1)) then			!oct06
	 write(666,*)
	 write(666,*)'*** weight objective function (subr. objpd1)'
	 write(666,*)'    ----------------------------------------'
	 write(666,*)'panneau/panel   poids-weight (n)'
	 write(666,42) nel,ppt
   42  format(5x,i3,15x,e14.7)
	endif

      if ((iprint.ne.0).and.(nel.eq.neto)) then		!oct06
	  write(666,*)
        write(666,*)' poids total pour toute la structure (subr.objpd1)'
        write(666,*)' -------------------------------------------------'
        write(666,*)'  poids - weight = ',obj,' n'
	endif

c     calcul des sensitivites
	do 101 i=1,nbrxi
	ii=nn+i
	if(nxi(i).eq.1) then
      dfct(ii)= temp
	else if (nxi(i).eq.2) then
      dfct(ii)= temp  * (1.0+dw3)*dya/epsa
	else if (nxi(i).eq.3) then
      dfct(ii)= temp  * (1.0+dw3)*hya/epsa
	else if (nxi(i).eq.4) then
      dfct(ii)= temp  * (1.0+dw3)*tya/epsa
	else if (nxi(i).eq.5) then
      dfct(ii)= -temp * (1.0+dw3)*(dya*hya+tya*wya)/(epsa*epsa) 
	else if (nxi(i).eq.6) then
      dfct(ii)= temp  * (1.0+dw2)*dxr/epsr
	else if (nxi(i).eq.7) then
      dfct(ii)= temp  * (1.0+dw2)*hxr/epsr
	else if (nxi(i).eq.8) then
      dfct(ii)= temp  * (1.0+dw2)*txr/epsr
	else if (nxi(i).eq.9) then
      dfct(ii)= -temp * (1.0+dw2)*(dxr*hxr+txr*wxr)/(epsr*epsr) 
	endif
  101 continue
  
      return
      end
