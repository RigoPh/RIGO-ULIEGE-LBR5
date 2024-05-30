      subroutine objpd2(n,xi,nxi,nbrxi,obj,width,iprint,nel,spec1,itt,
     *                  ityp1,isect1)
      use sharedvar
      implicit real*8(a-h,o-z)
      dimension xi(9),nxi(nbrxi)
c      dimension corroel(3)			!corrosion ; corroel = vecteur de dimension 3 qui ne reprend que les epaiss de corrosion liées au panneau nel

c ******************************************************************************
c     calcul de la fonction objectif poids (obj) après la subr. conlin
c
c     version du : nov 2001                                créer 11-5-95

c    - suppression de la lecture sur file 201 (10-6-96)
c    - extra épaisseur de corrosion (nov 2001)
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

c ******************************************************************************
	
	read(302)epsa,epsr,delta,hya,dya,wya,tya,hxr,dxr,wxr,txr,phil,q,	!extension neto	!fev2007
     *			epais													!14/10/04

 	do 123 i=1,nbrxi
	goto(124,125,120,126,127,128,121,129,130),nxi(i)
  124 delta=xi(i)
      goto 123
  125 hya=xi(i)
      goto 123
  120 dya=xi(i)
      goto 123
  126 wya=xi(i)
      goto 123
  127 epsa=xi(i)
      goto 123
  128 hxr=xi(i)
      goto 123
  121 dxr=xi(i)
      goto 123
  129 wxr=xi(i)
      goto 123
  130 epsr=xi(i)
  123 continue

c     dcor = epaisseur de corrosion pour bordé
c	if(nel.eq.1) then
c	  rewind 57
c	  read(57,'(////,a1)') abidon
c	  read(57,*) ipan,dcor,dcor2,dcor3
c	else
c	  read(57,*) ipan,dcor,dcor2,dcor3
c	endif

      corroel = corro(nel,:)	!aout2006					!corrosion
      dcor  = corroel(1)		!aout2006					!corrosion
	dcor2 = corroel(2)		!aout2006					!corrosion
	dcor3 = corroel(3)		!aout2006					!corrosion

	if(isect1.eq.3) then				!âme double t entree=1/2 âme	!19.05.04	!14/10/04
	  hyb=2*hya															!19.05.04
	endif																!19.05.04

	if(ityp1.ne.5) then								!19.05.04		!14/10/04
	  delta= delta+dcor
	else											!19.05.04
	  delta =0.000									!19.05.04
	endif											!19.05.04
c	dcor2   ! epaisseur de corrosion pour cadres
	dya  = dya +dcor2
	tya  = tya +dcor2
	epais = epais +dcor2							!19.05.04
c	dcor3   ! epaisseur de corrosion pour lisses
	dxr  = dxr +dcor3
	txr  = txr +dcor3



c     impressions (avec epaiss corrosion)
c     -----------
      if(itt.eq.1) then
      write (666,132)nel,'ep. bord','hâme cad','eâme cad','lsem cad',
     *      	'tsem cad','epsa cad','hâme rai','eâme rai','lsem rai',
     *        'tsem rai','epsr rai'  
      write (666,131)delta,hya,dya,wya,tya,epsa,hxr,dxr,wxr,txr,epsr
	endif

c     fct = poids = bordage + ames + semelles  (du panneau nel)
c     obj = poids total (structure complete)


	dens = spec1/9.81
      temp=  phil * q * width * pi/180.
      temp=  temp * spec1  !si poids en n

	if(ityp1.ne.5) then			  !plaque		!19.05.04		!14/10/04
      fct= delta 
     *  + (1.0+dw3)* ( dya * hya + tya* wya ) /epsa
     *  + (1.0+dw2)* ( dxr * hxr + txr* wxr ) /epsr
	else						    	! epontille						!19.05.04
	  if(isect1.eq.3)then												!19.05.04	!14/10/04
      fct = delta +														!19.05.04
c     *      (1.0+dw2)*(dxr*hxr+txr*wxr)/epsr +							!19.05.04
     *      (1.0+dw3)*(dya*hyb+2*tya*wya)/epsa    						!19.05.04
	  elseif(isect1.eq.1)then											!19.05.04	!14/10/04
	fct = delta +														!19.05.04
c     *      (1.0+dw2)*(dxr*hxr+txr*wxr)/epsr +							!19.05.04
     *      (1.0+dw3)*(pi*(hya*hya-(hya-2*epais)**2))						!19.05.04
     *	  	/(4*epsa)													!19.05.04
	  elseif(isect1.eq.2) then											!19.05.04	!14/10/04
      fct = delta +														!19.05.04
c     *      (1.0+dw2)*(dxr*hxr+txr*wxr)/epsr +							!19.05.04
     *      (1.0+dw3)*(hya*hya-(hya-2*epais)**2)/epsa						!19.05.04
	  endif
	endif																!19.05.04

	obj= obj + fct * temp	!14/10/04
	        
      return

  131 format('variables_:',11(e11.4))
  132 format('panel n°',i2,2x,11(1x,a8,2x))

      stop

      end
