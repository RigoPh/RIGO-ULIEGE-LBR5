      subroutine objct2(n,xi,nxi,nbrxi,obj,width,iprint,nel,spec1,itt,
     *                  ityp1,isect1)
      use sharedvar
      implicit real*8(a-h,o-z)
      dimension xi(9),nxi(nbrxi)

c **********************************************************************
c     ré-évaluation de la fonction objectif cout (obj) après la subr. conlin
c
c     version du : 16-11-2001    			          créer 18-5-98
c
c     16-11-2001 : sur-épaisseurs de corrosion
c
c ****************************************************************************************
c
c les variables de conception
c ---------------------------
c	1	delta =  épaisseur du bordage
c	2	hya   =  hauteur de l'âme des aiguilles
c	3	dya   =  épaisseur de l'âme des aiguilles
c	4	wya   =  largeur des semelles des aiguilles
c	5	epsa  =  entredistance entre aiguilles
c	6	hxr   =  hauteur de l'âme des raidisseurs
c	7	dxr   =  épaisseur de l'âme des raidisseurs
c	8	wxr   =  largeur des semelles des raidisseurs
c	9	epsr  =  entredistance entre raidisseurs
c
c variables associées :
c ---------------------
c	 	tya = épaisseur semelle aiguilles
c     	txr = épaisseur semelle raidisseurs 
c
c **************************************************************************************
	read(302)epsa,epsr,delta,hya,dya,wya,tya,hxr,dxr,wxr,txr,phil,q,			!extension neto	!fev2007
     *			epais												!14/10/04

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

c
c     epaisseur de corrosion
c     -----------------------
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

	if(isect1.eq.3) then			!âme double t entree=1/2 âme		!février 2004	!14/10/04
	  hyb=2*hya															!19.05.04
	endif																!19.05.04

	if(ityp1.ne.5) then												!19.05.04		!14/10/04
	  delta= delta+dcor
	else																!19.05.04
	  delta =0.000d00														!19.05.04
	endif																!19.05.04
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
     *    'tsem cad','epsa cad','hâme rai','eâme rai','lsem rai',
     *    'tsem rai','epsr rai'
      write (666,131)delta,hya,dya,wya,tya,epsa,hxr,dxr,wxr,txr,epsr
	endif

c  -------------------------

c     fct = poids = bordage + ames + semelles  (du panneau nel)
c     obj = poids total (structure complete)
c     fmat= coût des matériaux (acier)
c     fsou= coût du soudage (energie + consommables)
c     fmdo= coût main d'oeuvre
c     fct = fmat + fsou + fmdo  (d'un panneau nel)
c     obj = coût total          (de la structure complete)


      temp =  phil * q * width * pi/180.
	dens = (spec1/9.81)







	if((refthick_input.eq.1).and.(ityp1.ne.5)) then

	   if    (itype_transv(nel).eq.14)then   !Plat Boudin
            drefy=dref_b
               c3=c_pb
	        dw3=dw_b

	   elseif(itype_transv(nel).eq.7) then   ! Té Composite
	      drefy=dref_c
               c3=c_tc
              dw3=dw_c

	   elseif(itype_transv(nel).eq.17)then   ! Té Laminé
	      drefy=dref_l
               c3=c_tl
              dw3=dw_l

	   else 
	      write(* ,*)'type de profilé inexistant'
	   endif






	   if    (itype_longit(nel).eq.14)then   !Plat Boudin
            drefx=dref_b
               c2=c_pb
	        dw2=dw_b

	   elseif(itype_longit(nel).eq.7) then   ! Té Composite
	      drefx=dref_c
               c2=c_tc
	        dw2=dw_c

	   elseif(itype_transv(nel).eq.17)then   ! Té Laminé
	      drefx=dref_l
               c2=c_tl
	        dw2=dw_l

	   else 
	      write(* ,*)'type de profilé inexistant'
	   endif

      endif










	if(ityp1.ne.5) then						 !plaque			!19.05.04	!14/10/04
      fmat = temp * dens * 
     *  (c1 *(1.+(delta-dref)*1000.*dc1) * delta 
     * + c2 *(1.+(dxr  -dref)*1000.*dc1) * (1.0+dw2)   ! dref et pas drefx
     *                                   * (dxr*hxr+txr*wxr)/epsr 
     * + c3 *(1.+(dya  -dref)*1000.*dc1) * (1.0+dw3)   ! dref et pas drefy
     *                                   * (dya*hya+tya*wya)/epsa ) 

      fsou = temp * c8 * 
     *    ( (1.+(dxr-drefx)*1000.*dc8)*(2.-ialr)/epsr
     *     +(1.+(dya-drefy)*1000.*dc8)*(2.-ialt)/epsa )







	if((refthick_input.eq.1).and.(ityp1.ne.5)) then


         fmdo = temp * rend_global* rend_panel(nel) * eqp * 1000. * c1 *  
     *    (  ( p4 *(1.+(dxr-drefx)*1000.*dp4 ) + 
     *         p9x*(1.+(dxr-drefx)*1000.*dp9x)   )/epsr
     *     + ( p5 *(1.+(dya-drefy)*1000.*dp5 ) + 
     *         p9y*(1.+(dya-drefy)*1000.*dp9y)   )/epsa
     *     + (p6+ber*bet*p7) /(epsa*epsr)  
     *     +  p10*(1.+(delta-dref)*1000.*dp10)      )

      else


         fmdo = temp * rend_global* rend_panel(nel) * eqp * 1000. * c1 *  
     *    (  ( p4 *(1.+(dxr-drefx)*1000.*dp4 ) + 
     *         p9x*(1.+(dxr-drefx)*1000.*dp9x)   )/epsr
     *     + ( p5 *(1.+(dya-drefy)*1000.*dp5 ) + 
     *         p9y*(1.+(dya-drefy)*1000.*dp9y)   )/epsa
     *     + (p6+ber*bet*p7) /(epsa*epsr)  
     *     +  p10*(1.+(delta-dref)*1000.*dp10)      )



      endif








	else							!epontilles				!19.05.04
	if(isect1.eq.3) then														!14/10/04
	fmat=temp * dens * c3 *(1.+(dya  -dref)*1000.*dc1) * (1.0+dw3)   
     *                      * ((2*dya*hya+2*tya*wya)/epsa )			
	elseif(isect1.eq.1) then													!14/10/04
	fmat=temp * dens * c3 *(1.+(epais  -dref)*1000.*dc1) * (1.0+dw3)   
     *                 * ((pi*(hya*hya-(hya-2*epais)**2))/(4*epsa ))   	
	elseif(isect1.eq.2) then													!14/10/04
	fmat=temp * dens * c3 *(1.+(epais  -dref)*1000.*dc1) * (1.0+dw3)   
     *                 * ((hya*hya-(hya-2*epais)**2)/epsa )				
	endif													
	fsou=0.0001 !d00													!19.05.04
	fmdo=0.0001 !d00													!19.05.04
	endif												!20.02.04

      fct = fmat + fsou + fmdo

      obj  = obj + fct	        

      return

  131 format('variables_:',11(e11.4))
  132 format('panel n°',i2,2x,11(1x,a8,2x))

      stop

      end
