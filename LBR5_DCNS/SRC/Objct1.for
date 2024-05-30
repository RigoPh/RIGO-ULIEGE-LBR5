      subroutine objct1(nel,n,nn,xi,nxi,nbrxi,obj,dfct,width,
     *                  iprint,spec1,itype1,isect1)!,corroel)				!février 2004
      use sharedvar
      implicit real*8(a-h,o-z)
      dimension xi(n),dfct(n),nxi(nbrxi)


c **********************************************************************
c     calcul de la fonction objectif cout (obj) après la subr. conlin
c
c     16-11-2001 : sur-épaisseurs de corrosion
c     11-01-2002 : nouveaux paramètres coût (p9x,p9y,c2,c3,dw2,dw3)
c     23-05-2003 : correction ds le calcul des sensibilités (errata)
c
c     version du : 18-11-01   			          créer 18-5-98
c
c     modif: 25-5-2003
c
c ******************************************************************************
c  données relatives à la fonction coût (fonction objectif)                       
c      ---------------------------------------------------------                        
c     -icout  (0 = fct obj. poids ; 1 = fct obj. coût)    (integer) 
c
c     - rend_global, eqp                                         (real) 
c       rend_global  = rendement global du chantier (0 < rend_global < 10,0)
c               en principe rend_global=1,0 . 
c               on peut faire varier rend_global de manière à évaluer l'impact d'une variation
c               du rendement sur la fonction objectif mais aussi sur l'optimum
c               (analyse de sensibilité).
c               rend_global porte seulement sur la main-d'oeuvre (p4,p5,p6,p7,p9 et p10).
c       eqp  (0,01 < eqf < 0,1 t/h-h)
c             = equivalent poids de la main d'oeuvre (t/h-h)
c             = coût mdo (euro/h-h)  / cout mat. (euro/t)  
c             = 0.005 à 0.015 dans les pays à mdo bon marché (100 fb/h-h)
c             = 0.05 à 0.08   dans les pays occidentaux      (1500 à 2000 fb/h-h)
c          
c     - dref(bordé), drefx(lisse) et drefy(cadre)  (en m)    
c         dref = epaisseur de référence correspondant au coût c1(euro/kg) des tôles,
c                    (valeur recommandée : 0,01m càd 10 mm)    
c         drefx= epaisseur de référence (ame des lisses) correspondant au coût c2(euro/kg)
c                des lisses
c         drefy= epaisseur de référence (ame des cadres) correspondant au coût c3(euro/kg) 
c                des cadres,
c               
c               pour les charges de travail p10, l'épaisseur de référence  "dref" 
c               correspond à la tôle du bordé.
c               pour les charges de travail p4,p5 et p9x, p9y, et
c               pour le coût c8(euro/m) du soudage (énergie + consommables) cette épaisseur
c               de référence est drefx (ou drefy) qui correspond à la tôle des âmes des membrures
c               que l'on soude, soit sur le bordé (p4 et p5), soit sur la semelle (p9x, p9y).
c                        
c     - prix matér.: c1(tole,euro/kg), c2(long,euro/kg), c3(trans,euro/kg), dc1(tole)  (real)  
c             c1  = prix au kg des tôles dont l'épaisseur est "dref"  (euro/kg)
c                 = 23 fb/kg  pour acier ae 235) et 26 fb/kg pour ae355)
c             c2  = prix au kg des matériaux des longitudinaux (lisses) dont l'épaisseur 
c                   de l'âme est "dref" (et pas drefx)
c                 = variation en % par rapport au prix c1 pour des membrures longitudinales
c                   constituées de profilés standards (cornières,t, bulbes, ..) (en %) 
c                 = -10% selon van frachem (c.à.d 0.90*c1)
c                 = +25% au bengladesh selon rahman (c.à.d 1.25*c1).
c             c3  = prix au kg des matériaux des cadres 
c                   dont l'épaisseur de l'âme est "dref" (et pas drefy)
c                 = variation en % par rapport au prix c1 pour des membrures transversales
c                   constituées de profilés standards (cornières,t, bulbes, ..) (en %)
c                 = -10% selon van frachem (c.à.d 0.90*c1)
c                 = +40% au bengladesh selon rahman (c.à.d 1.40*c1).
c             dc1 = variation en % du prix c1 entre "dref" et "dref + 1mm" (%/mm)
c                 = -0.6% selon van frachem (c.à.d -0.006)
c                        
c     - mdo bordé  : p10(h-h/m),     pc10(variation par mm)                   (real)  
c             p10 = mdo nécessaire pour constituer le bordé à partir de tôles d'épaisseur
c                   "dref" (soudure bout à bout).
c                 = 1.00 h-h/m2 selon van frachem (bordé constitué de tôle de 6x3m)
c                 = 0.35 h-h/m2 (selon winkle et baird)
c             dp10= variation en % de la mdo p10 si on passe d'une épaisseur "dref" à
c                   "dref + 1mm" (%/mm)
c                 = +7.0% selon van frachem
c                        
c     - mdo assembl: p4(long,h-h/m), p5(trans,h-h/m), dp4(long),  dp5(trans)  (real)                  
c             p4  = mdo nécessaire pour souder une membrure long. sur le bordé
c                   (si épaisseur âme membrure = "dref" ).
c                 = +1.65 h-h/m pour dref=10mm et +0.9 h-h/m pour dref=5mm selon van frachem
c                 = +1.10 h-h/m selon rahman (bengladesh) 
c                 = +0.90 h-h/m selon winkle et baird 
c             dp4 = variation en % de la mdo p4 si on passe d'une épaisseur "dref" à
c                   "dref + 1mm" (%/mm)
c                 = +10% selon van frachem
c             p5  = mdo nécessaire pour souder une membrure transv. sur le bordé
c                   (si épaisseur âme membrure = "dref" ).
c                 = +1.65 h-h/m pour dref=10mm et +0.9 h-h/m pour dref=5mm selon van frachem
c                 = +1.25 h-h/m selon rahman (bengladesh) 
c                 = +0.90 h-h/m selon winkle et baird 
c             dp5 = variation en % de la mdo p5 si on passe d'une épaisseur "dref" à
c                   "dref + 1mm" (%/mm)
c                 = +10% selon van frachem 
c
c       nb: temps de soudage : donnees du cewac-ulg (mai 1998)
c           pour la soudure semi-automatique (temps de marche 30%): 
c              - si dref = 5 mm            - si dref = 10mm
c			 p=4min/m et dp=32%           p=20min/m et dp=25%
c           pour la soudure manuelle (à la baguette), (temps de marche 25%):
c              - si dref = 5 mm            - si dref = 10mm
c			 p=16min/m et dp=29%           p=62min/m et dp=20%
c                        
c     - mdo membr. :  p9x et p9y (h-h/m),  dp9x et dp9y    (4 real)
c             main d'oeuvre nécessaire à la construction des membrures (profilés 
c             reconstitués en atelier)
c
c             p9x (p9y)= mdo nécessaire pour souder l'âme sur la semelle
c                   (si épaisseur âme membrure = "drefx" pour lisse et "drefy" pour cadre).
c                 = 1.50 (h-h/m) selon rahman  
c                 = 0.60 (h-h/m) selon winkle et baird 
c                 = 0.50 (h-h/m) selon van frachem (dref = 10mm) 
c             dp9x= variation en % de la mdo p9 si on passe d'une épaisseur "drefx" à
c                   "drefx + 1mm" (%/mm)
c             dp9y= variation en % de la mdo p9 si on passe d'une épaisseur "drefy" à
c                   "drefy + 1mm" (%/mm)
c                 = 10% selon van frachem (dref = 10mm)
c
c     - mdo joints : p6(intersect,h-h/joint),p7(gousset,h-h/joint)  
c                      béta  r(long.), béta  t(transv)                      (real)  
c             main d'oeuvre nécessaire pour les intersections (p6) et les goussets (p7).
c
c             p6  = main d'oeuvre nécessaire pour une intersection (h-h/intersection)
c                 = 0.60 (h-h/m) selon rahman  
c                 = 0.30 (h-h/m) selon winkle et baird 
c             p7  = main d'oeuvre nécessaire pour un gousset (h-h/gousset)
c                   le nombre de gousset = betar x betat
c               avec betar = le nbre relatif de longitudinaux qui ont un gousset 
c                            (0 < betar < 1)
c                    betat = le nbre relatif de transversaux qui ont un gousset 
c                            (0 < betat < 1)
c                 = 1.15 (h-h/m) selon rahman  
c                 = 0.30 (h-h/m) selon winkle et baird 
c                 
c     - prix soud. : c8(euro/m), dc8(variation par mm), 
c                    alpha r(long.), alpha t(transv),                 (2 real, 2 int)                  
c             c8  = prix au mètre courant de l'énergie et des consommables pour souder
c                   une tôle d'âme d'épaisseur "drefx" si lisse et "drefy" si cadre
c                 = 54 fb/m selon rahman (0.90 h-h/m avec 1 h-h=60 fb au bengladesh)
c                 = 40 fb/m selon le cewac pour drefx (drefy) =10mm (semi automatique ou manuelle)
c                 = 10 fb/m selon le cewac pour drefx (drefy) = 5mm (semi automatique ou manuelle)
c             dc8 = variation en % du prix c8 si on passe de "dref" à "dref + 1mm" (%/mm)
c                 = 20% entre 3 et 7mm  : drefx (drefy) =  5mm (cfr données du cewac, mai 1998)     
c                 = 27% entre 8 et 15 mm: drefx (drefy) = 10mm (cfr données du cewac, mai 1998)     
c                 = 10% si la variation d'épaisseur est importante     
c             alpha r = 1 : les membrures longitudinales sont des profilés standards 
c             alpha t = 1 : les membrures transversales  sont des profilés standards 
c             alpha r = 0 : les membrures longitudinales sont des profilés reconstitués 
c             alpha t = 0 : les membrures transversales  sont des profilés reconstitués 
c             note: par dref (pour c8), il faut entendre drefx ou drefy selon le type de renforts considérés
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
	
	read(302)epsa,epsr,delta,hya,dya,wya,tya,hxr,dxr,wxr,txr,phil,q,	!extension neto	!fev2007
     *		epais										!février 2004

c     dcor = epaisseur de corrosion pour bordé	!aout2004
c	if(nel.eq.1) then							!aout2004
c	  rewind 57									!aout2004
c	  read(57,'(////,a1)') abidon				!aout2004
c	  read(57,*) ipan,dcor,dcor2,dcor3			!aout2004
c	else										!aout2004
c	  read(57,*) ipan,dcor,dcor2,dcor3			!aout2004
c	endif										!aout2004

      corroel = corro(nel,:)	!aout2006				 !corrosion
      dcor  = corroel(1)		!aout2006				 !corrosion
	dcor2 = corroel(2)		!aout2006				 !corrosion
	dcor3 = corroel(3)		!aout2006				 !corrosion


	if(isect1.eq.3) then			!âme double t entree=1/2 âme		!février 2004
	  hyb=2*hya															!février 2004
	endif																!février 2004

	if(itype1.ne.5) then												!février 2004
	delta= delta+dcor
	else																!février 2004
	delta =0.000														!février 2004
	endif																!février 2004
c	dcor2   ! epaisseur de corrosion pour cadres
	dya  = dya +dcor2
	tya  = tya +dcor2
	epais = epais +dcor2							!février 2004
c	dcor3   ! epaisseur de corrosion pour lisses
	dxr  = dxr +dcor3
	txr  = txr +dcor3

c -------------------------------------------------------------
c     fcp = poids = bordage + ames + semelles  (du panneau nel)
	if(nel.eq.1) poids=0.

      temp = phil * q * width * pi/180.
	dens = spec1/9.81


	if(((refthick_input.eq.1).and.(itype1.ne.5))) then

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

	   elseif(itype_longit(nel).eq.17)then   ! Té Laminé
	      drefx=dref_l
               c2=c_tl
	        dw2=dw_l

	   else 
	      write(* ,*)'type de profilé inexistant'
	   endif

      endif




	if(itype1.ne.5) then						 !plaque			!février 2004
      fcp= delta 
     *  +  (1.0+dw3)*( dya * hya + tya* wya ) /epsa
     *  +  (1.0+dw2)*( dxr * hxr + txr* wxr ) /epsr
 	else										 !epontille			!février 2004
	  if(isect1.eq.3)then											!février 2004
      fcp = delta +													!février 2004
c     *      (1.0+dw2)*(dxr*hxr+txr*wxr)/epsr +						!février 2004
     *      (1.0+dw3)*(dya*hyb+2*tya*wya)/epsa    					!février 2004
	  elseif(isect1.eq.1)then										!février 2004
	fcp = delta +													!février 2004
c     *      (1.0+dw2)*(dxr*hxr+txr*wxr)/epsr +						!février 2004
     *      (1.0+dw3)*(pi*(hya*hya-(hya-2*epais)**2))					!février 2004
     *	  	/(4*epsa)												!février 2004
	  elseif(isect1.eq.2) then									!février 2004
      fcp = delta +													!février 2004
c     *      (1.0+dw2)*(dxr*hxr+txr*wxr)/epsr +						!février 2004
     *      (1.0+dw3)*(hya*hya-(hya-2*epais)**2)/epsa    				!20.02.04
	  endif
	endif															!février 2004
      fcp=  fcp * temp * spec1  ! poids du panneau (borde, raid et cadre only)
	poids=poids+ fcp


c -------------------------------------------------------------
c     fmat= coût des matériaux (acier)
c     fsou= coût du soudage (energie + consommables)
c     fmdo= coût main d'oeuvre
c     fct = fmat + fsou + fmdo  (d'un panneau nel)
c     obj = coût total          (de la structure complete)

	if(itype1.ne.5) then									!février 2004
      fmat = temp * dens * 
     *  (c1 *(1.+(delta-dref)*1000.*dc1) * delta 
     * + c2 *(1.+(dxr  -dref)*1000.*dc1) * (1.0+dw2)   ! dref et pas drefx
     *                                   * (dxr*hxr+txr*wxr)/epsr 
     * + c3 *(1.+(dya  -dref)*1000.*dc1) * (1.0+dw3)   ! dref et pas drefy
     *                                   * (dya*hya+tya*wya)/epsa ) 

      fsou = temp * c8 * 
     *    ( (1.+(dxr-drefx)*1000.*dc8)*(2.-ialr)/epsr
     *     +(1.+(dya-drefy)*1000.*dc8)*(2.-ialt)/epsa )
      

	if((refthick_input.eq.1).and.(itype1.ne.5)) then


         fmdo = temp * rend_global* rend_panel(nel) * eqp * 1000. * c1 *  
     *    (  ( p4 *(1.+(dxr-drefx)*1000.*dp4 ) + 
     *         p9x*(1.+(dxr-drefx)*1000.*dp9x)   )/epsr
     *     + ( p5 *(1.+(dya-drefy)*1000.*dp5 ) + 
     *         p9y*(1.+(dya-drefy)*1000.*dp9y)   )/epsa ! errata mars 2004  --> p9y au lieu de p9x
     *     + (p6+ber*bet*p7) /(epsa*epsr)  
     *     +  p10*(1.+(delta-dref)*1000.*dp10)      )

      else


         fmdo = temp * rend_global* rend_panel(nel) * eqp * 1000. * c1 *  
     *    (  ( p4 *(1.+(dxr-drefx)*1000.*dp4 ) + 
     *         p9x*(1.+(dxr-drefx)*1000.*dp9x)   )/epsr
     *     + ( p5 *(1.+(dya-drefy)*1000.*dp5 ) + 
     *         p9y*(1.+(dya-drefy)*1000.*dp9y)   )/epsa ! errata mars 2004  --> p9y au lieu de p9x
     *     + (p6+ber*bet*p7) /(epsa*epsr)  
     *     +  p10*(1.+(delta-dref)*1000.*dp10)      )



      endif


	else							!epontilles				!février 2004
	if(isect1.eq.3) then
	fmat=temp * dens * c3 *(1.+(dya  -dref)*1000.*dc1) * (1.0+dw3)   !20.02.04
     *                      * ((2*dya*hya+2*tya*wya)/epsa )			!20.02.04
	elseif(isect1.eq.1) then
	fmat=temp * dens * c3 *(1.+(epais  -dref)*1000.*dc1) * (1.0+dw3)   !20.02.04
     *                 * ((pi*(hya*hya-(hya-2*epais)**2))/(4*epsa ))   	!20.02.04
	elseif(isect1.eq.2) then
	fmat=temp * dens * c3 *(1.+(epais  -dref)*1000.*dc1) * (1.0+dw3)   !20.02.04
     *                 * ((hya*hya-(hya-2*epais)**2)/epsa )				!20.02.04
	endif													!février 2004
	fsou=0.001												!février 2004
	fmdo=0.001												!février 2004
	endif												!20.02.04
      fct = fmat + fsou + fmdo
      obj = obj + fct

















	if (nel==1) then
       write(666,*) 
	 write(666,*)'*** cost objective function (subr. objct1)'
	 write(666,*)'    --------------------------------------'
	 write(666,*)'panneau/panel (with additional corrosion thickness)'
	endif
      write(666,42) nel,fmat,fsou,fmdo
   42 format(i3,4x,'fmat,fsoud,fmdo=',3(e14.7,1x),'euro ($ or ...)')
      write(666,43) fct,fcp
   43 format(8x,'cost= ',e14.7,'   poids= ',e14.7,' n')


      if (nel==neto) then
        write(666,*) 
        write(666,*)' total for the structure (with gross thickness)'
        write(666,*)' ----------------------------------------------'
        write(666,*)'  cout  - cost   = ',obj  ,' euro ($ or...)'
        write(666,*)'  poids - weight = ',poids,' n'
	endif

c     tests (cout positif ??)
c     -----------------------
	if ((fmat.le.0.0).or.(fsou.le.0.0).or.(fmdo.le.0.0)) then
       write(666,*) 
       write(666,*) ' certains couts sont negatifs'
       write(666,*) ' few costs are negative!!'
       write(666,*) 
       write(666,*) ' there are some problems in the unitary costs.'
       write(666,*) ' probably in the variation costs/mm of plate'
       write(*,*) 
       write(*,*) ' certains couts sont negatifs'
       write(*,*) ' few costs are negative!!'
       write(*,*) 
       write(*,*) ' there is some problems in your unitary costs'
       write(*,*) ' probably in the variation costs/mm of plate'
       write(*,*) 
       write(*,*) ' we will continue but you should check the results.'
       write(*,*) '   check your ouput file opt-**.**'

       write(6970,*) 
       write(6970,*) ' certains couts sont negatifs'
       write(6970,*) ' few costs are negative!!'
       write(6970,*) 
       write(6970,*) ' there is some problems in your unitary costs'
       write(6970,*) ' probably in the variation costs/mm of plate'
       write(6970,*) 
       write(6970,*) 
     *' we will continue but you should check the results.'
       write(6970,*) '   check your ouput file opt-**.**'


	 write(29,*)																!bug
       write(29,*) ' certains couts sont negatifs'								!bug
       write(29,*) ' few costs are negative!!'									!bug
       write(29,*)																!bug
       write(29,*) ' there is some problems in your unitary costs'				!bug
       write(29,*) ' probably in the variation costs/mm of plate'					!bug
       write(29,*)																!bug
       write(29,*) ' we will continue but you should check the results.'			!bug
       write(29,*) '   check your ouput file opt-**.**'
	 pause 'ok?'
	endif
c234567890123456789012345678901234567890123456789012345678901234567890123456789
c     calcul des sensibilites
c     ------------------------
	do 101 i=1,nbrxi
	ii=nn+i






	if(nxi(i).eq.1) then
	if((refthick_input.eq.1).and.(itype1.ne.5)) then

          dfct(ii)=temp*c1 * (  (1.+(2.*delta-dref)*1000.*dc1)*dens
     *      + rend_global* rend_panel(nel)*eqp*1000.*p10*dp10*1000.) 
      else

       dfct(ii)=temp*c1 * (  (1.+(2.*delta-dref)*1000.*dc1)*dens
     *      + rend_global* rend_panel(nel)*eqp*1000.*p10*dp10*1000.) 
      endif





     
	else if (nxi(i).eq.2) then
       dfct(ii)= temp * dens * (1.0+(dya-dref)*1000.*dc1)
     *                       * (1.+dw3) *c3   *dya/epsa








	else if (nxi(i).eq.3) then
	if((refthick_input.eq.1).and.(itype1.ne.5)) then

       dfct(ii)= (temp/epsa) * 
     *    (  (1.+(dya-dref)*1000.*dc1)*(1.0+dw3)*hya*c3*dens   +
     *       (1.0+dw3)*(dya*hya+tya*wya)*1000.*dc1*c3*dens     +  !23-05-2003 
     *       1000.0*c8*dc8*(2.-ialt)     +
     *rend_global* rend_panel(nel)*eqp*1000.0*c1*
     *1000.0*(p5*dp5+p9y*dp9y))
      else
       dfct(ii)= (temp/epsa) * 
     *    (  (1.+(dya-dref)*1000.*dc1)*(1.0+dw3)*hya*c3*dens   +
     *       (1.0+dw3)*(dya*hya+tya*wya)*1000.*dc1*c3*dens     +  !23-05-2003 
     *       1000.0*c8*dc8*(2.-ialt)     +
     *rend_global* rend_panel(nel)*eqp*1000.0*c1*
     *1000.0*(p5*dp5+p9y*dp9y))
      endif





	else if (nxi(i).eq.4) then
       dfct(ii)= temp  * dens * (1.0+(dya-dref)*1000.0*dc1)
     *                        * (1.0+dw3)  *  c3 * tya/epsa







	else if (nxi(i).eq.5) then

	if((refthick_input.eq.1).and.(itype1.ne.5)) then
         dfct(ii)= -(temp/(epsa*epsa)) * 
     *  ( c3*dens*(1.0+(dya-dref)*1000.0*dc1)*(1.+dw3)*(dya*hya+tya*wya)
     *   + (1.+(dya-drefy)*1000.*dc8)*c8*(2.-ialt) 
     *   + rend_global* rend_panel(nel)*eqp*1000.*c1*
     *            (   p5 *(1.+(dya-dref)*1000.*dp5)  
     *              + p9y*(1.+(dya-dref)*1000.*dp9y) 
     *              + (p6+ber*bet*p7)/epsr           )        )
	
      else
         dfct(ii)= -(temp/(epsa*epsa)) * 
     *  ( c3*dens*(1.0+(dya-dref)*1000.0*dc1)*(1.+dw3)*(dya*hya+tya*wya)
     *   + (1.+(dya-drefy)*1000.*dc8)*c8*(2.-ialt) 
     *   + rend_global* rend_panel(nel)*eqp*1000.*c1*
     *            (   p5 *(1.+(dya-dref)*1000.*dp5)  
     *              + p9y*(1.+(dya-dref)*1000.*dp9y) 
     *              + (p6+ber*bet*p7)/epsr           )        )
      endif





	else if (nxi(i).eq.6) then
       dfct(ii)= temp * dens * (1.0+(dxr-dref)*1000.*dc1)
     *                       * (1.0+dw2) * c2 * dxr/epsr






	else if (nxi(i).eq.7) then
	if((refthick_input.eq.1).and.(itype1.ne.5)) then
       dfct(ii)= (temp/epsr) * 
     *    (  (1.+(dxr-dref)*1000.*dc1)*(1.0+dw2)*hxr*c2*dens   +   
     *       (1.0+dw2)*(dxr*hxr+txr*wxr)*1000.*dc1*c2*dens     +  !23-05-2003 
     *       1000.0* c8*dc8*(2.-ialr)    + 
     *rend_global* rend_panel(nel)*eqp*1000.0*c1*
     *1000.0*(p4*dp4+p9x*dp9x)  )

      else

       dfct(ii)= (temp/epsr) * 
     *    (  (1.+(dxr-dref)*1000.*dc1)*(1.0+dw2)*hxr*c2*dens   +   
     *       (1.0+dw2)*(dxr*hxr+txr*wxr)*1000.*dc1*c2*dens     +  !23-05-2003 
     *       1000.0* c8*dc8*(2.-ialr)    + 
     *rend_global* rend_panel(nel)*eqp*1000.0*c1*
     *1000.0*(p4*dp4+p9x*dp9x)  )

	endif





	else if (nxi(i).eq.8) then
       dfct(ii)= temp  * dens * (1.+(dxr-dref)*1000.0*dc1) 
     *                        * (1.0+dw2)  * c2 * txr/epsr












	else if (nxi(i).eq.9) then
	if((refthick_input.eq.1).and.(itype1.ne.5)) then

       dfct(ii)= -(temp/(epsr*epsr)) * 
     *  ( c2*dens*(1.0+(dxr-dref)*1000.0*dc1)*(1.+dw2)*(dxr*hxr+txr*wxr)
     *   + (1.+(dxr-drefx)*1000.*dc8)*c8*(2.-ialr) 
     *   + rend_global* rend_panel(nel)*eqp*1000.*c1*
     *            (   p4 *(1.+(dxr-dref)*1000.*dp4)  
     *              + p9x*(1.+(dxr-dref)*1000.*dp9x) 
     *              + (p6+ber*bet*p7)/epsa           )        )

      else

       dfct(ii)= -(temp/(epsr*epsr)) * 
     *  ( c2*dens*(1.0+(dxr-dref)*1000.0*dc1)*(1.+dw2)*(dxr*hxr+txr*wxr)
     *   + (1.+(dxr-drefx)*1000.*dc8)*c8*(2.-ialr) 
     *   + rend_global* rend_panel(nel)*eqp*1000.*c1*
     *            (   p4 *(1.+(dxr-dref)*1000.*dp4)  
     *              + p9x*(1.+(dxr-dref)*1000.*dp9x) 
     *              + (p6+ber*bet*p7)/epsa           )        )
	endif






	endif

  101 continue
  
      return
      end
