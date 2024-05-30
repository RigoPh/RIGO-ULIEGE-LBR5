	subroutine couttotal(cttotal)
c	*****************************************************************************
c						Calcul du cout (simplifie) total de la structure
c
c	Avant, le calcul du cout de la structure était effectué un peu partout. Cette
c	sous-routine permet de centraliser ces calculs en un seul endroit.


c	Les variables
c	-------------
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
c  	Les variables de conception pour epontille
c     ------------------------------------------					
c	1	hya		=	hauteur demi âme ou diam ou côté ext.	
c	2	dya		=	épaisseur d'âme							
c	3	wya		=	largeur de semelle						
c	4	epais	=	épaisseur de paroi mince				
c	5	epsa	=	entredistance entre épontillesc
c
c	*****************************************************************************
      !use param_multi

      use param_section
!      use param_cout
!      use param_opti_local
      !use param_opti_global
      implicit double precision(a-h,o-z)
      	

	
		
	do nel=1,neto
	
	if(nel.eq.1) then
      fct= 0.d00
      poids=0.d00
	cttotal = 0.d00
	ffmat = 0.d00
	ffsou = 0.d00
      endif
      
	call coutpanneau(ctpan,nel)

	cttotal = cttotal + ctpan
	ffmat = fmat + ffmat
	ffsou = fsou + ffsou
	ffmdo = fmdo + ffmdo
		
      !cout_  = cout_  + fct


      enddo

c     tests (cout positif ??)
c     -----------------------
	if ((ffmat.le.0.0).or.(ffsou.le.0.0).or.(ffmdo.le.0.0)) then
       write(iu_31(iboat),*) 
       write(iu_31(iboat),*) ' certains couts sont negatifs'
       write(iu_31(iboat),*) ' few costs are negative!!'
       write(iu_31(iboat),*) 
       write(iu_31(iboat),*) ' there are some problems in the unitary co
     *sts.'
       write(iu_31(iboat),*) ' probably in the variation costs/mm of pla
     *te'
       write(*,*) 
       write(*,*) ' certains couts sont negatifs'
       write(*,*) ' few costs are negative!!'
       write(*,*) 
       write(*,*) ' there is some problems in your unitary costs'
       write(*,*) ' probably in the variation costs/mm of plate'
       write(*,*) 
       write(*,*) ' we will continue but you should check the results.'
       write(*,*) '   check your ouput file opt-**.**'



	 write(iu_14(iboat),*)																!bug
       write(iu_14(iboat),*) ' certains couts sont negatifs'								!bug
       write(iu_14(iboat),*) ' few costs are negative!!'									!bug
       write(iu_14(iboat),*)																!bug
       write(iu_14(iboat),*) ' there is some problems in your unitary co
     *sts'				!bug
       write(iu_14(iboat),*) ' probably in the variation costs/mm of pla
     *te'					!bug
       write(iu_14(iboat),*)																!bug
       write(iu_14(iboat),*) ' we will continue but you should check the
     * results.'			!bug
       write(iu_14(iboat),*) '   check your ouput file opt-**.**'
	 write(*,*) 'ok?'
	 read(*,*)
	endif

	return
	end


c	********************************************************************
	subroutine coutpanneau(ctpan,nel)


c	CALCUL DU COUT DU PANNEAU/EPONTILLE NEL
c
c	Input : nel
c
c	Output : ctpan (= coût simplifié du panneau nel)
c
c ******************************************************************************
c  données relatives à la fonction coût (fonction objectif)                       
c      ---------------------------------------------------------                        
c     -icout  (0 = fct obj. poids ; 1 = fct obj. coût)    (integer) 
c
c     - rend_global                                        (real) 
c       rend_global  = rendement global du chantier (0 < rend_global < 10,0)
c               en principe rend_global=1,0 . 
c               on peut faire varier rend_global de manière à évaluer l'impact d'une variation
c               du rendement sur la fonction objectif mais aussi sur l'optimum
c               (analyse de sensibilité).
c               rend_global porte seulement sur la main-d'oeuvre (p4,p5,p6,p7,p9 et p10).
c     - Par Panneau : rend, eqp			!jan09
c		rend = même principe que rend_global mais exprimé par panneau
c		eqp  (0,01 < eqf < 0,1 t/h-h) !!! N'existe plus : est remplacé par le paramètre Labour_cost !!!!
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
      !use param_multi
      use param_section
      !use param_opti_global
      implicit double precision(a-h,o-z)
       
    
      
	call coutpanneau_mat(nel)
	call coutpanneau_sou(nel)
	call coutpanneau_mdo(nel)
      ctpan = fmat  + fsou  + fmdo

	return
	end

ccccccccccccccccccc COUT DU MATERIAU cccccccccccccccccccccccccccccccc
	subroutine coutpanneau_mat(nel)
!	use param_multi

      use param_section
 !     use param_opti_global
      implicit double precision(a-h,o-z)
      
      if (nel .eq. 1) then
      fmat = 0.d00
      ffmat = 0.d00
      endif

   
	
      call coutpanneau_mat_bordes(fmat1,nel)
      call coutpanneau_mat_raidisseurs(fmat2,nel)
      call coutpanneau_mat_cadres(fmat3,nel)
      
      fmat  = fmat1 + fmat2 + fmat3								
	return
	end
	


ccccccccccccccccccc COUT DU MATERIAU pour bordés cccccccccccccccccccccccccccccccc

	subroutine coutpanneau_mat_bordes(fmat1,nel)

!      use param_multi
      use param_section
 !     use param_opti_global
      
      implicit double precision(a-h,o-z)
      
      if(nel.eq.1) then
      fmat1=0.d00
      fmat11=0.d00
      endif
      
       if(isect(nel).eq.3) then                            
      !âme double t entree=1/2 âme
       hyb=2*panneau(nel).hya
      endif


      dcor  = panneau(nel).corro(1) !dcor = epaisseur de corrosion pour bordé
      dcor2 = panneau(nel).corro(2) !dcor2= epaisseur de corrosion pour cadres  
      dcor3 = panneau(nel).corro(3) !dcor3= epaisseur de corrosion pour lisses  

      if(itype(nel).ne.5) then                                               
      deltac=panneau(nel).delta+dcor
      else                                                              
      deltac =0                                                         
      endif                                                 
      dyac   =  panneau(nel).dya  +dcor2
      tyac   =  panneau(nel).tya  +dcor2
      epaisc = panneau(nel).epais +dcor2                                

      dxrc  =    panneau(nel).dxr +dcor3
      txrc  =    panneau(nel).txr +dcor3

	delta = panneau(nel).delta
	hya = panneau(nel).hya
	dya = panneau(nel).dya
	wya = panneau(nel).wya
	tya = panneau(nel).tya
	epsa = panneau(nel).epsa
	hxr = panneau(nel).hxr
	dxr = panneau(nel).dxr
	wxr = panneau(nel).wxr
	txr = panneau(nel).txr
	epsr = panneau(nel).epsr
	phil = dabs(panneau(nel).phil) !!!
	q = panneau(nel).q
	epais = panneau(nel).epais
      

	if(itype(nel).ne.5) then												
	  delta= delta + panneau(nel).corro(1)
	else																
	  delta =0.000														
	endif																
	dya  = dya + panneau(nel).corro(2)
	tya  = tya + panneau(nel).corro(2)
	epais = epais + panneau(nel).corro(2)
	dxr  = dxr + panneau(nel).corro(3)
	txr  = txr + panneau(nel).corro(3)

      dens =  spec(indMateriau(nel))/9.81
      temp =  dabs(panneau(nel).phil) * panneau(nel).q * width *
     * pi/180.d00
      temp1=  temp * dens
      temp2=  temp * rend_global * panneau(nel).rend * 
     * labour_cost(indMateriau(nel))

	drefy_local=drefy(indMateriau(nel))
	c3_local=c3(indMateriau(nel))
	dw3_local=dw3(indMateriau(nel))
	drefx_local=drefx(indMateriau(nel))
	c2_local=c2(indMateriau(nel))
	dw2_local=dw2(indMateriau(nel))

	if(((refthick_input.eq.1).and.(itype(nel).ne.5))) then
	   if (panneau(nel).itype_transv.eq.14)then   !Plat Boudin
			drefy_local=dref_b(indMateriau(nel))
              c3_local=c_pb(indMateriau(nel))
	        dw3_local=dw_b(indMateriau(nel))
	   elseif(panneau(nel).itype_transv.eq.7) then   ! Té Composite
			drefy_local=dref_c(indMateriau(nel))
              c3_local=c_tc(indMateriau(nel))
              dw3_local=dw_c(indMateriau(nel))
	   elseif(panneau(nel).itype_transv.eq.17)then   ! Té Laminé
			drefy_local=dref_l(indMateriau(nel))
              c3_local=c_tl(indMateriau(nel))
              dw3_local=dw_l(indMateriau(nel))
	   else 
	      write(* ,*)'type de profilé inexistant'
	   endif

	   if (panneau(nel).itype_longit.eq.14)then   !Plat Boudin
			drefx_local=dref_b(indMateriau(nel))
              c2_local=c_pb(indMateriau(nel))
	        dw2_local=dw_b(indMateriau(nel))
	   elseif(panneau(nel).itype_longit.eq.7) then   ! Té Composite
			drefx_local=dref_c(indMateriau(nel))
              c2_local=c_tc(indMateriau(nel))
	        dw2_local=dw_c(indMateriau(nel))
	   elseif(panneau(nel).itype_longit.eq.17)then   ! Té Laminé
			drefx_local=dref_l(indMateriau(nel))
              c2_local=c_tl(indMateriau(nel))
	        dw2_local=dw_l(indMateriau(nel))
	   else 
	      write(* ,*)'type de profilé inexistant'
	   endif

      endif
!     Calcul pour subroutine cost
!     supplément de poids (en n) (sans les traverses)
      
      if(itype(nel).ne.5) then                                               
!plaque                          
      spoid1 = temp*spec(indMateriau(nel)) * ( dcor +                    
     *(1.0+dw2_local)*(dcor3*panneau(nel).hxr+dcor3*panneau(nel).wxr)/
     *panneau(nel).epsr + (1.0+dw3_local)*(dcor2*panneau(nel).hya+dcor2*
     *panneau(nel).wya)/panneau(nel).epsa  )
      poid1 = temp*spec(indMateriau(nel)) * ( deltac +                  
     *(1.0+dw2_local)*(dxrc*panneau(nel).hxr+txrc*panneau(nel).wxr)/
     *panneau(nel).epsr + (1.0+dw3_local)*(dyac*panneau(nel).hya+tyac*
     *panneau(nel).wya)/panneau(nel).epsa)
      else                                                              
!epontille                   
      if(isect(nel).eq.3)then        
     
      poid1 = temp*spec(indMateriau(nel)) * ( deltac + (1.0+dw3_local)*
     *( dyac*hyb+2*tyac *panneau(nel).wya)/panneau(nel).epsa   )      
      
      spoid1 = temp*spec(indMateriau(nel)) * ((1.0+dw3_local)*
     *(dcor2*hyb+2*dcor2*panneau(nel).wya)/panneau(nel).epsa   ) 
      
      elseif(isect(nel).eq.1)then                                           
      poid1 = temp*spec(indMateriau(nel)) * ( deltac +                  
     *(1.0+dw3_local)*(pi*(panneau(nel).hya*panneau(nel).hya-
     *(panneau(nel).hya-2*epaisc)**2))/(4*panneau(nel).epsa))           
      
      spoid1 = temp*spec(indMateriau(nel)) * ((1.0+dw3_local)*
     *(pi*(panneau(nel).hya*panneau(nel).hya-
     * (panneau(nel).hya-2*dcor2 )**2))/(4*panneau(nel).epsa))          
     
      elseif(isect(nel).eq.2) then                                          
      poid1 = temp*spec(indMateriau(nel)) *  (deltac+(1.0+dw3_local)*
     *( panneau(nel).hya*panneau(nel).hya-
     *(panneau(nel).hya-2*epaisc)**2) /panneau(nel).epsa) 
     
      spoid1 = temp*spec(indMateriau(nel)) * 
     *((1.0+dw3_local)*((panneau(nel).hya*panneau(nel).hya-
     *(panneau(nel).hya-2*dcor2 )**2))/panneau(nel).epsa)     
      endif                                                            
      endif                                                       
      spoids=spoids+spoid1 ! supplément de poids dû à la corrosion
      poids =poids+ poid1 ! poids + poids additionnel dû à dw2_local et dw3_local
      


      if(itype(nel).ne.5) then                                               
      deltac=panneau(nel).delta+panneau(nel).corro(1)
      else  
      deltac =0
      endif

      if(itype(nel).ne.5)then												
      fmat1 = temp1 *c1(indMateriau(nel))* 
     *       (1.+(deltac-dref(indMateriau(nel)))*1000.*
     *       dc1(indMateriau(nel))) * deltac 

      else														
      fmat1=0.d00																										
      endif
      
      return
      end

ccccccccccccccccccc COUT DU MATERIAU pour raidisseurs cccccccccccccccccccccccccccccccc

	subroutine coutpanneau_mat_raidisseurs(fmat2,nel)
	
      !use param_multi
      use param_section
      !use param_opti_global
      
      implicit double precision(a-h,o-z)
      
      if(nel.eq.1) then
      fmat2=0.d00
      fmat22=0.d00
      endif
      
      
      
      dens = spec(indMateriau(nel))/9.81
      temp =  dabs(panneau(nel).phil) * panneau(nel).q * width *
     *pi/180.d00
      temp1=  temp * dens
	
	c2_local=c2(indMateriau(nel))
	dw2_local=dw2(indMateriau(nel))

	if(((refthick_input.eq.1).and.(itype(nel).ne.5))) then
	   if (panneau(nel).itype_transv.eq.14)then   !Plat Boudin
			drefy_local=dref_b(indMateriau(nel))
              c3_local=c_pb(indMateriau(nel))
	        dw3_local=dw_b(indMateriau(nel))
	   elseif(panneau(nel).itype_transv.eq.7) then   ! Té Composite
			drefy_local=dref_c(indMateriau(nel))
              c3_local=c_tc(indMateriau(nel))
              dw3_local=dw_c(indMateriau(nel))
	   elseif(panneau(nel).itype_transv.eq.17)then   ! Té Laminé
			drefy_local=dref_l(indMateriau(nel))
              c3_local=c_tl(indMateriau(nel))
              dw3_local=dw_l(indMateriau(nel))
	   else 
	   endif

	   if (panneau(nel).itype_longit.eq.14)then   !Plat Boudin
			drefx_local=dref_b(indMateriau(nel))
              c2_local=c_pb(indMateriau(nel))
	        dw2_local=dw_b(indMateriau(nel))
	   elseif(panneau(nel).itype_longit.eq.7) then   ! Té Composite
			drefx_local=dref_c(indMateriau(nel))
              c2_local=c_tc(indMateriau(nel))
	        dw2_local=dw_c(indMateriau(nel))
	   elseif(panneau(nel).itype_longit.eq.17)then   ! Té Laminé
			drefx_local=dref_l(indMateriau(nel))
              c2_local=c_tl(indMateriau(nel))
	        dw2_local=dw_l(indMateriau(nel))
	   else 
	   endif

      endif
      dxrc  =    panneau(nel).dxr +panneau(nel).corro(3)
      txrc  =    panneau(nel).txr +panneau(nel).corro(3)
!     coût des lisses

      if (panneau(nel).hxr.gt.0.002) then 
      
!Petits raidisseurs => pas de coût, peu importe l'écartement des raidisseurs
		fmat2 = temp1 *c2_local* (1.+dw2_local) * 
     *		  (1.+(dxrc-dref(indMateriau(nel)))*1000.*dc1(indMateriau(nel)))
     *  *(dxrc*panneau(nel).hxr+txrc*panneau(nel).wxr)/panneau(nel).epsr
	else
		fmat2 = 0.d00
	endif
	
	return
	end
	
ccccccccccccccccccc COUT DU MATERIAU pour cadres cccccccccccccccccccccccccccccccc

	subroutine coutpanneau_mat_cadres(fmat3,nel)
	
      !use param_multi
      use param_section
      !use param_opti_global
      
      implicit double precision(a-h,o-z)
      
      if(nel.eq.1) then
      fmat3=0.d00
      fmat33=0.d00
      endif
      
      dens = spec(indMateriau(nel))/9.81
      temp =  dabs(panneau(nel).phil) * panneau(nel).q * width *
     * pi/180.d00
      temp1=  temp * dens
      dyac   =  panneau(nel).dya  +panneau(nel).corro(2)
      tyac   =  panneau(nel).tya  +panneau(nel).corro(2)
      epaisc = panneau(nel).epais +panneau(nel).corro(2)


	c3_local=c3(indMateriau(nel))
	dw3_local=dw3(indMateriau(nel))

	if(((refthick_input.eq.1).and.(itype(nel).ne.5))) then
	   if (panneau(nel).itype_transv.eq.14)then   !Plat Boudin
			drefy_local=dref_b(indMateriau(nel))
              c3_local=c_pb(indMateriau(nel))
	        dw3_local=dw_b(indMateriau(nel))
	   elseif(panneau(nel).itype_transv.eq.7) then   ! Té Composite
			drefy_local=dref_c(indMateriau(nel))
              c3_local=c_tc(indMateriau(nel))
              dw3_local=dw_c(indMateriau(nel))
	   elseif(panneau(nel).itype_transv.eq.17)then   ! Té Laminé
			drefy_local=dref_l(indMateriau(nel))
              c3_local=c_tl(indMateriau(nel))
              dw3_local=dw_l(indMateriau(nel))
	   else 
	   endif

	   if (panneau(nel).itype_longit.eq.14)then   !Plat Boudin
			drefx_local=dref_b(indMateriau(nel))
              c2_local=c_pb(indMateriau(nel))
	        dw2_local=dw_b(indMateriau(nel))
	   elseif(panneau(nel).itype_longit.eq.7) then   ! Té Composite
			drefx_local=dref_c(indMateriau(nel))
              c2_local=c_tc(indMateriau(nel))
	        dw2_local=dw_c(indMateriau(nel))
	   elseif(panneau(nel).itype_longit.eq.17)then   ! Té Laminé
			drefx_local=dref_l(indMateriau(nel))
              c2_local=c_tl(indMateriau(nel))
	        dw2_local=dw_l(indMateriau(nel))
	   else 
	   endif
      endif
      
      
      if (panneau(nel).hya.gt.0.002) then 
!Petits cadres => pas de coût, peu importe l'écartement des raidisseurs

	if(isect(nel).eq.0) then											
		fmat3 = temp1 *c3_local* (1.+dw3_local) * 
     *	(1.+(dyac-dref(indMateriau(nel)))*1000.*dc1(indMateriau(nel)))* 
     *		(dyac*panneau(nel).hya+tyac*panneau(nel).wya)/panneau(nel).epsa  
	elseif(isect(nel).eq.1) then										
		fmat3=temp1 * c3_local *(1.+(epaisc  -dref(indMateriau(nel)))*1000.*
     *	dc1(indMateriau(nel))) * (1.0+dw3_local)* ((pi*(panneau(nel).hya*
     *		panneau(nel).hya-(panneau(nel).hya-2*epaisc)**2))
     *		/(4*panneau(nel).epsa)) 
	elseif(isect(nel).eq.2) then									
		fmat3=temp1 * c3_local *(1.+(epaisc  -dref(indMateriau(nel)))*1000.*
     * 	dc1(indMateriau(nel)))*(1.0+dw3_local)*((panneau(nel).hya*
     *  panneau(nel).hya-(panneau(nel).hya-2*epaisc)**2)
     *  /panneau(nel).epsa)
     	elseif(isect(nel).eq.3) then
		fmat3=temp1 * c3_local *(1.+(dyac    -dref(indMateriau(nel)))*1000.*
     * dc1(indMateriau(nel)))*(1.0+dw3_local)*((2*dyac*panneau(nel).hya+
     * 2*tyac*panneau(nel).wya)     /   panneau(nel).epsa )			
	endif															

      else
	fmat3 = 0.d00
      endif
      
      return 
      end
      
ccccccccccccccccccc COUT DU SOUDAGE cccccccccccccccccccccccccccccccc
	subroutine coutpanneau_sou(nel) 
	! = cout des consommables

	!use param_multi
      use param_section
      !use param_opti_global

      implicit double precision(a-h,o-z)


      fsou= 0.d00
	
	call coutpanneau_sou_raidisseurs(fsou1,nel)
	call coutpanneau_sou_cadres(fsou2,fsou3,nel)


      fsou = fsou1+fsou2+fsou3

      return
      end
ccccccccccccccccccc COUT DU SOUDAGE pour les raidisseurs cccccccccccccccccccccccccccccccc											

	subroutine coutpanneau_sou_raidisseurs(fsou1,nel)
	
    !  use param_multi
      use param_section
    !  use param_opti_global
      implicit double precision(a-h,o-z)
      
      if(nel.eq.1) then
      fsou1=0.d00
      fsou11=0.d00
      endif
      
      dens = spec(indMateriau(nel))/9.81
       temp =  dabs(panneau(nel).phil) * panneau(nel).q * width *
     * pi/180.d00
      dxr = panneau(nel).dxr
	if(((refthick_input.eq.1).and.(itype(nel).ne.5))) then
	   if (panneau(nel).itype_transv.eq.14)then   !Plat Boudin
			drefy_local=dref_b(indMateriau(nel))
              c3_local=c_pb(indMateriau(nel))
	        dw3_local=dw_b(indMateriau(nel))
	   elseif(panneau(nel).itype_transv.eq.7) then   ! Té Composite
			drefy_local=dref_c(indMateriau(nel))
              c3_local=c_tc(indMateriau(nel))
              dw3_local=dw_c(indMateriau(nel))
	   elseif(panneau(nel).itype_transv.eq.17)then   ! Té Laminé
			drefy_local=dref_l(indMateriau(nel))
              c3_local=c_tl(indMateriau(nel))
              dw3_local=dw_l(indMateriau(nel))
	   else 
	   endif

	   if (panneau(nel).itype_longit.eq.14)then   !Plat Boudin
			drefx_local=dref_b(indMateriau(nel))
              c2_local=c_pb(indMateriau(nel))
	        dw2_local=dw_b(indMateriau(nel))
	   elseif(panneau(nel).itype_longit.eq.7) then   ! Té Composite
			drefx_local=dref_c(indMateriau(nel))
              c2_local=c_tc(indMateriau(nel))
	        dw2_local=dw_c(indMateriau(nel))
	   elseif(panneau(nel).itype_longit.eq.17)then   ! Té Laminé
			drefx_local=dref_l(indMateriau(nel))
              c2_local=c_tl(indMateriau(nel))
	        dw2_local=dw_l(indMateriau(nel))
	   else 
	   endif
      endif
      
      
      
	if(itype(nel).ne.5.and.panneau(nel).hxr.gt.0.002) then                 
		fsou1 = temp * c8(indMateriau(nel)) * (1.+(dxr-drefx_local)*
     *		1000.*dc8(indMateriau(nel)))*(2.-ialr(indMateriau(nel)))/
     *		panneau(nel).epsr
	else                                                                   
		fsou1=0.000                                                           
	endif   
	  
	return
	end                                                                    
ccccccccccccccccccc COUT DU SOUDAGE pour les cadres cccccccccccccccccccccccccccccccc											


	
	subroutine coutpanneau_sou_cadres(fsou2,fsou3,nel)
	
   !   use param_multi
      use param_section
   !  use param_opti_global
      
      implicit double precision(a-h,o-z)
      
      if(nel.eq.1) then
      fsou2=0.d00
      fsou3=0.d00
      fsou22=0.d00
      fsou33=0.d00
      endif
      
      dens = spec(indMateriau(nel))/9.81
      temp =  dabs(panneau(nel).phil) * panneau(nel).q * width *
     * pi/180.d00
      dya = panneau(nel).dya
	if(((refthick_input.eq.1).and.(itype(nel).ne.5))) then
	   if (panneau(nel).itype_transv.eq.14)then   !Plat Boudin
			drefy_local=dref_b(indMateriau(nel))
              c3_local=c_pb(indMateriau(nel))
	        dw3_local=dw_b(indMateriau(nel))
	   elseif(panneau(nel).itype_transv.eq.7) then   ! Té Composite
			drefy_local=dref_c(indMateriau(nel))
              c3_local=c_tc(indMateriau(nel))
              dw3_local=dw_c(indMateriau(nel))
	   elseif(panneau(nel).itype_transv.eq.17)then   ! Té Laminé
			drefy_local=dref_l(indMateriau(nel))
              c3_local=c_tl(indMateriau(nel))
              dw3_local=dw_l(indMateriau(nel))
	   else 
	   endif

	   if (panneau(nel).itype_longit.eq.14)then   !Plat Boudin
			drefx_local=dref_b(indMateriau(nel))
              c2_local=c_pb(indMateriau(nel))
	        dw2_local=dw_b(indMateriau(nel))
	   elseif(panneau(nel).itype_longit.eq.7) then   ! Té Composite
			drefx_local=dref_c(indMateriau(nel))
              c2_local=c_tc(indMateriau(nel))
	        dw2_local=dw_c(indMateriau(nel))
	   elseif(panneau(nel).itype_longit.eq.17)then   ! Té Laminé
			drefx_local=dref_l(indMateriau(nel))
              c2_local=c_tl(indMateriau(nel))
	        dw2_local=dw_l(indMateriau(nel))
	   else 
	   endif
      endif
      

	if(itype(nel).ne.5.and.panneau(nel).hya.gt.0.002) then                 
		fsou2 = temp * c8(indMateriau(nel)) * (1.+(dya-drefy_local)*
     *		1000.*dc8(indMateriau(nel)))*(2.-ialt(indMateriau(nel)))/
     *		panneau(nel).epsa
	else                                                                   
		fsou2 = 0.0000                                                       
	endif                                                                  

	!coût de la matière de soudage des nappes
	fsou3=0.  ! à introduire
	!fsou3 = temp * c8(indMateriau(nel)) * ??



	return
	end

ccccccccccccccccccc COUT DE LA MAIN D'OEUVRE cccccccccccccccccccccccccccccccc
	subroutine coutpanneau_mdo(nel)

!	use param_multi
      use param_section
 !     use param_opti_global

      implicit double precision(a-h,o-z)
      
      if(nel.eq.1) then
	fmdo1  = 0.d00
	fmdo2  = 0.d00
	fmdo3  = 0.d00
	fmdo4  = 0.d00
	fmdo5  = 0.d00
	fmdo6  = 0.d00
	fmdo7  = 0.d00
	fmdo11 = 0.d00
	fmdo22 = 0.d00
	fmdo33 = 0.d00
	fmdo44 = 0.d00
	fmdo55 = 0.d00
	fmdo66 = 0.d00
	fmdo77 = 0.d00
      endif

	delta = panneau(nel).delta
	hya = panneau(nel).hya
	dya = panneau(nel).dya
	wya = panneau(nel).wya
	tya = panneau(nel).tya
	epsa = panneau(nel).epsa
	hxr = panneau(nel).hxr
	dxr = panneau(nel).dxr
	wxr = panneau(nel).wxr
	txr = panneau(nel).txr
	epsr = panneau(nel).epsr
	phil = dabs(panneau(nel).phil) !!!
	q = panneau(nel).q
	epais = panneau(nel).epais

	if(itype(nel).ne.5) then												
	  delta= delta + panneau(nel).corro(1)
	else																
	  delta =0.000														
	endif																
	dya  = dya + panneau(nel).corro(2)
	tya  = tya + panneau(nel).corro(2)
	epais = epais + panneau(nel).corro(2)							
	dxr  = dxr + panneau(nel).corro(3)
	txr  = txr + panneau(nel).corro(3)

      temp = phil * q * width * pi/180.
	dens = spec(indMateriau(nel))/9.81

	drefy_local=drefy(indMateriau(nel))
	c3_local=c3(indMateriau(nel))
	dw3_local=dw3(indMateriau(nel))
	drefx_local=drefx(indMateriau(nel))
	c2_local=c2(indMateriau(nel))
	dw2_local=dw2(indMateriau(nel))

	if(((refthick_input.eq.1).and.(itype(nel).ne.5))) then
	   if (panneau(nel).itype_transv.eq.14)then   !Plat Boudin
			drefy_local=dref_b(indMateriau(nel))
              c3_local=c_pb(indMateriau(nel))
	        dw3_local=dw_b(indMateriau(nel))
	   elseif(panneau(nel).itype_transv.eq.7) then   ! Té Composite
			drefy_local=dref_c(indMateriau(nel))
              c3_local=c_tc(indMateriau(nel))
              dw3_local=dw_c(indMateriau(nel))
	   elseif(panneau(nel).itype_transv.eq.17)then   ! Té Laminé
			drefy_local=dref_l(indMateriau(nel))
              c3_local=c_tl(indMateriau(nel))
              dw3_local=dw_l(indMateriau(nel))
	   else 
	      write(* ,*)'type de profilé inexistant'
	   endif

	   if (panneau(nel).itype_longit.eq.14)then   !Plat Boudin
			drefx_local=dref_b(indMateriau(nel))
              c2_local=c_pb(indMateriau(nel))
	        dw2_local=dw_b(indMateriau(nel))
	   elseif(panneau(nel).itype_longit.eq.7) then   ! Té Composite
			drefx_local=dref_c(indMateriau(nel))
              c2_local=c_tc(indMateriau(nel))
	        dw2_local=dw_c(indMateriau(nel))
	   elseif(panneau(nel).itype_longit.eq.17)then   ! Té Laminé
			drefx_local=dref_l(indMateriau(nel))
              c2_local=c_tl(indMateriau(nel))
	        dw2_local=dw_l(indMateriau(nel))
	   else 
	      write(* ,*)'type de profile inexistant'
	   endif

      endif



!	if(itype(nel).ne.5) then
	    !if (panneau(nel).hxr.


!		fmdo = temp * rend_global* panneau(nel).rend *
!     *	  eqp_global * 1000. * c1(indMateriau(nel)) *  
!     *	  ( ( p4(indMateriau(nel)) *(1.+(dxr-drefx_local)*
!     *	  1000.*dp4(indMateriau(nel)) ) + p9x(indMateriau(nel))*
!     *	  (1.+(dxr-drefx_local)*1000.*
!     *      dp9x(indMateriau(nel))) )/epsr + ( p5(indMateriau(nel)) *
!     *	  (1.+(dya-drefy_local)*1000.*
!     *	  dp5(indMateriau(nel))) + p9y(indMateriau(nel))*(1.+
!     *	  (dya-drefy_local)*1000.*dp9y(indMateriau(nel))))
!     *	  /epsa ! errata mars 2004  --> p9y au lieu de p9x
!     *      + (p6(indMateriau(nel))+ber(indMateriau(nel))*
!     *	  bet(indMateriau(nel))*p7(indMateriau(nel))) /(epsa*epsr)  
!     *      + p10(indMateriau(nel))*(1.+(delta-dref(indMateriau(nel)))*
!     *	  1000.*dp10(indMateriau(nel))) )
!	else							!epontilles																
!		fmdo=0.001												
!	endif												


	temp2=  temp * rend_global * panneau(nel).rend *
     *			labour_cost(indMateriau(nel))
	
	!coût de la main d'oeuvre : soudage des lisses sur bordé :       p4
	if(itype(nel).ne.5) then                                               
		fmdo1 = temp2 * p4(indMateriau(nel)) *(1.+(dxr-drefx_local)
     *			*1000.*dp4(indMateriau(nel)))  /panneau(nel).epsr
		!coût de la main d'oeuvre : constitution des lisses + extra : p9x
		fmdo2 = temp2 * p9x(indMateriau(nel))*(1.+(dxr-drefx_local)
     *			*1000.*dp9x(indMateriau(nel))) /panneau(nel).epsr
		!coût de la main d'oeuvre : soudage des cadres : p5
		fmdo3 = temp2 * p5(indMateriau(nel)) *(1.+(dya-drefy_local)
     *			*1000.*dp5(indMateriau(nel)))  /panneau(nel).epsa
		!coût de la main d'oeuvre : constitution cadres + extra :     p9y
		fmdo4 = temp2 * p9y(indMateriau(nel))*(1.+(dya-drefy_local)
     *			*1000.*dp9y(indMateriau(nel))) /panneau(nel).epsa
		!coût de la main d'oeuvre : slot : p6
		fmdo5 = temp2 * p6(indMateriau(nel))/(panneau(nel).epsa*
     *			panneau(nel).epsr)
		!coût de la main d'oeuvre : goussets et tap: p7
		fmdo6 = temp2 * ber(indMateriau(nel))*bet(indMateriau(nel))*
     *	   p7(indMateriau(nel)) /(panneau(nel).epsa*panneau(nel).epsr)
		!coût de la main d'oeuvre : assemblmage des bordés nappes):   p10
		fmdo7 = temp2 * p10(indMateriau(nel))*(1.+(delta-
     *			dref(indMateriau(nel)))*1000.*dp10(indMateriau(nel)))
		
		if (panneau(nel).hya.le.0.002) then
			fmdo3 = 0.d00
			fmdo4 = 0.d00
			fmdo5 = 0.d00
			fmdo6 = 0.d00
		endif
		if (panneau(nel).hxr.le.0.002) then
			fmdo1 = 0.d00
			fmdo2 = 0.d00
			fmdo5 = 0.d00
			fmdo6 = 0.d00
		endif

	else                                                                                                       

	  fmdo1 =0.0000                                                                                           
		fmdo2 =0.0000                                                                                           
		fmdo3 =0.0000                                                                                           
		fmdo4 =0.0000       
		fmdo5 =0.0000                                                                                          
		fmdo6 =0.0000                                                                                          
		fmdo7 =0.0000                                                                                           
	endif 

	fmdo = fmdo1+fmdo2+fmdo3+fmdo4+fmdo5+fmdo6+fmdo7


	return
	end
