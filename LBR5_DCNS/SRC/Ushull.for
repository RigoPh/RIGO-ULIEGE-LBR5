      subroutine ushulls(nel,sect,sigy1,e1,q,delta,phil,
     *                   dya,tya,wya,hya,epsa,ksa,
     *                   hxr,dxr,wxr,txr,epsr,entr,ksr)
      use sharedvar
      implicit real*8(a-h,o-z) 

c *******************************************************************
c     subroutine ushulls
c     ==================
c
c  .  a computer program to evaluate the ultimate strength of ships .
c  .                                                                .
c  .  based on the model of prof. jeom kee paik (november 1995)     .
c  .                                                                .
c  .  adapted to lbr-5 by : dr. ph. rigo (february 1997)            .
c  .  ---------------------                                         .
c  .    department of naval architecture, anast                     .
c  .    university of liege                                         .
c  .    6 quai banning, 4000 liege, belgium                         .
c  .    (tel) +32 4 366 9225  (fax) +32 4 366 9133                  .
c  .    (e-mail) ph.rigo@ulg.ac.be                                  .
c  .                                                                .
c     modified: 
c        - limit states for h <0 or h > d                       (26-2-97)
c          h=zone of linear variation in the vertical stress distribution
c        - introduce specific stiffener yield stress             (3-3-97) 
c        - sys(up) _ sys(down) in the calculation of mp and mu   (5-3-97) 
c        - introduction of validity limit on column slenderness (10-3-97) 
c        - sensibility analysis (to consider the ultimate strength
c          of the ship girder as a global restriction)          (11-3-97) 
c        - correction of the equation of the static moment (subr ultipan)  (12-3-97) 
c          zo= ... + hw*tw*(tp+hw/2.0) + ...   instead of  ... + hw*tw*(tp+hw)/2.0 + ...
c
c     last modification: 25-5-99
c                
c *******************************************************************
c  organigramme
c  ------------
c  - ushul
c      - ultpan  (calcul de sult et dsult/dxi)
c          - ult (calcul de sult)
c
c      - dsect   (calcul de sect et dsect/dxi)
c
c      - ulthull (calcul de mult sagging et hogging.)
c
c      - dulthull(dmult/dxi, sagg. et hogg.)
c
c *******************************************************************

c     ship sizes
c     ----------
c  .  depth = depth of the vessel          (m.)
c  .  db    = depth of double-bottom       (m.)

c     modeling on the ship components (deck, bottoms, side) based on the neto panels
c     -------------------------------------------------------------------------------

c   nd = number of panels included in the upper deck   component
c   nb1= number of panels included in the inner bottom component (si double fond)
c   nb = number of panels included in the outer bottom component
c   ns = number of panels included in the two   sides  component

c   id (i=1,nd)  = list of panels included in the upper deck   component
c   ib1(i=1,nb1) = list of panels included in the inner bottom component 
c   ib (i=1,nb)  = list of panels included in the outer bottom component
c   is (i=1,ns)  = list of panels included in the two   sides  component

c   number of a panel to use to evaluate the ultimate strength of a component
c   kd   = nø of a panel for calculation of the upper deck   strength 
c   kb1  = nø of a panel for calculation of the inner bottom strength
c   kb   = nø of a panel for calculation of the outer bottom strength
c   ksup = nø of a panel for calculation of the upper part of the side plate  strength
c   ksdw = nø of a panel for calculation of the lower part of the side plate  strength

c rem : - il faut modéliser la structure complète (et pas une demi)
c       - si seule la moitié de la structures a été modéliésée pour le calcul avec lbr,
c         alors il faut répéter 2 fois certains éléments de façon à générer la structure entière.
c       - il faut donner les panneaux correspondants aux 2 côtés (side plates)
c
      if(nel.eq.neto) then
      write(66,96)
96    format(//50(1h=)/
     *     '  u l t i m a t e   h u l l   s t r e n g t h '/50(1h=)/)

      write(66,*)'evaluation of the ultimate strength of ship girder'	
      write(66,*) '    using the paik algorithm	(caldwell method)'
      write(66,'(50(1h-)/)')	

      write(66,'(a,4(i2,1x))') 
     *  'panels included in the ship components (nd,nb1,nb,ns)=',
     *             nd,nb1,nb,ns
      write(66,'(a,5(20(i2,1x)/))') 'upper deck   : ',(id(i), i=1,nd)
      write(66,'(a,5(20(i2,1x)/))') 'inner bottom : ',(ib1(i),i=1,nb1)
      write(66,'(a,5(20(i2,1x)/))') 'outer bottom : ',(ib(i), i=1,nb)
      write(66,'(a,5(20(i2,1x)/))') 'two   sides  : ',(is9(i), i=1,ns)
      write(66,'(/a,a)')'panels used for the evaluation of the ',
     *            'ult. str. of long. compressed ship components:'
      write(66,*) 'upper deck               : ',kd
      write(66,*) 'inner bottom             : ',kb1
      write(66,*) 'outer bottom             : ',kb
      write(66,*) 'upper part of side plate : ',ksup
      write(66,*) 'lower part of side plate : ',ksdw
      write(66,*)
	endif


c*************************************************************************
c*************************************************************************

c     descrition of data for ushull
c     ==============================

c               Œ y et sigma y
c               i
c               *****************************
c               *                           *  b=entr
c               *                           *
c               *                           *
c               ***************************** --------> x et sigma x
c               <----------- a = epsa ------>

c     epsa,entr	= a,b        (m)
c     sigy1		= sig elastique (n/m2), (ex, re=240 n/mm2)
  

c     characteristics if the stiffened elements (epsa,entr,delta,hxr,dxr,wxr,txr,sigy1,sigy1)
c     ------------------------------------------
c  .  sl = epsa : length of stiffener between transverse frames (m.)
c  .  bp = entr : longitudinal stiffener spacing   (m.)
c  .  tp = delta: thickness of parent plate        (m.)
c  .  hw = hxr  : height of stiffener web          (m.)
c  .  tw = dxr  : thickness of stiffener web       (m.)
c  .  bf = wxr  : breadth of stiffener flange      (m.)
c  .  tf = txr  : thickness of stiffener flange    (m.)
c  .  syp= sigy1 : yield strength of parent plate   (n/m2)   
c  .  syw= sigy1 : yield strength of stiffener      (n/m2)   

c lbr5 : epsa,entr,delta,hxr,dxr,wxr,txr,sigy1,sigy1
c paik : sl,  bp,  tp,   hw, tw, bf, tf, syp, syw

c     properties of hull section components
c     --------------------------------------
c   . ab  = total sectional area of outer bottom (m2)
c   . ab1 = total sectional area of inner bottom (m2)
c   . ad  = total sectional area of deck         (m2)
c   . as  = half-sectional area of all sides     (m2)
c           (including long. blkd, inner sides, and vertical/inclined members) 

c ********************************************************************
c ********************************************************************

c     principal dimensions of the ship

c  .  depth = depth of the vessel          (m.)
c  .  db    = depth of double-bottom       (m.)

      if(nel.eq.neto) write(66,16) depth,db
      
16    format('depth of the vessel    (m.)        =',f12.6/
     *       'depth of double-bottom (m.)        =',f12.6)


c 1.0 ultimate strength of the stiffened plates under compression
c     ============================================================
c     input
c     ------
c  .  sl = epsa : length of stiffener between transverse frames (m.)
c  .  bp = entr : longitudinal stiffener spacing   (m.)
c  .  tp = delta: thickness of parent plate        (m.)
c  .  hw = hxr  : height of stiffener web          (m.)
c  .  tw = dxr  : thickness of stiffener web       (m.)
c  .  bf = wxr  : breadth of stiffener flange      (m.)
c  .  tf = txr  : thickness of stiffener flange    (m.)
c  .  syp= sigy1 : yield strength of parent plate   (n/m2)   
c  .  syw= sigy1 : yield strength of stiffener      (n/m2)   

c lbr5 : epsa,entr,delta,hxr,dxr,wxr,txr,sigy1,sigy1
c paik : sl,  bp,  tp,   hw, tw, bf, tf, syp, syw

c     output
c     ------
c  .  syd  = equivalent yield strength of deck         (n/m2)  
c  .  syb  = equivalent yield strength of outer bottom (n/m2)  
c  .  syb1 = equivalent yield strength of inner bottom (n/m2)  
c  .  sysu = equivalent yield strength of side shell-up(n/m2)  
c  .  sysl = equivalent yield strength of side shell-lw(n/m2)  
c  .  sud  = ultimate strength of deck                 (n/m2)  
c  .  sub  = ultimate strength of outer bottom         (n/m2)  
c  .  sub1 = ultimate strength of inner bottom         (n/m2)  
c  .  susu = ultimate strength of upper side shell     (n/m2)  
c  .  susl = ultimate strength of lower side shell     (n/m2)  




c     si kopt _0 : calcul des sensibilités
c     si kopt =0 : pas de calcul des sensibilités
      kopt=iopti*irestr

c...  1.1) stiffened plate in deck (element kd)
c     =============================
      if(nel.eq.kd) then

      call ultpan(nel,e1,sigy1,sysd,syeq,
     *            delta,epsa,hxr,dxr,wxr,txr,epsr,entr,
     *            dsud(1,nel),sigu,kopt)

      sud=sigu
      syd=syeq
      sult=sigu/syeq

      write(66,46)
      write(66,47) epsa,entr,delta,hxr,dxr,wxr,txr,sigy1,sysd,
     *             syeq,sud,sult
      endif

c...  1.2) stiffened plate in outer bottom (element kb)
c     =====================================
      if(nel.eq.kb) then

      call ultpan(nel,e1,sigy1,sysb,syeq,
     *            delta,epsa,hxr,dxr,wxr,txr,epsr,entr,
     *            dsub(1,nel),sigu,kopt)

      sub=sigu
      syb=syeq
      sult=sigu/syeq

      write(66,56)
      write(66,47) epsa,entr,delta,hxr,dxr,wxr,txr,sigy1,sysb,
     *             syeq,sub,sult
      endif

c...  1.3) stiffened plate in inner bottom (element kb1)
c     =====================================
      if(nel.eq.kb1) then

      call ultpan(nel,e1,sigy1,sysb1,syeq,
     *            delta,epsa,hxr,dxr,wxr,txr,epsr,entr,
     *            dsub1(1,nel),sigu,kopt)

      sub1=sigu
      syb1=syeq
      sult=sigu/syeq

      write(66,66)
      write(66,47) epsa,entr,delta,hxr,dxr,wxr,txr,sigy1,sysb1,
     *             syeq,sub1,sult
      endif

c...  1.4) stiffened plate in upper side shell (element ksup)
c     =========================================
      if(nel.eq.ksup) then

      call ultpan(nel,e1,sigy1,syssup,syeq,
     *            delta,epsa,hxr,dxr,wxr,txr,epsr,entr,
     *            dsusu(1,nel),sigu,kopt)

      susu=sigu
      sysu=syeq
      sult=sigu/syeq

      write(66,76)
      write(66,47) epsa,entr,delta,hxr,dxr,wxr,txr,sigy1,syssup,
     *             syeq,susu,sult
      endif

c...  1.5) stiffened plate in lower side shell (element ksdw)
c     =========================================
      if(nel.eq.ksdw) then

      call ultpan(nel,e1,sigy1,syssdw,syeq,
     *            delta,epsa,hxr,dxr,wxr,txr,epsr,entr,
     *            dsusl(1,nel),sigu,kopt)

      susl=sigu
      sysl=syeq
      sult=sigu/syeq

      write(66,86)
      write(66,47) epsa,entr,delta,hxr,dxr,wxr,txr,sigy1,syssdw,
     *             syeq,susl,sult
	endif


 46    format(///,5x,'d e c k   p a n e l s :')
 56    format(///,5x,'o u t e r   b o t t o m   p a n e l s :')
 66    format(///,5x,'i n n e r   b o t t o m   p a n e l s :')
 76    format(///,5x,'u p p e r   s i d e   s h e l l s :')
 86    format(///,5x,'l o w e r   s i d e   s h e l l s :')
 47    format(
     *  /5x,'length of stiffener            (m.)       =',f12.6,
     *  /5x,'longitudinal stiffener spacing (m.)       =',f12.6,
     *  /5x,'thickness of parent plate      (m.)       =',f12.6,
     *  /5x,'height of stiffener web        (m.)       =',f12.6,
     *  /5x,'thickness of stiffener web     (m.)       =',f12.6,
     *  /5x,'breadth of stiffener flange    (m.)       =',f12.6,
     *  /5x,'thickness of stiffener flange  (m.)       =',f12.6,
     *  /5x,'yield strength of parent plate (n/m2)     =',e14.7,
     *  /5x,'yield strength of stiffener    (n/m2)     =',e14.7,
     *  /5x,'equivalent yield strength      (n/m2)     =',e14.7,
     *  /5x,'ultimate compressive strength  (n/m2)     =',e14.7
     *  /5x,'relative ultimate strength (sult/sy)      =',f12.6)


c23456789012345678901234567890123456789012345678901234567890123456789012

c 2.0 sectional area  of hull components
c     ===================================
c   . ab  = total sectional area of outer bottom (m2)
c   . ab1 = total sectional area of inner bottom (m2)
c   . ad  = total sectional area of deck         (m2)
c   . as  = half-sectional area of all sides     (m2)
c           (including long. blkd, inner sides, and vertical/inclined members) 

c     mise à zéro des sensibilités des sections dad,dab,dab1 et das : d(ad)/dxi
      if(kopt.ge.1) then
	  if(nel.eq.1) call annuld(dad,270*4)
	endif

	do 10 i=1,nd
	if(id(i).eq.nel) then
	  ad=ad+sect
        if(kopt.ge.1) then
          call dsect(nel,delta,hxr,dxr,wxr,txr,epsr,phil,q,dad(1,nel))
          endif
        endif
  10  continue

c23456789012345678901234567890123456789012345678901234567890123456789012

        do 11 i=1,nb1
        if(ib1(i).eq.nel) then
          ab1=ab1+sect
        if(kopt.ge.1) then
          call dsect(nel,delta,hxr,dxr,wxr,txr,epsr,phil,q,dab1(1,nel))
          endif
	endif	
  11  continue

	do 12 i=1,nb
	if(ib(i).eq.nel) then
	  ab=ab+sect
        if(kopt.ge.1) then
          call dsect(nel,delta,hxr,dxr,wxr,txr,epsr,phil,q,dab(1,nel))
          endif
        endif
  12  continue

        do 13 i=1,ns
        if(is9(i).eq.nel) then
	  as=as+sect/2.
        if(kopt.ge.1) then
          call dsect(nel,delta,hxr,dxr,wxr,txr,epsr,phil,q,das(1,nel))
         endif
        endif
  13  continue

c ********************************************************************
c 3.0 calculation of ultimate hull strength in normal condition 
c     ==========================================================
c ********************************************************************
c       end of subr. ushull (si nel<neto)'

      if(nel.eq.neto) then
           write(*,*) 'avt ulthull'
           write(6970,*) 'avt ulthull'

        if(kopt.eq.0) then
c         sans optimisation (kopt=0)
c         ---------------------------

          call ulthull
c         ============

        else
c         avec optimisation (kopt>0)
c         ---------------------------

c         car as et das ne sont relatif qu'à la moitié des parois verticales	
	    do 14 j=1,neto
	    do 14 i=1,9
	      das(i,j)=das(i,j)/2.0
   14     continue

          call dulthull
	
        endif
      endif

      return
      end


c*******************************************************************************
c*******************************************************************************
c*******************************************************************************
c*******************************************************************************

      subroutine ultpan(nel,e1,syp,syw,syeq,
     *                  delta,epsa,hxr,dxr,wxr,txr,epsr,entr,
     *                  dspaik,spaik,kopt)
      use sharedvar
      implicit real*8(a-h,o-z)
      real*8 is
      dimension dspaik(9)

c*********************************************************************
c     subroutine ultpan 
c     ==================
c     cette subrourine calcule la résistance ultime d'une plaque raidie
c     londitudinalement et comprimée, également, longitudinalement.
c     (la pression latéral n'est pas prise en compte)
c
c     il s'agit de la formule empirique de j. paik.  qui est basée 
c     sur le principe de la poutre colonne.
c                                                                       
c   input:e1,syp,syw,delta,epsa 
c           et  hxr,dxr,wxr,txr,epsr,entr
c                                                                       
c  output:dcpaik,spaik,omt,
c	omt = section raidiseur + bordage (complet)
c     spaik = résistance ultime selon la formule de j. paik
c     dcpaik = dérivée de la résistance ultime spaik par rapport aux xi
c
c     créer : le 22-7-96  par  ph. rigo
c
c     modifications : 
c	- restrictions nø 15 + sensibilités (voir subr. contr & paik) 22-7-96  
c	- limite sur beta et lamda	      				               1-8-96  
c	- prise en compte des différences entre d(raid) et de epsr	   6-3-97 
c                               
c     dernière modif : 10-3-97 				     	
c                                                                       
c*************************************************************************
c*************************************************************************
c               ^ y et sigma y
c               i
c               *****************************
c               *                           *  b=epsr (= variable de conception)
c               *                           *  b=entr = d (entredistance réelle)
c               *                           *
c               ***************************** --------> x et sigma x
c               <----------- a = epsa ------>
c
c      epsa  	= a        (m)
c      entr,epsr	= b        (m)
c      avec entr : entredistance réelle entre raidisseurs = d(raid)
c           epsr : largeur effective associée à 1 raid (= l/nbre de raid)
c      syp		= yield stress of plating (n/m2), (ex, re=240 n/mm2)
c      syw		= yield stress of stiffener (n/m2), (ex, re=240 n/mm2)
c
c*************************************************************************
  
c 1.0 calcul de spaik de référence
c     ****************************

c 1.1 caractéristiques géométriques et calcul de á et lamda  
c     -----------------------------------------------------
      om  =wxr*txr + dxr*hxr                    ! section raid
      omt =om + entr*delta                      ! section raid + d(raid)
      smcr=delta/2.+hxr+txr/2.
      smdr=(delta+hxr)/2.
      hx  =wxr*txr*smcr + dxr*hxr*smdr          ! moment statique (z=0)
	
c     equivalent yield strength for the whole panel
      gam=om/(entr*delta)
      syeq=(syp+gam*syw)/(1.0+gam)

	gx=2.0*hx		
      yg=gx/(2.0*omt)										! yg=position du centre de gravité.
        is=(entr*delta**3 + dxr*hxr**3 + wxr*txr**3)/12.	! is=inertie par rapport
     *    + delta*entr*yg*yg  +  hxr*dxr*(smdr-yg)**2		! au centre de gravité.
     *                        +  wxr*txr*(smcr-yg)**2
      rot=dsqrt(syeq/e1)
      beta=entr * rot / delta
      slen=epsa * rot* dsqrt(omt/is) / pi

c 1.2 test sur beta et lamda=slen (slim= élancement maximum pour un beta donné)
c     -----------------------------------------------------------------------
      ilim=0
	
	if(beta.le.2) then
	   slim=3.0+(beta-1.)/2.
	else
	   slim=3.5+(beta-2.)
	endif
	
	if(slen.gt.slim) then
	  write(66,*)'attention: slen max =',slim,' < slen =',slen
	  write(66,*)'=========='
	  write(29,*)'attention: slen max =',slim,' < slen =',slen		!sept06			!bug
	  write(29,*)'=========='										!sept06			!bug
	  ilim=1
	  slen=slim
	endif

c 1.3 calcul de spaik de référence
c     -----------------------------
	slen2=slen*slen
	beta2=beta*beta
	fct=dsqrt(0.995 + 0.936*slen2 + 0.17*beta2 + 0.188*beta2*slen2
     *                - 0.067*slen2*slen2)
      spaik=syeq/fct

c 1.4 impressions
c     ------------
      if(kopt.eq.0) then
 	  write(66,*)
 	  write(66,*)'ds subr ultpan, nel=',nel
 	  write(66,*)'--------------------------'
        write(66,*)'beta    = ',beta
        write(66,*)'lamda   = ',slen
        write(66,*)'spaik   = ',spaik
	  
	  return	  
	endif

c     calcul des sensibilités si kopt_0
c
c 2.0 dérivée de sig(paik) : dspaik
c     *****************************

      if(ilim.eq.0) goto 3
	
c 2.1 cas où les valeurs limites de béta et lamda sont dépassées
c     ---------------------------------------------------------
c     dans ce cas le calcul se fait par différences finies.

      nbrxi=nvar(nel)

      do 5 k=1,nbrxi

      kk=nxit(k,nel)

        if(kk.eq.1) then                         !  xi=delta
	   ratio=1.05
         xxx=ratio *delta
         call ult(e1,syeq,xxx,epsa,hxr,dxr,wxr,txr,entr,spaik1)
           dspaik(kk)=(spaik1-spaik)/(xxx-delta)
        else if (kk.eq.5) then                  ! xi=epsa
	   ratio=0.5
         xxx=ratio *epsa
         call ult(e1,syeq,delta,xxx,hxr,dxr,wxr,txr,entr,spaik1)
           dspaik(kk)=(spaik1-spaik)/(xxx-epsa)
        else if (kk.eq.6) then                 ! xi=epsa
           ind=0
   16    ind=ind+1
	   ratio=5.0*ind
         xxx=ratio *hxr
         call ult(e1,syeq,delta,epsa,xxx,dxr,wxr,txr,entr,spaik1)
           dspaik(kk)=(spaik1-spaik)/(xxx-hxr)
	   if((dabs(dspaik(kk)).le.1000.).and.(ind.le.4)) goto 16
        else if (kk.eq.7) then                  ! xi=epsa
	   ratio=10.0
         xxx=ratio *dxr
         call ult(e1,syeq,delta,epsa,hxr,xxx,wxr,txr,entr,spaik1)
           dspaik(kk)=(spaik1-spaik)/(xxx-dxr)
        else if (kk.eq.8) then                  ! xi=epsa
	   ratio=10.0
         xxx=ratio *wxr
         call ult(e1,syeq,delta,epsa,hxr,dxr,xxx,txr,entr,spaik1)
           dspaik(kk)=(spaik1-spaik)/(xxx-wxr)
        else if (kk.eq.9) then                  ! xi=epsa
	   ratio=0.95
         xxx=ratio *entr
         call ult(e1,syeq,delta,epsa,hxr,dxr,wxr,txr,xxx,spaik1)
           dspaik(kk)=(spaik1-spaik)/(xxx-entr)
c        prise en compte de  d(d)/d(epsr) = (d/epsr)**2
           dspaik(kk)=dspaik(kk)*(entr/epsr)**2
        else
           dspaik(kk)=0.
        endif
  5   continue
	
      goto 4

c 2.2 cas normal: les valeurs de á et lamda sont dans les limites
c     ---------------------------------------------------------
c     dans ce cas le calcul se fait par calcul direct (analytique)
   3  continue

     	fac1=(0.936+0.188*beta2-2.*0.067*slen2)*slen
      fac2=(0.170+0.188*slen2)*beta
	temp= delta*entr*yg - hxr*dxr*(smdr-yg) - wxr*txr*(smcr-yg)
      nbrxi=nvar(nel)

      do 1 k=1,nbrxi

        kk=nxit(k,nel)

          if(kk.eq.1) then                      !  xi=delta
            depsa=0.                            !  d(epsa)/dxi  * slen/epsa
            dbeta=-beta/delta                   !  dbeta=d(beta)/dxi
            dgx=om                              !          dgx  =d(gx) /dxi
            domt=entr                           !  domt =d(omt)/dxi
            dis1=entr*(0.25*delta**2+yg**2) + hxr*dxr*(smdr-yg) ! dis =d(is) /dxi
     *                                   + wxr*txr*(smcr-yg)
          else if (kk.eq.5) then                ! xi=epsa
            depsa=slen/epsa                     ! d(epsa)/dxi  * slen/epsa
	    dbeta=0.
	    dgx=0.	
	    domt=0.									
	    dis1=0.
          else if (kk.eq.6) then                !  xi=hxr
            depsa=0.
	    dbeta=0.
	    dgx=om*2.+dxr*delta	
	    domt=dxr									
	    dis1=0.25*dxr*hxr**2 + dxr*(smdr-yg)*(smdr-yg+hxr)
     *                        + 2.*wxr*txr*(smcr-yg)
          else if (kk.eq.7) then                !  xi=dxr=tweb
	    depsa=0.
	    dbeta=0.
	    dgx=2.*hxr*smdr	
	    domt=hxr									
	    dis1=(hxr**3)/12. +  hxr*(smdr-yg)**2
          else if (kk.eq.8) then                ! xi=wxr =lsem.
	    depsa=0.
	    dbeta=0.
	    dgx=2.*txr*smcr
	    domt=txr									
	    dis1=(txr**3)/12. +  txr*(smcr-yg)**2
          else if (kk.eq.9) then                ! xi=epsr
            depsa=0.
	    dbeta=beta/entr
	    dgx=0.	
	    domt=delta									
	    dis1=(delta**3)/12. +  delta* yg**2
	  else
	    depsa=0.
	    dbeta=0.
	    dgx=0.	
	    domt=0.									
	    dis1=0.
	  endif
        
	  dyg=(omt*dgx-gx*domt)/(2.*omt*omt)
          dis1=dis1 +2.*dyg* temp                                 ! dis =d(is) /dxi
          dslen=slen * (is*domt-omt*dis1) / (2.*omt*is)  + depsa
          dspaik(kk)=-syeq*(fac1*dslen+fac2*dbeta)/(fct**3)     ! d(spaik)/dxi
	  
c       prise en compte de  d(d)/d(epsr) = (d/epsr)**2 
        if (kk.eq.9) then
	     dspaik(kk)=dspaik(kk)*(entr/epsr)**2
	  endif
 
   1  continue
 
c 2.3 impressions
c     ------------
   4  continue

      return 
      end


c **********************************************************************
c **********************************************************************
c **********************************************************************
	
      subroutine ult(e1,sigy1,delta,epsa,hxr,dxr,wxr,txr,entr,spaik)
      implicit real*8(a-h,o-z)
      real*8 is

c***********************************************************************
c     subroutine ult
c     ===================
c     cette subrourine calcule la résistance ultime d'une plaque raidie
c     londitudinalement et comprimée, également, longitudinalement.
c     (la pression latérael n'est pas prise en compte)
c
c     il s'agit de la formule empirique de j. paik.  qui est basée 
c     sur le principe de la poutre colonne.
c                                                                      
c     entr = d(raid) remplace epsr qui était une approximation (bordé collaborant)
c
c*******************************************************************************

      pi=2.d00*asin(1.d00)

c   raidisseurs                                                   
c  -------------                                !
      omt =wxr*txr + dxr*hxr + entr*delta       ! section raid + d(raid)
      smcr=delta/2.+hxr+txr/2.
      smdr=(delta+hxr)/2.
      hx  =wxr*txr*smcr + dxr*hxr*smdr          ! moment statique (z=0)

      yg=hx/omt                                          ! yg=position du centre de gravité.
      is=(entr*delta**3 + dxr*hxr**3 + wxr*txr**3)/12.   ! is=inertie par rapport
     *    + delta*entr*yg*yg  +  hxr*dxr*(smdr-yg)**2    !    au centre de gravité.
     *                        +  wxr*txr*(smcr-yg)**2
      rot =dsqrt(sigy1/e1)
      beta=entr * rot / delta
      slen=epsa * rot* dsqrt(omt/is) / pi

c
c   test sur beta et lamda=slen (slim= élancement maximum pour un beta donné)
c   ----------------------------------------------------------------------
      ilim=0
	if(beta.le.2) then
	   slim=3.0+(beta-1.)/2.
	else
	   slim=3.5+(beta-2.)
	endif
	if(slen.gt.slim) then
	  ilim=1
	  slen=slim
	endif

c   calcul de spaik
c   ----------------
	beta2=beta*beta

      if(hxr.le.0.00001) then
        fct=dsqrt(0.995+0.17*beta2)
      else
	  slen2=slen*slen
	  fct=dsqrt(0.995 + 0.936*slen2 + 0.17*beta2 + 0.188*beta2*slen2
     *                - 0.067*slen2*slen2)
      endif
	
      spaik=sigy1/fct
      return 
      end


c **********************************************************************
c **********************************************************************
c **********************************************************************

      subroutine dsect(nel,delta,hxr,dxr,wxr,txr,epsr,phil,q,da)
      use sharedvar
      implicit real*8(a-h,o-z)
      dimension da(9)

c***********************************************************************
c     subroutine dsect
c     ==================
c     cette subrourine calcule les dérivées des sections par rapport
c     aux variables de conception (xi).
c     la section de chaque composant (deck, fond, side) est la somme des
c     sections de plusieurs panneaux.
c     la section tarnsversale d'un panneau est composé de
c       - section du bordage (phil*q*delta)
c       - section des raid.  (hxr*dxr+wxr*txr)* phil*q/epsr
c       - section des traverses
c
c     les variables de conception qui interviennent dans ces sections sont:
c      delta et   hxr,dxr,wxr,txr,epsr
c                                                                       
c   inputs : delta,hxr,dxr,wxr,txr,epsr  + q,phil
c                                                                       
c   outputs:
c	da = da +d(a)/dxi
c     avec a  la section d'un panneau inclus dans le composant (deck,fond, ..)
c          xi les variables de conception
c
c     créer : le 11-3-97  par  ph. rigo
c
c     modifications : 
c                               
c     dernière modif : 11-3-97 				     	
c                                                                       
c*************************************************************************

      qphil=q*phil*pi/180.

c 1.0 calcul des sensibilités
c     ***********************
      nbrxi=nvar(nel)

      do 1 k=1,nbrxi

        kk=nxit(k,nel)	


          if(kk.eq.1) then                      ! xi=delta
	    da(kk)=da(kk) + qphil

          else if (kk.eq.6) then                 !  xi=hxr
            da(kk)=da(kk) + qphil*dxr/epsr

          else if (kk.eq.7) then                 !  xi=dxr=tweb
            da(kk)=da(kk) + qphil*hxr/epsr

          else if (kk.eq.8) then                 !  xi=wxr =lsem.
            da(kk)=da(kk) + qphil*txr/epsr

          else if (kk.eq.9) then                 !  xi=epsr
	    da(kk)=da(kk) - qphil*(hxr*dxr+wxr*txr)/(epsr*epsr)
	  endif
        
   1  continue
 
      return 
      end


c ********************************************************************

      subroutine ulthull
      use sharedvar
      implicit real*8(a-h,o-z)

c ********************************************************************
c     prediction of ultimate hull strength
c     using the analytical formula derived by paik & mansour
c ********************************************************************

      write(66,26) syd,syb,syb1,sysu,sysl,sud,sub,sub1,susu,susl
  26  format(/
     * 'equivalent yield stress of deck         (n/m2) ',e14.7/
     * 'equivalent yield stress of outer bottom (n/m2) ',e14.7/ 
     * 'equivalent yield stress of inner bottom (n/m2) ',e14.7/
     * 'equivalent yield stress of side shell-up(n/m2) ',e14.7/
     * 'equivalent yield stress of side shell-lw(n/m2) ',e14.7//
     * 'ultimate strength of deck               (n/m2) ',e14.7/ 
     * 'ultimate strength of outer bottom       (n/m2) ',e14.7/ 
     * 'ultimate strength of inner bottom       (n/m2) ',e14.7/  
     * 'ultimate strength of upper side shell   (n/m2) ',e14.7/ 
     * 'ultimate strength of lower side shell   (n/m2) ',e14.7/) 

      if(syb1.ne.0) then
	  temp=sub1/syb1
	else
	  temp=0.
	endif
	
      write(66,27) sud/syd,sub/syb,temp,susu/sysu,susl/sysl
  27  format('relative ultimate strength (su/sy)'/
     * '     - deck             ',f6.4/ 
     * '     - outer bottom     ',f6.4/ 
     * '     - inner bottom     ',f6.4/  
     * '     - upper side shell ',f6.4/ 
     * '     - lower side shell ',f6.4/) 

      write(66,36) ab,ab1,ad,as
  36  format('p r o p e r t i e s   o f  ',
     *            'h u l l   s e c t i o n :'/
     *  'total sectional area of outer bottom (m2) =',e14.7/
     *  'total sectional area of inner bottom (m2) =',e14.7/
     *  'total sectional area of deck         (m2) =',e14.7/
     *  'half-sectional  area of all sides    (m2) =',e14.7//
     *  'results of analysis.'/23(1h*))
c--------------------------------------------------------------------

      d=depth
	
c 1.0 fully plastic bending moment (eqs 2.a & 3.a for "g" and "mp")
c     ------------------------------
      g=d/2.0/as/(sysu+sysl)
      g2=ad*syd+2.0*as*sysu-ab*syb
c     si g>db : g =(ad*syd+2.0*as*sysu-ab*syb-ab1*syb1 ) *d/2.0/as/(sysu+sysl)
        g1=(g2-ab1*syb1) * g
c     si g<db : g =(ad*syd+2.0*as*sysu-ab*syb+ab1*syb1 ) *d/2.0/as/(sysu+sysl)
        g2=(g2+ab1*syb1) * g

      if(g1.le.db.and.g2.ge.db) then
c       g=db (il n'y a pas d'équilibre pour sb1= ñ syb1 )
	  g=db
        write(66,*) ' cas 1: g=db:  g=',g
        fpbm=ad*(d-g)*syd+ab*g*syb
     *      +as/d*(sysu*(d-g)**2+sysl*g*g)
      elseif(g1.ge.db) then
c       g=g1 ò db
        g=g1
        write(66,*) ' cas 2: g>db  :  g=',g
        if(g.ge.d) then
         write(66,*) 
	   g=d
         write(66,*) '  g>d alors g=d=creux (sect. deck trop important)'
	  endif
        fpbm=ad*(d-g)*syd+ab*g*syb+ab1*(g-db)*syb1
     *      +as/d*(sysu*(d-g)**2+sysl*g*g)
      else
c       g=g2 < db
        g=g2
        write(66,*) ' cas 3: g<db:  g=',g
        if(g.le.0.001) then
	   g=0.
         write(66,*) '  g<0 alors g=0 (sect. fond trop important)'
	  endif
        fpbm=ad*(d-g)*syd+ab*g*syb+ab1*(db-g)*syb1
     *      +as/d*(sysu*(d-g)**2+sysl*g*g)
      endif

      write(66,706) fpbm,g
706   format(/5x,'fully plastic bending moment  (n.m)  =',e15.7/
     *        5x,'  (g = ',f12.3,' m)')

      fpshear= as*(sysu+sysl) / 1.732

      write(66,707) fpshear
707   format(/5x,'fully plastic shear force     (n)    =',e15.7)

c 2.0 neutral axis of hull section
c     ----------------------------
      h=(d*(ad+as)+ab1*db)/(ad+ab+ab1+2.0*as)
      
      write(66,710) h
710   format(/5x,'neutral axis of hull(from bott.)(m) =',f12.3)

c 3.0 elastic section modulus at deck
c     -------------------------------
c     zz=ad*(d-h)**2+ab*h*h+ab1*(h-db)**2+as*d*(2.0*d-3.0*h)/3.0 (as defined by paik)

      zz=ad*(d-h)**2 + ab*h*h + ab1*(h-db)**2
     *               +2*as*(d*d/12.+(d/2.-h)**2)
      zd=zz/(d-h)

c 3.1 elastic section modulus at bottom
c     ---------------------------------
      zb=zz/h

c 3.2 first yield strength
c     ---------------------
      dyield=zd*syd
      byield=zb*syb

      write(66,806) dyield,byield
806   format(/5x,'first yield bending moment (n.m):',
     *        /7x,'- at deck     =',e15.7,
     *        /7x,'- at bottom   =',e15.7)

      write(66,711) zd,zb
711   format(/5x,'section modulus, z=i/v (m3):',
     *       /7x,'- at deck      =',e15.7,
     *       /7x,'- at bottom    =',e15.7)

c 4.0 ultimate hull strength
c     =======================
c     if there is no double deck (ab1=0)
      if(ab1.lt.0.000001) ab1=0.000001
      if(db.lt.0.0001)  db=0.0001
      if((ab1/ab).lt.0.001) then
         db=0.000001*d
      endif

c 4.1 for sagging condition ("h" from eq.12.b ; "g" from eq.14; "usag" from eq.17.b)
c     ======================
      sus=susu
      sys=sysl
	ss=sus+sys
      
      bb=(ad*sud+2.0*as*sus-ab*syb-ab1*sys)/as/ss
      cc=ab1*db/as
      h=( bb*d+dsqrt(bb**2*d**2+4.0*cc*d) )/2.

      write(66,*) 
      write(66,*)'     h & hb = high of the linear zone',
     *           ' (of the vertical stress distribution)' 
      write(66,*)'     - sagging:'
 
      hb=0.
      if(h.le.db) then     
c     cas limite nø2 : h < db ou h < 0 : deck too strong or bottom too weak 
c     ---------------
c       we have to recompute h for the correct stress distribution in the inner bottom
c          sb' = stress in the inner bottom = sub1 (ultimate strength of the panel)
        write(29,*)'subr ushull'											!sept06		!bug
	  write(29,*)'cas limite nø2 : h < db ou h < 0						!sept06
     * deck too strong or bottom too weak'								!sept06

	  h=d*(ad*sud+2.0*as*sus+ab1*sub1-ab*syb)/as/ss
        write(66,*)'     as  h   = ',h,' <? <  db  = ',db
        if(h.le.0) then     
          h=d*1.00e-10
          g=0.
        else
	  g=h*sys/ss
        endif
        write(66,*)'         h   = ',h
        write(66,*)'         g   = ',g
        usag=-ad*(d-g)*sud - as/d*(d-h)*(d+h-2.0*g)*sus
     *       -ab*g*syb - ab1*(db-g)*sub1
     *       -as*h/3.0/d*((2.0*h-3.0*g)*sus-(h-3.0*g)*sys)
     
      elseif (h.gt.d) then

c     cas limite nø4 : h>d : tensile bottom too strong or compressed deck too weak 
c     ---------------
c        we assume h = 0 and
c                 a tensile yield zone is introduced = d-hb
         write(66,*)'  cas limite nø4 : (h=',h,' > d)'
         write(66,*)'     as h>d, a tensile zone (d-hb) is required'
         
	   write(29,*)'subr ushull'											!sept06			!bug
	   write(29,*)'cas limite nø4 : h>d									!sept06
     * tensile bottom too strong or compressed deck too weak'				!sept06			!bug

	   h=d
c        first, we suppose  d-hb > db (this means large tensile zone)
         hb=(-ad*sud+ab1*syb1+ab*syb+2.0*as*sys)*d/(as*ss)
	   if(hb.le.0.0001) hb=0.0001

         if((d-hb).ge.db) then
c          h>d and  d-hb > db (large tensile yield zone) : cas nø 4.a
c          ----------------------------------------------
           write(66,*)'   a large tensile zone (d-hb) is valid'
           write(66,*)'         hb   = ',hb
           write(66,*)'         d-hb = ',d-hb,' > db  = ',db
           g=hb*sus/ss
           usag=-ad*g*sud - as/d*(d-hb)*(d+hb-2.0*g)*sys
     *       -ab*(d-g)*syb - ab1*(d-g-db)*syb1
     *       -as*hb/3.0/d*((2.0*hb-3.0*g)*sys-(hb-3.0*g)*sus)
	   else
c          h>d and  d-hb < db (small tensile yield zone) : cas nø 4.b
c          ----------------------------------------------
           write(66,*)'   a small tensile zone (d-hb) is valid'
           bb=-d*(-ad*sud-ab1*sus+2.0*as*sys+ab*syb)/as/ss
           cc=-ab1*d*(d-db)/as
           hb=( -bb + dsqrt(bb*bb-4.0*cc) )/2.
           write(66,*)'         hb   = ',hb
           write(66,*)'         d-hb = ',d-hb,' < db  = ',db
           g=hb*sus/ss
           usag=-ad*g*sud - as/d*(d-hb)*(d+hb-2.0*g)*sys
     *         - ab*(d-g)*syb - ab1*(d-g-db)*(-sus+(sys+sus)*(d-db)/hb)
     *         - as*hb/3.0/d*((2.0*hb-3.0*g)*sys-(hb-3.0*g)*sus)
	   endif

      else 
c       normal case : db <h < d
c       ---------------
        g=h*sys/ss
        usag=-ad*(d-g)*sud - as/d*(d-h)*(d+h-2.0*g)*sus
     *       -ab*g*syb + ab1*(g-db)*(-sys+(db/h)*ss)
     *       -as*h/3.0/d*((2.0*h-3.0*g)*sus-(h-3.0*g)*sys)
      endif

      write(66,*)'         h   = ',h
      write(66,*)'         g   = ',g
      write(66,*)'         hb  = ',hb


c 4.2 for hogging condition ("h" from eq.15.b ; "g" from eq.16; "uhog" from eq.18.b)
c     ======================
      sus=susl
      sys=sysu
	ss=sus+sys
      
      h=ab*sub+ab1*sub1+2.0*as*sus-ad*syd
      h=h*d/(as*ss)

      write(66,*)'     - hogging:'

      hb=0.
      if(h.le.(d*1.00e-10)) then
c     cas limite nø1 : h<0 : deck too strong or too weak bottoms 
c     ---------------
        write(66,*)'      h=',h,' < 0 , then h=0.0'
        
	  write(29,*)'cas limite nø1 : h<0 : deck too strong				!sept06
     *  or too weak bottoms'												!sept06
	  h=0.
        g=0.
        uhog=ab*d*sub + ab1*(d-db)*sub1 + as*d*sus
	  
      elseif(h.gt.(d-db)) then
c     cas limite nø3 : h>d-db : bottom very strong or deck too weak
c     ---------------
c       we have to recompute h for the correct stress distribution in the inner bottom
c          sb' = stress in the inner bottom << sub1 (ultimate strength of the panel)
        
	  write(29,*)'														!sept06
     * cas limite nø3 : h>d-db : bottom very strong or deck too weak'		!sept06		!bug

	  write(66,*)'  cas limite nø3 : h=',h
        write(66,*)'    as h>d-db : h is recomputed'
        bb=d*(-ab*sub-2.0*as*sus+ad*syd+ab1*sys)/as/ss
        cc=-ab1*(d-db)*d/as
        h =( -bb + dsqrt(bb**2-4.0*cc) )/2.

        if(h.gt.d) then
c       cas limite nø3.a : h>d: tensile yield zone must be introduce = hb 
c       ------------------
	     h=d
           bb=-d*(ad*syd+2.0*as*sys-ab*sub-ab1*sus)/as/ss
           cc=-ab1*db*d/as
           hb=( -bb + dsqrt(bb**2-4.0*cc) )/2.
	     if(hb.le.db) hb=db+0.0001
           write(66,*)'  cas limite nø3 : h=',h
           write(66,*)'    as h>d : yield zone hb is introduced'
           g=hb*sus/ss
           uhog=ad*syd*(d-g) + ab*g*sub 
     *        - ab1*(g-db)*(-sus+db*(sys+sus)/hb)
     *        + as/d*(d-hb)*(d+hb-2.0*g)*sys
     *        + as*hb/3.0/d*((2.0*hb-3.0*g)*sys-(hb-3.0*g)*sus)
	  else
c       cas limite nø3.b : d>h>d-db: h can be used
c       -----------------
           g=h*sys/ss
           uhog=ad*syd*g + ab*(d-g)*sub 
     *         - ab1*(sys-(d-db)*(sys+sus)/h)*(d-db-g)
     *         + as/d*(d-h)*(d+h-2.0*g)*sus
     *         + as*h/3.0/d*((2.0*h-3.0*g)*sus-(h-3.0*g)*sys)
	  endif
      else
c     normal case: 0 < h < d-db 
c     ---------------
         write(29,*)'subr ushull'						!sept06							!bug
	   write(29,*)'normal case: 0 < h < d-db'		!sept06							!bug
	  
	  g=h*sys/ss
        uhog=ad*syd*g + ab*(d-g)*sub + ab1*sub1*(d-g-db)
     *      +as/d*(d-h)*(d+h-2.0*g)*sus
     *      +as*h/3.0/d*((2.0*h-3.0*g)*sus-(h-3.0*g)*sys)
      endif
      
      write(66,*)'         h   = ',h
      write(66,*)'         g   = ',g
      write(66,*)'         hb  = ',hb

c 4.3 outputs
c     --------

      usagr=usag/fpbm
      uhogr=uhog/fpbm
      
      write(66,816) usag,usagr,uhog,uhogr
816   format(/5x,'ultimate bending moment (n.m):'/5x,30(1h-)/
     *        7x,'- for sagging:'/
     *        7x,'     mult.     =',e15.7/
     *        7x,'     mult./mp  =',f10.5/
     *        7x,'- for hogging:'/
     *        7x,'     mult.     =',e15.7/
     *        7x,'     mult./mp  =',f10.5)


      return
      end


c ********************************************************************

      subroutine dulthull
      use sharedvar
      implicit real*8(a-h,o-z)
      character *30 vnom2   !dad
      dimension dhb(9,neto)

c ********************************************************************
c     calcul des sensibilités liés à la restriction d'ensemble
c     imposée sur la resistance ultime de la poutre navire
c         mult / m(max) < 1  en hogging et en sagging
c
c    créer : 13-mars 1997 dr. ph. rigo
c
c    modified:
c ********************************************************************
		
      write(66,26) syd,syb,syb1,sysu,sysl,sud,sub,sub1,susu,susl
  26  format(/
     * 'equivalent yield stress of deck         (n/m2) ',e14.7/
     * 'equivalent yield stress of outer bottom (n/m2) ',e14.7/ 
     * 'equivalent yield stress of inner bottom (n/m2) ',e14.7/
     * 'equivalent yield stress of side shell-up(n/m2) ',e14.7/
     * 'equivalent yield stress of side shell-lw(n/m2) ',e14.7//
     * 'ultimate strength of deck               (n/m2) ',e14.7/ 
     * 'ultimate strength of outer bottom       (n/m2) ',e14.7/ 
     * 'ultimate strength of inner bottom       (n/m2) ',e14.7/  
     * 'ultimate strength of upper side shell   (n/m2) ',e14.7/ 
     * 'ultimate strength of lower side shell   (n/m2) ',e14.7/) 

      if(syb1.ne.0) then
	  temp=sub1/syb1
	else
	  temp=0.
	endif
	
      write(66,27) sud/syd,sub/syb,temp,susu/sysu,susl/sysl
  27  format('relative ultimate strength (su/sy)'/
     * '     - deck             ',f6.4/ 
     * '     - outer bottom     ',f6.4/ 
     * '     - inner bottom     ',f6.4/  
     * '     - upper side shell ',f6.4/ 
     * '     - lower side shell ',f6.4/) 

      write(66,36) ab,ab1,ad,as
  36  format('p r o p e r t i e s   o f  ',
     *            'h u l l   s e c t i o n :'/
     *  'total sectional area of outer bottom (m2) =',e14.7/
     *  'total sectional area of inner bottom (m2) =',e14.7/
     *  'total sectional area of deck         (m2) =',e14.7/
     *  'half-sectional  area of all sides    (m2) =',e14.7//
     *  'results of analysis.'/23(1h*))
c--------------------------------------------------------------------

      d=depth
	
c 1.0 fully plastic bending moment (eqs 2.a & 3.a for "g" and "mp")
c     ------------------------------
      g=d/2.0/as/(sysu+sysl)
      g2=ad*syd+2.0*as*sysu-ab*syb
c     si g>db : g =(ad*syd+2.0*as*sysu-ab*syb-ab1*syb1 ) *d/2.0/as/(sysu+sysl)
        g1=(g2-ab1*syb1) * g
c     si g<db : g =(ad*syd+2.0*as*sysu-ab*syb+ab1*syb1 ) *d/2.0/as/(sysu+sysl)
        g2=(g2+ab1*syb1) * g

      if(g1.le.db.and.g2.ge.db) then
c       g=db (il n'y a pas d'équilibre pour sb1= ñ syb1 )
	  g=db
        write(66,*) ' cas 1: g=db:  g=',g
        fpbm=ad*(d-g)*syd+ab*g*syb
     *      +as/d*(sysu*(d-g)**2+sysl*g*g)
      elseif(g1.ge.db) then
c       g=g1 ò db
        g=g1
        write(66,*) ' cas 2: g>db  :  g=',g
        if(g.ge.d) then
         write(66,*) 
	   g=d
         write(66,*) '  g>d alors g=d=creux (sect. deck trop important)'
	   write(29,*) '  g>d alors g=d=creux (sect. deck trop important)'		!sept06			!bug
	  endif
        fpbm=ad*(d-g)*syd+ab*g*syb+ab1*(g-db)*syb1
     *      +as/d*(sysu*(d-g)**2+sysl*g*g)
      else
c       g=g2 < db
        g=g2
        write(66,*) ' cas 3: g<db:  g=',g
        if(g.le.0.001) then
	   g=0.
         write(66,*) '  g<0 alors g=0 (sect. fond trop important)'
	   write(29,*) '  g<0 alors g=0 (sect. fond trop important)'			!sept06			!bug
	  endif
        fpbm=ad*(d-g)*syd+ab*g*syb+ab1*(db-g)*syb1
     *      +as/d*(sysu*(d-g)**2+sysl*g*g)
      endif

      write(66,706) fpbm,g
706   format(/5x,'fully plastic bending moment  (n.m)  =',e15.7/
     *        5x,'  (g = ',f6.3,' )')

      fpshear= as*(sysu+sysl) / 1.732

      write(66,707) fpshear
707   format(/5x,'fully plastic shear force     (n)    =',e15.7)

c 2.0 neutral axis of hull section
c     ----------------------------
      h=(d*(ad+as)+ab1*db)/(ad+ab+ab1+2.0*as)
      
      write(66,710) h
710   format(/5x,'neutral axis of hull(from bott.)(m) =',f12.3)

c 3.0 elastic section modulus at deck
c     -------------------------------
c     zz=ad*(d-h)**2+ab*h*h+ab1*(h-db)**2+as*d*(2.0*d-3.0*h)/3.0 (as defined by paik)

      zz=ad*(d-h)**2 + ab*h*h + ab1*(h-db)**2
     *               +2*as*(d*d/12.+(d/2.-h)**2)
      zd=zz/(d-h)

c 3.1 elastic section modulus at bottom
c     ---------------------------------
      zb=zz/h

c 3.2 first yield strength
c     ---------------------
      dyield=zd*syd
      byield=zb*syb

c      dyield=dyield*1.0e-3
c      byield=byield*1.0e-3
      
      write(66,806) dyield,byield
806   format(/5x,'first yield bending moment (n.m):',
     *        /7x,'- at deck     =',e15.7,
     *        /7x,'- at bottom   =',e15.7)

      write(66,711) zd,zb
711   format(/5x,'section modulus, z=i/v (m3):',
     *       /7x,'- at deck      =',e15.7,
     *       /7x,'- at bottom    =',e15.7)

c ---------------------------------------------------------------------
	
c 4.0 ultimate hull strength
c     =======================
c     if there is no double deck (ab1=0)
      if(ab1.lt.0.000001) ab1=0.000001
      if(db.lt.0.0001)  db=0.0001
      if((ab1/ab).lt.0.001) then
         db=0.000001*d
      endif
	
c 5.0 for sagging condition ("h" from eq.12.b ; "g" from eq.14; "usag" from eq.17.b)
c     ======================
      sus=susu
      sys=sysl
	ss=sus+sys
      
      write(66,*) 
      write(66,*)'     h & hb = high of the linear zone',
     *           ' (of the vertical stress distribution)' 
      write(66,*)'     - sagging:'

      bb = - d*(ad*sud+2.0*as*sus-ab*syb-ab1*sys) /(as*ss)
      cc = - ab1*d*db /as
	rac= dsqrt(bb*bb-4.0*cc)
      h  = (-bb + rac)/2.

      do 1 nel=1,neto
      nbrxi=nvar(nel)
      do 2 k=1,nbrxi
        kk=nxit(k,nel)
        dcc=cc * ( dab1(kk,nel)/ab1-das(kk,nel)/as )
          dbb=-bb* ( das(kk,nel)/as + dsusu(kk,nel)/ss )
     *      - d* ( ad*dsud(kk,nel)+2.0*as*dsusu(kk,nel)
     *            +dad(kk,nel)*sud+2.0*das(kk,nel)*sus
     *            -dab(kk,nel)*syb-dab1(kk,nel)*sys  )/(as*ss)
	  dh(kk,nel)=-(dbb-(bb*dbb-2.*dcc)/rac)/2.
        
   2  continue
   1  continue
 	
      hb=0.
      if(h.le.db) then     
c     cas limite nø2 : h < db ou h < 0 : deck too strong or bottom too weak 
c     ---------------
c       we have to recompute h for the correct stress distribution in the inner bottom
c          sb' = stress in the inner bottom = sub1 (ultimate strength of the panel)
        
	  write(29,*)'cas limite nø2'											!sept06		!bug
	  write(29,*)'h < db ou h < 0 :deck too strong or bottom too weak'		!sept06		!bug

	  h=d*(ad*sud+2.0*as*sus+ab1*sub1-ab*syb)/as/ss
        write(66,*)'     as  h   = ',h,' <? <  db  = ',db
	  i1=1
        if(h.le.0) then   
	    i1=0
          h=d*1.00e-10
          g=0.
        else
	    g=h*sys/ss
        endif
        write(66,*)'         h   = ',h
        write(66,*)'         g   = ',g

        usag=-ad*(d-g)*sud - as/d*(d-h)*(d+h-2.0*g)*sus
     *       -ab*g*syb - ab1*(db-g)*sub1
     *       -as*h/3.0/d*((2.0*h-3.0*g)*sus-(h-3.0*g)*sys)     

c       calcul de dh/dxi, dg/dxi et dusag/dxi
        im=0
        do 46 nel=1,neto
          nbrxi=nvar(nel)
          do 47 k=1,nbrxi
           kk=nxit(k,nel)
	     	     
           if(i1.le.0) then   
	       dg(kk,nel)=0.
	       dh(kk,nel)=0.
	     else
	      dh(kk,nel)=-h * ( das(kk,nel)/as + dsusu(kk,nel)/ss )
     *      +d*(ad*dsud(kk,nel)+2.0*as*dsusu(kk,nel)+ab1*dsub1(kk,nel)
     *          +dad(kk,nel)*sud+2.0*das(kk,nel)*sus+dab1(kk,nel)*sub1
     *          -dab(kk,nel)*syb  )/(as*ss)
	      dg(kk,nel)=(dh(kk,nel)-g*dsusu(kk,nel)/sys)*g/h
           endif

           dult(im+k)=- ad * (d-g) * dsud(kk,nel)
     *      - (-ad*dg(kk,nel)+dad(kk,nel)*(d-g)) * sud 
     *      - (dab(kk,nel)*g+ab*dg(kk,nel)) * syb 
     *      - (dab1(kk,nel)*(db-g)-ab1*dg(kk,nel))*sub1
     *      -  ab1*(db-g) * dsub1(kk,nel)
     *      - (-as*dh(kk,nel)+das(kk,nel)*(d-h)) * (d+h-2.0*g)*sus/d
     *      -  as*(d-h)/d * 
     *        ((dh(kk,nel)-2.0*dg(kk,nel))*sus+(d+h-2.*g)*dsusu(kk,nel))
     *      - (as*dh(kk,nel)+das(kk,nel)*h)
     *               *((2.0*h-3.0*g)*sus-(h-3.0*g)*sys)/(3.0*d)
     *      - ( (2.0*dh(kk,nel)-3.0*dg(kk,nel))*sus
     *          +(2.*h-3.0*g)*dsusu(kk,nel)
     *          -(dh(kk,nel)-3.0*dg(kk,nel))*sys  ) * as*h/(3.0*d) 
           imm=im+k
  47      continue
          im=im+nbrxi
  46    continue

c23456789012345678901234567890123456789012345678901234567890123456789012

      elseif (h.gt.d) then
c     cas limite nø4 : h>d : tensile bottom too strong or compressed deck too weak 
c     ---------------
c        we assume h = 0 and
c                 a tensile yield zone is introduced = d-hb
         
	   write(29,*)'subr ushull'											!sept06		!bug
	   write(29,*)'cas limite nø4 : h>d									!sept06
     * tensile bottom too strong or compressed deck too weak'				!sept06		!bug

	   write(66,*)'  cas limite nø4 : (h=',h,' > d)'
         write(66,*)'     as h>d, a tensile zone (d-hb) is required'
         h=d
c        first, we suppose  d-hb > db (this means large tensile zone)
         hb=(-ad*sud+ab1*syb1+ab*syb+2.0*as*sys)*d/(as*ss)
	   if(hb.le.0.0001) hb=0.0001

         if((d-hb).ge.db) then
c          h>d and  d-hb > db (large tensile yield zone) : cas nø 4.a
c          ----------------------------------------------
           write(66,*)'   a large tensile zone (d-hb) is valid'
           write(66,*)'         hb   = ',hb
           write(66,*)'         d-hb = ',d-hb,' > db  = ',db
           g=hb*sus/ss
           usag=-ad*g*sud - as/d*(d-hb)*(d+hb-2.0*g)*sys
     *       -ab*(d-g)*syb - ab1*(d-g-db)*syb1
     *       -as*hb/3.0/d*((2.0*hb-3.0*g)*sys-(hb-3.0*g)*sus)

c          calcul de dh/dxi, dg/dxi et dusag/dxi
           im=0
           do 42 nel=1,neto
           nbrxi=nvar(nel)
           do 43 k=1,nbrxi
           kk=nxit(k,nel)

	     dhb(kk,nel)=-hb * ( das(kk,nel)/as + dsusu(kk,nel)/ss )
     *      +d*(-ad*dsud(kk,nel)
     *          -dad(kk,nel)*sud+dab1(kk,nel)*syb1
     *          +dab(kk,nel)*syb+2.0*das(kk,nel)*sys  )/(as*ss)
	     dg(kk,nel)=(dhb(kk,nel)/hb+dsusu(kk,nel)*(1.0-g/hb)/sus)*g

           dult(im+k)=- ad * g * dsud(kk,nel)
     *      - (ad*dg(kk,nel)+dad(kk,nel)*g) * sud 
     *      - (dab(kk,nel)*(d-g)-ab*dg(kk,nel)) * syb 
     *      - (dab1(kk,nel)*(d-g-db)-ab1*dg(kk,nel))*syb1
     *      - (-as*dhb(kk,nel)+das(kk,nel)*(d-hb)) * (d+hb-2.0*g)*sys/d
     *      -  as*(d-hb)/d * (dhb(kk,nel)-2.0*dg(kk,nel))*sys
     *      - (as*dhb(kk,nel)+das(kk,nel)*hb)
     *               *((2.0*hb-3.0*g)*sys-(hb-3.0*g)*sus)/(3.0*d)
     *      - ( (2.0*dhb(kk,nel)-3.0*dg(kk,nel))*sys
     *          -(hb-3.0*g)*dsusu(kk,nel)
     *          -(dhb(kk,nel)-3.0*dg(kk,nel))*sus  ) * as*hb/(3.0*d) 
           imm=im+k
  43       continue
           im=im+nbrxi
  42       continue

	   else
c          h>d and  d-hb < db (small tensile yield zone) : cas nø 4.b
c          ----------------------------------------------
           write(66,*)'   a small tensile zone (d-hb) is valid'
           bb=-d*(-ad*sud-ab1*sus+2.0*as*sys+ab*syb)/as/ss
           cc=-ab1*d*(d-db)/as
	     rac=dsqrt(bb*bb-4.0*cc)
           hb=( -bb + rac )/2.
           write(66,*)'         hb   = ',hb
           write(66,*)'         d-hb = ',d-hb,' < db  = ',db
           g=hb*sus/ss
	     
           usag=-ad*g*sud - as/d*(d-hb)*(d+hb-2.0*g)*sys
     *         - ab*(d-g)*syb - ab1*(d-g-db)*(-sus+(sys+sus)*(d-db)/hb)
     *         - as*hb/3.0/d*((2.0*hb-3.0*g)*sys-(hb-3.0*g)*sus)

c          calcul de dh/dxi, dg/dxi et dusag/dxi
           im=0
           do 44 nel=1,neto
           nbrxi=nvar(nel)
           do 45 k=1,nbrxi
           kk=nxit(k,nel)
	     	     
           dcc=cc * ( dab1(kk,nel)/ab1-das(kk,nel)/as )
	     dbb=-bb* ( das(kk,nel)/as + dsusu(kk,nel)/ss )
     *         - d* ( ad*dsud(kk,nel)-ab1*dsusu(kk,nel)
     *               -dad(kk,nel)*sud-dab1(kk,nel)*sus
     *               +2.0*das(kk,nel)*sys+dab(kk,nel)*syb  )/(as*ss)
	     dhb(kk,nel)=-(dbb-(bb*dbb-2.*dcc)/rac)/2.
	     dg(kk,nel)=(dhb(kk,nel)/hb+dsusu(kk,nel)*(1.0-g/hb)/sus)*g

           dult(im+k)=- ad * g * dsud(kk,nel)
     *      - (ad*dg(kk,nel)+dad(kk,nel)*g) * sud 
     *      - (dab(kk,nel)*(d-g)-ab*dg(kk,nel)) * syb 
     *      - (dab1(kk,nel)*(d-g-db)-ab1*dg(kk,nel)) 
     *                * (-sus+(d-db)*ss/hb)
     *      - ab1*(d-g-db) * 
     *       (-(hb-d+db)*dsusu(kk,nel)/hb-(d-db)*ss*dhb(kk,nel)/(hb*hb))
     *      - (-as*dhb(kk,nel)+das(kk,nel)*(d-hb)) * (d+hb-2.0*g)*sys/d
     *      -  as*(d-hb)/d * (dhb(kk,nel)-2.0*dg(kk,nel))*sys
     *      - (as*dhb(kk,nel)+das(kk,nel)*hb)
     *               *((2.0*hb-3.0*g)*sys-(hb-3.0*g)*sus)/(3.0*d)
     *      - ( (2.0*dhb(kk,nel)-3.0*dg(kk,nel))*sys
     *          -(hb-3.0*g)*dsusu(kk,nel)
     *          -(dhb(kk,nel)-3.0*dg(kk,nel))*sus  ) * as*hb/(3.0*d) 
           imm=im+k
  45       continue
          im=im+nbrxi
  44       continue

	   endif

c23456789012345678901234567890123456789012345678901234567890123456789012

      else 
c       normal case : db < h < d
c       ------------------------
        g=h*sys/ss
        usag=-ad*(d-g)*sud - as/d*(d-h)*(d+h-2.0*g)*sus
     *       -ab*g*syb + ab1*(g-db)*(-sys+(db/h)*ss)
     *       -as*h/3.0/d*((2.0*h-3.0*g)*sus-(h-3.0*g)*sys)

c       calcul de dg/dxi et dusag/dxi
        im=0
        do 40 nel=1,neto
          nbrxi=nvar(nel)
          do 41 k=1,nbrxi
           kk=nxit(k,nel)

           dcc= cc * ( dab1(kk,nel)/ab1- das(kk,nel)/as )
	     dbb=-bb * ( das(kk,nel)/as  + dsusu(kk,nel)/ss )
     *         - d * (+ad*dsud(kk,nel)+2.0*as*dsusu(kk,nel)
     *                +dad(kk,nel)*sud+2.0*das(kk,nel)*sus
     *                -dab(kk,nel)*syb-dab1(kk,nel)*sys   )/(as*ss)

	     dh(kk,nel)=-(dbb-(bb*dbb-2.*dcc)/rac)/2.
	     dg(kk,nel)=(dh(kk,nel)-g*dsusu(kk,nel)/sys)*g/h

           dult(im+k)=- ad * (d-g) * dsud(kk,nel)
     *      - (-ad*dg(kk,nel)+dad(kk,nel)*(d-g)) * sud 
     *      - (dab(kk,nel)*g+ab*dg(kk,nel)) * syb 
     *      + (dab1(kk,nel)*(g-db)+ab1*dg(kk,nel))*(-sys+db*ss/h)
     *      +  ab1*(g-db) * 
     *           (dsusl(kk,nel)*db - ss*db*dh(kk,nel)/h) /h
     *      - (-as*dh(kk,nel)+das(kk,nel)*(d-h)) * (d+h-2.0*g)*sus/d
     *      -  as*(d-h)/d * 
     *        ((dh(kk,nel)-2.0*dg(kk,nel))*sus+(d+h-2.*g)*dsusu(kk,nel))
     *      - (as*dh(kk,nel)+das(kk,nel)*h)
     *               *((2.0*h-3.0*g)*sus-(h-3.0*g)*sys)/(3.0*d)
     *      - ( (2.0*dh(kk,nel)-3.0*dg(kk,nel))*sus
     *          +(2.*h-3.0*g)*dsusu(kk,nel)
     *          -(dh(kk,nel)-3.0*dg(kk,nel))*sys  ) * as*h/(3.0*d) 

           imm=im+k
  41      continue
          im=im+nbrxi
  40    continue

      endif

      write(66,*)'         h   = ',h
      write(66,*)'         g   = ',g
      write(66,*)'         hb  = ',hb

c     sauvetage sur disque,  file 303		!extension neto	!fev2007 
c     -------------------------------
c     sagging: usag < 0 et usag (max) < 0
c     restriction : c(x)= usag(max)/usag(ult) < 1
c        dc(x)/dxi= - c(x)/usag(ult) * dusag(ult)/dxi
      vnom2='ult. sagging'
	c=usagm/usag
      cm=1.
	do 48 i=1,ntot
        dult(i)= - (c/usag) * dult(i)
   48 continue
	write(303) c,cm,vnom2			!extension neto	!fev2007
	write(303) (dult(i),i=1,ntot)	!extension neto	!fev2007
!	write(6969,*) c,cm,vnom2			!extension neto	!fev2007
!	write(6969,*) (dult(i),i=1,ntot)	!extension neto	!fev2007

c		do ii=1,ntot
c		open(666667, file = 'dult-ushull-1559-dyn.txt')
c		write (666667,*) dult(ii)
c		enddo
c		pause	

c23456789012345678901234567890123456789012345678901234567890123456789012

c     ============================================================================
c 6.0 for hogging condition ("h" from eq.15.b ; "g" from eq.16; "uhog" from eq.18.b)
c     ----------------------------------------------------------------------------
c     ============================================================================
      sus=susl
      sys=sysu
	ss=sus+sys

      write(66,*)'     - hogging:'

c     calcul de h (pour le cas normal)	
      h=(ab*sub+ab1*sub1+2.0*as*sus-ad*syd) * d/(as*ss)	
      hb=0.
	
      if(h.le.(d*1.00e-10)) then
c 6.1 cas limite nø1 : h<0 : deck too strong or too weak bottoms 
c     ---------------
        write(29,*)'cas limite nø1 : h<0'									!sept06			!bug
	  write(29,*)'deck too strong or too weak bottoms'					!sept06		!bug
	  
	  write(66,*)'      h=',h,' < 0 , then h=0.0'
        h=0.
        g=0.

c       calcul de uhog	
        uhog=ab*d*sub + ab1*(d-db)*sub1 + as*d*sus
 
c       calcul de duhog/dxi	
        im=0
        do 15 nel=1,neto
        nbrxi=nvar(nel)
        do 16 k=1,nbrxi
          kk=nxit(k,nel)
          dult(im+k)=  d * ( dab(kk,nel) *sub  + ab *dsub(kk,nel)  ) 
     *           + (d-db)* ( dab1(kk,nel)*sub1 + ab1*dsub1(kk,nel) ) 
     *              +  d * ( das(kk,nel) *sus  + as *dsusl(kk,nel) )   
          imm=im+k
  16    continue
        im=im+nbrxi
  15    continue
	  
      elseif(h.gt.(d-db)) then
c 6.2 cas limite nø3 : h>d-db : bottom very strong or deck too weak
c     ---------------
c       we have to recompute h for the correct stress distribution in the inner bottom
c          sb' = stress in the inner bottom << sub1 (ultimate strength of the panel)
         write(29,*)'subr ushull'											!sept06		!bug
	   write(29,*)'as limite nø3 : h>d-db								!sept06
     * bottom very strong or deck too weak'								!sept06		!bug
	  
	  write(66,*)'  cas limite nø3 : h=',h
        write(66,*)'    as h>d-db : h is recomputed'

c       nouveau calcul du h
        bb=d*(-ab*sub-2.0*as*sus+ad*syd+ab1*sys)/as/ss
        cc=-ab1*(d-db)*d/as
	  rac= dsqrt(bb*bb-4.0*cc)
        h =( -bb + rac )/2.

        if(h.gt.d) then
c 6.2.1    cas limite nø3.a : h>d: a tensile yield zone must be introduce = hb 
c          ------------------
           write(66,*)'  cas limite nø3.a : h=',h
           write(66,*)'    as h>d : yield zone hb is introduced'
           h=d

c          calcul du hb et du g
           bb=-d*(ad*syd+2.0*as*sys-ab*sub-ab1*sus)/as/ss
           cc=-ab1*db*d/as
	     rac= dsqrt(bb*bb-4.0*cc)
           hb =( -bb + rac )/2.
           write(66,*)'             hb  = ',hb
	     if(hb.le.db) hb=db+0.0001
           g=hb*sus/ss

c          calcul du uhog
           uhog=ad*syd*(d-g) + ab*g*sub 
     *        - ab1*(g-db)*(-sus+db*ss/hb)
     *        + as/d*(d-hb)*(d+hb-2.0*g)*sys
     *        + as*hb/3.0/d*((2.0*hb-3.0*g)*sys-(hb-3.0*g)*sus)
     
c          calcul du dh/dxi, dg/dxi et duhog/dxi 
           im=0
           do 19 nel=1,neto
           nbrxi=nvar(nel)
           do 20 k=1,nbrxi
           kk=nxit(k,nel)

           dcc= cc * ( dab1(kk,nel)/ab1- das(kk,nel)/as )
	     dbb=-bb * ( das(kk,nel)/as  + dsusl(kk,nel)/ss )
     *        - d* (-ab*dsub(kk,nel)-ab1*dsusl(kk,nel)
     *              +dad(kk,nel)*syd+2.0*das(kk,nel)*sys
     *              -dab(kk,nel)*sub-dab1(kk,nel)*sus   )/(as*ss)

	     dhb(kk,nel)=-(dbb-(bb*dbb-2.*dcc)/rac)/2.
           dg(kk,nel)=dhb(kk,nel)*g/hb + dsusl(kk,nel)*g*(1.-g/hb)/sus

           dult(im+k)=(-ad*dg(kk,nel)+dad(kk,nel)*(d-g)) * syd 
     *      + (dab(kk,nel)*g+ab*dg(kk,nel))*sub + ab*g*dsub(kk,nel) 
     *      - (dab1(kk,nel)*(g-db)+ab1*dg(kk,nel))*(-sus+db*ss/hb)
     *      -  ab1*(g-db) * 
     *           (dsusl(kk,nel)*(db/hb-1.) - ss*db*dhb(kk,nel)/(hb*hb))
     *      + (-as*dhb(kk,nel)+das(kk,nel)*(d-hb)) * (d+hb-2.0*g)*sys/d
     *      +  as*(d-hb) * (dhb(kk,nel)-2.0*dg(kk,nel)) *sys/d
     *      + (as*dhb(kk,nel)+das(kk,nel)*hb)
     *               *((2.0*hb-3.0*g)*sys-(hb-3.0*g)*sus)/(3.0*d)
     *      + ( (2.0*dhb(kk,nel)-3.0*dg(kk,nel))*sys
     *          -(hb-3.0*g)*dsusl(kk,nel)
     *          -(dhb(kk,nel)-3.0*dg(kk,nel))*sus  ) * as*hb/(3.0*d) 
           imm=im+k
  20       continue
           im=im+nbrxi
  19       continue

	  else
c 6.2.2    cas limite nø3.b : d>h>d-db: h can still be used
c          -----------------
           g=h*sys/ss
           write(66,*)' cas 3.b : d>h>d-db'
           uhog=ad*syd*g + ab*(d-g)*sub 
     *         - ab1*(sys-(d-db)*(sys+sus)/h)*(d-db-g)
     *         + as/d*(d-h)*(d+h-2.0*g)*sus
     *         + as*h/3.0/d*((2.0*h-3.0*g)*sus-(h-3.0*g)*sys)

c          calcul du dh/dxi , dg/dxi et  duhog/dxi 
           im=0
           do 17 nel=1,neto
           nbrxi=nvar(nel)
           do 18 k=1,nbrxi
           kk=nxit(k,nel)
           dcc=cc * ( dab1(kk,nel)/ab1-das(kk,nel)/as )
	     dbb=-bb* ( das(kk,nel)/as + dsusl(kk,nel)/ss )
     *        + d* (-ab*dsub(kk,nel) -2.0*as*dsusl(kk,nel)
     *              -dab(kk,nel)*sub -2.0*das(kk,nel)*sus
     *              +dab1(kk,nel)*sys+dad(kk,nel)*syd   )/(as*ss)

	     dh(kk,nel)=-(dbb-(bb*dbb-2.*dcc)/rac)/2.
           dg(kk,nel)=(dh(kk,nel)-g*dsusl(kk,nel)/sys) * g/h

           dult(im+k)=
     *        (ad*dg(kk,nel)+dad(kk,nel)*g) * syd 
     *      + (dab(kk,nel)*(d-g)-ab*dg(kk,nel)) * sub 
     *      +  ab*(d-g)*dsub(kk,nel) 
     *      - (dab1(kk,nel)*(d-g-db)-ab1*dg(kk,nel))*(sys-(d-db)*ss/h)
     *      -  ab1*(d-g-db)*(d-db)*(ss*dh(kk,nel)/h-dsusl(kk,nel))/h 
     *      + (-as*dh(kk,nel)+das(kk,nel)*(d-h)) * (d+h-2.0*g)*sus/d
     *      +  as*(d-h) * ( (dh(kk,nel)-2.0*dg(kk,nel))*sus+
     *                      (d+h-2.0*g)*dsusl(kk,nel)   )/d
     *      + (as*dh(kk,nel)+das(kk,nel)*h)
     *               *((2.0*h-3.0*g)*sus-(h-3.0*g)*sys)/(3.0*d)
     *      + ( (2.0*dh(kk,nel)-3.0*dg(kk,nel))*sus+
     *          (2.0*h-3.0*g)*dsusl(kk,nel)-
     *          (dh(kk,nel)-3.0*dg(kk,nel))*sys  ) * as*h/(3.0*d) 
           imm=im+k
  18       continue

           im=im+nbrxi
  17       continue
	  endif
	  
      else
c 6.3 normal case: 0 < h < d-db 
c     ---------------
        g=h*sys/ss

c       calcul de uhog	
        uhog=ad*syd*g + ab*(d-g)*sub + ab1*sub1*(d-g-db)
     *      +as/d*(d-h)*(d+h-2.0*g)*sus
     *      +as*h/3.0/d*((2.0*h-3.0*g)*sus-(h-3.0*g)*sys)

c       calcul de dh/dxi, dg/dxi  et duhog/dxi	
        im=0
        do 13 nel=1,neto
        nbrxi=nvar(nel)
        do 14 k=1,nbrxi
          kk=nxit(k,nel)
	    dh(kk,nel)=-h * ( das(kk,nel)/as + dsusl(kk,nel)/ss )
     *    + d*( ab*dsub(kk,nel)+ab1*dsub1(kk,nel)+2.0*as*dsusl(kk,nel)
     *          +dab(kk,nel)*sub+dab1(kk,nel)*sub1+2.0*das(kk,nel)*sus
     *          -dad(kk,nel)*syd  )/(as*ss)
          dg(kk,nel)=(dh(kk,nel)-g*dsusl(kk,nel)/sys) * g/h
          dult(im+k)=
     *        (ad*dg(kk,nel)+dad(kk,nel)*g) * syd 
     *      + (dab(kk,nel)*(d-g)-ab*dg(kk,nel)) * sub 
     *      +  ab*(d-g)*dsub(kk,nel) 
     *      + (dab1(kk,nel)*(d-g-db)-ab1*dg(kk,nel)) * sub1
     *      +  ab1*(d-g-db)*dsub1(kk,nel) 
     *      + (-as*dh(kk,nel)+das(kk,nel)*(d-h)) * (d+h-2.0*g)*sus/d
     *      +  as*(d-h) * ( (dh(kk,nel)-2.0*dg(kk,nel))*sus+
     *                      (d+h-2.0*g)*dsusl(kk,nel)   )/d
     *      + (as*dh(kk,nel)+das(kk,nel)*h)
     *               *((2.0*h-3.0*g)*sus-(h-3.0*g)*sys)/(3.0*d)
     *      + ( (2.0*dh(kk,nel)-3.0*dg(kk,nel))*sus+
     *          (2.0*h-3.0*g)*dsusl(kk,nel)-
     *          (dh(kk,nel)-3.0*dg(kk,nel))*sys  ) * as*h/(3.0*d) 
        imm=im+k
  14    continue

        im=im+nbrxi
  13    continue

      endif
      
      write(66,*)'         h   = ',h
      write(66,*)'         g   = ',g
      write(66,*)'         hb  = ',hb

c     sauvetage sur disque,  file 303			!extension neto	!fev2007
c     -------------------------------
c     hogging: uhog > 0 et uhog (max) > 0
c     restriction : c(x)= uhog(max)/uhog(ult) < 1
c        dc(x)/dxi= - c(x)/uhog(ult) * duhog(ult)/dxi
      vnom2='ult. hogging'
	c=uhogm/uhog
      cm=1.
	do 49 i=1,ntot
        dult(i)= - (c/uhog) * dult(i)
   49 continue
	write(303) c,cm,vnom2				!extension neto	!fev2007
	write(303) (dult(i),i=1,ntot)		!extension neto	!fev2007
!	write(6969,*) c,cm,vnom2				!extension neto	!fev2007
!	write(6969,*) (dult(i),i=1,ntot)		!extension neto	!fev2007

c		do ii=1,ntot
c		open(666668, file = 'dult-ushull-1788-dyn.txt')
c		write (666668,*) dult(ii)
c		enddo
c		pause	

c23456789012345678901234567890123456789012345678901234567890123456789012


c 7.0 outputs
c     --------
      usagr=usag/fpbm
      uhogr=uhog/fpbm
      
      write(66, 816) usag,usagr,uhog,uhogr
816   format(/'ultimate bending moment (n.m):'/30(1h-)/
     * '- for sagging:','     mult. =',e15.7,'     mult./mp  =',f10.5/
     * '- for hogging:','     mult. =',e15.7,'     mult./mp  =',f10.5)

      return
      end
