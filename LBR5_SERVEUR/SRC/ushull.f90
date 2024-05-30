subroutine ushulls(nel,sect,sigy1,e1,q,phil,&
                   epsa,hxr,dxr,wxr,txr,epsr,entr)


use param_section



implicit double precision(a-h,o-z) 

! *******************************************************************
!     subroutine ushulls
!     ==================
!
!  .  a computer program to evaluate the ultimate strength of ships .
!  .                                                                .
!  .  based on the model of prof. jeom kee paik (november 1995)     .
!  .                                                                .
!  .  adapted to lbr-5 by : dr. ph. rigo (february 1997)            .
!  .  ---------------------                                         .
!  .    department of naval architecture, anast                     .
!  .    university of liege                                         .
!  .    6 quai banning, 4000 liege, belgium                         .
!  .    (tel) +32 4 366 9225  (fax) +32 4 366 9133                  .
!  .    (e-mail) ph.rigo@ulg.ac.be                                  .
!  .                                                                .
!     modified: 
!        - limit states for h <0 or h > d                       (26-2-97)
!          h=zone of linear variation in the vertical stress distribution
!        - introduce specific stiffener yield stress             (3-3-97) 
!        - sys(up) _ sys(down) in the calculation of mp and mu   (5-3-97) 
!        - introduction of validity limit on column slenderness (10-3-97) 
!        - sensibility analysis (to consider the ultimate strength
!          of the ship girder as a global restriction)          (11-3-97) 
!        - correction of the equation of the static moment (subr ultipan)  (12-3-97) 
!          zo= ... + hw*tw*(tp+hw/2.0) + ...   instead of  ... + hw*tw*(tp+hw)/2.0 + ...
!
!     last modification: 25-5-99
!                
! *******************************************************************
!  organigramme
!  ------------
!  - ushul
!      - ultpan  (calcul de sult et dsult/dxi)
!          - ult (calcul de sult)
!
!      - dsect   (calcul de sect et dsect/dxi)
!
!      - ulthull (calcul de mult sagging et hogging.)
!
!      - dulthull(dmult/dxi, sagg. et hogg.)
!
! *******************************************************************
!
!     ship sizes
!     ----------
!  .  depth = depth of the vessel          (m.)
!  .  db    = depth of double-bottom       (m.)

!     modeling on the ship components (deck, bottoms, side) based on the neto panels
!     -------------------------------------------------------------------------------
!
!   nd = number of panels included in the upper deck   component
!   nb1= number of panels included in the inner bottom component (si double fond)
!   nb = number of panels included in the outer bottom component
!   ns = number of panels included in the two   sides  component
!
!   id (i=1,nd)  = list of panels included in the upper deck   component
!   ib1(i=1,nb1) = list of panels included in the inner bottom component 
!   ib (i=1,nb)  = list of panels included in the outer bottom component
!   is (i=1,ns)  = list of panels included in the two   sides  component
!
!   number of a panel to use to evaluate the ultimate strength of a component
!   kd   = nø of a panel for calculation of the upper deck   strength 
!   kb1  = nø of a panel for calculation of the inner bottom strength
!   kb   = nø of a panel for calculation of the outer bottom strength
!   ksup = nø of a panel for calculation of the upper part of the side plate  strength
!   ksdw = nø of a panel for calculation of the lower part of the side plate  strength
!
! rem : - il faut modéliser la structure complète (et pas une demi)
!       - si seule la moitié de la structures a été modéliésée pour le calcul avec lbr,
!         alors il faut répéter 2 fois certains éléments de façon à générer la structure entière.
!       - il faut donner les panneaux correspondants aux 2 côtés (side plates)
!
if(nel.eq.neto) then
   write(iu_11(iboat),96)
96 format(//50(1h=)/'  ultimate   hull   strength '/50(1h=)/)

   write(iu_11(iboat),*)'evaluation of the ultimate strength of ship girder'	
   write(iu_11(iboat),*) '    using the paik algorithm	(caldwell method)'
   write(iu_11(iboat),'(50(1h-)/)')	

   write(iu_11(iboat),'(a,4(i2,1x))')                        &
   'panels included in the ship components (nd,nb1,nb,ns)=', &
   nd,nb1,nb,ns

   write(iu_11(iboat),'(a,5(20(i2,1x)/))') 'upper deck   : ',(id(i), i=1,nd)
   write(iu_11(iboat),'(a,5(20(i2,1x)/))') 'inner bottom : ',(ib1(i),i=1,nb1)
   write(iu_11(iboat),'(a,5(20(i2,1x)/))') 'outer bottom : ',(ib(i), i=1,nb)
   write(iu_11(iboat),'(a,5(20(i2,1x)/))') 'two   sides  : ',(is9(i), i=1,ns)
   write(iu_11(iboat),'(/a,a)')'panels used for the evaluation of the ', &
   'ult. str. of long. compressed ship components:'
   write(iu_11(iboat),*) 'upper deck               : ',kd
   write(iu_11(iboat),*) 'inner bottom             : ',kb1
   write(iu_11(iboat),*) 'outer bottom             : ',kb
   write(iu_11(iboat),*) 'upper part of side plate : ',ksup
   write(iu_11(iboat),*) 'lower part of side plate : ',ksdw
   write(iu_11(iboat),*)
	endif


!*************************************************************************
!*************************************************************************
!
!     descrition of data for ushull
!     ==============================
!
!               Œ y et sigma y
!               i
!               *****************************
!               *                           *  b=entr
!               *                           *
!               *                           *
!               ***************************** --------> x et sigma x
!               <----------- a = epsa ------>
!
!     epsa,entr	= a,b        (m)
!     sigy1		= sig elastique (n/m2), (ex, re=240 n/mm2)
! 
!
!     characteristics if the stiffened elements (epsa,entr,delta,hxr,dxr,wxr,txr,sigy1,sigy1)
!     ------------------------------------------
!  .  sl = epsa : length of stiffener between transverse frames (m.)
!  .  bp = entr : longitudinal stiffener spacing   (m.)
!  .  tp = delta: thickness of parent plate        (m.)
!  .  hw = hxr  : height of stiffener web          (m.)
!  .  tw = dxr  : thickness of stiffener web       (m.)
!  .  bf = wxr  : breadth of stiffener flange      (m.)
!  .  tf = txr  : thickness of stiffener flange    (m.)
!  .  syp= sigy1 : yield strength of parent plate   (n/m2)   
!  .  syw= sigy1 : yield strength of stiffener      (n/m2)   
!
! lbr5 : epsa,entr,delta,hxr,dxr,wxr,txr,sigy1,sigy1
! paik : sl,  bp,  tp,   hw, tw, bf, tf, syp, syw
!
!     properties of hull section components
!     --------------------------------------
!   . ab  = total sectional area of outer bottom (m2)
!   . ab1 = total sectional area of inner bottom (m2)
!   . ad  = total sectional area of deck         (m2)
!   . as  = half-sectional area of all sides     (m2)
!           (including long. blkd, inner sides, and vertical/inclined members) 
!
! ********************************************************************
! ********************************************************************
!
!     principal dimensions of the ship
!
!  .  depth = depth of the vessel          (m.)
!  .  db    = depth of double-bottom       (m.)

if(nel.eq.neto) write(iu_11(iboat),16) depth,db
16              format('depth of the vessel    (m.)        =',f12.6/ &
                       'depth of double-bottom (m.)        =',f12.6)


! 1.0 ultimate strength of the stiffened plates under compression
!     ============================================================
!     input
!     ------
!  .  sl = epsa : length of stiffener between transverse frames (m.)
!  .  bp = entr : longitudinal stiffener spacing   (m.)
!  .  tp = delta: thickness of parent plate        (m.)
!  .  hw = hxr  : height of stiffener web          (m.)
!  .  tw = dxr  : thickness of stiffener web       (m.)
!  .  bf = wxr  : breadth of stiffener flange      (m.)
!  .  tf = txr  : thickness of stiffener flange    (m.)
!  .  syp= sigy1 : yield strength of parent plate   (n/m2)   
!  .  syw= sigy1 : yield strength of stiffener      (n/m2)   
!
! lbr5 : epsa,entr,delta,hxr,dxr,wxr,txr,sigy1,sigy1
! paik : sl,  bp,  tp,   hw, tw, bf, tf, syp, syw
!
!     output
!     ------
!  .  syd  = equivalent yield strength of deck         (n/m2)  
!  .  syb  = equivalent yield strength of outer bottom (n/m2)  
!  .  syb1 = equivalent yield strength of inner bottom (n/m2)  
!  .  sysu = equivalent yield strength of side shell-up(n/m2)  
!  .  sysl = equivalent yield strength of side shell-lw(n/m2)  
!  .  sud  = ultimate strength of deck                 (n/m2)  
!  .  sub  = ultimate strength of outer bottom         (n/m2)  
!  .  sub1 = ultimate strength of inner bottom         (n/m2)  
!  .  susu = ultimate strength of upper side shell     (n/m2)  
!  .  susl = ultimate strength of lower side shell     (n/m2)  




!si kopt _0 : calcul des sensibilités
!si kopt =0 : pas de calcul des sensibilités
kopt=iopti*irestr

!...  1.1) stiffened plate in deck (element kd)
!     =============================
if(nel.eq.kd) then

   call ultpan(nel,e1,sigy1,sysd,syeq,               &
               delta,epsa,hxr,dxr,wxr,txr,epsr,entr, &
               dsud(1,nel),sigu,kopt)

   sud=sigu
   syd=syeq
   sult=sigu/syeq

   write(iu_11(iboat),46)
   write(iu_11(iboat),47) epsa,entr,delta,hxr,dxr,wxr,txr,sigy1,sysd,syeq,sud,sult
   
endif

!...  1.2) stiffened plate in outer bottom (element kb)
!     =====================================
if(nel.eq.kb) then

   call ultpan(nel,e1,sigy1,sysb,syeq,              &
               delta,epsa,hxr,dxr,wxr,txr,epsr,entr,&
               dsub(1,nel),sigu,kopt)

   sub=sigu
   syb=syeq
   sult=sigu/syeq

   write(iu_11(iboat),56)
   write(iu_11(iboat),47) epsa,entr,delta,hxr,dxr,wxr,txr,sigy1,sysb, &
                          syeq,sub,sult
endif

!...  1.3) stiffened plate in inner bottom (element kb1)
!     =====================================
if(nel.eq.kb1) then

   call ultpan(nel,e1,sigy1,sysb1,syeq,              &
               delta,epsa,hxr,dxr,wxr,txr,epsr,entr, &
               dsub1(1,nel),sigu,kopt)

   sub1=sigu
   syb1=syeq
   sult=sigu/syeq

   write(iu_11(iboat),66)
   write(iu_11(iboat),47) epsa,entr,delta,hxr,dxr,wxr,txr,sigy1,sysb1,&
                          syeq,sub1,sult
endif

!...  1.4) stiffened plate in upper side shell (element ksup)
!     =========================================
if(nel.eq.ksup) then

   call ultpan(nel,e1,sigy1,syssup,syeq,             &
               delta,epsa,hxr,dxr,wxr,txr,epsr,entr, &
               dsusu(1,nel),sigu,kopt)

   susu=sigu
   sysu=syeq
   sult=sigu/syeq

   write(iu_11(iboat),76)
   write(iu_11(iboat),47) epsa,entr,delta,hxr,dxr,wxr,txr,sigy1,syssup,  &
                          syeq,susu,sult
endif

!...  1.5) stiffened plate in lower side shell (element ksdw)
!     =========================================
if(nel.eq.ksdw) then

   call ultpan(nel,e1,sigy1,syssdw,syeq,             &
               delta,epsa,hxr,dxr,wxr,txr,epsr,entr, &
               dsusl(1,nel),sigu,kopt)

   susl=sigu
   sysl=syeq
   sult=sigu/syeq

   write(iu_11(iboat),86)
   write(iu_11(iboat),47) epsa,entr,delta,hxr,dxr,wxr,txr,sigy1,syssdw, &
                          syeq,susl,sult
endif


46    format(///,5x,'deck    panels :')
56    format(///,5x,'outer   bottom  panels :')
66    format(///,5x,'inner   bottom  panels :')
76    format(///,5x,'upper   side   shells  :')
86    format(///,5x,'lower   side   shells  :')
47    format(                                                 &
      /5x,'length of stiffener            (m.)       =',f12.6,&
      /5x,'longitudinal stiffener spacing (m.)       =',f12.6,&
      /5x,'thickness of parent plate      (m.)       =',f12.6,&
      /5x,'height of stiffener web        (m.)       =',f12.6,&
      /5x,'thickness of stiffener web     (m.)       =',f12.6,&
      /5x,'breadth of stiffener flange    (m.)       =',f12.6,&
      /5x,'thickness of stiffener flange  (m.)       =',f12.6,&
      /5x,'yield strength of parent plate (n/m2)     =',e14.7,&
      /5x,'yield strength of stiffener    (n/m2)     =',e14.7,&
      /5x,'equivalent yield strength      (n/m2)     =',e14.7,&
      /5x,'ultimate compressive strength  (n/m2)     =',e14.7,&
      /5x,'relative ultimate strength (sult/sy)      =',f12.6)
       
        
        

! 2.0 sectional area  of hull components
!     ===================================
!   . ab  = total sectional area of outer bottom (m2)
!   . ab1 = total sectional area of inner bottom (m2)
!   . ad  = total sectional area of deck         (m2)
!   . as  = half-sectional area of all sides     (m2)
!           (including long. blkd, inner sides, and vertical/inclined members) 
!
!     mise à zéro des sensibilités des sections dad,dab,dab1 et das : d(ad)/dxi

if(kopt.ge.1) then
	 if(nel.eq.1) call annuld(dad,270*4)
endif

	do 10 i=1,nd
	if(id(i).eq.nel) then
	  ad=ad+sect
        if(kopt.ge.1) then
          call dsect(nel,hxr,dxr,wxr,txr,epsr,phil,q,dad(1,nel))
          endif
        endif
  10  continue



        do 11 i=1,nb1
        if(ib1(i).eq.nel) then
          ab1=ab1+sect
        if(kopt.ge.1) then
          call dsect(nel,hxr,dxr,wxr,txr,epsr,phil,q,dab1(1,nel))
          endif
	endif	
  11  continue

	do 12 i=1,nb
	if(ib(i).eq.nel) then
	  ab=ab+sect
        if(kopt.ge.1) then
          call dsect(nel,hxr,dxr,wxr,txr,epsr,phil,q,dab(1,nel))
          endif
        endif
  12  continue

        do 13 i=1,ns
        if(is9(i).eq.nel) then
	  as=as+sect/2.
        if(kopt.ge.1) then
          call dsect(nel,hxr,dxr,wxr,txr,epsr,phil,q,das(1,nel))
         endif
        endif
  13  continue

! ********************************************************************
! 3.0 calculation of ultimate hull strength in normal condition 
!     ==========================================================
! ********************************************************************
!       end of subr. ushull (si nel<neto)'

      if(nel.eq.neto) then
           write(*,*) 'avt ulthull'

        if(kopt.eq.0) then
!         sans optimisation (kopt=0)
!         ---------------------------

          call ulthull
!         ============

        else
!         avec optimisation (kopt>0)
!         ---------------------------

!         car as et das ne sont relatif qu'à la moitié des parois verticales	
	    do 14 j=1,neto
	    do 14 i=1,9
	      das(i,j)=das(i,j)/2.0
   14     continue

          call dulthull
	
        endif
      endif

      return
      end


!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************

subroutine ultpan(nel,e1,syp,syw,syeq,                  &
                  delta,epsa,hxr,dxr,wxr,txr,epsr,entr, &
                  dspaik,spaik,kopt)

use param_section, IS_=>IS
implicit double precision(a-h,o-z)
double precision is
dimension dspaik(9)

!*********************************************************************
!     subroutine ultpan 
!     ==================
!     cette subrourine calcule la résistance ultime d'une plaque raidie
!     londitudinalement et comprimée, également, longitudinalement.
!     (la pression latéral n'est pas prise en compte)
!
!     il s'agit de la formule empirique de j. paik.  qui est basée 
!     sur le principe de la poutre colonne.
!                                                                       
!   input:e1,syp,syw,delta,epsa 
!           et  hxr,dxr,wxr,txr,epsr,entr
!                                                                       
!  output:dcpaik,spaik,omt,
!	omt = section raidiseur + bordage (complet)
!     spaik = résistance ultime selon la formule de j. paik
!     dcpaik = dérivée de la résistance ultime spaik par rapport aux xi
!
!     créer : le 22-7-96  par  ph. rigo
!
!     modifications : 
!	- restrictions nø 15 + sensibilités (voir subr. contr & paik) 22-7-96  
!	- limite sur beta et lamda	      				               1-8-96  
!	- prise en compte des différences entre d(raid) et de epsr	   6-3-97 
!                               
!     dernière modif : 10-3-97 				     	
!                                                                       
!*************************************************************************
!*************************************************************************
!               ^ y et sigma y
!               i
!               *****************************
!               *                           *  b=epsr (= variable de conception)
!               *                           *  b=entr = d (entredistance réelle)
!               *                           *
!               ***************************** --------> x et sigma x
!               <----------- a = epsa ------>
!
!      epsa  	= a        (m)
!      entr,epsr	= b        (m)
!      avec entr : entredistance réelle entre raidisseurs = d(raid)
!           epsr : largeur effective associée à 1 raid (= l/nbre de raid)
!      syp		= yield stress of plating (n/m2), (ex, re=240 n/mm2)
!      syw		= yield stress of stiffener (n/m2), (ex, re=240 n/mm2)
!
!*************************************************************************
! 
! 1.0 calcul de spaik de référence
!     ****************************
!
! 1.1 caractéristiques géométriques et calcul de á et lamda  
!     -----------------------------------------------------
om  =wxr*txr + dxr*hxr                    ! section raid
omt =om + entr*delta                      ! section raid + d(raid)
smcr=delta/2.+hxr+txr/2.
smdr=(delta+hxr)/2.
hx  =wxr*txr*smcr + dxr*hxr*smdr          ! moment statique (z=0)

!     equivalent yield strength for the whole panel
gam=om/(entr*delta)
syeq=(syp+gam*syw)/(1.0+gam)

gx=2.0*hx		
! yg=position du centre de gravité.  
yg=gx/(2.0*omt)			
! is=inertie par rapport      							
! au centre de gravité.         
is=(entr*delta**3 + dxr*hxr**3 + wxr*txr**3)/12.	&
   + delta*entr*yg*yg  +  hxr*dxr*(smdr-yg)**2		&
   +  wxr*txr*(smcr-yg)**2  
rot=dsqrt(syeq/e1)
beta=entr * rot / delta
slen=epsa * rot* dsqrt(omt/is) / pi

! 1.2 test sur beta et lamda=slen (slim= élancement maximum pour un beta donné)
!     -----------------------------------------------------------------------
ilim=0

if(beta.le.2) then
   slim=3.0+(beta-1.)/2.
else
   slim=3.5+(beta-2.)
endif
	
if(slen.gt.slim) then
   write(iu_11(iboat),*)'attention: slen max =',slim,' < slen =',slen
   write(iu_11(iboat),*)'=========='
   write(iu_14(iboat),*)'attention: slen max =',slim,' < slen =',slen		
   write(iu_14(iboat),*)'=========='									
   ilim=1
   slen=slim
endif

! 1.3 calcul de spaik de référence
!     -----------------------------
slen2=slen*slen
beta2=beta*beta
fct=dsqrt(0.995 + 0.936*slen2 + 0.17*beta2 + 0.188*beta2*slen2      &
        - 0.067*slen2*slen2)
spaik=syeq/fct

! 1.4 impressions
!     ------------
if(kopt.eq.0) then
 	 write(iu_11(iboat),*)
 	 write(iu_11(iboat),*)'ds subr ultpan, nel=',nel
 	 write(iu_11(iboat),*)'--------------------------'
   write(iu_11(iboat),*)'beta    = ',beta
   write(iu_11(iboat),*)'lamda   = ',slen
   write(iu_11(iboat),*)'spaik   = ',spaik
	 
	 return	  
endif

!  calcul des sensibilités si kopt_0
!
! 2.0 dérivée de sig(paik) : dspaik
!     *****************************

if(ilim.eq.0) goto 3

! 2.1 cas où les valeurs limites de béta et lamda sont dépassées
!     ---------------------------------------------------------
!     dans ce cas le calcul se fait par différences finies.

      nbrxi=nvar(nel,iboat)

      do 5 k=1,nbrxi

      kk=nxit(k,nel,iboat)

      if(kk.eq.1) then                         
      !  xi=delta
	       ratio=1.05
         xxx=ratio *delta
         call ult(e1,syeq,xxx,epsa,hxr,dxr,wxr,txr,entr,spaik1)
         dspaik(kk)=(spaik1-spaik)/(xxx-delta)
      else if (kk.eq.5) then                  
      !  xi=epsa
	       ratio=0.5
         xxx=ratio *epsa
         call ult(e1,syeq,delta,xxx,hxr,dxr,wxr,txr,entr,spaik1)
         dspaik(kk)=(spaik1-spaik)/(xxx-epsa)
      else if (kk.eq.6) then                 
      !  xi=epsa
         ind=0
   16    ind=ind+1
	       ratio=5.0*ind
         xxx=ratio *hxr
         call ult(e1,syeq,delta,epsa,xxx,dxr,wxr,txr,entr,spaik1)
         dspaik(kk)=(spaik1-spaik)/(xxx-hxr)
	       if((dabs(dspaik(kk)).le.1000.).and.(ind.le.4)) goto 16
            else if (kk.eq.7) then                  
            !  xi=epsa
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
!        prise en compte de  d(d)/d(epsr) = (d/epsr)**2
           dspaik(kk)=dspaik(kk)*(entr/epsr)**2
        else
           dspaik(kk)=0.
        endif
  5   continue
	
      goto 4

! 2.2 cas normal: les valeurs de á et lamda sont dans les limites
!     ---------------------------------------------------------
!     dans ce cas le calcul se fait par calcul direct (analytique)
   3  continue

     	fac1=(0.936+0.188*beta2-2.*0.067*slen2)*slen
      fac2=(0.170+0.188*slen2)*beta
	    temp= delta*entr*yg - hxr*dxr*(smdr-yg) - wxr*txr*(smcr-yg)
      nbrxi=nvar(nel,iboat)

      do 1 k=1,nbrxi

        kk=nxit(k,nel,iboat)
        ! xi=delta
          if(kk.eq.1) then       
		!   d(epsa)/dxi  * slen/epsa               
            depsa=0.       
        !   dbeta=d(beta)/dxi			                      
            dbeta=-beta/delta  
        !   dgx  =d(gx) /dxi			                 
            dgx=om   
	    !   domt =d(omt)/dxi                           
            domt=entr             
	    ! dis =d(is) /dxi               
            dis1=entr*(0.25*delta**2+yg**2) + hxr*dxr*(smdr-yg)  &
                                            + wxr*txr*(smcr-yg)
          else if (kk.eq.5) then 
		    ! xi=epsa
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
	    dis1=0.25*dxr*hxr**2 + dxr*(smdr-yg)*(smdr-yg+hxr) &
                           + 2.*wxr*txr*(smcr-yg)
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
	       ! dis =d(is) /dxi      
          dis1=dis1 +2.*dyg* temp                                 
          dslen=slen * (is*domt-omt*dis1) / (2.*omt*is)  + depsa
          ! d(spaik)/dxi  
          dspaik(kk)=-syeq*(fac1*dslen+fac2*dbeta)/(fct**3)     
	  
!       prise en compte de  d(d)/d(epsr) = (d/epsr)**2 
        if (kk.eq.9) then
	     dspaik(kk)=dspaik(kk)*(entr/epsr)**2
	  endif
 
   1  continue
 
! 2.3 impressions
!     ------------
   4  continue

return 
end


! **********************************************************************
! **********************************************************************
! **********************************************************************

subroutine ult(e1,sigy1,delta,epsa,hxr,dxr,wxr,txr,entr,spaik)



implicit double precision(a-h,o-z)
double precision is

!***********************************************************************
!     subroutine ult
!     ===================
!     cette subrourine calcule la résistance ultime d'une plaque raidie
!     londitudinalement et comprimée, également, longitudinalement.
!     (la pression latérael n'est pas prise en compte)
!
!     il s'agit de la formule empirique de j. paik.  qui est basée 
!     sur le principe de la poutre colonne.
!                                                                      
!     entr = d(raid) remplace epsr qui était une approximation (bordé collaborant)
!
!*******************************************************************************

pi=2.d00*asin(1.d00)

!   raidisseurs                                                   
!  -------------                                !
omt =wxr*txr + dxr*hxr + entr*delta       ! section raid + d(raid)
smcr=delta/2.+hxr+txr/2.
smdr=(delta+hxr)/2.
hx  =wxr*txr*smcr + dxr*hxr*smdr          ! moment statique (z=0)
! yg=position du centre de gravité.   
yg=hx/omt               
! is=inertie par rapport         
!    au centre de gravité.                     
is=(entr*delta**3 + dxr*hxr**3 + wxr*txr**3)/12.    &
    + delta*entr*yg*yg  +  hxr*dxr*(smdr-yg)**2     &
                        +  wxr*txr*(smcr-yg)**2
rot =dsqrt(sigy1/e1)
beta=entr * rot / delta
slen=epsa * rot* dsqrt(omt/is) / pi

!
!   test sur beta et lamda=slen (slim= élancement maximum pour un beta donné)
!   ----------------------------------------------------------------------
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

!   calcul de spaik
!   ----------------
beta2=beta*beta

if(hxr.le.0.00001) then
   fct=dsqrt(0.995+0.17*beta2)
else
   slen2=slen*slen
   fct=dsqrt(0.995 + 0.936*slen2 + 0.17*beta2 + 0.188*beta2*slen2  &
                   - 0.067*slen2*slen2)
endif

spaik=sigy1/fct
return 
end


! **********************************************************************
! **********************************************************************
! **********************************************************************

subroutine dsect(nel,hxr,dxr,wxr,txr,epsr,phil,q,da)



use param_section

implicit double precision(a-h,o-z)
dimension da(9)

!***********************************************************************
!     subroutine dsect
!     ==================
!     cette subrourine calcule les dérivées des sections par rapport
!     aux variables de conception (xi).
!     la section de chaque composant (deck, fond, side) est la somme des
!     sections de plusieurs panneaux.
!     la section tarnsversale d'un panneau est composé de
!       - section du bordage (phil*q*delta)
!       - section des raid.  (hxr*dxr+wxr*txr)* phil*q/epsr
!       - section des traverses
!
!     les variables de conception qui interviennent dans ces sections sont:
!      delta et   hxr,dxr,wxr,txr,epsr
!                                                                       
!   inputs : delta,hxr,dxr,wxr,txr,epsr  + q,phil
!                                                                       
!   outputs:
!	da = da +d(a)/dxi
!     avec a  la section d'un panneau inclus dans le composant (deck,fond, ..)
!          xi les variables de conception
!
!     créer : le 11-3-97  par  ph. rigo
!
!     modifications : 
!                               
!     dernière modif : 11-3-97 				     	
!                                                                       
!*************************************************************************

qphil=q*phil*pi/180.

! 1.0 calcul des sensibilités
!     ***********************
nbrxi=nvar(nel,iboat)

do k=1,nbrxi

   kk=nxit(k,nel,iboat)	
   if(kk.eq.1) then                      
!     xi=delta
	    da(kk)=da(kk) + qphil

   else if (kk.eq.6) then                 
   !  xi=hxr
      da(kk)=da(kk) + qphil*dxr/epsr

   else if (kk.eq.7) then                 
   !  xi=dxr=tweb
      da(kk)=da(kk) + qphil*hxr/epsr
   
   else if (kk.eq.8) then                 
   !  xi=wxr =lsem.
      da(kk)=da(kk) + qphil*txr/epsr

   else if (kk.eq.9) then                 
   !  xi=epsr
	    da(kk)=da(kk) - qphil*(hxr*dxr+wxr*txr)/(epsr*epsr)
   endif
        
enddo

return 
end


! ********************************************************************

subroutine ulthull

use param_section


implicit double precision(a-h,o-z)

! ********************************************************************
!     prediction of ultimate hull strength
!     using the analytical formula derived by paik & mansour
! ********************************************************************

write(iu_11(iboat),26) syd,syb,syb1,sysu,sysl,sud,sub,sub1,susu,susl
26  format(/                                                   &
    'equivalent yield stress of deck         (n/m2) ',e14.7/   &
    'equivalent yield stress of outer bottom (n/m2) ',e14.7/   &
    'equivalent yield stress of inner bottom (n/m2) ',e14.7/   &
    'equivalent yield stress of side shell-up(n/m2) ',e14.7/   &
    'equivalent yield stress of side shell-lw(n/m2) ',e14.7//  &
    'ultimate strength of deck               (n/m2) ',e14.7/   &
    'ultimate strength of outer bottom       (n/m2) ',e14.7/   & 
    'ultimate strength of inner bottom       (n/m2) ',e14.7/   &  
    'ultimate strength of upper side shell   (n/m2) ',e14.7/   & 
    'ultimate strength of lower side shell   (n/m2) ',e14.7/) 

if(syb1.ne.0) then
	 temp=sub1/syb1
else
   temp=0.
endif
	
write(iu_11(iboat),27) sud/syd,sub/syb,temp,susu/sysu,susl/sysl
27  format('relative ultimate strength (su/sy)'/    &
    '     - deck             ',f6.4/                &
    '     - outer bottom     ',f6.4/                &
    '     - inner bottom     ',f6.4/                &  
    '     - upper side shell ',f6.4/                & 
    '     - lower side shell ',f6.4/) 

write(iu_11(iboat),36) ab,ab1,ad,as
36  format('properties   of  ',                          &
    'hull   section :'/                                  & 
    'total sectional area of outer bottom (m2) =',e14.7/ &
    'total sectional area of inner bottom (m2) =',e14.7/ &
    'total sectional area of deck         (m2) =',e14.7/ &
    'half-sectional  area of all sides    (m2) =',e14.7//&
    'results of analysis.'/23(1h*))
!--------------------------------------------------------------------

d=depth
	
! 1.0 fully plastic bending moment (eqs 2.a & 3.a for "g" and "mp")
!     ------------------------------
g=d/2.0/as/(sysu+sysl)
g2=ad*syd+2.0*as*sysu-ab*syb
! si g>db : g =(ad*syd+2.0*as*sysu-ab*syb-ab1*syb1 ) *d/2.0/as/(sysu+sysl)
g1=(g2-ab1*syb1) * g
! si g<db : g =(ad*syd+2.0*as*sysu-ab*syb+ab1*syb1 ) *d/2.0/as/(sysu+sysl)
g2=(g2+ab1*syb1) * g

if(g1.le.db.and.g2.ge.db) then
!  g=db (il n'y a pas d'équilibre pour sb1= ñ syb1 )
   g=db
   write(iu_11(iboat),*) ' cas 1: g=db:  g=',g
   fpbm=ad*(d-g)*syd+ab*g*syb        &
       +as/d*(sysu*(d-g)**2+sysl*g*g)
elseif(g1.ge.db) then
!  g=g1 ò db
   g=g1
   write(iu_11(iboat),*) ' cas 2: g>db  :  g=',g
   if(g.ge.d) then
      write(iu_11(iboat),*) 
	    g=d
      write(iu_11(iboat),*) '  g>d alors g=d=creux (sect. deck trop important)'
	 endif
   fpbm=ad*(d-g)*syd+ab*g*syb+ab1*(g-db)*syb1 &
       +as/d*(sysu*(d-g)**2+sysl*g*g)
else
!  g=g2 < db
   g=g2
   write(iu_11(iboat),*) ' cas 3: g<db:  g=',g
   if(g.le.0.001) then
	    g=0.
      write(iu_11(iboat),*) '  g<0 alors g=0 (sect. fond trop important)'
	 endif
   fpbm=ad*(d-g)*syd+ab*g*syb+ab1*(db-g)*syb1 &
       +as/d*(sysu*(d-g)**2+sysl*g*g)
endif

write(iu_11(iboat),706) fpbm,g
706   format(/5x,'fully plastic bending moment  (n.m)  =',e15.7/ &
              5x,'  (g = ',f12.3,' m)')

fpshear= as*(sysu+sysl) / 1.732

write(iu_11(iboat),707) fpshear
707   format(/5x,'fully plastic shear force     (n)    =',e15.7)

! 2.0 neutral axis of hull section
!     ----------------------------
h=(d*(ad+as)+ab1*db)/(ad+ab+ab1+2.0*as)

write(iu_11(iboat),710) h
710   format(/5x,'neutral axis of hull(from bott.)(m) =',f12.3)

! 3.0 elastic section modulus at deck
!     -------------------------------
!     zz=ad*(d-h)**2+ab*h*h+ab1*(h-db)**2+as*d*(2.0*d-3.0*h)/3.0 (as defined by paik)

zz=ad*(d-h)**2 + ab*h*h + ab1*(h-db)**2  &
               +2*as*(d*d/12.+(d/2.-h)**2)
zd=zz/(d-h)

! 3.1 elastic section modulus at bottom
!     ---------------------------------
zb=zz/h

! 3.2 first yield strength
!     ---------------------
dyield=zd*syd
byield=zb*syb

write(iu_11(iboat),806) dyield,byield
806   format(/5x,'first yield bending moment (n.m):', &
             /7x,'- at deck     =',e15.7,             &
             /7x,'- at bottom   =',e15.7)             

      write(iu_11(iboat),711) zd,zb
711   format(/5x,'section modulus, z=i/v (m3):',      &
             /7x,'- at deck      =',e15.7,            &
             /7x,'- at bottom    =',e15.7)

! 4.0 ultimate hull strength
!     =======================
!     if there is no double deck (ab1=0)
if(ab1.lt.0.000001) ab1=0.000001
if(db.lt.0.0001)  db=0.0001
if((ab1/ab).lt.0.001) then
   db=0.000001*d
endif

! 4.1 for sagging condition ("h" from eq.12.b ; "g" from eq.14; "usag" from eq.17.b)
!     ======================
sus=susu
sys=sysl
ss=sus+sys

bb=(ad*sud+2.0*as*sus-ab*syb-ab1*sys)/as/ss
cc=ab1*db/as
h=( bb*d+dsqrt(bb**2*d**2+4.0*cc*d) )/2.

write(iu_11(iboat),*) 
write(iu_11(iboat),*)'     h & hb = high of the linear zone',    &
                     '    (of the vertical stress distribution)' 
write(iu_11(iboat),*)'     - sagging:'

hb=0.
if(h.le.db) then     
!     cas limite nø2 : h < db ou h < 0 : deck too strong or bottom too weak 
!     ---------------
!     we have to recompute h for the correct stress distribution in the inner bottom
!     sb' = stress in the inner bottom = sub1 (ultimate strength of the panel)
   write(iu_14(iboat),*)'subr ushull'
	 write(iu_14(iboat),*)'cas limite nø2 : h < db ou h < 0	       &
                         deck too strong or bottom too weak'

	 h=d*(ad*sud+2.0*as*sus+ab1*sub1-ab*syb)/as/ss
   write(iu_11(iboat),*)'     as  h   = ',h,' <? <  db  = ',db
   
   if(h.le.0.0001) then     
      h=d*1.00e-10
      g=0.
   else
	    g=h*sys/ss
   endif
   
   write(iu_11(iboat),*)'         h   = ',h
   write(iu_11(iboat),*)'         g   = ',g
   usag=-ad*(d-g)*sud - as/d*(d-h)*(d+h-2.0*g)*sus    &
        -ab*g*syb - ab1*(db-g)*sub1                   &
        -as*h/3.0/d*((2.0*h-3.0*g)*sus-(h-3.0*g)*sys)
     
elseif (h.gt.d) then

!  cas limite nø4 : h>d : tensile bottom too strong or compressed deck too weak 
!  ---------------
!  we assume h = 0 and a tensile yield zone is introduced = d-hb

   write(iu_11(iboat),*)'  cas limite nø4 : (h=',h,' > d)'
   write(iu_11(iboat),*)'  as h>d, a tensile zone (d-hb) is required'
      
	 write(iu_14(iboat),*)'subr ushull'											
	 write(iu_14(iboat),*)'cas limite nø4 : h>d	tensile bottom too strong or compressed deck too weak'			

   h=d
!  first, we suppose  d-hb > db (this means large tensile zone)
   hb=(-ad*sud+ab1*syb1+ab*syb+2.0*as*sys)*d/(as*ss)
	 if(hb.le.0.0001) hb=0.0001

   if((d-hb).ge.db) then
!     h>d and  d-hb > db (large tensile yield zone) : cas nø 4.a
!     ----------------------------------------------
      write(iu_11(iboat),*)'   a large tensile zone (d-hb) is valid'
      write(iu_11(iboat),*)'         hb   = ',hb
      write(iu_11(iboat),*)'         d-hb = ',d-hb,' > db  = ',db
      g=hb*sus/ss
      usag=-ad*g*sud - as/d*(d-hb)*(d+hb-2.0*g)*sys            &
           -ab*(d-g)*syb - ab1*(d-g-db)*syb1                   &
           -as*hb/3.0/d*((2.0*hb-3.0*g)*sys-(hb-3.0*g)*sus)
   else
!    h>d and  d-hb < db (small tensile yield zone) : cas nø 4.b
!    ----------------------------------------------
     write(iu_11(iboat),*)'   a small tensile zone (d-hb) is valid'
     bb=-d*(-ad*sud-ab1*sus+2.0*as*sys+ab*syb)/as/ss
     cc=-ab1*d*(d-db)/as
     hb=( -bb + dsqrt(bb*bb-4.0*cc) )/2.
     write(iu_11(iboat),*)'         hb   = ',hb
     write(iu_11(iboat),*)'         d-hb = ',d-hb,' < db  = ',db
     g=hb*sus/ss
     usag=-ad*g*sud - as/d*(d-hb)*(d+hb-2.0*g)*sys                      &
          - ab*(d-g)*syb - ab1*(d-g-db)*(-sus+(sys+sus)*(d-db)/hb)      &
          - as*hb/3.0/d*((2.0*hb-3.0*g)*sys-(hb-3.0*g)*sus)
	 endif

else 
!    normal case : db <h < d
!    ---------------
     g=h*sys/ss
     usag=-ad*(d-g)*sud - as/d*(d-h)*(d+h-2.0*g)*sus                    &
          -ab*g*syb + ab1*(g-db)*(-sys+(db/h)*ss)                       &
          -as*h/3.0/d*((2.0*h-3.0*g)*sus-(h-3.0*g)*sys)

endif

write(iu_11(iboat),*)'         h   = ',h
write(iu_11(iboat),*)'         g   = ',g
write(iu_11(iboat),*)'         hb  = ',hb


! 4.2 for hogging condition ("h" from eq.15.b ; "g" from eq.16; "uhog" from eq.18.b)
!     ======================
sus=susl
sys=sysu
ss=sus+sys

h=ab*sub+ab1*sub1+2.0*as*sus-ad*syd
h=h*d/(as*ss)

write(iu_11(iboat),*)'     - hogging:'

hb=0.
if(h.le.(d*1.00e-10)) then
!  cas limite nø1 : h<0 : deck too strong or too weak bottoms 
!  ---------------
   write(iu_11(iboat),*)'      h=',h,' < 0 , then h=0.0'
        
	 write(iu_14(iboat),*)'cas limite nø1 : h<0 : deck too strong	or too weak bottoms'
	 h=0.
   g=0.
   uhog=ab*d*sub + ab1*(d-db)*sub1 + as*d*sus

elseif(h.gt.(d-db)) then
!  cas limite nø3 : h>d-db : bottom very strong or deck too weak
!  ---------------
!  we have to recompute h for the correct stress distribution in the inner bottom
!  sb' = stress in the inner bottom << sub1 (ultimate strength of the panel)
        
	 write(iu_14(iboat),*)'cas limite nø3 : h>d-db : bottom very strong or deck too weak'	

	 write(iu_11(iboat),*)'  cas limite nø3 : h=',h
   write(iu_11(iboat),*)'    as h>d-db : h is recomputed'
   bb=d*(-ab*sub-2.0*as*sus+ad*syd+ab1*sys)/as/ss
   cc=-ab1*(d-db)*d/as
   h =( -bb + dsqrt(bb**2-4.0*cc) )/2.

   if(h.gt.d) then
   
!     cas limite nø3.a : h>d: tensile yield zone must be introduce = hb 
!     ------------------
	    h=d
      bb=-d*(ad*syd+2.0*as*sys-ab*sub-ab1*sus)/as/ss
      cc=-ab1*db*d/as
      hb=( -bb + dsqrt(bb**2-4.0*cc) )/2.
	    if(hb.le.db) hb=db+0.0001
      write(iu_11(iboat),*)'  cas limite nø3 : h=',h
      write(iu_11(iboat),*)'    as h>d : yield zone hb is introduced'
      g=hb*sus/ss
      uhog=ad*syd*(d-g) + ab*g*sub                             &
         - ab1*(g-db)*(-sus+db*(sys+sus)/hb)                   &
         + as/d*(d-hb)*(d+hb-2.0*g)*sys                        &
         + as*hb/3.0/d*((2.0*hb-3.0*g)*sys-(hb-3.0*g)*sus)

   else
!     cas limite nø3.b : d>h>d-db: h can be used
!     -----------------
      g=h*sys/ss
      uhog=ad*syd*g + ab*(d-g)*sub                             &
         - ab1*(sys-(d-db)*(sys+sus)/h)*(d-db-g)               &
         + as/d*(d-h)*(d+h-2.0*g)*sus                          &
         + as*h/3.0/d*((2.0*h-3.0*g)*sus-(h-3.0*g)*sys)
	 endif
else
!     normal case: 0 < h < d-db 
!     ---------------
   write(iu_14(iboat),*)'subr ushull'					
	 write(iu_14(iboat),*)'normal case: 0 < h < d-db'	
	 
	 g=h*sys/ss
   uhog=ad*syd*g + ab*(d-g)*sub + ab1*sub1*(d-g-db)            &
       +as/d*(d-h)*(d+h-2.0*g)*sus                             &
       +as*h/3.0/d*((2.0*h-3.0*g)*sus-(h-3.0*g)*sys)
endif
      
write(iu_11(iboat),*)'         h   = ',h
write(iu_11(iboat),*)'         g   = ',g
write(iu_11(iboat),*)'         hb  = ',hb

! 4.3 outputs
!     --------

usagr=usag/fpbm
uhogr=uhog/fpbm
   
write(iu_11(iboat),816) usag,usagr,uhog,uhogr
816   format(/5x,'ultimate bending moment (n.m):'/5x,30(1h-)/  &
              7x,'- for sagging:'/                             &
              7x,'     mult.     =',e15.7/                     &
              7x,'     mult./mp  =',f10.5/                     &
              7x,'- for hogging:'/                             &
              7x,'     mult.     =',e15.7/                     &
              7x,'     mult./mp  =',f10.5)


return
end


! ********************************************************************

subroutine dulthull

use param_section


implicit double precision(a-h,o-z)
character*30 vnom2_
!dimension dhb(9,neto)

double precision, allocatable, save :: dhb(:,:)

allocate (dhb(9,neto))


! ********************************************************************
!     calcul des sensibilités liés à la restriction d'ensemble
!     imposée sur la resistance ultime de la poutre navire
!         mult / m(max) < 1  en hogging et en sagging
!
!    créer : 13-mars 1997 dr. ph. rigo
!
!    modified:
! ********************************************************************

write(iu_11(iboat),26) syd,syb,syb1,sysu,sysl,sud,sub,sub1,susu,susl
26    format(/                                                   &
     'equivalent yield stress of deck         (n/m2) ',e14.7/    &
     'equivalent yield stress of outer bottom (n/m2) ',e14.7/    &
     'equivalent yield stress of inner bottom (n/m2) ',e14.7/    &
     'equivalent yield stress of side shell-up(n/m2) ',e14.7/    &
     'equivalent yield stress of side shell-lw(n/m2) ',e14.7//   &
     'ultimate strength of deck               (n/m2) ',e14.7/    &
     'ultimate strength of outer bottom       (n/m2) ',e14.7/    &
     'ultimate strength of inner bottom       (n/m2) ',e14.7/    &
     'ultimate strength of upper side shell   (n/m2) ',e14.7/    &
     'ultimate strength of lower side shell   (n/m2) ',e14.7/) 

if(syb1.ne.0) then
	 temp=sub1/syb1
else
	 temp=0.
endif
	
write(iu_11(iboat),27) sud/syd,sub/syb,temp,susu/sysu,susl/sysl
27  format('relative ultimate strength (su/sy)'/ &
           '     - deck             ',f6.4/      &
           '     - outer bottom     ',f6.4/      &
           '     - inner bottom     ',f6.4/      &
           '     - upper side shell ',f6.4/      &
           '     - lower side shell ',f6.4/) 

write(iu_11(iboat),36) ab,ab1,ad,as
36  format('properties   of  ',                                   &
           'hull   section :'/                                    &
           'total sectional area of outer bottom (m2) =',e14.7/   &
           'total sectional area of inner bottom (m2) =',e14.7/   &
           'total sectional area of deck         (m2) =',e14.7/   &
           'half-sectional  area of all sides    (m2) =',e14.7//  &
           'results of analysis.'/23(1h*))
!--------------------------------------------------------------------

d=depth
	
! 1.0 fully plastic bending moment (eqs 2.a & 3.a for "g" and "mp")
!     ------------------------------
g=d/2.0/as/(sysu+sysl)
g2=ad*syd+2.0*as*sysu-ab*syb
! si g>db : g =(ad*syd+2.0*as*sysu-ab*syb-ab1*syb1 ) *d/2.0/as/(sysu+sysl)
g1=(g2-ab1*syb1) * g
! si g<db : g =(ad*syd+2.0*as*sysu-ab*syb+ab1*syb1 ) *d/2.0/as/(sysu+sysl)
g2=(g2+ab1*syb1) * g

if(g1.le.db.and.g2.ge.db) then
!  g=db (il n'y a pas d'équilibre pour sb1= ñ syb1 )
	 g=db
   write(iu_11(iboat),*) ' cas 1: g=db:  g=',g
   fpbm=ad*(d-g)*syd+ab*g*syb    &
       +as/d*(sysu*(d-g)**2+sysl*g*g)
elseif(g1.ge.db) then
!  g=g1 ò db
   g=g1
   write(iu_11(iboat),*) ' cas 2: g>db  :  g=',g
   if(g.ge.d) then
      write(iu_11(iboat),*) 
	    g=d
      write(iu_11(iboat),*) '  g>d alors g=d=creux (sect. deck trop important)'
	    write(iu_14(iboat),*) '  g>d alors g=d=creux (sect. deck trop important)'		
	 endif
   fpbm=ad*(d-g)*syd+ab*g*syb+ab1*(g-db)*syb1 +as/d*(sysu*(d-g)**2+sysl*g*g)
else
!       g=g2 < db
        g=g2
        write(iu_11(iboat),*) ' cas 3: g<db:  g=',g
        if(g.le.0.001) then
	         g=0.
           write(iu_11(iboat),*) '  g<0 alors g=0 (sect. fond trop important)'
	         write(iu_14(iboat),*) '  g<0 alors g=0 (sect. fond trop important)'			
        endif
        fpbm=ad*(d-g)*syd+ab*g*syb+ab1*(db-g)*syb1  &
            +as/d*(sysu*(d-g)**2+sysl*g*g)
endif

write(iu_11(iboat),706) fpbm,g
706   format(/5x,'fully plastic bending moment  (n.m)  =',e15.7/   &
              5x,'  (g = ',f6.3,' )')

fpshear= as*(sysu+sysl) / 1.732

write(iu_11(iboat),707) fpshear
707   format(/5x,'fully plastic shear force     (n)    =',e15.7)

! 2.0 neutral axis of hull section
!     ----------------------------
h=(d*(ad+as)+ab1*db)/(ad+ab+ab1+2.0*as)

write(iu_11(iboat),710) h
710   format(/5x,'neutral axis of hull(from bott.)(m) =',f12.3)

! 3.0 elastic section modulus at deck
!     -------------------------------
!     zz=ad*(d-h)**2+ab*h*h+ab1*(h-db)**2+as*d*(2.0*d-3.0*h)/3.0 (as defined by paik)

zz=ad*(d-h)**2 + ab*h*h + ab1*(h-db)**2   &
               +2*as*(d*d/12.+(d/2.-h)**2)
zd=zz/(d-h)

! 3.1 elastic section modulus at bottom
!     ---------------------------------
zb=zz/h

! 3.2 first yield strength
!     ---------------------
dyield=zd*syd
byield=zb*syb

! dyield=dyield*1.0e-3
! byield=byield*1.0e-3
      
write(iu_11(iboat),806) dyield,byield
806   format(/5x,'first yield bending moment (n.m):',  &
             /7x,'- at deck     =',e15.7,              &
             /7x,'- at bottom   =',e15.7)

write(iu_11(iboat),711) zd,zb
711   format(/5x,'section modulus, z=i/v (m3):',       &
             /7x,'- at deck      =',e15.7,             &
             /7x,'- at bottom    =',e15.7)

! ---------------------------------------------------------------------
	
! 4.0 ultimate hull strength
!     =======================
!     if there is no double deck (ab1=0)
if(ab1.lt.0.000001) ab1=0.000001
if(db.lt.0.0001)  db=0.0001
if((ab1/ab).lt.0.001) then
   db=0.000001*d
endif
	
! 5.0 for sagging condition ("h" from eq.12.b ; "g" from eq.14; "usag" from eq.17.b)
!     ======================
sus=susu
sys=sysl
ss=sus+sys

write(iu_11(iboat),*) 
write(iu_11(iboat),*)'     h & hb = high of the linear zone (of the vertical stress distribution)- sagging:'

bb = - d*(ad*sud+2.0*as*sus-ab*syb-ab1*sys) /(as*ss)
cc = - ab1*d*db /as
rac= dsqrt(bb*bb-4.0*cc)
h  = (-bb + rac)/2.

do 1 nel=1,neto
   nbrxi=nvar(nel,iboat)
   do 2 k=1,nbrxi
      kk=nxit(k,nel,iboat)
      dcc=cc * ( dab1(kk,nel)/ab1-das(kk,nel)/as )
      dbb=-bb* ( das(kk,nel)/as + dsusu(kk,nel)/ss )           &
          - d* ( ad*dsud(kk,nel)+2.0*as*dsusu(kk,nel)          &
                    +dad(kk,nel)*sud+2.0*das(kk,nel)*sus       &
                   -dab(kk,nel)*syb-dab1(kk,nel)*sys  )/(as*ss) 
	    dh(kk,nel)=-(dbb-(bb*dbb-2.*dcc)/rac)/2.
        
   2  continue
1  continue
 	
hb=0.
if(h.le.db) then     
!  cas limite nø2 : h < db ou h < 0 : deck too strong or bottom too weak 
!  ---------------
!  we have to recompute h for the correct stress distribution in the inner bottom
!  sb' = stress in the inner bottom = sub1 (ultimate strength of the panel)
        
	 write(iu_14(iboat),*)'cas limite nø2'											!sept06		!bug
	 write(iu_14(iboat),*)'h < db ou h < 0 :deck too strong or bottom too weak'		!sept06		!bug

	 h=d*(ad*sud+2.0*as*sus+ab1*sub1-ab*syb)/as/ss
       write(iu_11(iboat),*)'     as  h   = ',h,' <? <  db  = ',db
	 i1=1
   if(h.le.0.0001) then   
	    i1=0
      h=d*1.00e-10
      g=0.
   else
	    g=h*sys/ss
   endif
   write(iu_11(iboat),*)'         h   = ',h
   write(iu_11(iboat),*)'         g   = ',g

   usag=-ad*(d-g)*sud - as/d*(d-h)*(d+h-2.0*g)*sus        &
        -ab*g*syb - ab1*(db-g)*sub1                       &
        -as*h/3.0/d*((2.0*h-3.0*g)*sus-(h-3.0*g)*sys)     

!  calcul de dh/dxi, dg/dxi et dusag/dxi
   im=0
   do 46 nel=1,neto
      nbrxi=nvar(nel,iboat)
      do 47 k=1,nbrxi
         kk=nxit(k,nel,iboat)
	     	   
         if(i1.le.0) then   
	          dg(kk,nel)=0.
	          dh(kk,nel)=0.
	       else
	          dh(kk,nel)=-h * ( das(kk,nel)/as + dsusu(kk,nel)/ss )      &
            +d*(ad*dsud(kk,nel)+2.0*as*dsusu(kk,nel)+ab1*dsub1(kk,nel) &
            +dad(kk,nel)*sud+2.0*das(kk,nel)*sus+dab1(kk,nel)*sub1     &
            -dab(kk,nel)*syb  )/(as*ss)                                
	          dg(kk,nel)=(dh(kk,nel)-g*dsusu(kk,nel)/sys)*g/h
         endif

         dult(im+k)=- ad * (d-g) * dsud(kk,nel)                           &
            - (-ad*dg(kk,nel)+dad(kk,nel)*(d-g))*sud                      &
            - (dab(kk,nel)*g+ab*dg(kk,nel))*syb                           &
            - (dab1(kk,nel)*(db-g)-ab1*dg(kk,nel))*sub1                   &
            -  ab1*(db-g)*dsub1(kk,nel)                                   &
            - (-as*dh(kk,nel)+das(kk,nel)*(d-h))*(d+h-2.0*g)*sus/d        &
            -  as*(d-h)/d*                                                &
              ((dh(kk,nel)-2.0*dg(kk,nel))*sus+(d+h-2.*g)*dsusu(kk,nel))  &
            - (as*dh(kk,nel)+das(kk,nel)*h)                               &
            * ((2.0*h-3.0*g)*sus-(h-3.0*g)*sys)/(3.0*d)                   &
            - ( (2.0*dh(kk,nel)-3.0*dg(kk,nel))*sus                       &
            + (2.*h-3.0*g)*dsusu(kk,nel)                                  &
            - (dh(kk,nel)-3.0*dg(kk,nel))*sys)*as*h/(3.0*d) 
          imm=im+k
  47  continue
      im=im+nbrxi
46 continue



elseif (h.gt.d) then
!  cas limite nø4 : h>d : tensile bottom too strong or compressed deck too weak 
!  ---------------
!  we assume h = 0 and
!  a tensile yield zone is introduced = d-hb
         
	 write(iu_14(iboat),*)'subr ushull'											
	 write(iu_14(iboat),*)'cas limite nø4 : h>d	tensile bottom too strong or compressed deck too weak'			

	 write(iu_11(iboat),*)'  cas limite nø4 : (h=',h,' > d)'
   write(iu_11(iboat),*)'     as h>d, a tensile zone (d-hb) is required'
   h=d
!  first, we suppose  d-hb > db (this means large tensile zone)
   hb=(-ad*sud+ab1*syb1+ab*syb+2.0*as*sys)*d/(as*ss)
	 if(hb.le.0.0001) hb=0.0001

   if((d-hb).ge.db) then

!     h>d and  d-hb > db (large tensile yield zone) : cas nø 4.a
!     ----------------------------------------------
      write(iu_11(iboat),*)'   a large tensile zone (d-hb) is valid'
      write(iu_11(iboat),*)'         hb   = ',hb
      write(iu_11(iboat),*)'         d-hb = ',d-hb,' > db  = ',db
      g=hb*sus/ss
    
      usag=-ad*g*sud - as/d*(d-hb)*(d+hb-2.0*g)*sys            &
           -ab*(d-g)*syb - ab1*(d-g-db)*syb1                   &
           -as*hb/3.0/d*((2.0*hb-3.0*g)*sys-(hb-3.0*g)*sus)
    
!     calcul de dh/dxi, dg/dxi et dusag/dxi
      im=0
      do nel=1,neto
         nbrxi=nvar(nel,iboat)
            do k=1,nbrxi
               kk=nxit(k,nel,iboat)
               
	             dhb(kk,nel)=-hb * ( das(kk,nel)/as + dsusu(kk,nel)/ss )        &
                           +d*(-ad*dsud(kk,nel)                               &
                           -dad(kk,nel)*sud+dab1(kk,nel)*syb1                 &
                           +dab(kk,nel)*syb+2.0*das(kk,nel)*sys  )/(as*ss)    
	             dg(kk,nel)=(dhb(kk,nel)/hb+dsusu(kk,nel)*(1.0-g/hb)/sus)*g
               
               dult(im+k)=- ad * g * dsud(kk,nel)                                       &
                          - (ad*dg(kk,nel)+dad(kk,nel)*g) * sud                         &
                          - (dab(kk,nel)*(d-g)-ab*dg(kk,nel)) * syb                     &
                          - (dab1(kk,nel)*(d-g-db)-ab1*dg(kk,nel))*syb1                 &
                          - (-as*dhb(kk,nel)+das(kk,nel)*(d-hb)) * (d+hb-2.0*g)*sys/d   &
                          -  as*(d-hb)/d * (dhb(kk,nel)-2.0*dg(kk,nel))*sys             &
                          - (as*dhb(kk,nel)+das(kk,nel)*hb)                             &
                          * ((2.0*hb-3.0*g)*sys-(hb-3.0*g)*sus)/(3.0*d)                 &
                          - ((2.0*dhb(kk,nel)-3.0*dg(kk,nel))*sys                       &
                          - (hb-3.0*g)*dsusu(kk,nel)                                    &
                          - (dhb(kk,nel)-3.0*dg(kk,nel))*sus)*as*hb/(3.0*d)             
               imm=im+k
            enddo
         im=im+nbrxi
      enddo
    
	 else
!     h>d and  d-hb < db (small tensile yield zone) : cas nø 4.b
!     ----------------------------------------------
      write(iu_11(iboat),*)'   a small tensile zone (d-hb) is valid'
      bb=-d*(-ad*sud-ab1*sus+2.0*as*sys+ab*syb)/as/ss
      cc=-ab1*d*(d-db)/as
	    rac=dsqrt(bb*bb-4.0*cc)
      hb=( -bb + rac )/2.
      write(iu_11(iboat),*)'         hb   = ',hb
      write(iu_11(iboat),*)'         d-hb = ',d-hb,' < db  = ',db
      g=hb*sus/ss
	    
      usag=-ad*g*sud - as/d*(d-hb)*(d+hb-2.0*g)*sys                   &
           - ab*(d-g)*syb - ab1*(d-g-db)*(-sus+(sys+sus)*(d-db)/hb)   &
           - as*hb/3.0/d*((2.0*hb-3.0*g)*sys-(hb-3.0*g)*sus)
    
!     calcul de dh/dxi, dg/dxi et dusag/dxi
      im=0
      do nel=1,neto
         nbrxi=nvar(nel,iboat)
         do k=1,nbrxi
            kk=nxit(k,nel,iboat)
	             
            dcc=cc * ( dab1(kk,nel)/ab1-das(kk,nel)/as )
	          dbb=-bb* ( das(kk,nel)/as + dsusu(kk,nel)/ss )        &
                - d* ( ad*dsud(kk,nel)-ab1*dsusu(kk,nel)          &
                -dad(kk,nel)*sud-dab1(kk,nel)*sus                 &
                +2.0*das(kk,nel)*sys+dab(kk,nel)*syb  )/(as*ss)
            
	          dhb(kk,nel)=-(dbb-(bb*dbb-2.*dcc)/rac)/2.
	          dg(kk,nel)=(dhb(kk,nel)/hb+dsusu(kk,nel)*(1.0-g/hb)/sus)*g
            
            dult(im+k)=- ad * g * dsud(kk,nel)                                   &
                  - (ad*dg(kk,nel)+dad(kk,nel)*g) * sud                          &
                  - (dab(kk,nel)*(d-g)-ab*dg(kk,nel)) * syb                      &
                  - (dab1(kk,nel)*(d-g-db)-ab1*dg(kk,nel))                       &
                  * (-sus+(d-db)*ss/hb)                                          &
                  - ab1*(d-g-db) *                                               &
                    (-(hb-d+db)*dsusu(kk,nel)/hb-(d-db)*ss*dhb(kk,nel)/(hb*hb))  &
                  - (-as*dhb(kk,nel)+das(kk,nel)*(d-hb)) * (d+hb-2.0*g)*sys/d    &
                  -  as*(d-hb)/d * (dhb(kk,nel)-2.0*dg(kk,nel))*sys              &
                  - (as*dhb(kk,nel)+das(kk,nel)*hb)                              &
                  * ((2.0*hb-3.0*g)*sys-(hb-3.0*g)*sus)/(3.0*d)                  &
                  - ( (2.0*dhb(kk,nel)-3.0*dg(kk,nel))*sys                       &
                   -(hb-3.0*g)*dsusu(kk,nel)                                     &
                   -(dhb(kk,nel)-3.0*dg(kk,nel))*sus  ) * as*hb/(3.0*d) 
            imm=im+k
         enddo
         im=im+nbrxi
         
      enddo          
	 endif
                          
else 

!  normal case : db < h < d
!  ------------------------
   g=h*sys/ss
   usag=-ad*(d-g)*sud - as/d*(d-h)*(d+h-2.0*g)*sus     &
        -ab*g*syb + ab1*(g-db)*(-sys+(db/h)*ss)        &
        -as*h/3.0/d*((2.0*h-3.0*g)*sus-(h-3.0*g)*sys)
         
!  calcul de dg/dxi et dusag/dxi
   im=0
   do nel=1,neto
      nbrxi=nvar(nel,iboat)
      do k=1,nbrxi
      kk=nxit(k,nel,iboat)
   
      dcc= cc * ( dab1(kk,nel)/ab1- das(kk,nel)/as )
	    dbb=-bb * ( das(kk,nel)/as  + dsusu(kk,nel)/ss )                &
               - d * (+ad*dsud(kk,nel)+2.0*as*dsusu(kk,nel)           &
                      +dad(kk,nel)*sud+2.0*das(kk,nel)*sus            &
                      -dab(kk,nel)*syb-dab1(kk,nel)*sys   )/(as*ss)
    
	    dh(kk,nel)=-(dbb-(bb*dbb-2.*dcc)/rac)/2.
	    dg(kk,nel)=(dh(kk,nel)-g*dsusu(kk,nel)/sys)*g/h
    
      dult(im+k)=- ad * (d-g) * dsud(kk,nel)                               &
            - (-ad*dg(kk,nel)+dad(kk,nel)*(d-g)) * sud                     &
            - (dab(kk,nel)*g+ab*dg(kk,nel)) * syb                          &
            + (dab1(kk,nel)*(g-db)+ab1*dg(kk,nel))*(-sys+db*ss/h)          &
            +  ab1*(g-db) *                                                &
                 (dsusl(kk,nel)*db - ss*db*dh(kk,nel)/h) /h                &
            - (-as*dh(kk,nel)+das(kk,nel)*(d-h)) * (d+h-2.0*g)*sus/d       &
            -  as*(d-h)/d *                                                &
              ((dh(kk,nel)-2.0*dg(kk,nel))*sus+(d+h-2.*g)*dsusu(kk,nel))   &
            - (as*dh(kk,nel)+das(kk,nel)*h)                                &
                     *((2.0*h-3.0*g)*sus-(h-3.0*g)*sys)/(3.0*d)            &
            - ( (2.0*dh(kk,nel)-3.0*dg(kk,nel))*sus                        &
                +(2.*h-3.0*g)*dsusu(kk,nel)                                &
                -(dh(kk,nel)-3.0*dg(kk,nel))*sys  ) * as*h/(3.0*d) 
    
      imm=im+k
   enddo
   im=im+nbrxi
enddo

endif

write(iu_11(iboat),*)'         h   = ',h
write(iu_11(iboat),*)'         g   = ',g
write(iu_11(iboat),*)'         hb  = ',hb

!     sauvetage sur disque,  file iu_26(iboat)		
!     -------------------------------
!     sagging: usag < 0 et usag (max) < 0
!     restriction : c(x)= usag(max)/usag(ult) < 1
!        dc(x)/dxi= - c(x)/usag(ult) * dusag(ult)/dxi

vnom2_='ult. sagging'
c=usagm/usag
cm=1.

do 48 i=1,ntot(iboat)
   dult(i)= - (c/usag) * dult(i)
48 continue


kcontr = kcontr+1
do i=1,nbrxi
	cijopt(i+ncont,kcontr,iboat)=dult(i)
enddo
cjopt(kcontr,iboat) = c
cjmopt(kcontr,iboat) = cm
vnom2(kcontr,iboat) = vnom2_

!write(iu_26(iboat),*) c
!write(iu_26(iboat),*) cm
!write(iu_26(iboat),*) vnom2_			
!write(iu_26(iboat),*) (dult(i),i=1,ntot(iboat))


!     ============================================================================
! 6.0 for hogging condition ("h" from eq.15.b ; "g" from eq.16; "uhog" from eq.18.b)
!     ----------------------------------------------------------------------------
!     ============================================================================
sus=susl
sys=sysu
ss=sus+sys

write(iu_11(iboat),*)'     - hogging:'

! calcul de h (pour le cas normal)	
h=(ab*sub+ab1*sub1+2.0*as*sus-ad*syd) * d/(as*ss)	
hb=0.

if(h.le.(d*1.00e-10)) then
!  6.1 cas limite nø1 : h<0 : deck too strong or too weak bottoms 
!   ---------------
   write(iu_14(iboat),*)'cas limite nø1 : h<0'									
	 write(iu_14(iboat),*)'deck too strong or too weak bottoms'		
	  
	 write(iu_11(iboat),*)'      h=',h,' < 0 , then h=0.0'
   h=0.
   g=0.

!  calcul de uhog	
   uhog=ab*d*sub + ab1*(d-db)*sub1 + as*d*sus
 
!  calcul de duhog/dxi	
   im=0
   do nel=1,neto
   nbrxi=nvar(nel,iboat)
        do k=1,nbrxi
          kk=nxit(k,nel,iboat)
          dult(im+k)=  d * ( dab(kk,nel) *sub  + ab *dsub(kk,nel)  )  &
                 + (d-db)* ( dab1(kk,nel)*sub1 + ab1*dsub1(kk,nel) )  &
                    +  d * ( das(kk,nel) *sus  + as *dsusl(kk,nel) )  
        imm=im+k
        enddo
        im=im+nbrxi
   enddo
	  
elseif(h.gt.(d-db)) then

!  6.2 cas limite nø3 : h>d-db : bottom very strong or deck too weak
!  ---------------
!  we have to recompute h for the correct stress distribution in the inner bottom
!  sb' = stress in the inner bottom << sub1 (ultimate strength of the panel)

   write(iu_14(iboat),*)'subr ushull'										
   write(iu_14(iboat),*)'as limite nø3 : h>d-db	bottom very strong or deck too weak'						

   write(iu_11(iboat),*)'  cas limite nø3 : h=',h
   write(iu_11(iboat),*)'   as h>d-db : h is recomputed'

!  nouveau calcul du h
   bb=d*(-ab*sub-2.0*as*sus+ad*syd+ab1*sys)/as/ss
   cc=-ab1*(d-db)*d/as
   rac= dsqrt(bb*bb-4.0*cc)
   h =( -bb + rac )/2.

   if(h.gt.d) then 
   
!  6.2.1 cas limite nø3.a : h>d: a tensile yield zone must be introduce = hb 
!  -----------------------

      write(iu_11(iboat),*)'  cas limite nø3.a : h=',h
      write(iu_11(iboat),*)'   as h>d : yield zone hb is introduced'
      h=d

!     calcul du hb et du g

      bb=-d*(ad*syd+2.0*as*sys-ab*sub-ab1*sus)/as/ss
      cc=-ab1*db*d/as
	    rac= dsqrt(bb*bb-4.0*cc)
      hb =( -bb + rac )/2.
      write(iu_11(iboat),*)'   hb  = ',hb
	    if(hb.le.db) hb=db+0.0001
      g=hb*sus/ss

!     calcul du uhog
      uhog=ad*syd*(d-g) + ab*g*sub                         &
         - ab1*(g-db)*(-sus+db*ss/hb)                      &
         + as/d*(d-hb)*(d+hb-2.0*g)*sys                    &
         + as*hb/3.0/d*((2.0*hb-3.0*g)*sys-(hb-3.0*g)*sus)
     
!     calcul du dh/dxi, dg/dxi et duhog/dxi 
      im=0
      do nel=1,neto
         nbrxi=nvar(nel,iboat)
         do k=1,nbrxi
            kk=nxit(k,nel,iboat)

            dcc= cc * ( dab1(kk,nel)/ab1- das(kk,nel)/as )        
	          dbb=-bb * ( das(kk,nel)/as  + dsusl(kk,nel)/ss )       &
                - d * (-ab*dsub(kk,nel)-ab1*dsusl(kk,nel)          &
                   +dad(kk,nel)*syd+2.0*das(kk,nel)*sys            &
                   -dab(kk,nel)*sub-dab1(kk,nel)*sus   )/(as*ss)

	          dhb(kk,nel)=-(dbb-(bb*dbb-2.*dcc)/rac)/2.
            dg(kk,nel)=dhb(kk,nel)*g/hb + dsusl(kk,nel)*g*(1.-g/hb)/sus

           dult(im+k)=(-ad*dg(kk,nel)+dad(kk,nel)*(d-g)) * syd            &
            + (dab(kk,nel)*g+ab*dg(kk,nel))*sub + ab*g*dsub(kk,nel)       &
            - (dab1(kk,nel)*(g-db)+ab1*dg(kk,nel))*(-sus+db*ss/hb)        &
            -  ab1*(g-db) *                                               &
                 (dsusl(kk,nel)*(db/hb-1.) - ss*db*dhb(kk,nel)/(hb*hb))   &
            + (-as*dhb(kk,nel)+das(kk,nel)*(d-hb)) * (d+hb-2.0*g)*sys/d   &
            +  as*(d-hb) * (dhb(kk,nel)-2.0*dg(kk,nel)) *sys/d            &
            + (as*dhb(kk,nel)+das(kk,nel)*hb)                             &
                     *((2.0*hb-3.0*g)*sys-(hb-3.0*g)*sus)/(3.0*d)         &
            + ( (2.0*dhb(kk,nel)-3.0*dg(kk,nel))*sys                      &
                -(hb-3.0*g)*dsusl(kk,nel)                                 &
                -(dhb(kk,nel)-3.0*dg(kk,nel))*sus  ) * as*hb/(3.0*d)      
         enddo
         imm=im+k                                                       
                                                            
           im=im+nbrxi
      enddo

	 else

! 6.2.2 cas limite nø3.b : d>h>d-db: h can still be used
! ----------------------
      g=h*sys/ss
      write(iu_11(iboat),*)' cas 3.b : d>h>d-db'
      uhog=ad*syd*g + ab*(d-g)*sub                 &
         - ab1*(sys-(d-db)*(sys+sus)/h)*(d-db-g)   &
         + as/d*(d-h)*(d+h-2.0*g)*sus              &
         + as*h/3.0/d*((2.0*h-3.0*g)*sus-(h-3.0*g)*sys)

!     calcul du dh/dxi , dg/dxi et  duhog/dxi 
      im=0
      do nel=1,neto
         nbrxi=nvar(nel,iboat)
         do k=1,nbrxi
            kk=nxit(k,nel,iboat)
            dcc=cc * ( dab1(kk,nel)/ab1-das(kk,nel)/as )
	          dbb=-bb* ( das(kk,nel)/as + dsusl(kk,nel)/ss )       &
                + d* (-ab*dsub(kk,nel) -2.0*as*dsusl(kk,nel)       &
                      -dab(kk,nel)*sub -2.0*das(kk,nel)*sus        &
                      +dab1(kk,nel)*sys+dad(kk,nel)*syd   )/(as*ss)
            
	          dh(kk,nel)=-(dbb-(bb*dbb-2.*dcc)/rac)/2.
            dg(kk,nel)= (dh(kk,nel)-g*dsusl(kk,nel)/sys)*g/h
            
            dult(im+k)=                                                 &
              (ad*dg(kk,nel)+dad(kk,nel)*g) * syd                       &
            + (dab(kk,nel)*(d-g)-ab*dg(kk,nel)) * sub                   &
            +  ab*(d-g)*dsub(kk,nel)                                    &
            - (dab1(kk,nel)*(d-g-db)-ab1*dg(kk,nel))*(sys-(d-db)*ss/h)  &
            -  ab1*(d-g-db)*(d-db)*(ss*dh(kk,nel)/h-dsusl(kk,nel))/h    &
            + (-as*dh(kk,nel)+das(kk,nel)*(d-h)) * (d+h-2.0*g)*sus/d    &
            +  as*(d-h) * ( (dh(kk,nel)-2.0*dg(kk,nel))*sus+            &
                            (d+h-2.0*g)*dsusl(kk,nel)   )/d             &
            + (as*dh(kk,nel)+das(kk,nel)*h)                             &
                     *((2.0*h-3.0*g)*sus-(h-3.0*g)*sys)/(3.0*d)         &
            + ( (2.0*dh(kk,nel)-3.0*dg(kk,nel))*sus+                    &
                (2.0*h-3.0*g)*dsusl(kk,nel)-                            &
                (dh(kk,nel)-3.0*dg(kk,nel))*sys  ) * as*h/(3.0*d)       
            imm=im+k                                                    
         enddo
         im=im+nbrxi
      enddo
	 endif
	  
else
!  6.3 normal case: 0 < h < d-db 
!  ---------------
   g=h*sys/ss

!  calcul de uhog	
   uhog=ad*syd*g + ab*(d-g)*sub + ab1*sub1*(d-g-db)  &
       +as/d*(d-h)*(d+h-2.0*g)*sus                   &
       +as*h/3.0/d*((2.0*h-3.0*g)*sus-(h-3.0*g)*sys)

!  calcul de dh/dxi, dg/dxi  et duhog/dxi	
   im=0
   do nel=1,neto
      nbrxi=nvar(nel,iboat)
      do k=1,nbrxi
         kk=nxit(k,nel,iboat)
	       dh(kk,nel)=-h * ( das(kk,nel)/as + dsusl(kk,nel)/ss )            &
            + d*( ab*dsub(kk,nel)+ab1*dsub1(kk,nel)+2.0*as*dsusl(kk,nel)  &
                  +dab(kk,nel)*sub+dab1(kk,nel)*sub1+2.0*das(kk,nel)*sus  &
                  -dad(kk,nel)*syd  )/(as*ss)

         dg(kk,nel)=(dh(kk,nel)-g*dsusl(kk,nel)/sys) * g/h
         dult(im+k)=                                                     & 
            (ad*dg(kk,nel)+dad(kk,nel)*g) * syd                          &
          + (dab(kk,nel)*(d-g)-ab*dg(kk,nel)) * sub                      &
          +  ab*(d-g)*dsub(kk,nel)                                       &
          + (dab1(kk,nel)*(d-g-db)-ab1*dg(kk,nel)) * sub1                &
          +  ab1*(d-g-db)*dsub1(kk,nel)                                  &
          + (-as*dh(kk,nel)+das(kk,nel)*(d-h)) * (d+h-2.0*g)*sus/d       &
          +  as*(d-h) * ( (dh(kk,nel)-2.0*dg(kk,nel))*sus+               &
                          (d+h-2.0*g)*dsusl(kk,nel)   )/d                &
          + (as*dh(kk,nel)+das(kk,nel)*h)                                &
                   *((2.0*h-3.0*g)*sus-(h-3.0*g)*sys)/(3.0*d)            &
          + ( (2.0*dh(kk,nel)-3.0*dg(kk,nel))*sus+                       &
              (2.0*h-3.0*g)*dsusl(kk,nel)-                               &
              (dh(kk,nel)-3.0*dg(kk,nel))*sys  ) * as*h/(3.0*d)          
          imm=im+k
      enddo

      im=im+nbrxi
   enddo

endif

write(iu_11(iboat),*)'         h   = ',h
write(iu_11(iboat),*)'         g   = ',g
write(iu_11(iboat),*)'         hb  = ',hb

! sauvetage sur disque,  file iu_26(iboat)			
! -------------------------------
! hogging: uhog > 0 et uhog (max) > 0
! restriction : c(x)= uhog(max)/uhog(ult) < 1
!    dc(x)/dxi= - c(x)/uhog(ult) * duhog(ult)/dxi


vnom2_='ult. hogging'
c=uhogm/uhog
cm=1.

do i=1,ntot(iboat)
   dult(i)= - (c/uhog) * dult(i)
enddo

!write(iu_26(iboat),*) c
!write(iu_26(iboat),*) cm
!write(iu_26(iboat),*) vnom2_				
!write(iu_26(iboat),*) (dult(i),i=1,ntot(iboat))



! 7.0 outputs
! -----------
usagr=usag/fpbm
uhogr=uhog/fpbm

write(iu_11(iboat), 816) usag,usagr,uhog,uhogr
816   format(/'ultimate bending moment (n.m):'/30(1h-)/                        &
              '- for sagging:','     mult. =',e15.7,'     mult./mp  =',f10.5/  &
              '- for hogging:','     mult. =',e15.7,'     mult./mp  =',f10.5)   

deallocate (dhb)

return
end
