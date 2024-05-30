subroutine hughes(iprint_,chamax,ploc1,e1,eta1,syldp,sigm1,              &
                  phil,q,mt,               &
                  eff,tplmin,tpla,tplr,tpld,                            &
                  idd,iaa,irr,istop,nel,ioption,itera_,               &
                  effcomb,sigg)

use param_section

implicit double precision(a-h,o-z)
character*10 typ(3)
character*18 posit
dimension var(2),sx(5),sy(5),tau(5),eff(9690)
dimension sxcomb(5),sycomb(5),taucomb(5),effcomb(9690)
dimension chamax(5),xp(5)
dimension gv1(2),gv2(2)
data typ/'yielding','buckling','undefined'/

!***********************************************************************
!     subroutine hughes 
!     =================
!
!  ioption = 1 : avec calcul des sensibilités (si restriction d'optimisation)
!  ioption = 0 : sans calcul des sensibilités (uniquement calcul du dmin)
!
!     subroutine de calcul de l'épaisseur minimale d'un plaque non raidie 
!     sollicitée dans son plan (in plane loads) et par une pression latérale.
!
!     l'épaisseur minimale est calculée de 2 manières (la + grande est retenue):
!      - subr.  pltben 
!        il s'agit d'un simple analyse statique élastique.
!        cette subroutine n'intègre pas de vérification au voilement du
!        panneau étudié.
!        seul l'effet de magnification des contraintes est considéré mais
!        indépendamment d'un risque de voilement.
!        cette subroutine est une adaptation du programme de o. hughes 
!	   (ref : sname, "ship structure design", chap.9: plate bending.)
!          (last revision: 21 march 1993).
!        safety on load (=sy/smax).
!      - subr.  buckle
!        cette subroutine intègre une vérification au voilement du panneau
!        étudié soumis à sx, sy et tau
!        elle sert au calcul de l'épaisseur minimale requise pour éviter
!        le voilement.
!        elle a été développée par dr. ph. rigo, anast, ulg (1996)
!        safety on load (=sy/smax) and safety on strength =1.10
!
!  output: tplmin = epaisseur minimale
!          tpla   = dérivée de tplmin selon epsa
!          tplr   = dérivée de tplmin selon epsr avec epsr= fct(d=entr)
!          tpld   = dérivée de tplmin selon delta
!
!    créer : avril 96  pour lbr-5 (janvier 1996), anast par le dr. ph. rigo	
!    -------
!    modif : 22 mai 96 : epaiss. minimale pour le  voilement; subr. buckle
!    ------  31 mai 96 : dérivée de dmin par delta calculée par différence
!                        finie
!            6 mars 97 : introduction de d(raid) c.à.d. entr non égal à epsr
!                         d(c)/d(epsr)= d(c)/d(d) * (d/epsr)**2
!           16 juil 97 : correction subr. buckle , rs= ... *alpha2**2
!            6 déc  99   problème de convergence avec plsolv
!           13 janv 02 : avec et sans calcul des sensibilités (ioption),
!                        impression des contraintes critiques obtenues.
!             mai 2003 : combinaison  sx(resul)  +  sx(stiff)  (f. bair)   !avril2003
!            juin 2003 : combinaison isx(resul)i + isx(stiff)i (f. bair)   !juin2003
!           
!   dernière modif: 26 nov 2003
!   --------------
!*************************************************************************

if (dabs(panneau(nel).hight).lt.0.005) return


ispecial = 0 !0=> calcul normal; 1 = calcul en DIS(2)
!!! certaines boucles sont supprimées
!if (ispecial.eq.0) then
!	ix = 5
!else
!	ix = 2
!endif

tpl = 0.d00
dtpl = 0.d00
tpla = 0.d00
tplr = 0.d00
tpld = 0.d00

piq=pi*q/180.0d00
jprint=iprint_  ! impression standard : iprint= 1 si iopti=3

hya = panneau(nel).hya
epsa = panneau(nel).epsa
epsa2 = panneau(nel).epsa2
hya2 = panneau(nel).hya2
hxr = panneau(nel).hxr
epsr = panneau(nel).epsr
entr = panneau(nel).entr
entr2 = panneau(nel).entr2
delta = panneau(nel).delta

!***********************************************************************
!***********************************************************************
!               Œ y et sigma y
!               i
!               *****************************
!               *                           *  b=xlen
!               *                           *
!               *                           *
!               ***************************** --------> x et sigma x
!               <----------- a = ylen ------>
!
!  1.0 données à déterminer
!  ************************
!      xlen,ylen        = a,b       (m)
!      syldp            = contr. elastique  (n/m2), (ex, re=240 n/mm2)
!      sigm1             = contr. admissible (n/m2)
!      xp               = p         (m d'eau)
!      sigbx,sigby,tau  = sx,sy,tau (n/m2)
!
!  1.1 gammas, le coef de sécurité sur les charges (secu ds buckle et pltben)
!  =========================================================================

!gammas = syldp/sigm1

gammas=1.1
!if (is.eq.1.or.is.eq.2) then
!	gammas = 1.1
	!sigm1 = 413.6
!	sigm1 = 322.7
!elseif (is.eq.3.or.is.eq.4.or.is.eq.5) then
!	gammas = 1.35
	!sigm1 = 337.
!	sigm1 = 262.9
!elseif (is.eq.6.or.is.eq.7) then
!	gammas = 1.
	!sigm1 = 455.
!	sigm1 = 355
!endif



!gammas = 1.
!sigm1 = 355

!gammas = 1. !3 ! Vaut ptêt mieux prendre un coefficient fixe...

if (gammas.gt.1.5.and.ioption.eq.1) then
	if (langue.eq.1) then
		write(*,*) 'Attention ! Dans Hughes coeff de securite superieur a 1.5 !'
		write(*,*) 'Par experience, un coeff trop eleve peut conduire a des difficultes pour'
		write(*,*) ' l''optimiseur a satisfaire la restriction de Hughes'
	elseif (langue.eq.2) then
		write(*,*) 'Attention! In subroutine Hughes security coeff > 1.5!'
		write(*,*) 'This could lead to some difficulties for the optimizer to solve the problem'
	endif
endif

! en plus, il y a "the safety on strength" = 1.10 dans buckle

!  1.2 xlen,ylen  les dimensions (a et b) du panneau
!  ==================================================

!xlen=epsa
!ylen=entr
ia=0 !=1 => pas de calcul de dérivée
ir=0

if ((epsa2.gt.0.001).and.(epsa2.lt.epsa).and.(hya2.gt.0.001)) then
	write(iu_12(iboat),*)'warning: sbr. hughes : on prend les cadres secondaires !!!'
	write(iu_12(iboat),*) 'Epsa2 =',epsa2
	write(iu_12(iboat),*)'Panneau :',nel
	write(iu_14(iboat),*)'warning: sbr. hughes : on prend les cadres secondaires !!!'
	write(iu_14(iboat),*)'Panneau :',nel
	write(iu_14(iboat),*) 'Epsa2 =',epsa2
	xlen=epsa2
	ia=1
else
	xlen=epsa
	if((hya/delta).le.2) then
		ia=1
		xlen=width
		write(iu_12(iboat),*)'warning: sbr. hughes : cadres trop petits !!!'
		write(iu_12(iboat),*)'on prend portée de la plaque=width=',width,' (m)',  &
                        ' au lieu de l''entredistance entre cadres.'
		write(iu_14(iboat),*)'warning: sbr. hughes : cadres trop petits !!!'		
		write(iu_14(iboat),*)'on prend portée de la plaque=width=',width,' (m)',  &
                        ' au lieu de l''entredistance entre cadres.'	
	else
		if(epsa.gt.width) then
			ia=1
			xlen=width
		endif
		ia=0
	endif
endif

if ((panneau(nel).hxr2.gt.0.001).and.(entr2.lt.entr)) then
	write(iu_12(iboat),*)'warning: sbr. hughes : on prend les raidisseurs secondaires !!!'
	write(iu_12(iboat),*)'Panneau :',nel
	write(iu_14(iboat),*)'warning: sbr. hughes : on prend les raidisseurs secondaires !!!'
	write(iu_14(iboat),*)'Panneau :',nel
	ylen=entr2
	ia = 1
else
	ylen=entr
	ir = 0

	haut=q*phil*pi/180.

	if((hxr/delta).le.2) then
		ir=1
		ylen=haut
		write(iu_12(iboat),'(a,i3,a)') 'panel n°',nel,                     &
                      ' raidisseurs trop petits !!! (sbr. hughes)'
		write(iu_14(iboat),'(a,i3,a)') 'panel n°',nel,									    &	
                      ' raidisseurs trop petits !!! (sbr. hughes)'			
		if(mt.gt.0) then
			!ir=0
			ya=0.d00
			y1=panneau(nel).abtr(1)*piq
			yt=y1-ya
			ylen=yt

			do i=1,mt-1
				y1=panneau(nel).abtr(i)*piq
				y2=panneau(nel).abtr(i+1)*piq
				yt=y2-y1
				if(ylen.lt.yt) ylen=yt
			end do

			ymt=panneau(nel).abtr(mt)*piq
			yb=haut
			yt=yb-ymt
			if(ylen.lt.yt) ylen=yt
			write(iu_12(iboat),'(2a,f8.4,a)') 'il y a des traverses alors ',  &
				'on prend l''entredistance max = ',ylen,'(m)'
			write(iu_14(iboat),'(2a,f8.4,a)') 'il y a des traverses alors ',	&
				'on prend l''entredistance max = ',ylen,'(m)'					
		else
			write(iu_12(iboat),'(2a,f8.4,a)') 'il n''y a pas de traverses alors ',  &
				'on prend comme largeur le panneau= ',ylen,'(m)'
			write(iu_14(iboat),'(2a,f8.4,a)') 'il n''y a pas de traverses alors ',  &
				'on prend comme largeur le panneau= ',ylen,'(m)'				
		endif
	 
	else
		if(entr.gt.haut) then
			ir=1
			ylen=haut
		endif
	endif
endif

!  1.3 charge = xp, la pression (en y = 0; 1/3; 1/3; 2/3 et 1)
!  ============================================================
do  i=1,5 !!! ispecial
!!!	i = ix !!!
	  xp(i)=dabs(chamax(i)) ! le sens n'est pas important
	  if(xp(i).le.dabs(ploc1)) xp(i)=dabs(ploc1)   ! ploc1 la pression max. localisée (charge locale)
enddo   !!! ispecial


!     xp(1 à 5) pressions en mètre d'eau (calculées dans subr. ent)
!
!  1.4 sigbx,sigby,tau   les contraintes (en y = 0; 1/3; 1/2; 2/3 et 1)
!  ====================================================================
sx(1)= eff(1+4335+204) ! sx dans le bordage + 204 => on est en X=L/2
sy(1)= eff(1+4080+204)
tau(1)=eff(1+4590)	   ! + 0 => on est en X=0
sx(2)= eff(11+4335+204)
sy(2)= eff(11+4080+204)
tau(2)=eff(11+4590)
sx(3)= eff(16+4335+204)
sy(3)= eff(16+4080+204)
tau(3)=eff(16+4590)
sx(4)= eff(21+4335+204)
sy(4)= eff(21+4080+204)
tau(4)=eff(21+4590)
sx(5)= eff(31+4335+204)
sy(5)= eff(31+4080+204)
tau(5)=eff(31+4590)

sxcomb(1)= effcomb(1+4335+204) !avril2003
sycomb(1)= effcomb(1+4080+204) !avril2003
taucomb(1)=effcomb(1+4590)  !avril2003
sxcomb(2)= effcomb(11+4335+204) !avril2003
sycomb(2)= effcomb(11+4080+204) !avril2003
taucomb(2)=effcomb(11+4590) !avril2003
sxcomb(3)= effcomb(16+4335+204) !avril2003
sycomb(3)= effcomb(16+4080+204) !avril2003
taucomb(3)=effcomb(16+4590) !avril2003
sxcomb(4)= effcomb(21+4335+204) !avril2003
sycomb(4)= effcomb(21+4080+204) !avril2003
taucomb(4)=effcomb(21+4590) !avril2003
sxcomb(5)= effcomb(31+4335+204) !avril2003
sycomb(5)= effcomb(31+4080+204) !avril2003
taucomb(5)=effcomb(31+4590)


! 2. impression des donnees:
!   -----------------------
!     jprint=1  ! pour debugging
if(jprint.ge.1) then
  write (iu_12(iboat), 999)
  write (iu_12(iboat),1003)
  write (iu_12(iboat),1007) xlen,ylen,e1,syldp,sigm1
  write (iu_12(iboat),1011)
endif

!***********************************************************************
! 3.0 solve for the unknown: thickness=? (first yielding)
!     -----------------------------------------------------
!     (le calcul des dérivées est effectués par différence finies en xi
!***********************************************************************
!
!***********************************************************************
! 3.1 cas 1 : calcul avec epsa et entr (données courantes)
!***********************************************************************
tplmin=0.00d00
zero=0.0e+00
ic=0

! boucle sur les 5 points de calculs (y=0, 1/3, 1/2, 2/3 et 1)
! ------------------------------------------------------------

do icas =1,5    !!! ispecial
!!!	icas = ix
   
   !     comme épaisseur de départ (tpl) pour les itérations on prend la somme de
   !     l'épaisseur minimale en flexion "short * sqrt(0.5*xp(icas)*9810/syldp)
   !     et de l'épaisseur minimale de voilement "(short/2.) * sqrt(syldp/e)
   !     et on impose un élancement de 2 (béta = plate slenderness).
   
   short = min(xlen,ylen)
   tpl   = short * sqrt(0.5d00*xp(icas)*9810/syldp) +(short/2.d00) * sqrt(syldp/e1)
   
   !     bending in both directions is examined (two loops : loop 24).
   !     we select the more conservative case (greater tpl)
   
   !     calcul de l'épaisseur minimale pour sig x et sig y (tau=0)
   !     ----------------------------------------------------------
   !     (iloop= 1 flexion selon x et iloop=2 flexion selon y (cfr subr pltben)
   
   
   do iloop = 1,2
      var(1) = tpl
      var(2) = 0.8d00*tpl

!     (we need to get two initial estimates for tpl = var(1) and var(2) )
!     (for static and quasistatic pressure, use the initial yield value)
!	    if ((is.eq.1) .and. (nel.eq.48) .and.(itera.eq.0)) pause


      call plsolv(iloop,var,xlen,ylen,gv1,gv2,xp(icas),             &
                sx(icas),sy(icas),zero,e1,eta1,syldp,gammas,istop,  &
                ih,sxcomb(icas),sycomb(icas),zero,sigmag,nel,ioption)        

      if(istop.eq.1) then
		 write (iu_12(iboat),1010) xp(icas),sx(icas),sy(icas),zero
		 return
      endif
   
      if(jprint.ge.1) then
      ! pas intéressant d'imprimer pour iloop=2 car dmin tjrs plus faible
         if(iloop.eq.1) then 
            sr11=2.0d00/(1.0d00+gv1(1)) - 1.0d00 
            sr12=2.0d00/(1.0d00+gv1(2)) - 1.0d00
            sr21=2.0d00/(1.0d00+gv2(1)) - 1.0d00
            sr22=2.0d00/(1.0d00+gv2(2)) - 1.0d00
            write(iu_12(iboat),1010)xp(icas),sx(icas),sy(icas),zero,var(2),typ(ih),sr11,sr12,sr21,sr22
            write (iu_12(iboat),*)'    combined stress=', sxcomb(icas),sycomb(icas)
	       endif
	    endif
      
      if(tplmin.le.var(2)) then
         ihh   = ih
         tplmin= var(2)
         posit = 'sx and sy (x=l/2)'
      	 ic    = icas
      	 ssx   = sx(icas)
      	 ssy   = sy(icas)
      	 txy   = 0.0d00
         ssxcomb   = sxcomb(icas) 
	 	 ssycomb   = sycomb(icas) 
	 	 txycomb   = 0.0d00        
   	     sigg = sigmag            
   	  	 plat  = xp(icas)
   	  	 ind1  = iloop 
   	  	 ! soit 1 ou 2
      endif
   enddo    
   
   ! calcul de l'épaisseur minimale avec tau (sig x =0 et sig y =0)
   var(1) = tpl
   var(2) = 0.8d00*tpl
   
   call plsolv(1,var,xlen,ylen,gv1,gv2,xp(icas),                   &
               zero,zero,tau(icas),e1,eta1,syldp,gammas,istop,ih,  &
               zero,zero,taucomb(icas),sigmag,nel,ioption)             
   
   if(istop.eq.1) then
      write (iu_12(iboat),1010) xp(icas),zero,zero,tau(icas)
	    return
	 endif
   
   if(jprint.ge.1) then
      sr11=2.0d00/(1.0d00+gv1(1)) - 1.0d00
      sr12=2.0d00/(1.0d00+gv1(2)) - 1.0d00
      sr21=2.0d00/(1.0d00+gv2(1)) - 1.0d00
      sr22=2.0d00/(1.0d00+gv2(2)) - 1.0d00
      write (iu_12(iboat),1010) xp(icas),zero,zero,tau(icas),var(2),typ(ih),sr11,sr12,sr21,sr22
   endif
   
   if(tplmin.le.var(2)) then
	    ihh       = ih
	    tplmin    = var(2)
		posit     = 'shear stress (x=0)'
   		ic    	   = icas
   		ssx   	   = 0.0d00
   		ssy   	   = 0.0d00
   		txy   	   = tau(icas)
   		ssxcomb   = 0.0d00          
	 	ssycomb   = 0.0d00        
	 	txycomb   = taucomb(icas) 
	 	sigg      = sigmag             
	 	plat      = xp(icas)
   endif

enddo  ! boucle sur icas    !!! ispecial


!   resultats finals (dmin) pour cas de base (cas 1):
!   -------------------------------------------------
pic = (1.0d00*ic-1.d00)/4.d00  ! ic=1 -> pic=0.0  et ic=5 --> pic= 1.0
if(ic.eq.2) pic=0.33d00
if(ic.eq.4) pic=0.66d00

!  calcul des sécurités (sr1 et sr2) correspondant au delta (épaiss) en cours
!  ---------------------------------------------------------------------------
lcheck=0    ! 1 si dernière itération
icyc=1      ! soit itération initiale
istop=0
!   subr pltben
!   si dmin correspond à iloop=1 --> ind1=1 
!   si dmin correspond à iloop=2 --> ind1=2 
    
gload=plat*9810.d00
ig=0  
if(dabs(plat).le.(0.01)) ig=1             

! ***********
! s'il n'y a pas de pression latérale la subr.pltben ne peux pas être utiliséé. 
! pourquoi ?
! pltben prend en compte les contraintes membranaires et les contraintes
! flexionnelles engendrées par la pression latérale.
! l'epaisseur de la plaque influence le niveau des contraintes flexionnelles
! mais il est supposé que cette épaisseur n'influence pas (en réalité très peu)
! les contraintes membranaires qui résultent du comportement d'ensemble.
! en conséquence, en l'absence de pression latérale, l'épaisseur n'a pas d'effet sur les
! contraintes et il est par conséquent pas possible de rechercher l'épaisseur minimale.
! dans ce cas on donne à sr1=0 (valeur par défaut de sr1= sécurité vis à vis de la plastification)
! - si les contraintes membranaires sont trop fortes, c'est via un renforcement global 
!   de la structure que l'on fera chuter les contraintes. cela est effectué via d'autres restrictions.
! - si les contraintes membranaires sont trop faibles, l'épaisseur minimale sera définie via
!   la subr. buckle.

	 
if (ig.eq.0) then  !(ig=0 : avec pression laterale)

   if (ylen .lt. xlen) then



      call pltben(ind1,delta,ylen,xlen,e1,eta1,syldp,gammas,      &
           ssycomb,ssxcomb,txycomb,gload,ind1,gval1,icyc,lcheck,  &
           istop,sigmag,nel,ioption)                                          



   else
      call pltben(3-ind1,delta,xlen,ylen,e1,eta1,syldp,gammas,         &
                ssxcomb,ssycomb,txycomb,gload,ind1,gval1,icyc,lcheck,  &
                istop,sigmag,nel,ioption)                                          
   end if

   if (istop.eq.1) then
      write(iu_12(iboat),'(a,a)')'sr1: calculation of ratio (applied stress/', &
                         'yield stress) failed (must be checked manually).'
	 else
	    sr1=2.0d00/(1.0d00+gval1) - 1.0d00
	 endif
else  
!(ig=1 : sans pression laterale)
!       write(iu_12(iboat),'(a,a)')'the ratio (applied stress/yield stress)',  &
!                                  ' is not evaluated (sr1=0).'
   sr1=0.0d00
end if


!  subr buckle
igg=0
istop=0

if(ylen .lt. xlen) then
   call buckle(delta,xlen,ylen,e1,syldp,gammas,    & 
               ssx,ssy,txy,gval2,icyc,lcheck,istop,igg, &
               sr2,sxcr,sycr,taucr,nel,ioption)
else
   call buckle(delta,ylen,xlen,e1,syldp,gammas,    &
               ssy,ssx,txy,gval2,icyc,lcheck,istop,igg, &
               sr2,sycr,sxcr,taucr,nel,ioption)
endif

if ((istop.eq.1).or.(sr2.gt.1.3)) then
   write(iu_12(iboat),'(a,a)')'*** sr2 too large or calculation of buckling ', &
                                 'critical stresses failed (please check!!).'
endif

! minimum plate thickness assessment : yielding and buckling
!--------------------------------------------------------------
!      sr1 = ratio applied stress/yield stress (?<? 1.0)
!      sr2 = global interaction ratio: applied stress/critical stress (?<? 1.0)
!
!panel <-- panel dimensions -->  minimum thickness <- collapse scenario & applied stress --->  yielding   buckling  critical stresses
!      l(long) b(trans) d(thick)  d(min)  d/d(min)   mode  section   sx    sy     tau    plat  (s/sy<1)  (s/scr<1) sx(cr) sy(cr) t(cr)
!         (m)     (m)    (mm)      (mm)                      y/yo   n/mm2  n/mm2  n/mm2  (m)   von-mises interact  n/mm2  n/mm2  n/mm2
!                                                                                                sr1     form(sr2)
!  1    0.371   0.873     8.00     7.82    1.02   yielding   1.00    47.1   51.7    0.0   0.00  1.000      0.000   337.9  117.8  513.0

                                                                                                              
if (typ(ihh).eq.'buckling  ') then                                                            
   write (iu_12(iboat),1023) nel,xlen,ylen,1000.*delta,1000.*tplmin, &                                           
         delta/tplmin,typ(ihh),pic,ssx/1.0e06,ssy/1.0e06,txy/1.0e06, &                                          
         plat,sr1,sr2,sxcr/1.0e06,sycr/1.0e06,taucr/1.0e06
else                                                        
   write (iu_12(iboat),1023) nel,xlen,ylen,1000.*delta,1000.*tplmin, &   
         delta/tplmin,typ(ihh),pic,ssxcomb/1.0e06,ssycomb/1.0e06,    &
             txycomb/1.0e06,plat,sr1,sr2,sxcr/1.0e06,sycr/1.0e06,    &    
             taucr/1.0e06                                             
endif                                                       

if((itera_.ge.0).and.(iopti.ge.1))  then
	 if (typ(ihh).eq.'buckling  ') then                      
	   write(iu_31(iboat),1023) nel,xlen,ylen,1000.*delta,1000.*tplmin, &
          delta/tplmin,typ(ihh),pic,ssx/1.0e06,ssy/1.0e06,txy/1.0e06, &
          plat,sr1,sr2,sxcr/1.0e06,sycr/1.0e06,taucr/1.0e06
	 else                                                    
        write (iu_31(iboat),1023) nel,xlen,ylen,1000.*delta,1000.*tplmin,  &
        delta/tplmin,typ(ihh),pic,ssxcomb/1.0e06,ssycomb/1.0e06,           &
        txycomb/1.0e06,plat,sr1,sr2,sxcr/1.0e06,sycr/1.0e06,               &
        taucr/1.0e06                                            
	 endif                                                    
endif

1023 format(i3,3x,f7.3,2x,f7.3,3x,f6.2,3x,f6.2,2x,f6.2,3x,a10,f5.2,  &
            1x,3(f7.1),f7.2,f7.3,4x,f7.3,1x,3(f9.2))
! -------------------------------------------------

if(ioption.eq.0) return  
! si calcul des sensibilités pas requis pour ce panneau


! igg=0  
!	ig=0  
!***********************************************************************
! 3.2 cas 2 : 0.99*epsa et entr  (pour calcul par différence finie)
!***********************************************************************
icas=ic
tpla=0.d00

if((ia.eq.1).or.(iaa.eq.0)) then
   tpla=0.d00
else
   
   xlen2=0.99d00*xlen
   short = min(xlen2,ylen)
   tpl = short * sqrt(0.5d00*xp(icas)*9810/syldp) +(short/2.d00) * sqrt(syldp/e1)
   
   ! calcul de l'épaisseur minimale pour sig x, sig y  et tau=0.
   do iloop = 1,2
      var(1) = tpl
      var(2) = 0.8d00*tpl
   
      call plsolv(iloop,var,xlen2,ylen,gv1,gv2,xp(icas),              &
               sx(icas),sy(icas),zero,e1,eta1,syldp,gammas,istop,ih,  &
               sxcomb(icas),sycomb(icas),zero,sigmag,nel,ioption) !avril2003
   
      if(istop.eq.1) then
         write (iu_31(iboat),1010) xp(icas),sx(icas),sy(icas),zero
	       return
	    endif
   
      tpla = max(tpla,var(2))
   enddo
   
   if(jprint.ge.1) write (iu_31(iboat),1010) xp(icas),sx(icas),sy(icas),zero,tpla
   
   ! calcul de l'épaisseur minimale avec tau (sig x =0 et sig y =0)
   var(1) = tpl
   var(2) = 0.8d00*tpl
   call plsolv(1,var,xlen2,ylen,gv1,gv2,xp(icas),                 &
               zero,zero,tau(icas),e1,eta1,syldp,gammas,istop,ih, &
               zero,zero,taucomb(icas),sigmag,nel,ioption) !avril2003
   
   if(istop.eq.1) then
      write (iu_31(iboat),1010) xp(icas),zero,zero,tau(icas)
      return
   endif

   if(jprint.ge.1) write (iu_31(iboat),1010) xp(icas),zero,zero,tau(icas),var(2)
	 
	! calcul de l'épaisseur minimale absolu pour sig x, sig y et tau non nuls.
    tpla = max(var(2),tpla)
    tpla=-100.d00 *(tpla-tplmin)/epsa
endif

!   resultats :
!   -----------------
if(jprint.ge.1) write (iu_31(iboat),1025) tpla

!***********************************************************************
! 3.3 cas 3 :  epsa et 0.99*entr (pour calcul par différence finie)
!                 d=entr
!              c'est la dérivée par rapport à epsr que l'on calcule
!              d(c)/d(d)   = -100. *(tplr-tplmin)/d
!              d(c)/d(epsr)= d(c)/d(d)  *  (d/epsr)**2
!                          = -100. *(tplr-tplmin) * (d/epsr)/epsr
!***********************************************************************
tplr=0.d00

if((ir.eq.1).or.(irr.eq.0)) then
   tplr=0.d00
else
   ylen2=0.99d00*ylen
   short = min(xlen,ylen2)
   tpl = short * sqrt(0.5d00*xp(icas)*9810/syldp) + (short/2.d00) * sqrt(syldp/e1)

! calcul de l'épaisseur minimale pour sig x, sig y  et tau=0.
      do 224 iloop = 1,2
         var(1) = tpl
         var(2) = 0.8d00*tpl

         call plsolv(iloop,var,xlen,ylen2,gv1,gv2,xp(icas),         &
              sx(icas),sy(icas),zero,e1,eta1,syldp,gammas,istop,ih, &
              sxcomb(icas),sycomb(icas),zero,sigmag,nel,ioption) !avril2003
         if(istop.eq.1) then
            write (iu_31(iboat),1010) xp(icas),sx(icas),sy(icas),zero
            return
         endif

        tplr = max(var(2),tplr)
 224  continue

        if(jprint.ge.1) write (iu_31(iboat),1010) xp(icas),sx(icas),sy(icas),zero,tplr

! calcul de l'épaisseur minimale avec tau (sig x =0 et sig y =0)
        var(1) = tpl
        var(2) = 0.8d00*tpl

        call plsolv(1,var,xlen,ylen2,gv1,gv2,xp(icas),                &
                  zero,zero,tau(icas),e1,eta1,syldp,gammas,istop,ih,  &
                  zero,zero,taucomb(icas),sigmag,nel,ioption) !avril2003
        if(istop.eq.1) then
            write (iu_31(iboat),1010) xp(icas),zero,zero,tau(icas)
            return
        endif

        if(jprint.ge.1) write (iu_31(iboat),1010) xp(icas),zero,zero,tau(icas),var(2)

! calcul de l'épaisseur minimale absolu pour sig x, sig y et tau non nuls.
        tplr = max(var(2),tplr)

!       d(c)/d(epsr)= d(c)/d(d)  *  (d/epsr)**2
!                   = -100. *(tplr-tplmin) * (d/epsr)/epsr
        tplr=-100.d00*(tplr-tplmin) * (entr/epsr)/epsr

      endif

!   resultats :
!   -----------------
      if(jprint.ge.1) write (iu_31(iboat),1026) tplr

!***********************************************************************
! 3.4 cas 4 : 0.99*delta (pour calcul par différence finie)
!                     avec sx/0.99,  sy/0.99 et  tau/0.99
!***********************************************************************
      tpld=0.d00
      if(idd.eq.0) then
        tpld=0.d00
      else
!       si delta2=0.99*delta alors les contraintes (sx,sy,tau)
!                                  doivent être divisée par 0.99
        sx2=sx(icas)/0.99d00
        sy2=sy(icas)/0.99d00
        tau2=tau(icas)/0.99d00
        sx2comb=sxcomb(icas)/0.99d00 !avril2003
        sy2comb=sycomb(icas)/0.99d00 !avril2003
        tau2comb=taucomb(icas)/0.99d00 !avril2003
        short = min(xlen,ylen)
        tpl = short * sqrt(0.5d00*xp(icas)*9810/syldp) +(short/2.d00) * sqrt(syldp/e1)

! calcul de l'épaisseur minimale pour sig x, sig y  et tau=0.
      do 324 iloop = 1,2
         var(1) = tpl
         var(2) = 0.8d00*tpl

         call plsolv(iloop,var,xlen,ylen,gv1,gv2,xp(icas),   &
                  sx2,sy2,zero,e1,eta1,syldp,gammas,istop,ih,&
                  sx2comb,sy2comb,zero,sigmag,nel,ioption) !avril2003
         if(istop.eq.1) then
            write (iu_31(iboat),1010) xp(icas),sx(icas),sy(icas),zero
            return
         endif

        tpld = max(tpld,var(2))
 324  continue

        if(jprint.ge.1) write (iu_31(iboat),1010) xp(icas),sx(icas),sy(icas),zero,tpld

! calcul de l'épaisseur minimale avec tau (sig x =0 et sig y =0)
        var(1) = tpl
        var(2) = 0.8d00*tpl

        call plsolv(1,var,xlen,ylen,gv1,gv2,xp(icas),         &
                 zero,zero,tau2,e1,eta1,syldp,gammas,istop,ih,&
                 zero,zero,tau2comb,sigmag,nel,ioption) !avril2003
        if(istop.eq.1) then
            write (iu_31(iboat),1010) xp(icas),zero,zero,tau(icas)
            return
        endif

        if(jprint.ge.1) write (iu_31(iboat),1010) xp(icas),zero,zero,tau(icas),var(2)

! calcul de l'épaisseur minimale absolu pour sig x, sig y et tau non nuls.
        tpld = max(var(2),tpld)
        tpld=-100.*(tpld-tplmin)/delta

      endif

!   resultats :
!   -----------------
      if(jprint.ge.1) write (iu_31(iboat),1027) tpld


!   impressions finales : avec tpld= dérivées de (dmin-d) par rapport à
!   **********************
      tpld=tpld-1.
      if(jprint.eq.1)write(iu_31(iboat),198)nel,tplmin,tpla,tplr,tpld,typ(ihh),posit,ic
      

!  les formats
!  -----------
 198 format('nel=',i2,': d min =',e13.6,' d tpl/epsa =',e11.4,     &
                      ' d tpl/epsr =',e11.4,' d tpl/delta=',e11.4, &
                      ' (',a10,' et ',a10,i2,')')
 999 format(10x,'input data and results (en m et n)'/10x,23(1h*)/)
1003 format(2x,'size(m) (x,y)',5x,'modulus(n/m2) yield stress(n/m2)',  &
               '  max. stress(n/m2)')
1007 format(f8.3,' x',f8.3,3x,g9.3,5x,g9.3,5x,g9.3/)
1010 format(1x,5(e14.7,2x),a10,4(1x,f6.3))
1011 format(2x'pressure(m)',7x,'xstress',7x,'ystress',9x,'tau(n/m2)'   &
           ,7x,'thickness(m)',13x,'sr1 yield(1et2) sr2 buckl(1et2)')
1025 format('dérivée selon epsa   =',e14.7)
1026 format('dérivée selon epsr   =',e14.7)
1027 format('dérivée selon delta  =',e14.7)
return
end


! ********************************************************************
! ********************************************************************
      subroutine plsolv(iloop,var,xlen,ylen,gval1,gval2,xp,              &
                       sigbx,sigby,tau,e1,eta1,syldp,gammas,istop,ih,    &
                       sigbxcomb,sigbycomb,taucomb,sigmag,nel,ioption)
! ********************************************************************
! program segment: not in maestro
!
! purpose: uses newton-raphson to get the minimum plate thickness.
!
! common blocks: nil
!
! called by: main
!
! calls to: pleval
!
! external files: nil
! ********************************************************************
use param_section, DENOM_=>DENOM

implicit double precision(a-h,o-z)
dimension var(2),gval1(2),gval2(2)
!pi=2.d00*acos(0.d00)
!  xp en m eau et gload en n/m2  
gload=xp*9810.0   
! actibe mode = undefined 
ih=3  
!
! vérification :  gload>0 ?
! --------------------------
! ig =0 avec une pression latérale sur la plaque
! ig =1 sans pression latérale sur la plaque (donc pas de flexion),
!       dans ce cas la subr pltben n'est pas utilisable (voir explications ci-avant)
! igg=0 avec des charges ds le plan (sx, sx, tau)
! igg=1 si les charges ds le plan sont nulles(subr buckle pas utilisable)

ig=0  
if(dabs(xp).le.(0.01)) then      
   ig=1
   if(iloop.eq.2) then
      var(2)=0.0001d00
      return
   endif
endif

!  ------------------
!    gval1(i) valeurs obtenues pour la subr pltben
!    gval2(i) valeurs obtenues pour la subr buckle
!     i = 1  pour var(1)
!     i = 2  pour var(2)
!  ------------------
lcheck = 0 ! lcheck=1 lorsque la convergence est atteinte et on fait une dernière itération.
tol = 1.e-4
icycm=100 ! nbre maxi d'itérations

itry=1 ! compteur de nbre d'essais (avec différentes valeurs initiales de var1 et var2)
! itry = 1 correspond à var1 = tpl défini ci-avant et var2=0.8*tpl


200  continue

call pleval(iloop,var(1),xlen,ylen,gload,gval1(1),gval2(1),0,     &
            sigbx,sigby,tau,e1,syldp,eta1,gammas,lcheck,istop,ig, &
            igg,sigbxcomb,sigbycomb,taucomb,sigmag,nel,ioption)                 

if(istop.eq.1) return
!
! ------------- debut des iterations --------
do 32 icyc = 1,icycm

18 call pleval(iloop,var(2),xlen,ylen,gload,gval1(2),gval2(2),icyc, &
             sigbx,sigby,tau,e1,syldp,eta1,gammas,lcheck,istop,ig,  &
             igg,sigbxcomb,sigbycomb,taucomb,sigmag,nel,ioption) !avril2003

   if(istop.eq.1) return
   if((igg.eq.1).and.(ig.eq.1)) then ! si aucune sollicition n'existe
      var(2)=0.0001d00
      return
   endif

   if (lcheck.eq.1) then
!     if converged, go back and do one last cycle to check the validity lim
      if(itry.gt.1) write(iu_12(iboat),*)'itry=',itry,' nbre itération hughes =',icyc
      return
   else ! tests de convergence
      if (ig.eq.1) then ! si plat = 0 (seul la subr buckle est utilisée
        if (abs(gval2(2)).lt.tol) then
          lcheck = 1
          go to 18 ! vers une dernière itération (lcheck=1)
        endif
      else if ((igg.eq.1).or.(iloop.eq.2)) then ! si sx=0 (seul la subr pltben est utilisée
        if (abs(gval1(2)).lt.tol) then
          lcheck = 1
          go to 18
        endif
      else
        if ((abs(gval1(2)).lt.tol).and.(gval2(2).ge.0.d00)) then
          lcheck = 1
          go to 18
!         gval1=0 : car sig = sig(von-mises) c.a.d. que la limitation à la plastification (pltben) est active
!                                     et pas la limitation au voilement (buckle)
        end if
        if ((abs(gval2(2)).lt.tol).and.(gval1(2).ge.0.d00)) then
          lcheck = 1
!         gval2=0 : car sig = sig(bukling) c.a.d. que la limitation au voilement (buckle) est active
!                       et pas la limitation à la plastification (pltben)
          go to 18
        end if
      end if
   end if

!  calcul de la variation de l'épaisseur dltvar en vue de la prochaine itération

   if (ig.eq.0) then

      denom = gval1(2) - gval1(1)

      if (denom.eq.0.d00) then
         write (iu_12(iboat),'(a,i3)') 'Warning in Panel:',nel
		 write (iu_12(iboat),1001)
		 if (ioption.eq.1) write (*,'(a,i3)') 'Warning in Panel:',nel
         if (ioption.eq.1) write (*, 1001)
		 write (iu_14(iboat),'(a,i3)') 'Warning in Panel:',nel
	     write (iu_14(iboat),1001)			
         return
      end if

      dltvar = -gval1(2) * (var(2) - var(1)) / denom

20    if(((var(2)+dltvar).le.0.0d00).or.(abs(dltvar/var(1)).gt.0.3d00))then
         dltvar = 0.5d00*dltvar
         go to 20
      end if
   else
      dltvar=-999.d00
   end if

!  calcul de la variation de l'épaisseur dltvar2 en vue de la prochaine itération
   if ((iloop.eq.1).and.(igg.eq.0)) then
      denom = gval2(2) - gval2(1)

      if (denom.eq.0.d00) then
         write (iu_12(iboat),'(a,i3)') 'Warning in Panel:',nel
		 write (iu_12(iboat),1001)
		 if (ioption.eq.1) write (*,'(a,i3)') 'Warning in Panel:',nel
         if (ioption.eq.1) write (*, 1001)
		 write (iu_14(iboat),'(a,i3)') 'Warning in Panel:',nel
	     write (iu_14(iboat), 1001)
         return
      end if

      if((icyc.ge.15).and.(gval2(2).ge.(0.95)).and.(ig.eq.0)) then
         dltvar2=-999.d00
      else
         dltvar2= -gval2(2) * (var(2) - var(1)) / denom
21       if(((var(2)+dltvar2).le.0.0d00).or.(abs(dltvar2/var(1)).gt.0.3d00))then
            dltvar2= 0.5d00*dltvar2
            go to 21
         end if
      end if

   else
     dltvar2=-999.d00
   end if

!  calcul de la nouvelle épaisseur var(2) en vue de la prochaine itération

   !  via pltben
   ih=1  
   if(dltvar.le.dltvar2) then
      ih=2  ! via buckle
      dltvar=dltvar2
   endif

   var(1) = var(2)
   var(2) = var(2) + dltvar
	 if(var(2).le.0.0d00) var(2)= 0.001d00
   gval1(1) = gval1(2)
   gval2(1) = gval2(2)

32 continue  ! end of iteration loop

itry=itry + 1  ! compteur d'essais

if(itry.eq.2) then
   var(1)=0.001d00
   var(2)=0.0015d00
   goto 200
endif

if(itry.eq.3) then
   var(1)=0.001d00
   var(2)=0.0011d00
   icycm=100
   goto 200
endif

if(itry.eq.4) then
	 var(1)=var(1)*2.d00
	 var(2)=var(1)*1.05d00
	 goto 200
endif

! si itry > 4 : stop plsolv failed !!!
if (ioption.eq.1) write (*,'(a,i3)') 'Warning in Panel:',nel
if (ioption.eq.1) write (*,  1000)
write (iu_12(iboat),'(a,i3)') 'Warning in Panel:',nel
write (iu_12(iboat),1000)
write (iu_14(iboat),'(a,i3)') 'Warning in Panel:',nel
write (iu_14(iboat), 1000)

return

1000 format(/,' *******************error***********************'/   &
      ,' solution routine plsolv failed to converge in 30 cycles'/  &
      ,' press "return" to continue...')

1001 format(/,' error'/ '*********'/                     &
      ,' in routine plsolv the solution function gval1'  &
      ,' has become stationery (zero derivative)'/       &
      ,' press "return" to continue...')

end


! ********************************************************************
! ********************************************************************
subroutine pleval(iloop,tpl,xlen,ylen,gload,gval1,gval2,icyc,     &
       sigbx,sigby,tau,e1,syldp,eta1,gammas,lcheck,istop,ig,igg,  &
       sigbxcomb,sigbycomb,taucomb,sigmag,nel,ioption)                   

use param_section, ONLY : iu_14,iu_12,iboat

implicit double precision(a-h,o-z)

! ********************************************************************
! program segment: not in maestro
!                         
! purpose: manages calls to appropriate plate responses: yielding and buckling
!                         
! called by: plsolv       
!                         
! calls to: pltben, buckle (modified by rigo ph.) 
!                         
! external files: nil     
! ********************************************************************
                   
pi=2.d00*acos(0.d00)

!for static pressure load, call pltben to calculate the plate bendi
!iloop  = 1: bending across the short span
!       = 2: bending across the long  span
!ig     = 0: with lateral load
!       = 1: without lateral load
!igg    = 0: with in plane loads
!       = 1: without in plane loads


if (ig.eq.0) then  !(ig=0 : avec pression lat)
   if (ylen .lt. xlen) then
      call pltben(iloop,tpl,ylen,xlen,e1,eta1,syldp,gammas,         &
                sigbycomb,sigbxcomb,taucomb,gload,iloop,gval1,icyc, &
                lcheck,istop,sigmag,nel,ioption)                              
   else
      call pltben(3-iloop,tpl,xlen,ylen,e1,eta1,syldp,gammas, &
          sigbxcomb,sigbycomb,taucomb,gload,iloop,gval1,icyc, &
          lcheck,istop,sigmag,nel,ioption)                              
   end if
else
   gval1=0.d00  ! ig=1 , pltben pas utilisé
end if

if (iloop.eq.1) then   
   igg=0  ! càd avec contraintes membranaires (sx, ..)
   if (ylen .lt. xlen) then
     call buckle(tpl,xlen,ylen,e1,syldp,gammas,               &
                      sigbx,sigby,tau,gval2,icyc,lcheck,istop,igg, &
                      a1,a2,a3,a4,nel,ioption)
   else
     call buckle(tpl,ylen,xlen,e1,syldp,gammas,               &
                      sigby,sigbx,tau,gval2,icyc,lcheck,istop,igg, &
                      a1,a2,a3,a4,nel,ioption)
   endif
else
   gval2=0.d00   ! igg=1 , buckle pas utilisé car sx,sy, ..; = 0
end if

return
end

! ********************************************************************
! ********************************************************************
subroutine pltben(iloop2,tpl,short,rlong,e1,eta1,syldp,gammas, &
     sgshrt,sglong,tau,prsabs,iloop,gval1,icyc,lcheck,istop,   &
     sigmag,nel,ioption)     

use param_section, ONLY : iu_14,iu_12,iboat
	                                              
implicit double precision(a-h,o-z)
dimension pbcoef(2), sigmem(2)

! ********************************************************************
!
! program segment: part6
!
! purpose: evaluate the adequacy parameter for local plate bending
!          in the short or long direction (iloop2 = 1 or 2) to cause
!          surface yield or, if relevant, a specified amount of
!          permanent set wplim.  these are used in the two
!          serviceability constraints for plate bending -
!          pspbt,pspbl (panel serviceability, plate bending, transversal
!          or longitudinal).
!          - for bending in the short direction calculate the clamped edge
!            stress due to plate bending.
!          - in the second case allow for magnification due to compression
!            the short direction (eq. 12.5.11 of reference 1).
!          - if no permanent set, calculate the von mises equivalent stress
!            the combination of membrane (sig1, sig2 and tau) and bending
!            (+ and -, corresp. to upper and lower plate surfaces).
!
! rem : cette subroutine n'intègre pas de vérification au voilement
!       du panneau étudié. il s'agit d'un simple analyse statique élastique
!       seul l'effet de magnification des contraintes est considéré mais
!       indépendamment d'un risque de voilement.
!
!
! arguments: iloop2 : 1 or 2 for bending across short or long direction
!            wplim  : allowable permanent set (deflection)
!            tpl    : plate thickness
!            short,rlong   : plate dimensions
!            sgshrt,sglong : membrane stress in short and long direction
!            tau    : shear stress
!            prsabs : pressure
!            iloop  : tells which direction of bending in terms of panel
!                     1 = transverse (y axis); 2 = longitudinal (x axis)
!            gval1  : constraint function value (von mises)
!            icyc   : number of iterations already used < cyc(max)=30 (icycm)
!            syldp  : sig elastique = yield stress (n/m2)
!            gammas : security factor
!            istop  : 0 convergence , 1 non convergence
!
! common blocks: nil
!
! called by: pleval
!
! calls to : psetpr
!
! external files: nil
!
! ********************************************************************

pi=2.d00*acos(0.d00)
data pbcoef / 0.19, 0.03 /
!**********************end of all declarations**************************
!
!***********************************************************************
!                 logic is explained via
!                 principal algorithm steps
!                    § 1.0   to   §  9.0
!***********************************************************************
!
!      write(iu_12(iboat),*) 
!      write(iu_12(iboat),*) 'départ   sgshrt=',sgshrt,' sglong=',sglong
!      write(iu_12(iboat),*) '         iloop=',iloop,  ' iloop2=',iloop2
!
!** ***
!** 1.0 calculate basic parameters (voir fig. 9.6 p.338)
!** ***
sigmem(1) = sgshrt
sigmem(2) = sglong
sgmem1 = sigmem(iloop2)
sgmem2 = sigmem(3-iloop2)
bovera = short/rlong
arplt = 1.0d00/bovera
bot2 = (short/tpl)**2
skben = 0.31d00 + pbcoef(iloop2)*tanh(2.d00*iloop2*(arplt-1.d00))
sigben = skben*prsabs*bot2
phi = 1.0d00

!      write(iu_12(iboat),*) 'syldp                =',syldp
!      write(iu_12(iboat),*) 'sigben (effet plaque)=',sigben

!
!  for bending in the long direction, magnification is not relevant
!      write(iu_12(iboat),*) 'avt 3.0  skben =',skben
!      write(iu_12(iboat),*) '         sgmem1=',sgmem1,' sgmem2=',sgmem2
!      write(iu_12(iboat),*) '         bovera=',bovera,' tpl=',tpl
!      write(iu_12(iboat),*) '         sigben=',sigben
!      write(iu_12(iboat),*) 'syldp=',syldp

if (iloop2 .eq. 2) go to 24  
! skip; no magnification (phi=1) as bending in the long direction.


! ----------------------------------------------------------
! 3.0 bending across the short span; check for magnification
! ----------------------------------------------------------


if (sgshrt .ge. 0.d00) go to 24  
! skip; no membrane stress in the short direction ==> no need for ampllification
!


! 3.1  for use in eq. 12.5.11, calculate sgshcr, the critical value
!      of sgshort that would cause elastic buckling of a plate with
!      loaded edges simply supported and sides clamped.  the buckling
!      coefficient is given in fig. 12.5b, but we here change it to a
!      wide plate form (skwide) and use a "curve-fitted" expression.
!
! note: in chapter 12 - buckling of wide plates - "a" and "b" are
!       the reverse of the convention used here (for laterally loaded
!       plates, as in chapter 9).
!

skwide = 0.96d00*(0.9d00 + 1.8d00*bovera**2)**2
sgshcr = skwide*e1/bot2


! --------------------------------------------------------------
! 3.2  calculate the magnification factor, phi, from eq. 12.5.11.
! --------------------------------------------------------------
omega = -sgshrt/sgshcr
xipanl = 0.5d00*pi*sqrt(abs(omega))
xizero = (1.0d00 + 0.35d00*bovera) * xipanl

if (xizero .lt. 0.02d00) then
   xzero2 = xizero*xizero
   xzero4 = xzero2*xzero2
   phi = (1.d00 + 0.4d00*xzero2 + 17.d00/105.d00*xzero4) / (1.d00 + xzero2/3.0d00 + 2.0d00/15.0d00*xzero4)

else if (xizero .le. 3.10d00) then
   phi = 3.d00 * ( tan(xizero) - xizero ) / ( xizero**2 * tan(xizero) )

else
!  phi = 99999.0
   phi = 2.5d00
   !if (icyc .eq. 30 .or. lcheck .eq. 1) then
   if (icyc .eq. 100 .or. lcheck .eq. 1) then
      if (ioption.eq.1) write (*,'(a,i3)') 'Warning in Panel:',nel
	  if (ioption.eq.1) write (*, 1000) sgshrt, sgshcr, phi
	  write (iu_12(iboat),'(a,i3)') 'Warning in Panel:',nel
      write (iu_12(iboat),1000) sgshrt, sgshcr, phi
	  write (iu_14(iboat),'(a,i3)') 'Warning in Panel:',nel
	  write (iu_14(iboat),1000) sgshrt, sgshcr, phi	
!     pause'ok?'
!     istop=1
   end if
end if

! --------------------------------------------------
! 3.4  calculate the magnified plate bending stress.
! --------------------------------------------------
24  sigmag = phi*sigben

!   write(iu_12(iboat),*) 'sigmag (effet plaque*phi)=',sigmag
!
! --------------------------------------------------------------
! 4.0  calculate the von mises stress for the combination of membrane
!      and bending stresses (for both tensile and compressive values o
!      the latter) and take the larger of the two : sigvm.
!--------------------------------------------------------------



!  verification of the in plane stress level:
if (icyc .eq. 0 ) then
   sigvmt = gammas * sqrt(sgmem1**2 + sgmem2**2 + 3.*tau**2 - sgmem1*sgmem2)
   if (sigvmt.gt.syldp) then
      istop=1
	  if (ioption.eq.1) write (*,'(a,i3)') 'Warning in Panel:',nel
      if (ioption.eq.1) write (*, 1005) sigvmt
      write (iu_12(iboat),'(a,i3)') 'Warning in Panel:',nel
	  write (iu_12(iboat),1005) sigvmt
	  write (iu_14(iboat),'(a,i3)') 'Warning in Panel:',nel
	  write (iu_14(iboat),1005) sigvmt		
      return
   end if
end if

!  tensile (t) bending stress:
!  stress in primary (1) bending direction

sig1t = sgmem1 + sigmag
!  stress in secondary (2) direction (with poisson's ratio bending stre


sig2t = sgmem2 + eta1*sigmag
   sigvmt = sqrt(sig1t**2 + sig2t**2 + 3.*tau**2 - sig1t*sig2t)
!
!  compressive (c) bending stress
!  stress in primary (1) bending direction

sig1c = sgmem1 - sigmag

!  stress in secondary (2) direction (with poisson's ratio bending stre
sig2c = sgmem2 - eta1*sigmag
sigvmc = sqrt(sig1c**2 + sig2c**2 + 3.*tau**2 - sig1c*sig2c)

if (sigvmt .gt. sigvmc) then
    sigvm = sigvmt
else
    sigvm = sigvmc
end if

!  gammas = safety factor
!  ----------------------
gamstr = gammas*sigvm


! ------------------------------------------------------------------
! 5.0  calculate the factored strength ratio sr1, and the
!      constraint function value gval1
!      si sr1=1, alors gval1=0, on est a la solution.
! ------------------------------------------------------------------
sr1 = gamstr/syldp
gval1 = 2.d00/(1.+sr1) - 1.d00

if (sr1 .gt. 1.02d00) then
   !if (icyc .eq. 30 .or. lcheck .eq. 1) then
   if (icyc .eq. 100 .or. lcheck .eq. 1) then
      istop=1
	  if (ioption.eq.1) write (*,'(a,i3)') 'Warning in Panel:',nel
      if (ioption.eq.1) write (*, 1005) sigvm
	  write (iu_12(iboat),'(a,i3)') 'Warning in Panel:',nel
	  write (iu_14(iboat),'(a,i3)') 'Warning in Panel:',nel
      write (iu_12(iboat),1005) sigvm
      write (iu_14(iboat),1005) sigvm	
   end if
end if

return

1000 format(/,' ****************error*************************'/,              &
              ' excessive in-plane compressive stress in the short direction'/ &
              ' (',g10.3,', compared to a simply supported buckling stress',   &
              ' of',g10.3,').'/                                                &
              ' hence the magnification factor, calculated to be',g10.3,       &
              ' from eq. 12.5.11, is not valid.')
1005 format(/' excessive in-plane compressive stress,'                &
             ,' giving a von mises stress of ',g10.3,' n/m2'/         &
              ' this is too large to permit the calculation of a'     &
             ,' pressure that would just initiate yield.'/            &
              ' therefore we will not consider this constraint'/)
end


! ********************************************************************
subroutine buckle(tpl,xlen,ylen,e1,syldp,gammas, &
      sigbx,sigby,tau,gval2,icyc,lcheck,istop,igg,    &
      sr2,rxc,ryc,rsc,nel,ioption)

use param_section, ONLY : iu_14,iu_12,iboat

implicit double precision(a-h,o-z)

! ********************************************************************
! name: buckle                             created on   : 28-5-96
!                                          last revision: 16-7-97
! author: dr. ph. rigo
!
! purpose: cette subroutine intègre une vérification au voilement
!          du panneau étudié.
!          elle sert au calcul de l'épaisseur minimale requise pour`
!          éviter le voilement.
!
!          cette soubroutine est valable pour xlen > ylen
!
!             Œ y et sigma y
!             i
!             *****************************
!             *                           *  b=ylen
!             *                           *
!             *                           *
!             ***************************** --------> x et sigma x
!             <---------- a=xlen --------->
!
!
! arguments: tpl    : plate thickness
!            xlen,ylen   : plate dimensions (long and short)
!            sigbx,sigby : membrane stress in long and short direction
!            tau    : shear stress
!            gval2  : constraint function value (buckling)
!            icyc   : number of iterations already used < cyc(max)=30
!            syldp  : sig elastique = yield stress (n/m2)
!            secu   : security factor on the strength = 1.1 (as recommended by eurocode for buckling)
!            secu2  : security factor on the stress  (= gammas = sadm/sy)
!            lcheck : 1 dernière itération (sinon 0)
!            istop  : 0 convergence , 1 non convergence
!            igg    : =1 si sollicitation (in plane loads) nulle (0 sinon)
!
! called by: pleval
!
! ********************************************************************
!********************end of all declarations**************************

pi=2.d00*acos(0.d00)

! secu  : safety on strength

secu=1.10d00    ! 1.10 on the strength (see eurocode)


!     secu2 : safety on loads  
!     (as recommended by eurocode for non-permanent loads)
!     secu2=1.0
secu2=gammas ! gammas = sadm/sy; (usually 1.50 for eurocode)
!          sadm and sy are defined in the data file of lbr5
!
! ------------------------------------------------------------------
! 1.0 calculate basic parameters
! ------------------------------------------------------------------
if(sigbx.gt.0) then
  sx=0.d00
else
  sx=-sigbx*secu2
endif
if(sigby.gt.0) then
  sy=0.d00
else
  sy=-sigby*secu2
endif

alpha = xlen/ylen
alpha2= 1.d00/alpha
beta=ylen/tpl*sqrt(syldp/e1)

! ------------------------------------------------------------------
! 2.0 calculate rx, ry and rs, eq. 12.4.7  p.416 de hughes
!      (4 simply supported boundaries)
! ------------------------------------------------------------------
tb =e1*(tpl/ylen)**2
rxc=3.62d00 * tb
rx =sx/rxc
ryc=(0.905d00*(1.d00+alpha2**2)**2) * tb
ry =sy/ryc
rsc=0.905d00*(5.35d00+4.d00*alpha2**2) * tb
rs =dabs(tau*secu2)/rsc

!  write(iu_12(iboat),*) 'rx,ry,rs=',rx,ry,rs

! ------------------------------------------------------------------
! 3.0  calculate the factored strength ratio sr2, and the
!      constraint function value gval2
!      sr2=0.625*(1.+0.6/alpha)*ry / (1.-0.625*rx) +  rs*rs/(1.-rx)
!      si sr2=1, alors gval2=0, on est a la solution.
! ------------------------------------------------------------------
if(rs.le.0.001d00) then
   if(rx.le.0.01d00) then
      sr2=ry
   else if(ry.le.0.01d00) then
      sr2=rx
   else
      if(rx.gt.0.99d00) rx=0.99d00
      sr2=0.625d00*(1.+0.6d00/alpha)*ry / (1.d00-0.625d00*rx)
   endif
else
   sr2=rs
endif

if (sr2 .le.0.0d00) then
   igg=1
   gval2=1
   return
end if

!  secu = safety factor on strength
!  -----------------------------------
sr2 = secu*sr2
gval2 = 2.d00/(1.d00+sr2) - 1.d00

if (sr2 .gt. 1.02d00) then
   !if (icyc .eq. 30 .or. lcheck .eq. 1) then
   if (icyc .eq. 100 .or. lcheck .eq. 1) then
      istop=1
	  if (ioption.eq.1) write (*,'(a,i3)') 'Warning in Panel:',nel
      if (ioption.eq.1) write (*, 1005) sr2
	  write (iu_12(iboat),'(a,i3)') 'Warning in Panel:',nel
	  write (iu_14(iboat),'(a,i3)') 'Warning in Panel:',nel
      write (iu_12(iboat),1005) sr2
      write (iu_14(iboat),1005) sr2
   end if
end if

return

1005 format(/,' *********** error in bukle ********'/  &
     ,' after 30 iterations !!!'                       &
     ,' excessive  stress, giving a ratio'             &
     ,' of ',g10.3/                                    &
      ' therefore we can not consider this constraint'/)
end
