      subroutine hughes(iprint,chamax,ploc1,e1,eta1,syldp,sigm1,
     *                  width,phil,q,mt,hya,epsa,hxr,epsr,entr,delta,
     *                  eff,tplmin,tpla,tplr,tpld,
     *                  idd,iaa,irr,istop,nel,ioption,itera,is,
     *                  effcomb,sigg)
      use sharedvar
      implicit real*8(a-h,o-z)
      character*10 typ(3)
      character*18 posit
      dimension var(2),sx(5),sy(5),tau(5),eff(9690)
	dimension sxcomb(5),sycomb(5),taucomb(5),effcomb(9690)
      dimension chamax(5),xp(5)
      dimension gv1(2),gv2(2)
      data typ/'yielding','buckling','undefined'/

c***********************************************************************
c     subroutine hughes 
c     =================
c
c  ioption = 1 : avec calcul des sensibilités (si restriction d'optimisation)
c  ioption = 0 : sans calcul des sensibilités (uniquement calcul du dmin)
c
c     subroutine de calcul de l'épaisseur minimale d'un plaque non raidie 
c     sollicitée dans son plan (in plane loads) et par une pression latérale.
c
c     l'épaisseur minimale est calculée de 2 manières (la + grande est retenue):
c      - subr.  pltben 
c        il s'agit d'un simple analyse statique élastique.
c        cette subroutine n'intègre pas de vérification au voilement du
c        panneau étudié.
c        seul l'effet de magnification des contraintes est considéré mais
c        indépendamment d'un risque de voilement.
c        cette subroutine est une adaptation du programme de o. hughes 
c	   (ref : sname, "ship structure design", chap.9: plate bending.)
c          (last revision: 21 march 1993).
c        safety on load (=sy/smax).
c      - subr.  buckle
c        cette subroutine intègre une vérification au voilement du panneau
c        étudié soumis à sx, sy et tau
c        elle sert au calcul de l'épaisseur minimale requise pour éviter
c        le voilement.
c        elle a été développée par dr. ph. rigo, anast, ulg (1996)
c        safety on load (=sy/smax) and safety on strength =1.10
c
c  output: tplmin = epaisseur minimale
c          tpla   = dérivée de tplmin selon epsa
c          tplr   = dérivée de tplmin selon epsr avec epsr= fct(d=entr)
c          tpld   = dérivée de tplmin selon delta
c
c    créer : avril 96  pour lbr-5 (janvier 1996), anast par le dr. ph. rigo	
c    -------
c    modif : 22 mai 96 : epaiss. minimale pour le  voilement; subr. buckle
c    ------  31 mai 96 : dérivée de dmin par delta calculée par différence
c                        finie
c            6 mars 97 : introduction de d(raid) c.à.d. entr non égal à epsr
c                         d(c)/d(epsr)= d(c)/d(d) * (d/epsr)**2
c           16 juil 97 : correction subr. buckle , rs= ... *alpha2**2
c            6 déc  99   problème de convergence avec plsolv
c           13 janv 02 : avec et sans calcul des sensibilités (ioption),
c                        impression des contraintes critiques obtenues.
c             mai 2003 : combinaison  sx(resul)  +  sx(stiff)  (f. bair)   !avril2003
c            juin 2003 : combinaison isx(resul)i + isx(stiff)i (f. bair)   !juin2003
c           
c   dernière modif: 26 nov 2003
c   --------------
c*************************************************************************

      piq=pi*q/180.0
	
 	jprint=iprint  ! impression standard : iprint= 1 si iopti=3

c***********************************************************************
c***********************************************************************
c               Œ y et sigma y
c               i
c               *****************************
c               *                           *  b=xlen
c               *                           *
c               *                           *
c               ***************************** --------> x et sigma x
c               <----------- a = ylen ------>

c  1.0 données à déterminer
c  ************************
c      xlen,ylen        = a,b       (m)
c      syldp            = contr. elastique  (n/m2), (ex, re=240 n/mm2)
c      sigm1             = contr. admissible (n/m2)
c      xp               = p         (m d'eau)
c      sigbx,sigby,tau  = sx,sy,tau (n/m2)

c  1.1 gammas, le coef de sécurité sur les charges (secu ds buckle et pltben)
c  =========================================================================
       gammas = syldp/sigm1   ! en plus, il y a "the safety on strength" = 1.10 dans buckle

c  1.2 xlen,ylen  les dimensions (a et b) du panneau
c  ==================================================
      xlen=epsa
      ylen=entr
      ia=0
      ir=0

      if((hya/delta).le.2) then
        ia=1
        xlen=width
        write(67,*)'warning: sbr. hughes : cadres trop petits !!!'
        write(67,*)'on prend portée de la plaque=width=',width,' (m)',
     *              ' au lieu de l''entredistance entre cadres.'
        write(29,*)'warning: sbr. hughes : cadres trop petits !!!'		!sept06			!bug
        write(29,*)'on prend portée de la plaque=width=',width,' (m)',	!sept06				!bug
     *              ' au lieu de l''entredistance entre cadres.'			!sept06				!bug

	else
        if(epsa.gt.width) then
          ia=1
          xlen=width
        endif
      endif

      haut=q*phil*pi/180.
      if((hxr/delta).le.2) then
        ir=1
        ylen=haut
        write(67,'(a,i3,a)') 'panel n°',nel,
     *               ' raidisseurs trop petits !!! (sbr. hughes)'

	  write(29,'(a,i3,a)') 'panel n°',nel,											!bug
     *               ' raidisseurs trop petits !!! (sbr. hughes)'						!bug

      if(mt.gt.0) then
          ir=0
          ya=0.
          y1=abtr(1)*piq
          yt=y1-ya
          ylen=yt
          do 2 i=1,mt-1
             y1=abtr(i)*piq
             y2=abtr(i+1)*piq
             yt=y2-y1
             if(ylen.lt.yt) ylen=yt
   2      continue
          ymt=abtr(mt)*piq
          yb=haut
          yt=yb-ymt
          if(ylen.lt.yt) ylen=yt
          write(67,'(2a,f8.4,a)') 'il y a des traverses alors ',
     *         'on prend l''entredistance max = ',ylen,'(m)'
          write(29,'(2a,f8.4,a)') 'il y a des traverses alors ',				!sept06		!bug
     *         'on prend l''entredistance max = ',ylen,'(m)'					!sept06		!bug

	  else
          write(67,'(2a,f8.4,a)') 'il n''y a pas de traverses alors ',
     *         'on prend comme largeur le panneau= ',ylen,'(m)'
          write(29,'(2a,f8.4,a)') 'il n''y a pas de traverses alors ',		!sept06		!bug
     *         'on prend comme largeur le panneau= ',ylen,'(m)'				!sept06		!bug
	  endif
      else
        if(entr.gt.haut) then
          ir=1
          ylen=haut
        endif
      endif

c  1.3 charge = xp, la pression (en y = 0; 1/3; 1/3; 2/3 et 1)
c  ============================================================
      do  i=1,5
	  xp(i)=dabs(chamax(i)) ! le sens n'est pas important
	  if(xp(i).le.dabs(ploc1)) xp(i)=dabs(ploc1)   ! ploc1 la pression max. localisée (charge locale)
	enddo


c     xp(1 à 5) pressions en mètre d'eau (calculées dans subr. ent)

c  1.4 sigbx,sigby,tau   les contraintes (en y = 0; 1/3; 1/2; 2/3 et 1)
c  ====================================================================
      sx(1)= eff(1+4335+204)
      sy(1)= eff(1+4080+204)
      tau(1)=eff(1+4590)
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


c 2. impression des donnees:
c   -----------------------
c     jprint=1  ! pour debugging
      if(jprint.ge.1) then
        write (67, 999)
        write (67,1003)
        write (67,1007) xlen,ylen,e1,syldp,sigm1
        write (67,1011)
      endif

c***********************************************************************
c 3.0 solve for the unknown: thickness=? (first yielding)
c     -----------------------------------------------------
c     (le calcul des dérivées est effectués par différence finies en xi
c***********************************************************************

c***********************************************************************
c 3.1 cas 1 : calcul avec epsa et entr (données courantes)
c***********************************************************************
      tplmin=0.00
      zero=0.0e+00
      ic=0

c boucle sur les 5 points de calculs (y=0, 1/3, 1/2, 2/3 et 1)
c ------------------------------------------------------------

      do 26 icas =1,5

c     comme épaisseur de départ (tpl) pour les itérations on prend la somme de
c     l'épaisseur minimale en flexion "short * sqrt(0.5*xp(icas)*9810/syldp)
c     et de l'épaisseur minimale de voilement "(short/2.) * sqrt(syldp/e)
c     et on impose un élancement de 2 (béta = plate slenderness).

      short = min(xlen,ylen)
      tpl   = short * sqrt(0.5*xp(icas)*9810/syldp) +
     *                (short/2.) * sqrt(syldp/e1)

c     bending in both directions is examined (two loops : loop 24).
c     we select the more conservative case (greater tpl)

c     calcul de l'épaisseur minimale pour sig x et sig y (tau=0)
c     ----------------------------------------------------------
c     (iloop= 1 flexion selon x et iloop=2 flexion selon y (cfr subr pltben)
      do 24 iloop = 1,2
         var(1) = tpl
         var(2) = 0.8*tpl
c        (we need to get two initial estimates for tpl = var(1) and var(2) )
c        (for static and quasistatic pressure, use the initial yield value)
c	if ((is.eq.1) .and. (nel.eq.48) .and.(itera.eq.0)) pause
         call plsolv(iloop,var,xlen,ylen,gv1,gv2,xp(icas),
     *              sx(icas),sy(icas),zero,e1,eta1,syldp,gammas,istop,
     *              ih,sxcomb(icas),sycomb(icas),zero,sigmag)             !avril2003


         if(istop.eq.1) then
		  write (67,1010) xp(icas),sx(icas),sy(icas),zero
            return
         endif
         if(jprint.ge.1) then
          if(iloop.eq.1) then ! pas intéressant d'imprimer pour iloop=2 car dmin tjrs plus faible
           sr11=2.0/(1.0+gv1(1)) - 1.0 
           sr12=2.0/(1.0+gv1(2)) - 1.0
           sr21=2.0/(1.0+gv2(1)) - 1.0
           sr22=2.0/(1.0+gv2(2)) - 1.0
           write(67,1010)xp(icas),sx(icas),sy(icas),zero,var(2),typ(ih),
     *                            sr11,sr12,sr21,sr22
           write (67,*)'    combined stress=', sxcomb(icas),sycomb(icas)
	    endif
	   endif

         if(tplmin.le.var(2)) then
           ihh   = ih
           tplmin= var(2)
           posit = 'sx and sy (x=l/2)'
	     ic    = icas
	     ssx   = sx(icas)
	     ssy   = sy(icas)
	     txy   = 0.0
           ssxcomb   = sxcomb(icas) !avril2003
	     ssycomb   = sycomb(icas) !avril2003
	     txycomb   = 0.0          !avril2003
	     sigg = sigmag            !avril2003
	     plat  = xp(icas)
	     ind1  = iloop ! soit 1 ou 2
         endif
  24  continue

c     calcul de l'épaisseur minimale avec tau (sig x =0 et sig y =0)
        var(1) = tpl
        var(2) = 0.8*tpl

      call plsolv(1,var,xlen,ylen,gv1,gv2,xp(icas),
     *               zero,zero,tau(icas),e1,eta1,syldp,gammas,istop,ih,
     *               zero,zero,taucomb(icas),sigmag)                   !avril2003

        if(istop.eq.1) then
           write (67,1010) xp(icas),zero,zero,tau(icas)
           return
        endif

        if(jprint.ge.1) then
         sr11=2.0/(1.0+gv1(1)) - 1.0
         sr12=2.0/(1.0+gv1(2)) - 1.0
         sr21=2.0/(1.0+gv2(1)) - 1.0
         sr22=2.0/(1.0+gv2(2)) - 1.0
         write (67,1010) xp(icas),zero,zero,tau(icas),var(2),typ(ih),
     *                  sr11,sr12,sr21,sr22
	  endif

        if(tplmin.le.var(2)) then
           ihh   = ih
           tplmin= var(2)
           posit = 'shear stress (x=0)'
	     ic    = icas
	     ssx   = 0.0
	     ssy   = 0.0
	     txy   = tau(icas)
	     ssxcomb   = 0.0           !avril2003
	     ssycomb   = 0.0           !avril2003
	     txycomb   = taucomb(icas) !avril2003
	     sigg = sigmag             !avril2003
	     plat  = xp(icas)
	  endif

  26  continue  ! boucle sur icas


c   resultats finals (dmin) pour cas de base (cas 1):
c   -------------------------------------------------
      pic = (1.0*ic-1.)/4.  ! ic=1 -> pic=0.0  et ic=5 --> pic= 1.0
	if(ic.eq.2) pic=0.33
	if(ic.eq.4) pic=0.66

c  calcul des sécurités (sr1 et sr2) correspondant au delta (épaiss) en cours
c  ---------------------------------------------------------------------------
      lcheck=0    ! 1 si dernière itération
	icyc=1      ! soit itération initiale
	istop=0
c   subr pltben
c      si dmin correspond à iloop=1 --> ind1=1 
c      si dmin correspond à iloop=2 --> ind1=2 
      
	gload=plat*9810.
      ig=0  
      if(dabs(plat).le.(0.01)) ig=1             ! ***********
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
	 ! - si les contraintes membranaires sont trop fortes, c'est via un remforcement global 
	 !   de la structure que l'on fera chuter les contraintes. cela est effectué via d'autres restrictions.
	 ! - si les contraintes membranaires sont trop faibles, l'épaisseur minimale sera définie via
	 !   la subr. buckle.
	 
      if (ig.eq.0) then  !(ig=0 : avec pression laterale)
        if (ylen .lt. xlen) then
           call pltben(ind1,delta,ylen,xlen,e1,eta1,syldp,gammas,
     *           ssycomb,ssxcomb,txycomb,gload,ind1,gval1,icyc,lcheck,  !avril2003
     *           istop,sigmag)                                          !avril2003
        else
           call pltben(3-ind1,delta,xlen,ylen,e1,eta1,syldp,gammas,
     *           ssxcomb,ssycomb,txycomb,gload,ind1,gval1,icyc,lcheck,  !avril2003
     *           istop,sigmag)                                          !avril2003
        end if
        if (istop.eq.1) then
          write(67,'(a,a)')'sr1: calculation of ratio (applied stress/',
     *               'yield stress) failed (must be checked manually).'
	  else
	    sr1=2.0/(1.0+gval1) - 1.0
	  endif
      else  !(ig=1 : sans pression laterale)
c       write(67,'(a,a)')'the ratio (applied stress/yield stress)',
c    *             ' is not evaluated (sr1=0).'
	  sr1=0.0
      end if


c  subr buckle
      igg=0
	istop=0
      if (ylen .lt. xlen) then
        call buckle(delta,xlen,ylen,e1,eta1,syldp,gammas,
     *                 ssx,ssy,txy,gval2,icyc,lcheck,istop,igg,
     *                 sr2,sxcr,sycr,taucr)
      else
        call buckle(delta,ylen,xlen,e1,eta1,syldp,gammas,
     *                 ssy,ssx,txy,gval2,icyc,lcheck,istop,igg,
     *                 sr2,sycr,sxcr,taucr)				               !mars2004
      endif

      if ((istop.eq.1).or.(sr2.gt.1.3)) then
       write(67,'(a,a)')'*** sr2 too large or calculation of buckling ',
     *            'critical stresses failed (please check!!).'
	endif

c minimum plate thickness assessment : yielding and buckling
c--------------------------------------------------------------
c      sr1 = ratio applied stress/yield stress (?<? 1.0)
c      sr2 = global interaction ratio: applied stress/critical stress (?<? 1.0)
c
cpanel <-- panel dimensions -->  minimum thickness <- collapse scenario & applied stress --->  yielding   buckling  critical stresses
c      l(long) b(trans) d(thick)  d(min)  d/d(min)   mode  section   sx    sy     tau    plat  (s/sy<1)  (s/scr<1) sx(cr) sy(cr) t(cr)
c         (m)     (m)    (mm)      (mm)                      y/yo   n/mm2  n/mm2  n/mm2  (m)   von-mises interact  n/mm2  n/mm2  n/mm2
c                                                                                                sr1     form(sr2)
c  1    0.371   0.873     8.00     7.82    1.02   yielding   1.00    47.1   51.7    0.0   0.00  1.000      0.000   337.9  117.8  513.0
c1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890

	if (typ(ihh).eq.'buckling  ') then                           !avril2003
	  write (67,1023) nel,xlen,ylen,1000.*delta,1000.*tplmin,
     *    delta/tplmin,typ(ihh),pic,ssx/1.0e06,ssy/1.0e06,txy/1.0e06,
     *    plat,sr1,sr2,sxcr/1.0e06,sycr/1.0e06,taucr/1.0e06
      else                                                         !avril2003
	  write (67,1023) nel,xlen,ylen,1000.*delta,1000.*tplmin,    !avril2003
     *    delta/tplmin,typ(ihh),pic,ssxcomb/1.0e06,ssycomb/1.0e06, !avril2003
     *    txycomb/1.0e06,plat,sr1,sr2,sxcr/1.0e06,sycr/1.0e06,     !avril2003
     *    taucr/1.0e06                                             !avril2003
	endif                                                        !avril2003

      if((itera.ge.0).and.(iopti.ge.1))  then
	  if (typ(ihh).eq.'buckling  ') then                         !avril2003
	    write(666,1023) nel,xlen,ylen,1000.*delta,1000.*tplmin,
     *      delta/tplmin,typ(ihh),pic,ssx/1.0e06,ssy/1.0e06,txy/1.0e06,
     *      plat,sr1,sr2,sxcr/1.0e06,sycr/1.0e06,taucr/1.0e06
	  else                                                       !avril2003
          write (666,1023) nel,xlen,ylen,1000.*delta,1000.*tplmin, !avril2003
     *    delta/tplmin,typ(ihh),pic,ssxcomb/1.0e06,ssycomb/1.0e06, !avril2003
     *    txycomb/1.0e06,plat,sr1,sr2,sxcr/1.0e06,sycr/1.0e06,     !avril2003
     *    taucr/1.0e06                                             !avril2003
	  endif                                                      !avril2003
	endif

 1023 format(i3,3x,f6.3,2x,f6.3,3x,f6.2,3x,f6.2,2x,f6.2,3x,a10,f5.2,
     *          1x,3(f7.1),f7.2,f7.3,4x,f7.3,1x,3(f7.1))
c -------------------------------------------------

      if(ioption.eq.0) return  ! si calcul des sensibilités pas requis pour ce panneau
c      igg=0  
c	ig=0  
c***********************************************************************
c 3.2 cas 2 : 0.99*epsa et entr  (pour calcul par différence finie)
c***********************************************************************
      icas=ic
      tpla=0.

      if((ia.eq.1).or.(iaa.eq.0)) then
        tpla=0.
      else

        xlen2=0.99*xlen
        short = min(xlen2,ylen)
        tpl = short * sqrt(0.5*xp(icas)*9810/syldp) +
     *        (short/2.) * sqrt(syldp/e1)

c calcul de l'épaisseur minimale pour sig x, sig y  et tau=0.
      do 124 iloop = 1,2
         var(1) = tpl
         var(2) = 0.8*tpl

         call plsolv(iloop,var,xlen2,ylen,gv1,gv2,xp(icas),
     *             sx(icas),sy(icas),zero,e1,eta1,syldp,gammas,istop,ih,
     *             sxcomb(icas),sycomb(icas),zero,sigmag) !avril2003

         if(istop.eq.1) then
            write (666,1010) xp(icas),sx(icas),sy(icas),zero
            return
         endif

         tpla = max(tpla,var(2))
 124  continue
       if(jprint.ge.1)
     *    write (666,1010) xp(icas),sx(icas),sy(icas),zero,tpla

c calcul de l'épaisseur minimale avec tau (sig x =0 et sig y =0)
       var(1) = tpl
       var(2) = 0.8*tpl
       call plsolv(1,var,xlen2,ylen,gv1,gv2,xp(icas),
     *             zero,zero,tau(icas),e1,eta1,syldp,gammas,istop,ih,
     *             zero,zero,taucomb(icas),sigmag) !avril2003

       if(istop.eq.1) then
          write (666,1010) xp(icas),zero,zero,tau(icas)
          return
       endif
       if(jprint.ge.1)
     *    write (666,1010) xp(icas),zero,zero,tau(icas),var(2)

c calcul de l'épaisseur minimale absolu pour sig x, sig y et tau non nuls.
       tpla = max(var(2),tpla)
       tpla=-100. *(tpla-tplmin)/epsa
      endif

c   resultats :
c   -----------------
      if(jprint.ge.1) write (666,1025) tpla

c***********************************************************************
c 3.3 cas 3 :  epsa et 0.99*entr (pour calcul par différence finie)
c                 d=entr
c              c'est la dérivée par rapport à epsr que l'on calcule
c              d(c)/d(d)   = -100. *(tplr-tplmin)/d
c              d(c)/d(epsr)= d(c)/d(d)  *  (d/epsr)**2
c                          = -100. *(tplr-tplmin) * (d/epsr)/epsr
c***********************************************************************
      tplr=0.
      if((ir.eq.1).or.(irr.eq.0)) then
        tplr=0.
      else

        ylen2=0.99*ylen
        short = min(xlen,ylen2)
        tpl = short * sqrt(0.5*xp(icas)*9810/syldp) +
     *        (short/2.) * sqrt(syldp/e1)

c calcul de l'épaisseur minimale pour sig x, sig y  et tau=0.
      do 224 iloop = 1,2
         var(1) = tpl
         var(2) = 0.8*tpl

         call plsolv(iloop,var,xlen,ylen2,gv1,gv2,xp(icas),
     *            sx(icas),sy(icas),zero,e1,eta1,syldp,gammas,istop,ih,
     *            sxcomb(icas),sycomb(icas),zero,sigmag) !avril2003
         if(istop.eq.1) then
            write (666,1010) xp(icas),sx(icas),sy(icas),zero
            return
         endif

        tplr = max(var(2),tplr)
 224  continue

        if(jprint.ge.1)
     *     write (666,1010) xp(icas),sx(icas),sy(icas),zero,tplr

c calcul de l'épaisseur minimale avec tau (sig x =0 et sig y =0)
        var(1) = tpl
        var(2) = 0.8*tpl

        call plsolv(1,var,xlen,ylen2,gv1,gv2,xp(icas),
     *             zero,zero,tau(icas),e1,eta1,syldp,gammas,istop,ih,
     *             zero,zero,taucomb(icas),sigmag) !avril2003
        if(istop.eq.1) then
            write (666,1010) xp(icas),zero,zero,tau(icas)
            return
        endif

        if(jprint.ge.1)
     *     write (666,1010) xp(icas),zero,zero,tau(icas),var(2)

c calcul de l'épaisseur minimale absolu pour sig x, sig y et tau non nuls.
        tplr = max(var(2),tplr)

c       d(c)/d(epsr)= d(c)/d(d)  *  (d/epsr)**2
c                   = -100. *(tplr-tplmin) * (d/epsr)/epsr
        tplr=-100. *(tplr-tplmin) * (entr/epsr)/epsr

      endif

c   resultats :
c   -----------------
      if(jprint.ge.1) write (666,1026) tplr

c***********************************************************************
c 3.4 cas 4 : 0.99*delta (pour calcul par différence finie)
c                     avec sx/0.99,  sy/0.99 et  tau/0.99
c***********************************************************************
      tpld=0.
      if(idd.eq.0) then
        tpld=0.
      else
c       si delta2=0.99*delta alors les contraintes (sx,sy,tau)
c                                  doivent être divisée par 0.99
        sx2=sx(icas)/0.99
        sy2=sy(icas)/0.99
        tau2=tau(icas)/0.99
        sx2comb=sxcomb(icas)/0.99 !avril2003
        sy2comb=sycomb(icas)/0.99 !avril2003
        tau2comb=taucomb(icas)/0.99 !avril2003
        short = min(xlen,ylen)
        tpl = short * sqrt(0.5*xp(icas)*9810/syldp) +
     *        (short/2.) * sqrt(syldp/e1)

c calcul de l'épaisseur minimale pour sig x, sig y  et tau=0.
      do 324 iloop = 1,2
         var(1) = tpl
         var(2) = 0.8*tpl

         call plsolv(iloop,var,xlen,ylen,gv1,gv2,xp(icas),
     *            sx2,sy2,zero,e1,eta1,syldp,gammas,istop,ih,
     *            sx2comb,sy2comb,zero,sigmag) !avril2003
         if(istop.eq.1) then
            write (666,1010) xp(icas),sx(icas),sy(icas),zero
            return
         endif

        tpld = max(tpld,var(2))
 324  continue

        if(jprint.ge.1)
     *     write (666,1010) xp(icas),sx(icas),sy(icas),zero,tpld

c calcul de l'épaisseur minimale avec tau (sig x =0 et sig y =0)
        var(1) = tpl
        var(2) = 0.8*tpl

        call plsolv(1,var,xlen,ylen,gv1,gv2,xp(icas),
     *             zero,zero,tau2,e1,eta1,syldp,gammas,istop,ih,
     *             zero,zero,tau2comb,sigmag) !avril2003
        if(istop.eq.1) then
            write (666,1010) xp(icas),zero,zero,tau(icas)
            return
        endif

        if(jprint.ge.1)
     *     write (666,1010) xp(icas),zero,zero,tau(icas),var(2)

c calcul de l'épaisseur minimale absolu pour sig x, sig y et tau non nuls.
        tpld = max(var(2),tpld)
        tpld=-100.*(tpld-tplmin)/delta

      endif

c   resultats :
c   -----------------
      if(jprint.ge.1) write (666,1027) tpld


c   impressions finales : avec tpld= dérivées de (dmin-d) par rapport à
c   **********************
      tpld=tpld-1.
      if(jprint.eq.1)
     *  write(666,198)nel,tplmin,tpla,tplr,tpld,typ(ihh),posit,ic
      
c23456789012345678901234567890123456789012345678901234567890123456789012
c     les formats
ccc   -----------
  198 format('nel=',i2,': d min =',e13.6,' d tpl/epsa =',e11.4,
     *                  ' d tpl/epsr =',e11.4,' d tpl/delta=',e11.4,
     *                  ' (',a10,' et ',a10,i2,')')
  999 format(10x,'input data and results (en m et n)'/10x,23(1h*)/)
 1003 format(2x,'size(m) (x,y)',5x,'modulus(n/m2) yield stress(n/m2)',
     *         '  max. stress(n/m2)')
 1007 format(f8.3,' x',f8.3,3x,g9.3,5x,g9.3,5x,g9.3/)
 1010 format(1x,5(e14.7,2x),a10,4(1x,f6.3))
 1011 format(2x'pressure(m)',7x,'xstress',7x,'ystress',9x,'tau(n/m2)'
     *  ,7x,'thickness(m)',13x,'sr1 yield(1et2) sr2 buckl(1et2)')
 1025 format('dérivée selon epsa   =',e14.7)
 1026 format('dérivée selon epsr   =',e14.7)
 1027 format('dérivée selon delta  =',e14.7)
      return
      end


ccc ********************************************************************
ccc ********************************************************************
      subroutine plsolv(iloop,var,xlen,ylen,gval1,gval2,xp,
     *                  sigbx,sigby,tau,e1,eta1,syldp,gammas,istop,ih,
     *                  sigbxcomb,sigbycomb,taucomb,sigmag)
ccc ********************************************************************
ccc name: plsolv                             last revision: 23 apr 1992
ccc													(dec. 99 by rigo ph.)
ccc                                                   (mai 2003, f. bair )
ccc program segment: not in maestro
ccc
ccc purpose: uses newton-raphson to get the minimum plate thickness.
ccc
ccc common blocks: nil
ccc
ccc called by: main
ccc
ccc calls to: pleval
ccc
ccc external files: nil
ccc ********************************************************************
      implicit real*8(a-h,o-z)
      dimension var(2),gval1(2),gval2(2)
      pi=2.d00*acos(0.d00)
      gload=xp*9810.0   !  xp en m eau et gload en n/m2
	ih=3  ! actibe mode = undefined
c
c  vérification :  gload>0 ?
c  --------------------------
c     ig =0 avec une pression latérale sur la plaque
c     ig =1 sans pression latérale sur la plaque (donc pas de flexion),
c           dans ce cas la subr pltben n'est pas utilisable (voir explications ci-avant)
c     igg=0 avec des charges ds le plan (sx, sx, tau)
c     igg=1 si les charges ds le plan sont nulles(subr buckle pas utilisable)

      ig=0  
      if(dabs(xp).le.(0.01)) then      !*********
         ig=1
         if(iloop.eq.2) then
            var(2)=0.0001
            return
         endif
      endif

c  ------------------
c    gval1(i) valeurs obtenues pour la subr pltben
c    gval2(i) valeurs obtenues pour la subr buckle
c     i = 1  pour var(1)
c     i = 2  pour var(2)
c  ------------------
      lcheck = 0 ! lcheck=1 lorsque la convergence est atteinte et on fait une dernière itération.
      tol = 1.e-4
      icycm=30 ! nbre maxi d'itérations

	itry=1 ! compteur de nbre d'essais (avec différentes valeurs initiales de var1 et var2)
             ! itry = 1 correspond à var1 = tpl défini ci-avant et var2=0.8*tpl
 200  continue

      call pleval(iloop,var(1),xlen,ylen,gload,gval1(1),gval2(1),0,
     *           sigbx,sigby,tau,e1,syldp,eta1,gammas,lcheck,istop,ig,
     *           igg,sigbxcomb,sigbycomb,taucomb,sigmag)                  !avril2003

      if(istop.eq.1) return
c
c ------------- debut des iterations --------
      do 32 icyc = 1,icycm

   18 call pleval(iloop,var(2),xlen,ylen,gload,gval1(2),gval2(2),icyc,
     *           sigbx,sigby,tau,e1,syldp,eta1,gammas,lcheck,istop,ig,
     *           igg,sigbxcomb,sigbycomb,taucomb,sigmag) !avril2003

        if(istop.eq.1) return
        if((igg.eq.1).and.(ig.eq.1)) then ! si aucune sollicition n'existe
          var(2)=0.0001
          return
        endif

      if (lcheck.eq.1) then
c       if converged, go back and do one last cycle to check the validity lim
        if(itry.gt.1)
     *         write(67,*)'itry=',itry,' nbre itération hughes =',icyc
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
          if ((abs(gval1(2)).lt.tol).and.(gval2(2).ge.0.)) then
            lcheck = 1
            go to 18
c           gval1=0 : car sig = sig(von-mises) c.a.d. que la limitation à la plastification (pltben) est active
c                                       et pas la limitation au voilement (buckle)
          end if
          if ((abs(gval2(2)).lt.tol).and.(gval1(2).ge.0.)) then
            lcheck = 1
c           gval2=0 : car sig = sig(bukling) c.a.d. que la limitation au voilement (buckle) est active
c                         et pas la limitation à la plastification (pltben)
            go to 18
          end if
        end if
      end if

c     calcul de la variation de l'épaisseur dltvar en vue de la prochaine itération
      if (ig.eq.0) then
        denom = gval1(2) - gval1(1)
        if (denom.eq.0.) then
           write (67,1001)
           write (*, 1001)
           write (6970, 1001)
	     write (29, 1001)			!sept06
           return
        end if
        dltvar = -gval1(2) * (var(2) - var(1)) / denom
   20   if(((var(2)+dltvar).le.0.).or.(abs(dltvar/var(1)).gt.0.3))then
          dltvar = 0.5*dltvar
          go to 20
        end if
      else
        dltvar=-999.
      end if

c    calcul de la variation de l'épaisseur dltvar2 en vue de la prochaine itération
      if ((iloop.eq.1).and.(igg.eq.0)) then
        denom = gval2(2) - gval2(1)

        if (denom.eq.0.) then
           write (67,1001)
           write (*, 1001)
           write (6970, 1001)
	     write (29, 1001)
           return
        end if
        if((icyc.ge.15).and.(gval2(2).ge.(0.95)).and.(ig.eq.0)) then
          dltvar2=-999.
        else
        dltvar2= -gval2(2) * (var(2) - var(1)) / denom
   21   if(((var(2)+dltvar2).le.0.).or.(abs(dltvar2/var(1)).gt.0.3))then
          dltvar2= 0.5*dltvar2
          go to 21
        end if
        end if
      else
        dltvar2=-999.
      end if

c    calcul de la nouvelle épaisseur var(2) en vue de la prochaine itération
      ih=1  !  via pltben
      if(dltvar.le.dltvar2) then
        ih=2  ! via buckle
        dltvar=dltvar2
      endif
      var(1) = var(2)
      var(2) = var(2) + dltvar
	if(var(2).le.0) var(2)= 0.001
      gval1(1) = gval1(2)
      gval2(1) = gval2(2)

   32 continue  ! end of iteration loop

      itry=itry + 1  ! compteur d'essais

	if(itry.eq.2) then
	  var(1)=0.001
	  var(2)=0.0015
	  goto 200
	endif

	if(itry.eq.3) then
	  var(1)=0.001
	  var(2)=0.0011
	  icycm=100
	  goto 200
	endif

	if(itry.eq.4) then
	  var(1)=var(1)*2.
	  var(2)=var(1)*1.05
	  goto 200
	endif

	! si itry > 4 : stop plsolv failed !!!
      write (*,  1000)
      write (6970,  1000)
      write (67,1000)
	write (29, 1000)
      return

 1000 format(/,' *******************error***********************'/
     1 ,' solution routine plsolv failed to converge in 30 cycles'/
     2 ,' press "return" to continue...')
 1001 format(/,' error'/ '*********'/
     1 ,' in routine plsolv the solution function gval1'
     2 ,' has become stationery (zero derivative)'/
     3 ,' press "return" to continue...')
      end


ccc ********************************************************************
ccc ********************************************************************
      subroutine pleval(iloop,tpl,xlen,ylen,gload,gval1,gval2,icyc,
     *        sigbx,sigby,tau,e1,syldp,eta1,gammas,lcheck,istop,ig,igg,
     *        sigbxcomb,sigbycomb,taucomb,sigmag)                   !avril2003
      implicit real*8(a-h,o-z)

ccc ********************************************************************
ccc name: pleval                             last revision: 1997 (subr buckle - rigo)
c                                                           2003 mai (f. bair)
ccc
ccc program segment: not in maestro
ccc
ccc purpose: manages calls to appropriate plate responses: yielding and buckling
ccc
ccc called by: plsolv
ccc
ccc calls to: pltben, buckle (modified by rigo ph.) 
ccc
ccc external files: nil
ccc ********************************************************************

      pi=2.d00*acos(0.d00)

c     for static pressure load, call pltben to calculate the plate bendi
c     iloop  = 1: bending across the short span
c            = 2: bending across the long  span
c     ig     = 0: with lateral load
c            = 1: without lateral load
c     igg    = 0: with in plane loads
c            = 1: without in plane loads


      if (ig.eq.0) then  !(ig=0 : avec pression lat)
         if (ylen .lt. xlen) then
            call pltben(iloop,tpl,ylen,xlen,e1,eta1,syldp,gammas,
     *           sigbycomb,sigbxcomb,taucomb,gload,iloop,gval1,icyc,
     *           lcheck,istop,sigmag)                                !avril2003
         else
            call pltben(3-iloop,tpl,xlen,ylen,e1,eta1,syldp,gammas,
     *           sigbxcomb,sigbycomb,taucomb,gload,iloop,gval1,icyc,
     *           lcheck,istop,sigmag)                                !avril2003
         end if
      else
         gval1=0.  ! ig=1 , pltben pas utilisé
      end if

      if (iloop.eq.1) then   !subr buckle'
         igg=0  ! càd avec contraintes membranaires (sx, ..)
         if (ylen .lt. xlen) then
           call buckle(tpl,xlen,ylen,e1,eta1,syldp,gammas,
     *                 sigbx,sigby,tau,gval2,icyc,lcheck,istop,igg,
     *                 a1,a2,a3,a4)
         else
           call buckle(tpl,ylen,xlen,e1,eta1,syldp,gammas,
     *                 sigby,sigbx,tau,gval2,icyc,lcheck,istop,igg,
     *                 a1,a2,a3,a4)
         endif
      else
         gval2=0.   ! igg=1 , buckle pas utilisé car sx,sy, ..; = 0
      end if

      return
      end

ccc ********************************************************************
ccc ********************************************************************
      subroutine pltben(iloop2,tpl,short,rlong,e1,eta1,syldp,gammas,
     *      sgshrt,sglong,tau,prsabs,iloop,gval1,icyc,lcheck,istop,  !avril2003
     *      sigmag)                                                  !avril2003
      implicit real*8(a-h,o-z)
      dimension pbcoef(2), sigmem(2)

ccc ********************************************************************
ccc name: pltben                             last revision: 23 apr 1992
ccc
ccc program segment: part6
ccc
ccc purpose: evaluate the adequacy parameter for local plate bending
ccc          in the short or long direction (iloop2 = 1 or 2) to cause
ccc          surface yield or, if relevant, a specified amount of
ccc          permanent set wplim.  these are used in the two
ccc          serviceability constraints for plate bending -
ccc          pspbt,pspbl (panel serviceability, plate bending, transversal
ccc          or longitudinal).
ccc          - for bending in the short direction calculate the clamped edge
ccc            stress due to plate bending.
ccc          - in the second case allow for magnification due to compression
ccc            the short direction (eq. 12.5.11 of reference 1).
ccc          - if no permanent set, calculate the von mises equivalent stress
ccc            the combination of membrane (sig1, sig2 and tau) and bending
ccc            (+ and -, corresp. to upper and lower plate surfaces).

ccc rem : cette subroutine n'intègre pas de vérification au voilement
ccc       du panneau étudié. il s'agit d'un simple analyse statique élastique
ccc       seul l'effet de magnification des contraintes est considéré mais
ccc       indépendamment d'un risque de voilement.

ccc
ccc arguments: iloop2 : 1 or 2 for bending across short or long direction
ccc            wplim  : allowable permanent set (deflection)
ccc            tpl    : plate thickness
ccc            short,rlong   : plate dimensions
ccc            sgshrt,sglong : membrane stress in short and long direction
ccc            tau    : shear stress
ccc            prsabs : pressure
ccc            iloop  : tells which direction of bending in terms of panel
ccc                     1 = transverse (y axis); 2 = longitudinal (x axis)
ccc            gval1  : constraint function value (von mises)
ccc            icyc   : number of iterations already used < cyc(max)=30 (icycm)
ccc            syldp  : sig elastique = yield stress (n/m2)
ccc            gammas : security factor
ccc            istop  : 0 convergence , 1 non convergence
ccc
ccc common blocks: nil
ccc
ccc called by: pleval
ccc
ccc calls to : psetpr
ccc
ccc external files: nil
ccc
ccc ********************************************************************

      pi=2.d00*asin(1.d00)
      data pbcoef / 0.19, 0.03 /
c**********************end of all declarations**************************

c  *********************************************************************
c                 logic is explained via
c                 principal algorithm steps
c                    § 1.0   to   §  9.0
ccc ********************************************************************
c
c      write(67,*) 
c      write(67,*) 'départ   sgshrt=',sgshrt,' sglong=',sglong
c      write(67,*) '         iloop=',iloop,  ' iloop2=',iloop2

ccc ***
ccc 1.0 calculate basic parameters (voir fig. 9.6 p.338)
ccc ***
      sigmem(1) = sgshrt
      sigmem(2) = sglong
      sgmem1 = sigmem(iloop2)
      sgmem2 = sigmem(3-iloop2)
      bovera = short/rlong
      arplt = 1.0/bovera
      bot2 = (short/tpl)**2
      skben = 0.31 + pbcoef(iloop2)*tanh(2.*iloop2*(arplt-1.))
      sigben = skben*prsabs*bot2
      phi = 1.0

c      write(67,*) 'syldp                =',syldp
c      write(67,*) 'sigben (effet plaque)=',sigben

c
c  for bending in the long direction, magnification is not relevant
c      write(67,*) 'avt 3.0  skben =',skben
c      write(67,*) '         sgmem1=',sgmem1,' sgmem2=',sgmem2
c      write(67,*) '         bovera=',bovera,' tpl=',tpl
c      write(67,*) '         sigben=',sigben
c      write(67,*) 'syldp=',syldp

      if (iloop2 .eq. 2) go to 24  ! skip; no magnification (phi=1) as bending in the long direction.
ccc ***
ccc 3.0 bending across the short span; check for magnification
ccc ***
      if (sgshrt .ge. 0.) go to 24  ! skip; no membrane stress in the short direction ==> no need for ampllification
ccc
ccc 3.1  for use in eq. 12.5.11, calculate sgshcr, the critical value
ccc      of sgshort that would cause elastic buckling of a plate with
ccc      loaded edges simply supported and sides clamped.  the buckling
ccc      coefficient is given in fig. 12.5b, but we here change it to a
ccc      wide plate form (skwide) and use a "curve-fitted" expression.
ccc
ccc  note:  in chapter 12 - buckling of wide plates - "a" and "b" are
ccc         the reverse of the convention used here (for laterally loaded
ccc         plates, as in chapter 9).
ccc
      skwide = 0.96*(0.9 + 1.8*bovera**2)**2
      sgshcr = skwide*e1/bot2
ccc
ccc 3.2  calculate the magnification factor, phi, from eq. 12.5.11.
ccc
      omega = -sgshrt/sgshcr
      xipanl = 0.5*pi*sqrt(abs(omega))
      xizero = (1.0 + 0.35*bovera) * xipanl
      if (xizero .lt. 0.02) then
         xzero2 = xizero*xizero
         xzero4 = xzero2*xzero2
         phi = (1. + 0.4*xzero2 + 17./105.*xzero4) /
     1         (1. + xzero2/3.0 + 2.0/15.0*xzero4)
      else if (xizero .le. 3.10) then
         phi = 3. * ( tan(xizero) - xizero ) /
     1                    ( xizero**2 * tan(xizero) )
      else
c        phi = 99999.0
         phi = 2.5
         if (icyc .eq. 30 .or. lcheck .eq. 1) then
            write (*, 1000) sgshrt, sgshcr, phi
            write (6970, 1000) sgshrt, sgshcr, phi
            write (67,1000) sgshrt, sgshcr, phi
		  write (29,1000) sgshrt, sgshcr, phi	!sept06
c           pause'ok?'
c           istop=1
         end if
      end if

ccc
ccc 3.4  calculate the magnified plate bending stress.
ccc
  24  sigmag = phi*sigben

c      write(67,*) 'sigmag (effet plaque*phi)=',sigmag

ccc ***
ccc 4.0  calculate the von mises stress for the combination of membrane
ccc ***  and bending stresses (for both tensile and compressive values o
ccc      the latter) and take the larger of the two : sigvm.
ccc
c  verification of the in plane stress level:
      if (icyc .eq. 0 ) then
         sigvmt = gammas *
     *        sqrt(sgmem1**2 + sgmem2**2 + 3.*tau**2 - sgmem1*sgmem2)
         if (sigvmt.gt.syldp) then
            istop=1
            write (*, 1005) sigvmt
            write (6970, 1005) sigvmt
            write (67,1005) sigvmt
		  write (29,1005) sigvmt		!sept06
            return
         end if
      end if

c  tensile (t) bending stress:
c     stress in primary (1) bending direction
      sig1t = sgmem1 + sigmag
c     stress in secondary (2) direction (with poisson's ratio bending stre
      sig2t = sgmem2 + eta1*sigmag
      sigvmt = sqrt(sig1t**2 + sig2t**2 + 3.*tau**2 - sig1t*sig2t)
c
c  compressive (c) bending stress
c     stress in primary (1) bending direction
      sig1c = sgmem1 - sigmag
c     stress in secondary (2) direction (with poisson's ratio bending stre
      sig2c = sgmem2 - eta1*sigmag
      sigvmc = sqrt(sig1c**2 + sig2c**2 + 3.*tau**2 - sig1c*sig2c)

      if (sigvmt .gt. sigvmc) then
          sigvm = sigvmt
      else
          sigvm = sigvmc
      end if

c     gammas = safety factor
c     ----------------------
      gamstr = gammas*sigvm


ccc ***
ccc 5.0  calculate the factored strength ratio sr1, and the
ccc ***  constraint function value gval1
ccc      si sr1=1, alors gval1=0, on est a la solution.
ccc
      sr1 = gamstr/syldp
      gval1 = 2./(1.+sr1) - 1.

      if (sr1 .gt. 1.02) then
         if (icyc .eq. 30 .or. lcheck .eq. 1) then
            istop=1
            write (*, 1005) sigvm
            write (6970, 1005) sigvm
            write (67,1005) sigvm
		  write (29,1005) sigvm		!sept06
         end if
      end if

      return
c
 1000 format(/,' ****************error*************************'/,
     * ' excessive in-plane compressive stress in the short direction'/
     * ' (',g10.3,', compared to a simply supported buckling stress',
     * ' of',g10.3,').'/
     * ' hence the magnification factor, calculated to be',g10.3,
     * ' from eq. 12.5.11, is not valid.')
 1005 format(/' excessive in-plane compressive stress,'
     * ,' giving a von mises stress of ',g10.3,' n/m2'/
     *  ' this is too large to permit the calculation of a'
     * ,' pressure that would just initiate yield.'/
     *  ' therefore we will not consider this constraint'/)
      end


ccc ********************************************************************
      subroutine buckle(tpl,xlen,ylen,e1,eta1,syldp,gammas,
     *       sigbx,sigby,tau,gval2,icyc,lcheck,istop,igg,
     *       sr2,rxc,ryc,rsc)
      implicit real*8(a-h,o-z)

ccc ********************************************************************
ccc name: buckle                             created on   : 28-5-96
ccc                                          last revision: 16-7-97
ccc author: dr. ph. rigo
ccc
ccc purpose: cette subroutine intègre une vérification au voilement
ccc          du panneau étudié.
ccc          elle sert au calcul de l'épaisseur minimale requise pour`
ccc          éviter le voilement.
ccc
ccc          cette soubroutine est valable pour xlen > ylen

c               Œ y et sigma y
c               i
c               *****************************
c               *                           *  b=ylen
c               *                           *
c               *                           *
c               ***************************** --------> x et sigma x
ccc             <---------- a=xlen --------->
ccc
ccc
ccc arguments: tpl    : plate thickness
ccc            xlen,ylen   : plate dimensions (long and short)
ccc            sigbx,sigby : membrane stress in long and short direction
ccc            tau    : shear stress
ccc            gval2  : constraint function value (buckling)
ccc            icyc   : number of iterations already used < cyc(max)=30
ccc            syldp  : sig elastique = yield stress (n/m2)
ccc            secu   : security factor on the strength = 1.1 (as recommended by eurocode for buckling)
ccc            secu2  : security factor on the stress  (= gammas = sadm/sy)
ccc            lcheck : 1 dernière itération (sinon 0)
ccc            istop  : 0 convergence , 1 non convergence
ccc            igg    : =1 si sollicitation (in plane loads) nulle (0 sinon)
ccc
ccc called by: pleval
ccc
ccc ********************************************************************
c**********************end of all declarations**************************

      pi=2.d00*asin(1.d00)

c     secu  : safety on strength
      secu=1.10    ! 1.10 on the strength (see eurocode)
c     secu=1.0   

c     secu2 : safety on loads  
c     (as recommended by eurocode for non-permanent loads)
c     secu2=1.0
	secu2=gammas ! gammas = sadm/sy; (usually 1.50 for eurocode)
c          sadm and sy are defined in the data file of lbr5

ccc ***
ccc 1.0 calculate basic parameters
ccc ***
      if(sigbx.gt.0) then
        sx=0.
      else
        sx=-sigbx*secu2
      endif
      if(sigby.gt.0) then
        sy=0.
      else
        sy=-sigby*secu2
      endif

      alpha = xlen/ylen
      alpha2= 1./alpha
      beta=ylen/tpl*sqrt(syldp/e1)

ccc ***
ccc 2.0 calculate rx, ry and rs, eq. 12.4.7  p.416 de hughes
ccc      (4 simply supported boundaries)
ccc ***
      tb =e1*(tpl/ylen)**2
      rxc=3.62 * tb
      rx =sx/rxc
      ryc=(0.905*(1.+alpha2**2)**2) * tb
      ry =sy/ryc
      rsc=0.905*(5.35+4.*alpha2**2) * tb
      rs =dabs(tau*secu2)/rsc

c      write(67,*) 'rx,ry,rs=',rx,ry,rs

ccc ***
ccc 3.0  calculate the factored strength ratio sr2, and the
ccc ***  constraint function value gval2
ccc      sr2=0.625*(1.+0.6/alpha)*ry / (1.-0.625*rx) +  rs*rs/(1.-rx)
ccc      si sr2=1, alors gval2=0, on est a la solution.

      if(rs.le.0.001) then
        if(rx.le.0.01) then
          sr2=ry
        else if(ry.le.0.01) then
          sr2=rx
        else
          if(rx.gt.0.99) rx=0.99
          sr2=0.625*(1.+0.6/alpha)*ry / (1.-0.625*rx)
        endif
      else
        sr2=rs
      endif
      if (sr2 .le.0.0001) then
         igg=1
         gval2=1
         return
      end if

c     secu = safety factor on strength
c     -----------------------------------
      sr2 = secu*sr2
      gval2 = 2./(1.+sr2) - 1.

      if (sr2 .gt. 1.02) then
         if (icyc .eq. 30 .or. lcheck .eq. 1) then
            istop=1
            write (*, 1005) sr2
            write (6970, 1005) sr2
            write (67,1005) sr2
		  write (29,1005) sr2
         end if
      end if

      return
c
 1005 format(/,' *********** error in bukle ********'/
     1 ,' after 30 iterations !!!'
     1 ,' excessive  stress, giving a ratio'
     2 ,' of ',g10.3/
     *  ' therefore we can not consider this constraint'/)
      end
