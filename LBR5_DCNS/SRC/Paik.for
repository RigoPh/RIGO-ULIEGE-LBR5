      subroutine paik(iprint,nel,e1,sigy1,width,lamb,q,delta,
     *                phil,epsa,hxr,dxr,wxr,txr,epsr,entr,ksr,
     *                dspaik,spaik,omt)
      use sharedvar
      implicit real*8(a-h,o-z)
      real*8 lamb,lamb2,is
      dimension dspaik(9)

c*******************************************************************************
c     subroutine paik 
c     ===================
c     cette subrourine calcule la résistance ultime d'une plaque raidie
c     londitudinalement et comprimée, également, longitudinalement.
c     (la pression latéral n'est pas prise en compte)
c
c     il s'agit de la formule empirique de j. paik.  qui est basée 
c     sur le principe de la poutre colonne.
c                                                                       
c   input:e,eta,sigy,width,lamb,q,delta,phil 
c         epsa  et  hxr,dxr,wxr,txr,epsr,entr,ksr
c         avec entr : entredistance réelle entre raidisseurs = d(raid)
c              epsr : largeur effective associée à 1 raid (= l/nbre de raid)
c                                                                       
c  output: dcpaik,spaik,omt 
c	omt = section raidiseur + bordage (complet)
c	om  = section du raid
c     spaik = résistance ultime selon la formule de j. paik
c     dcpaik = dérivée de la résistance ultime spaik par rapport aux xi
c
c     créer : le 22-7-96  par  ph. rigo
c
c     modifications : 
c       - formulations de paik + sensibilités (pour subr csens4) ! 22-7-96
c       - limite sur beta et slen                                ! 1-8-96
c       - prise en compte des _ entre d(raid) et de epsr         ! 6-3-97
c          d(c)/d(epsr)= d(c)/d(d)  *  (d/epsr)**2
c                               
c     dernière modif : 15-07-1997 				     	
c                    :  5-03-2001				     	
c                                                                       
c*************************************************************************
c*************************************************************************
c*************************************************************************
c               ^ y et sigma y
c               i
c               *****************************
c               *                           *  b=epsr (pour lbr4)
c               *                           *  b=entr = d (pour paik)
c               *                           *
c               ***************************** --------> x et sigma x
c               <----------- a = epsa ------>
c
c      epsa  	= a        (m)
c      entr,epsr	= b        (m)
c      sigy		= sig elastique (n/m2), (ex, re=240 n/mm2)
  
c 1.0 calcul de spaik de référence
c     ****************************

c 1.1 caractéristiques géométriques et calcul de á et lamda  
c     -----------------------------------------------------
      tmrx=wxr*txr                              ! section semelle
      om  =tmrx+dxr*hxr                         ! section raid
      omt =om+entr*delta                        ! section raid + d(raid)
      smcr=delta/2.+hxr+txr/2.                                   	
      smdr=delta/2.+hxr/2.                                   		
      tpbr=tmrx*smcr                                				
      hx=tpbr+dxr*hxr*smdr                       ! moment statique (z=0)
	
	gx=2.*hx		
      yg=gx/(2.*omt)                                      ! yg=position du centre de gravité.
      is=(entr*delta**3 + dxr*hxr**3 + wxr*txr**3)/12.  ! is=inertie par rapport au centre de gravité
     *    + delta*entr*yg*yg  +  hxr*dxr*(smdr-yg)**2     !    au centre de gravité.
     *                        +  wxr*txr*(smcr-yg)**2
      rot=dsqrt(sigy1/e1)
      beta=entr * rot / delta
      slen=epsa * rot* dsqrt(omt/is) / pi

c 1.2 test sur beta et lamb=slen (slim= élancement maximum pour un beta donné)
c     -----------------------------------------------------------------------
      ilim=0
	
	if(beta.le.2) then
	   slim=3.0+(beta-1.)/2.
	else
	   slim=3.5+(beta-2.)
	endif
	
	if(slen.gt.slim) then
	  write(666,*)'attention: slen limite dépassé =',slim,
     *                  ' << slen(réel) =',slen
	  write(666,*)'=========='
	  write(29,*)'attention: slen limite dépassé =',slim,					!sept06		!bug
     *                  ' << slen(réel) =',slen								!sept06		!bug
	  write(29,*)'=========='												!sept06		!bug
	  
	  ilim=1
	  slen=slim
	endif

c 1.3 calcul de spaik de référence
c     -----------------------------
	slen2=slen*slen
	beta2=beta*beta
	fct=dsqrt(0.995 + 0.936*slen2 + 0.17*beta2 + 0.188*beta2*slen2
     *                - 0.067*slen2*slen2)
      spaik=sigy1/fct


c 2.0 dérivée de sig(paik) : dspaik
c     *****************************
      if(ilim.eq.0) goto 3
	
c 2.1 cas ou les valeurs limites de béta et lamda  sont dépassées
c     ---------------------------------------------------------
c     dans ce cas le calcul des sensibilités se fait par différences finies.

      nbrxi=nvar(nel)

      do 5 k=1,nbrxi

      kk=nxit(k,nel)

        if(kk.eq.1) then
           ratio=1.05
         xxx=ratio *delta
         call ulk(iprint,e1,sigy1,xxx,epsa,hxr,dxr,wxr,txr,entr,spaik1)
           dspaik(kk)=(spaik1-spaik)/(xxx-delta)
        else if (kk.eq.5) then                      ! xi=epsa
	   ratio=0.5
         xxx=ratio *epsa
         call ulk(iprint,e1,sigy1,delta,xxx,hxr,dxr,wxr,txr,entr,spaik1)
	   dspaik(kk)=(spaik1-spaik)/(xxx-epsa)
        else if (kk.eq.6) then                      ! xi=hxr
	   ind=0
   16    ind=ind+1
	   ratio=5.0*ind
         xxx=ratio *hxr
         call ulk(iprint,e1,sigy1,delta,epsa,xxx,dxr,wxr,txr,entr,
     *            spaik1)
           dspaik(kk)=(spaik1-spaik)/(xxx-hxr)
           if((dabs(dspaik(kk)).le.1000.).and.(ind.le.4)) goto 16
        else if (kk.eq.7) then                      ! xi=tweb
           ratio=10.0
         xxx=ratio *dxr
         call ulk(iprint,e1,sigy1,delta,epsa,hxr,xxx,wxr,txr,entr,
     *            spaik1)
           dspaik(kk)=(spaik1-spaik)/(xxx-dxr)
        else if (kk.eq.8) then                       ! xi= d flange
	   ratio=10.0
         xxx=ratio *wxr
         call ulk(iprint,e1,sigy1,delta,epsa,hxr,dxr,xxx,txr,entr,
     *            spaik1)
	   dspaik(kk)=(spaik1-spaik)/(xxx-wxr)
        else if (kk.eq.9) then                  ! xi=epsr
	   ratio=0.95
         xxx=ratio *entr
         call ulk(iprint,e1,sigy1,delta,epsa,hxr,dxr,wxr,txr,xxx,spaik1)
           dspaik(kk)=(spaik1-spaik)/(xxx-entr)
c        prise en compte de  d(d)/d(epsr) = (d/epsr)**2
	   dspaik(kk)=dspaik(kk)*(entr/epsr)**2
	else
	   dspaik(kk)=0.
	endif
  5   continue
      return
   3  continue

c 2.2 cas normal: les valeurs de béta et lamda sont dans les limites
c     ---------------------------------------------------------
c     dans ce cas le calcul des sensibilités se fait par calcul direct (analytique)

     	fac1=(0.936+0.188*beta2-2.*0.067*slen2)*slen
      fac2=(0.17+0.188*slen2)*beta
	temp= delta*entr*yg - hxr*dxr*(smdr-yg) - wxr*txr*(smcr-yg)
      nbrxi=nvar(nel)

      do 1 k=1,nbrxi

        kk=nxit(k,nel)	
          if(kk.eq.1) then                           ! xi=delta
            depsa=0.                                 ! d(epsa)/dxi  * slen/epsa
            dbeta=-beta/delta                        ! dbeta=d(beta)/dxi
            dgx=om                                   ! dgx  =d(gx) /dxi
            domt=entr                                ! domt =d(omt)/dxi
            dis1=entr*(0.25*delta**2+yg**2) + hxr*dxr*(smdr-yg)  ! dis1 =d(is) /dxi
     *                                   + wxr*txr*(smcr-yg)    !
          else if (kk.eq.5) then                                ! xi=epsa
            depsa=slen/epsa                          ! d(epsa)/dxi  * slen/epsa
	    dbeta=0.
	    dgx=0.	
	    domt=0.									
	    dis1=0.
          else if (kk.eq.6) then                     ! xi=hxr
	    depsa=0.
	    dbeta=0.
	    dgx=om*2.+dxr*delta	
	    domt=dxr									
	    dis1=0.25*dxr*hxr**2 + dxr*(smdr-yg)*(smdr-yg+hxr)
     *                        + 2.*wxr*txr*(smcr-yg)
          else if (kk.eq.7) then                     ! xi=dxr=tweb
	    depsa=0.
	    dbeta=0.
	    dgx=2.*hxr*smdr	
	    domt=hxr									
            dis1=(hxr**3)/12. +  hxr*(smdr-yg)**2     !
          else if (kk.eq.8) then                     ! xi=wxr =lsem.
	    depsa=0.
	    dbeta=0.
	    dgx=2.*txr*smcr
	    domt=txr									
	    dis1=(txr**3)/12. +  txr*(smcr-yg)**2
          else if (kk.eq.9) then                     ! xi=epsr
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
        dis1=dis1 +2.*dyg* temp
	  dslen=slen * (is*domt-omt*dis1) / (2.*omt*is) + depsa
        dspaik(kk)=-sigy1*(fac1*dslen+fac2*dbeta)/(fct**3)
	  
c       prise en compte de  d(d)/d(epsr) = (d/epsr)**2 
        if (kk.eq.9) then
	     dspaik(kk)=dspaik(kk)*(entr/epsr)**2
	  endif
    1  continue
      return 
      end

c ******************************************************************************
c ******************************************************************************
c ******************************************************************************
	
      subroutine ulk(iprint,e1,sigy1,delta,epsa,hxr,dxr,wxr,txr,entr,
     *               spaik)
      implicit real*8(a-h,o-z)
      real*8 is

c*******************************************************************************
c     subroutine ulk
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
c  -------------
      tmrx=wxr*txr                               ! section semelle
      om=tmrx+dxr*hxr                            ! section raid
      omt=om+entr*delta                          ! section raid + bordé
      smcr=delta/2.+hxr+txr/2.                                   	
      smdr=delta/2.+hxr/2.
      tpbr=tmrx*smcr
      hx=tpbr+dxr*hxr*smdr                       ! moment statique (z=0)
	
	gx=2.*hx		
      yg=gx/(2.*omt)                                     ! yg=position du centre de gravité.
        is=(entr*delta**3 + dxr*hxr**3 + wxr*txr**3)/12. ! is=inertie par rapport au centre de gravité
     *    + delta*entr*yg*yg  +  hxr*dxr*(smdr-yg)**2    !    au centre de gravité.
     *                        +  wxr*txr*(smcr-yg)**2
      rot=dsqrt(sigy1/e1)
      beta=entr * rot / delta
      slen=epsa * rot* dsqrt(omt/is) / pi


c   test sur beta et lamb=slen (slim= élancement maximum pour un beta donné)
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
