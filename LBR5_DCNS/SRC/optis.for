      subroutine optis(itera,iteram,width,iprint,nsol,txr_dcns,tya_dcns)!,negalt)
      use sharedvar

c  *****************************************************************
c  soit "nvmax" variables et "mmax" restrictions
c        ****************     ******************
c            avec nsol le nombre de cas de charge (max ismax=5)
c
c  créer: 25-4-94
c  modif: 1-12-95
c         8- 5-96 - suppression des relectures du fichier 201. maintenant,
c                   les variables de conception sont stockées ds le
c                   common/opti5/
c         4-06-96 - restrictions géométriques (m2 et m2cont)
c         1-08-96 - formatage des write(iuin,*)
c        20-03-97 - restrictions relatives à la resistance ultime d'ensemble
c                   (ushull)
c        18-05-98 - fonction objectif cout (subr objec2 et objec3)
c        10-04-99 - allocation mémoire pour lbr-5.1
c        09-11-00 - restr. sur centre de gravite lbr-5.3
c        18-11-01 - sur-épaisseur de corrosion, lbr5-5
c	   05-02-04 - optimisation plaques sans opti épontilles
c
c  dernière modif: 05-02-04
c  *****************************************************************

      implicit real*8 (a-h,o-z)
      character *30 titre
      character *30 vnom2(mmmax)    !vnom2(700)       !juin 2006   dad
	character*3  mode								!janvier 2005
	dimension vnom(9)
	double precision txr_dcns(nmax)
	double precision tya_dcns(nmax)
                   
c ******************************************************************************

      data vnom/'ep. bord','hâme cad','eâme cad','lsem cad','epsa cad',
     *      	  'hâme rai','eâme rai','lsem rai','epsr rai'/
c ******************************************************************************

c	lcont(1,j+im) = nø de référence de la restriction j du panneau nel
c
c  ====================================================================
c
c     ms = dimension maximale pour le vecteur s(1) utile pour la subr.conlin
c     ms =8*n+3*m+n*m+2*max0(n,m)+3
c     ms = 145.103   si n=200 et m=700  (neto=30
c     ms = 813.203   si n=400 et m=2000 (neto=50)
c     ms =5641.403   si n=800 et m=7000 (neto=100)

	pi1=pi/180.

      ms=8*nvmax+3*mmmax+nvmax*mmmax+2*max0(nvmax,mmmax)+3

c     file 301 : contient les données pour conlin.	!extension neto	!fev2007
      iuin=301  !sept06	!extension neto	!fev2007

c     file 302 : contient les dimensions des panneaux pour le calcul				!extension neto	!fev2007
c                des fcts objectifs (cfr. subr ent et object)
c     file 303 : les sensibilités calculées par les subr. geom, contr et ushull	!extension neto	!fev2007
c     le common/opti5/ conserve les variables de conception entre 2 itérations

c     indice d'impression (pour subr. conlin)
      iop=iopti
c    
c     error indicator of the subr. conlin (initially set at 0 )
      non=0
   
c     m1= le nombre de restrictions structurelles pour chaque cas de charge 
c         max=m1tmax pour 1 cas de charge
c     m2= le nombre de restrictions géométriques (max 20 par panneau),
c     m3= le nombre de restrictions particulières
c         soit : rest. sur centre de gravite (subr. coord).
c                   si igrav=1 ou 2, on impose 1 restriction  (min ou max)
c                   si igrav=3     , on impose 2 restrictions (min et max)
c         soit : ult. strength of ship hull (subr. hull).
c                   si irestr=1, on impose 2 restrictions (hogging et sagging)
      m3tot=0
      if(irestr.ge.1)		m3tot=m3tot+2
      if(igrav==1)		m3tot=m3tot+1
      if(igrav==2)		m3tot=m3tot+1
      if(igrav==3)		m3tot=m3tot+2
	if(iweight.ne.0)	m3tot=m3tot+1		!sept06		!restri poids
	if(iprice.ne.0)		m3tot=m3tot+1		!fev2007	!restri cout
	if(inert.ne.0)		m3tot=m3tot+1		!sept06		!restri inertie
	if(imod.ne.0)		m3tot=m3tot+1		!sept06		!restri module

c     m = le nombre total de restrictions pour les nsol cas de charge.
c         il faut m  < mmax (nombre maximum)
      
	m1tot = 0							!sept06
	m2tot = 0							!sept06
	do nel=1, neto						!sept06
		m1tot = m1tot + m1cont(nel)		!sept06
		m2tot = m2tot + m2cont(nel)		!sept06
	enddo								!sept06
	m=m1tot+m2tot+m3tot
    
      if(m.gt.mmmax) then
        write(*,*)
        write(6970,*)
        write(29,*)  'error: le nbre total de restrictions =',m,'>',mmmax
        write(666,*)'error: le nbre total de restrictions =',m,'>',mmmax
        write(*,*)
        write(6970,*)
	  pause 'stop'
        stop
      endif

c23456789012345678901234567890123456789012345678901234567890123456789012

c  ====================================================================
c 1.   reduction des vecteurs nvar et nxit en nvarr et nxitr
c     ******************************************************
c     soit nr le nbre total de variables xi indépendantes

      nr=ntot-negalt
	
      if(itera.eq.1) then
         do 100 nel=1,neto
           nvarr(nel)=nvar(nel)-negal(nel)
           do 100 k=1,9
             nxitr(k,nel)=nxit(k,nel)
  100    continue

  
         if(negalt.gt.0) then
         write(666,*)
         write(666,*) 'variables de conception (réduites), nxi(reduit)'
         ine=0
         ivar=0

         do 101 nel=1,neto                   ! nø du panneau concerné
           do 102 i=1,negal(nel)             
             ine=ine+1                       ! nø de la contr d'égalité
             kk=nxi2(mega(ine,1),nel) - ivar ! ordre du xi concerné ds panneau concerné
             do 103 k=kk+1-i,(nvar(nel)-1)
               nxitr(k,nel)=nxitr(k+1,nel)
  103        continue
	     nxitr(nvar(nel),nel)=0
  102      continue
         ivar=ivar+nvar(nel)
         write(666,'(a,i3,a,9i3)')'nxitr(',nel,')=',(nxitr(l,nel),l=1,9)
  101    continue                            
         endif                               ! if(negalt.gt.0)
	
        endif                                ! if(itera.eq.1)

   
c 2.  restrictions d'egalite entre variables xi
c     (calcul de dx =dxi1/dxi2)
c     ******************************************************
      if(negalt.gt.0) then
	do 116 i=1,ntot
	do 115 j=1,ntot
  115 dxopt(i,j)=0.
  116 dxopt(i,i)=1.

	do 105 l=1,negalt
	i=nxi2(mega(l,1),mega(l,2))
	j=nxi2(mega(l,3),mega(l,4))
      dxopt(i,j)=ega(l)
  105 continue

      endif

c  ====================================================================
c  ====================================================================
		
c  ====================================================================
c 3.0 préparation des données pour conlin avec les résultats de lbr-5
c  ====================================================================

      if(itera.eq.1) then
        write(666,20)
      endif

      rewind(iuin)

      if(iprint.ge.1) write(*,51)
      if(iprint.ge.1) write(*,*)' nr=',nr,' m1=',m1tot,'et m2=',m2tot

      if(iprint.ge.1) write(6970,51)
      if(iprint.ge.1) write(6970,*)' nr=',nr,' m1=',m1tot,'et m2=',m2tot

      k=0
      do 922 nel=1,neto
        if(nvar(nel).ne.0) then
          k2=k+nvar(nel)
          k=k2
        endif
  922 continue

c 3.1 données générales
c     -----------------
	tol=0.0
	cut=0.0
	prec=0.0
	pds=0.0
	jt=0
      jobj=0
	
      write (iuin,*) nr,m             ! nr=n*nsol, m=nbr restrict.
      write (iuin,*) tol,cut,prec,pds


c 3.2 variables de conception xi et les bornes
c     ---------------------------------------
                                      !
      do 39 i=1,ntot
   39 xiopt(i)=xicou(i)                  ! tous les xi (1 à n)
	
      call red1d(xiopt,xiopt,ntot,nxi2,mega,negalt,neto,ngmax) ! réduction à nr
      call red1d(ximin,ximin,ntot,nxi2,mega,negalt,neto,ngmax)				
      call red1d(ximax,ximax,ntot,nxi2,mega,negalt,neto,ngmax)		



      ximin_dcns(:)=ximin(:)
	ximax_dcns(:)=ximax(:)


	
c     impressions des variables de conception xi et des bornes
	if(iopti.le.3) goto 40
      write (*,53)
      write (6970,53)
  3   k=0
      do 38 nel=1,neto
      if(nvarr(nel).ne.0) then
        k2=k+nvarr(nel)
        write (*,32) nel,(vnom(nxitr(i,nel)),i=1,nvarr(nel))
        write (*,33) (ximin(i),i=k+1,k2)
        write (*,34) (xiopt(i),   i=k+1,k2)
        write (*,35) (ximax(i),i=k+1,k2)

        write (6970,32) nel,(vnom(nxitr(i,nel)),i=1,nvarr(nel))
        write (6970,33) (ximin(i),i=k+1,k2)
        write (6970,34) (xiopt(i),   i=k+1,k2)
        write (6970,35) (ximax(i),i=k+1,k2)


	  k=k2
      endif
   38 continue

c     modifications variables de conception xi et des bornes
      write (*,*) ' '
      write (6970,*) ' '
	ico=0
      write (*,*) ' voulez vous modifiez une borne (min ou max) ?'
      write (*,*) ' si non : taper o ( par défaut)'
      write (*,*) ' si oui : taper 1 ( excécution stoppée) '

      write (6970,*) ' voulez vous modifiez une borne (min ou max) ?'
      write (6970,*) ' si non : taper o ( par défaut)'
      write (6970,*) ' si oui : taper 1 ( excécution stoppée) '

      read(*,*) ico
	if(ico.eq.1)  stop

  40  continue

c     sauvetage pour conlin des variables de conception xi et des bornes
      write (iuin,'(9e14.7)') (xiopt(i),   i=1,nr)      ! xi (1 à nr)
      write (iuin,'(9e14.7)') (ximin(i),i=1,nr)
      write (iuin,'(9e14.7)') (ximax(i),i=1,nr)


c	+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c	+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c 4.0  fct objectif et restrictions (géométriques, structurelles et particulières)
c	******************************
c	+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


c 4.1 fonction objectif
c     -----------------
      obj=0.
	objmulti=0.						!multi obj
	call annuld(fiopt,ntot)
	call annuld(fimulti,ntot)		!multi obj
	ncont=0
	fact=0							!multi obj
	
	if(imulti.eq.1) then													!multi obj
	  
c	  if(iicout.eq.(-1)) then												!multi obj
c	    call objin1(obj,fiopt,ntot)											!multi obj
c		fk(3)=obj															!multi obj
c        endif																	!multi obj

c	  if(iicout.eq.0) then													!multi obj
c	    do nel=1,neto														!multi obj
c            nbrxi=nvar(nel)													!multi obj
c            call objpd1(nel,ntot,ncont,nxit(1,nel),nbrxi,obj,fiopt,			!multi obj
c     *                  width,1,spec(nel),itype(nel),isect(nel))				!multi obj
c		  ncont=ncont+nbrxi													!multi obj
c	    enddo																!multi obj
c	    fk(2)=obj															!multi obj
c        endif																	!multi obj

c	  if(iicout.eq.1) then													!multi obj
c	    do nel=1,neto														!multi obj
c            nbrxi=nvar(nel)													!multi obj
c            call objct1(nel,ntot,ncont,xiopt,nxit(1,nel),nbrxi,				!multi obj
c     *                  obj,fiopt,width,iprint,spec(nel),						!multi obj
c     *				  itype(nel),isect(nel))								!multi obj
c	      ncont=ncont+nbrxi													!multi obj
c	    enddo																!multi obj
c		fk(1)=obj															!multi obj
c        endif																	!multi obj
	  
c	  if(iicout.gt.1) then													!multi obj
c	    call sensibcout(fiopt,obj,ntot,width)								!multi obj
c		fk(1)=obj															!multi obj
c	  endif																	!multi obj
	  
	  
c	  if(iicout.eq.(-2)) then												!multi obj
        
 	    if(w3.ne.0) then													!multi obj
		  call objin1(objmulti,fimulti,ntot)								!multi obj
	      fact=fact+(w3*fk(3)/objmulti)**rho								!multi obj
	      do i=1,ntot														!multi obj
	        fiopt(i)=fiopt(i)+rho*(w3*fk(3)/objmulti)**(rho-1.)*			!multi obj
     *		         w3*fk(3)*(-1.)/(objmulti**2.)*(-fimulti(i))			!multi obj
	      enddo																!multi obj
	      objmulti=0.														!multi obj
	      call annuld(fimulti,ntot)											!multi obj
	      rewind 302														!multi obj
		endif																!multi obj
  
	    if(w2.ne.0) then													!multi obj
		  do nel=1,neto														!multi obj
              nbrxi=nvar(nel)													!multi obj
              call objpd1(nel,ntot,ncont,nxit(1,nel),nbrxi,objmulti,			!multi obj
     *                    fimulti,width,1,spec(nel),itype(nel),				!multi obj
     *					isect(nel))											!multi obj
		    ncont=ncont+nbrxi												!multi obj
	      enddo																!multi obj
	      fact=fact+(w2*objmulti/fk(2))**rho								!multi obj
	      do i=1,ntot														!multi obj
	        fiopt(i)=fiopt(i)+rho*(w2*objmulti/fk(2))**(rho-1.)*			!multi obj
     *                 w2/fk(2)*fimulti(i)									!multi obj
	      enddo																!multi obj
	      objmulti=0.														!multi obj
	      call annuld(fimulti,ntot)											!multi obj
	      ncont=0															!multi obj
	      rewind 302														!multi obj
		endif																!multi obj
	  
	    if(w1.ne.0) then													!multi obj


!	    write(7778,*) 'iteration  ',itera                               !redressage !septembre 2007
!	    write(7778,*) '******************'                              !redressage !septembre 2007
!	    write(7778,*) '   '                                             !redressage !septembre 2007
!	    write(7778,*) 'panneau, ','redressage(h/m²) ',                  !redressage !septembre 2007
!     *                  'redressage(éuro) ','poidslot'                    !redressage !septembre 2007



		  if(icout.eq.1) then												!multi obj
              do nel=1,neto													!multi obj
                nbrxi=nvar(nel)												!multi obj
                call objct1(nel,ntot,ncont,xiopt,nxit(1,nel),nbrxi,			!multi obj
     *                      objmulti,fimulti,width,iprint,spec(nel),			!multi obj
     *				      itype(nel),isect(nel))							!multi obj
	          ncont=ncont+nbrxi												!multi obj
	        enddo															!multi obj
		    fact=fact+(w1*objmulti/fk(1))**rho								!multi obj
              do i=1,ntot														!multi obj
		      fiopt(i)=fiopt(i)+rho*(w1*objmulti/fk(1))**(rho-1.)*			!multi obj
     *			       w1/fk(1)*fimulti(i)									!multi obj
	        enddo															!multi obj
	      endif																!multi obj

	      if(icout.gt.1) then												!multi obj
	        call sensibcout(fimulti,objmulti,ntot,width)					!multi obj
		    fact=fact+(w1*objmulti/fk(1))**rho								!multi obj
              do i=1,ntot														!multi obj
		      fiopt(i)=fiopt(i)+rho*(w1*objmulti/fk(1))**(rho-1.)*			!multi obj
     *			       w1/fk(1)*fimulti(i)									!multi obj
	        enddo															!multi obj
	      endif																!multi obj
	    endif																!multi obj

	    obj=fact**(1./rho)													!multi obj
	    do i=1,ntot															!multi obj
	      fiopt(i)=(1./rho)*fact**((1.-rho)/rho)*fiopt(i)					!multi obj
	    enddo																!multi obj

!	    alfbet=obj                                                      !redressage !septembre 2007	  
!	    call spepite(obj,nn,width,totalstrait)                          !redressage !septembre 2007
!         write(7778,*) 'cout sans redress =',alfbet                      !redressage !septembre 2007
!         write(7778,*) 'cout total=',obj		                            !redressage !septembre 2007
!	    do i=1,nvmax												    !redressage !septembre 2007
!	      uuxx=fiopt(i)                                                 !redressage !septembre 2007
!	      fiopt(i)=fiopt(i)+derredress(i)		    					!redressage !septembre 2007
!	      write(7779,*) i,uuxx,derredress(i),fiopt(i)                   !redressage !septembre 2007
!	    enddo															!redressage !septembre 2007



c	  endif																	!multi obj

	  goto 36																!multi obj

	endif																	!multi obj
	
	if(icout.eq.(-1)) then													!sept2006		!obj inertie
        call objin1(obj,fiopt,ntot)											!fev2007
	endif																	!sept2006
		
	if(icout.eq.0) then   ! obj poids										!multi obj
	  do nel=1,neto
          nbrxi=nvar(nel)
          call objpd1(nel,ntot,ncont,nxit(1,nel),nbrxi,obj,					!sept2006
     *              fiopt,width,
     *              1,spec(nel),itype(nel),isect(nel))
	    ncont=ncont+nbrxi
	  enddo
	endif

      if ((icout.gt.0).and.(iredress.eq.1)) then                         !redressage !septembre 2007
	  write(7778,*) 'iteration  ',itera                               !redressage !septembre 2007
	  write(7778,*) '******************'                              !redressage !septembre 2007
	  write(7778,*) '   '                                             !redressage !septembre 2007
	  write(7778,*) 'panneau, ','redressage(h/m²) ',                  !redressage !septembre 2007
     *                'redressage(éuro) ','poidslot'                    !redressage !septembre 2007

	  call spepite(obj,nn,width,totalstrait)                          !redressage !septembre 2007

	          obj =totalstrait                       !dad
	          obj2=totalstrait                       !dad

	endif            

	if(icout.eq.1) then   ! obj cout										!multi obj
	  do nel=1,neto
          nbrxi=nvar(nel)
          call objct1(nel,ntot,ncont,xiopt,nxit(1,nel),nbrxi,obj,
     *              fiopt,width,
     *              iprint,spec(nel),itype(nel),isect(nel))       			!février 2004
	    ncont=ncont+nbrxi
	  enddo
	endif
	
      if (icout.gt.1) then                                              !newcost
	  call sensibcoutopt(fiopt,obj,ntot,width,totalstrait)                           !newcost	!18.03.04
	endif                                                             !newcost
	

      if ((icout.gt.0).and.(iredress.eq.1)) then                        !redressage !septembre 2007

        obj_temp = obj - totalstrait     !dad
        write(7778,*) 'cout sans redress =',obj_temp                    !redressage !septembre 2007
        write(7778,*) 'cout total=',obj		                          !redressage !septembre 2007


	  do i=1,nvmax												      !redressage !septembre 2007
	    uuxx=fiopt(i)                                                 !redressage !septembre 2007
	    fiopt(i)=fiopt(i)+derredress(i)		    					  !redressage !septembre 2007
	    write(7779,*) i,uuxx,derredress(i),fiopt(i)                   !redressage !septembre 2007
	  enddo															  !redressage !septembre 2007
	endif		


   36 if(iprint.ge.1) then
        write(*,54) obj
        write(6970,54) obj
	endif

	if (impr2.ge.0) then			!sept06
      write (666,*)'dérivées fct obj. n variables (dér. non modifiées)' ! juin 2003
      write (666,16) (fiopt(i),i=1,ntot)
	endif						!sept06
	

		
c 4.2 restrictions géométriques
c     --------------------------
	do 21 i=1,ntot
	do 21 j=1,m
  21  cijopt(i,j)=0.  

      kcont=0

	if(iprint.ge.1) then
	   write(*,*) 'restrictions geometriques'		!sept06
	   write(6970,*) 'restrictions geometriques'		!sept06
	endif
	
	do 10 nel=1,neto
	  mnel=m2cont(nel)
      do 12 j=1,mnel
	  kcontr=kcont+j
	  read(303) ncont											!extension neto	!fev2007
	  read(303) (cijopt(i+ncont,kcontr),i=1,nvar(nel)),
     *            cjopt(kcontr),cjmopt(kcontr),vnom2(kcontr),
     *            ic_rest(kcontr),ratio_rest(kcontr)                !extension neto	!fev2007 dad


!	  read(6969,*) ncont											!extension neto	!fev2007
!	  read(6969,*) (cijopt(i+ncont,kcontr),i=1,nvar(nel)),
!     *            cjopt(kcontr),cjmopt(kcontr),vnom2(kcontr),
!     *            ic_rest(kcontr),ratio_rest(kcontr)                !extension neto	!fev2007 dad


   12 continue
        kcont=kcont+mnel
   10 continue

c 4.3 restrictions structurelles + restrictions particulières (sagging & hogging)
c     ===========================================================================

c 4.3.1 restrictions sur centre de gravite
c       ---------------------------------
c        if(iprint.ge.1) write(*,*) 'restrictions geometriques'	!sept06
        if(igrav.ge.1) then
           if(iprint.ge.1) then
		    write(*,*)'restrictions centre de gravite'
		    write(6970,*)'restrictions centre de gravite'
	     endif
           ig=1
	     if(igrav==3) ig=2
           do j=1,ig
             kcont=kcont+1
             read(303) cjopt(kcont),cjmopt(kcont),vnom2(kcont)                !extension neto	!fev2007
             read(303) (cijopt(i,kcont),i=1,ntot)								!extension neto	!fev2007
!             read(6969,*) cjopt(kcont),cjmopt(kcont),vnom2(kcont)                !extension neto	!fev2007
!             read(6969,*) (cijopt(i,kcont),i=1,ntot)								!extension neto	!fev2007
           enddo
        endif

c	  restriction sur inertie							!restri inertie		!sept06
c	  -----------------------
	  if(inert.ne.0) then													!sept06
          if(iprint.ge.1) then
		   write(*,*)'restrictions sur inertie'				!sept06
		   write(6970,*)'restrictions sur inertie'				!sept06
	    endif
          kcont=kcont+1														!sept06
          read(303) cjopt(kcont),cjmopt(kcont),vnom2(kcont)					!extension neto	!fev2007
          read(303) (cijopt(i,kcont),i=1,ntot)								!extension neto	!fev2007
!          read(6969,*) cjopt(kcont),cjmopt(kcont),vnom2(kcont)					!extension neto	!fev2007
!          read(6969,*) (cijopt(i,kcont),i=1,ntot)								!extension neto	!fev2007
c         if(iprint.ge.1) then												!sept06
c         write(*,*)'la restriction nø',kcont,':',vnom2(kcont),				!sept06
c     *           '       cj(',kcont,') =',cj(kcont)							!sept06
c         write(*,17)(cij(i,kcont),i=1,n)										!sept06
c         endif																!sept06
c         if(iopti.ge.3) pause 'pause'										!sept06
        endif												!restri inertie		!sept06

c	  restriction sur module							!restri module		!sept06
c	  ----------------------
	  if(imod.ne.0) then													!sept06
          if(iprint.ge.1) then
		   write(*,*)'restrictions sur module sectionnel'		!sept06
		   write(6970,*)'restrictions sur module sectionnel'		!sept06
	    endif
          kcont=kcont+1														!sept06
          read(303) cjopt(kcont),cjmopt(kcont),vnom2(kcont)					!extension neto	!fev2007
          read(303) (cijopt(i,kcont),i=1,ntot)								!extension neto	!fev2007
!          read(6969,*) cjopt(kcont),cjmopt(kcont),vnom2(kcont)					!extension neto	!fev2007
!          read(6969,*) (cijopt(i,kcont),i=1,ntot)								!extension neto	!fev2007
c         if(iprint.ge.1) then												!sept06
c         write(*,*)'la restriction nø',kcont,':',vnom2(kcont),				!sept06
c     *           '       cj(',kcont,') =',cj(kcont)							!sept06
c         write(*,17)(cij(i,kcont),i=1,n)										!sept06
c         endif																!sept06
c         if(iopti.ge.3) pause 'pause'										!sept06
        endif												!restri module		!sept06


c	  restriction sur poids								!restri poids
c       ---------------------
	  if(iweight.ne.0) then													!sept06
          if(iprint.ge.1) then
		   write(*,*)'restrictions sur poids'					!sept06
		   write(6970,*)'restrictions sur poids'					!sept06
	    endif
          kcont=kcont+1														!sept06
          read(303) cjopt(kcont),cjmopt(kcont),vnom2(kcont)					!extension neto	!fev2007
          read(303) (cijopt(i,kcont),i=1,ntot)								!extension neto	!fev2007
!          read(6969,*) cjopt(kcont),cjmopt(kcont),vnom2(kcont)					!extension neto	!fev2007
!          read(6969,*) (cijopt(i,kcont),i=1,ntot)								!extension neto	!fev2007
c         if(iprint.ge.1) then												!sept06
c         write(*,*)'la restriction nø',kcont,':',vnom2(kcont),				!sept06
c     *           '       cj(',kcont,') =',cj(kcont)							!sept06
c         write(*,17)(cij(i,kcont),i=1,n)										!sept06
c         endif																!sept06
c         if(iopti.ge.3) pause 'pause'										!sept06
        endif																	!sept06


c	  restriction sur cout								!restri cout
c       ---------------------
	  if(iprice.ne.0) then													!fev2007
          if(iprint.ge.1) then 
		   write(*,*)'restrictions sur cout'					!fev2007
	       write(6970,*)'restrictions sur cout'					!fev2007
	    endif
          kcont=kcont+1														!fev2007
          read(303) cjopt(kcont),cjmopt(kcont),vnom2(kcont)					!extension neto	!fev2007
          read(303) (cijopt(i,kcont),i=1,ntot)								!extension neto	!fev2007
!          read(6969,*) cjopt(kcont),cjmopt(kcont),vnom2(kcont)					!extension neto	!fev2007
!          read(6969,*) (cijopt(i,kcont),i=1,ntot)								!extension neto	!fev2007
c         if(iprint.ge.1) then												!fev2007
c         write(*,*)'la restriction nø',kcont,':',vnom2(kcont),				!fev2007
c     *           '       cj(',kcont,') =',cj(kcont)							!fev2007
c         write(*,17)(cij(i,kcont),i=1,n)										!fev2007
c         endif																!fev2007
c         if(iopti.ge.3) pause 'pause'										!fev2007
        endif												!restri cout


c 4.3.2 restrictions structurelles standards
c       -----------------------------------
      if(iprint.ge.1) then
	   write(*,*) 'restrictions structurelles'
	   write(6970,*) 'restrictions structurelles'
      endif
	do 71 nel=1,neto
	  mnel=m1cont(nel)
      do 72 j=1,mnel
	  kcontr=kcont+j
	  read(303) cjopt(kcontr),cjmopt(kcontr),vnom2(kcontr),		!extension neto	!fev2007
     *            ic_rest(kcontr),ratio_rest(kcontr)                !extension neto	!fev2007 dad
	  read(303) (cijopt(i,kcontr),i=1,ntot)						!extension neto	!fev2007
!	  read(6969,*) cjopt(kcontr),cjmopt(kcontr),vnom2(kcontr),		!extension neto	!fev2007
!     *            ic_rest(kcontr),ratio_rest(kcontr)                !extension neto	!fev2007 dad
!	  read(6969,*) (cijopt(i,kcontr),i=1,ntot)						!extension neto	!fev2007
   72 continue
        kcont=kcont+mnel
   71 continue

c 4.3.2 restrictions sur le moment ultime (poutre navire)
c       ---------------------------------------------------
        if(irestr.ge.1) then
           if(iprint.ge.1) then
		    write(*,*)'restrictions moment ultime'
		    write(6970,*)'restrictions moment ultime'
	     endif
c          restriction sur ult (sagging)
c          ............................
           kcont=kcont+1
           read(303) cjopt(kcont),cjmopt(kcont),vnom2(kcont)		!extension neto	!fev2007
           read(303) (cijopt(i,kcont),i=1,ntot)					!extension neto	!fev2007
c          restriction sur ult (hogging)
c          ............................
	     kcont=kcont+1
	     read(303) cjopt(kcont),cjmopt(kcont),vnom2(kcont)		!extension neto	!fev2007
	     read(303) (cijopt(i,kcont),i=1,ntot)					!extension neto	!fev2007
        endif
	
c 4.4 calcul des dérivées réduites pour les nr variables (var. indépendantes)
c     -----------------------------------------------------------------------
      if(negalt.gt.0) then
	call annuld(z1opt,ntot)
      do 106 j=1,ntot
        do 107 i=1,ntot
        z1opt(j)=z1opt(j)+fiopt(i)*dxopt(i,j)
  107   continue
  106 continue
      do 108 j=1,ntot
  108 fiopt(j)=z1opt(j)

      call red1d(fiopt,fiopt,ntot,nxi2,mega,negalt,neto,ngmax)

c		do jj=1,ntot
c	    do ii=1,ntot
c		open(666669, file = 'dxopt&cijopt-dyn.txt')
c		write (666669,*) cijopt(ii,943),dxopt(ii,3)
c		enddo
c		enddo
c		pause

      do 113 k=1,m
	  call annuld(z1opt,ntot)

        do 110 j=1,ntot
          do 111 i=1,ntot
c	    if ((j.eq.3).and.(k.eq.943).and.(i.eq.ntot)) pause
            z1opt(j)=z1opt(j)+cijopt(i,k)*dxopt(i,j)
  111     continue
  110   continue  

        call red1d(z1opt,z1opt,ntot,nxi2,mega,negalt,neto,ngmax)

        do 112 j=1,nr
c	  if ((j.eq.3).and.(k.eq.943)) pause
  112   cijopt(j,k)=z1opt(j)
  113   continue
      endif
	
c 4.5 sauvetage pour conlin (fct objectif)
c     ------------------------------------     !
      write (iuin,*) jobj                      ! type fct object
      write (iuin,'(9e14.7)') obj              ! fct obj
      write (iuin,'(9e14.7)') (fiopt(i),i=1,nr)   ! dérivées

c 4.7 sauvetage pour conlin (restrictions)
c     ------------------------------------
      temp =0.0

      do 7 j=1,m
        write (iuin,*) jt
        write (iuin,'(9e14.7)') cjopt(j),cjmopt(j),temp,temp
        write (iuin,'(9e14.7)') (cijopt(i,j),i=1,nr)
   7  continue
   
      rewind(iuin)
   
c 4.8 impressions des données (xi,ximax et ximin) avant conlin
c     --------------------------------------------------------
c     utilisation des vecteurs temporaires z1opt=xi,z2opt=ximin et z3opt=ximax

      write(666,50)itera
      if(itera.eq.1) then
      do 913 i=1,nr
        z1opt(i)=xiopt(i)
	  z2opt(i)=ximin(i)
	  z3opt(i)=ximax(i)
  913 continue
      if(negalt.gt.0) then
      do 914 i=1,negalt
	  j=nxi2(mega(i,1),mega(i,2))
        do 915 k=1,ntot-j
          z2opt(ntot-k+1)=z2opt(ntot-k)
          z3opt(ntot-k+1)=z3opt(ntot-k)
  915     z1opt(ntot-k+1)=z1opt(ntot-k)
  914 continue
  
      do 916 i=1,negalt
	  j =nxi2(mega(i,1),mega(i,2))
        jj=nxi2(mega(i,3),mega(i,4))
	  if(mega(i,1).eq.9) then									!janvier 2005
	    philj=philn(mega(i,2))
	    philjj=philn(mega(i,4))
	    qj=qn(mega(i,2))
	    qjj=qn(mega(i,4))
	    wj=dabs(philj)*qj*pi1
	    wjj=dabs(philjj)*qjj*pi1
	    mode=modes(mega(i,2))
		if(mode.eq.'EE1'.or.mode.eq.'ES4') then
            xmodej=-1.0
          elseif(mode.eq.'ES1'.or.mode.eq.'ES3'.or.
     *      mode.eq.'EC1'.or.mode.eq.'EL3')  then
            xmodej=-0.5
          elseif(mode.eq.'SL1'.or.mode.eq.'SL2'.or.mode.eq.'EL2') then
            xmodej=+0.5
          elseif(mode.eq.'LL1') then
            xmodej=+1.0
          else
            xmodej=0.0
          endif
	    mode=modes(mega(i,4))
		if(mode.eq.'EE1'.or.mode.eq.'ES4') then
            xmodejj=-1.0
          elseif(mode.eq.'ES1'.or.mode.eq.'ES3'.or.
     *      mode.eq.'EC1'.or.mode.eq.'EL3')  then
            xmodejj=-0.5
          elseif(mode.eq.'SL1'.or.mode.eq.'SL2'.or.mode.eq.'EL2') then
            xmodejj=+0.5
          elseif(mode.eq.'LL1') then
            xmodejj=+1.0
          else
            xmodejj=0.0
          endif
	    z1opt(j)=1./(1./(ega(i)*z1opt(jj))-xmodejj/(ega(i)*wjj)+
     *			 xmodej/wj)											!juil07
		z2opt(j)=1./(1./(ega(i)*z2opt(jj))-xmodejj/(ega(i)*wjj)+
     *			 xmodej/wj)											!juil07
	    z3opt(j)=1./(1./(ega(i)*z3opt(jj))-xmodejj/(ega(i)*wjj)+
     *			 xmodej/wj)											!juil07		!janvier 2005
	  else
	    z1opt(j)=z1opt(jj)*ega(i)
	    z2opt(j)=z2opt(jj)*ega(i)										!janvier 2005
	    z3opt(j)=z3opt(jj)*ega(i)										!janvier 2005
	  endif
  916 continue
      endif

      k=0
      do 414 nel=1,neto
        if(nvar(nel).ne.0) then
          k2=k+nvar(nel)
          write (666,132) nel,(vnom(nxit(i,nel)),i=1,nvar(nel))
          write (666,133) (z2opt(i),i=k+1,k2)
          write (666,131) (z1opt(i),i=k+1,k2)
          write (666,135) (z3opt(i),i=k+1,k2)
          k=k2
        endif
  414 continue
      endif

c  ====================================================================
c 5.0 conlin (s vecteur de travail et ms taille max. de ce vecteur)
c  ====================================================================

      if(langue.eq.1)then
       write(6,'(1x,a,i3,a,i3,a)')
     *  'optimiseur conlin ... (iteration no',itera,')'
       write(6970,'(1x,a,i3,a,i3,a)')
     *  'optimiseur conlin ... (iteration no',itera,')'
      else
       write(6,'(1x,a,i3,a,i3,a)')
     *  'starting conlin optimizer ... (iteration nr',itera,')'
       write(6970,'(1x,a,i3,a,i3,a)')
     *  'starting conlin optimizer ... (iteration nr',itera,')'
      endif
       call conlin (sopt,ms,iuin,iop,non,neto,vnom2,nvarr,nxitr,m1tabl,	!octobre2003	!dcn	!fev2007
     *              m2cont,ig,irestr,nsol,iweight,iprice,inert,imod,
     *              ic_rest,mmmax,ratio_rest)                               !fev2007  dad


c     ++++++++++++++++++++++++++++++++++++++++++++

      if(non.ge.899) then
	write(666,*)' error dans conlin (non=',non,')'
      write(29,*)  ' error dans conlin (non=',non,')'	!sept06
      if(non.ge.900) stop
c     si non=899, on continue !
      endif
	
c  ====================================================================
c 6.0 estimation des restrictions par conlin sur base des dérivées
c  ====================================================================
	do 223 j=1,m
	 tpy=0.
	 do 224 i=1,nr
         tpy= tpy + cijopt(i,j)*(sopt(i)-xiopt(i))
  224  continue
       tpy=cjopt(j)+tpy
       if(iopti.ge.2) write(666,*)'estimation de c(',j,') =',tpy    ! facultatif
  223 continue

c 7.0 vecteur solution xi pour les n var. de conception (xi1 et xi2 ensemble)
c     et reconstruction des vecteurs complets ximin et ximax (n variables)
c     ---------------------------------------------------------------------
      if(negalt.gt.0) then
      do 122 i=1,negalt
	  j=nxi2(mega(i,1),mega(i,2))
        do 120 k=1,ntot-j
          ximin(ntot-k+1)=ximin(ntot-k)
          ximax(ntot-k+1)=ximax(ntot-k)
          sopt(ntot-k+1) =sopt(ntot-k)
  120   continue
  122 continue
  
      do 121 i=1,negalt
	  j =nxi2(mega(i,1),mega(i,2))
        jj=nxi2(mega(i,3),mega(i,4))
	  if(mega(i,1).eq.9) then									!janvier 2005
	    philj=philn(mega(i,2))
	    philjj=philn(mega(i,4))
	    qj=qn(mega(i,2))
	    qjj=qn(mega(i,4))
	    wj=dabs(philj)*qj*pi1
	    wjj=dabs(philjj)*qjj*pi1
	    mode=modes(mega(i,2))
		if(mode.eq.'EE1'.or.mode.eq.'ES4') then
            xmodej=-1.0
          elseif(mode.eq.'ES1'.or.mode.eq.'ES3'.or.
     *      mode.eq.'EC1'.or.mode.eq.'EL3')  then
            xmodej=-0.5
          elseif(mode.eq.'SL1'.or.mode.eq.'SL2'.or.mode.eq.'EL2') then
            xmodej=+0.5
          elseif(mode.eq.'LL1') then
            xmodej=+1.0
          else
            xmodej=0.0
          endif
	    mode=modes(mega(i,4))
		if(mode.eq.'EE1'.or.mode.eq.'ES4') then
            xmodejj=-1.0
          elseif(mode.eq.'ES1'.or.mode.eq.'ES3'.or.
     *      mode.eq.'EC1'.or.mode.eq.'EL3')  then
            xmodejj=-0.5
          elseif(mode.eq.'SL1'.or.mode.eq.'SL2'.or.mode.eq.'EL2') then
            xmodejj=+0.5
          elseif(mode.eq.'LL1') then
            xmodejj=+1.0
          else
            xmodejj=0.0
          endif
	    sopt(j)=1./(1./(ega(i)*sopt(jj))-xmodejj/(ega(i)*wjj)+xmodej/wj)
		ximin(j)=1./(1./(ega(i)*ximin(jj))-xmodejj/(ega(i)*wjj)+
     *    xmodej/wj)
	    ximax(j)=1./(1./(ega(i)*ximax(jj))-xmodejj/(ega(i)*wjj)+
     *    xmodej/wj)														!janvier 2005
	  else
	    sopt(j)    =sopt(jj)*ega(i)
	    ximin(j)=ximin(jj)*ega(i)
	    ximax(j)=ximax(jj)*ega(i)
	  endif
  121 continue
      endif

c 7.1 impressions
c     -----------
      write(666,59)
      k=0

      do 130 nel=1,neto
      if(nvar(nel).ne.0) then
        k2=k+nvar(nel)
        write (666,132)nel,(vnom(nxit(i,nel)),i=1,nvar(nel))  ! noms des xi
        write (666,131)(sopt(i),i=k+1,k2)						! var. concept.
	  k=k2
      endif
  130 continue

c 7.2 sauvetage de la solution si(i) (variables de conception)
c     ---------------------------------------------------------
      do 56 i=1,ntot
   56  xicou(i)=sopt(i)	!sept06
c   56 continue			!sept06


        do 123 i=1,nbrxi
          goto(124,125,126,127,128,129,138,139,140),nxit(i,nel)
  124     delta=xicou(i+ntot)
          goto 123
  125     hya=xicou(i+ntot)
          goto 123
  126     dya=xicou(i+ntot)
          goto 123
  127     wya=xicou(i+ntot)
          goto 123
  128     epsa=xicou(i+ntot)
          goto 123
  129     hxr=xicou(i+ntot)
          goto 123
  138     dxr=xicou(i+ntot)
          goto 123
  139     wxr=xicou(i+ntot)
          goto 123
  140     epsr=xicou(i+ntot)
  123   continue







	rewind 302				

      k=0

      do nel=1,neto


	  read(302) epsa,epsr,delta,hya,dya,wya,tya,		
     *            hxr,dxr,wxr,txr

        nbrxi=nvarr(nel)
        k2=k+nvarr(nel)

        if(isema(nel).ge.1) then
           do 205 i=1,nbrxi

	        if(isema(nel).gt.5) then 
                 if ((itype(nel).ne.5).and.(nxitr(i,nel).eq.3))then !on vérifie si l'on
	              nn=i+k                                 ! a bien sélectionné 
                                                              !comme variable de 
                                                              !conception dya
         
                    if      (isema(nel).eq.6) then 
                               call dcns_geom_n50_51(hya,dya,wya,
     *  						                tya_dcns(nel),              
     *                                        ximin_dcns(nn),
     *                                        ximax_dcns(nn),
     *                                        inda,iprint,nel,
     *                                        ityp1,isect1,impr2)

	              endif


                 elseif ((itype(nel).ne.5).and.(nxitr(i,nel).eq.4))then !on vérifie si l'on
	              nn=i+k                                 ! a bien sélectionné 
                                                              !comme variable de 
                                                              !conception wya



                    if (isema(nel).eq.7) then
                               call dcns_geom_n52_53(hya,dya,wya,     
     *  						                tya_dcns(nel),              
     *                                        ximin_dcns(nn),
     *                                        ximax_dcns(nn),
     *                                        inda,iprint,nel,
     *                                        ityp1,isect1,impr2)


	              endif



                 endif


              endif
  205      continue

        endif








        if(isemr(nel).ge.1) then
           do 206 i=1,nbrxi

	        if(isemr(nel).gt.3) then 
                 if ((itype(nel).ne.5).and.(nxitr(i,nel).eq.7))then !on vérifie si l'on
	              nn=i+k                                 ! a bien sélectionné 
                                                              !comme variable de 
                                                              !conception dxr
         
                    if      (isemr(nel).eq.6) then
                               call dcns_geom_n50_51(hxr,dxr,wxr,              
     *  						                txr_dcns(nel),              
     *                                        ximin_dcns(nn),
     *                                        ximax_dcns(nn),
     *                                        inda,iprint,nel,
     *                                        ityp1,isect1,impr2)


	              endif


                 elseif ((itype(nel).ne.5).and.(nxitr(i,nel).eq.8))then !on vérifie si l'on
	              nn=i+k                                 ! a bien sélectionné 
                                                              !comme variable de 
                                                              !conception wxr



                    if (isemr(nel).eq.7) then
                               call dcns_geom_n52_53(hxr,dxr,wxr,              
     *  						                txr_dcns(nel),              
     *                                        ximin_dcns(nn),
     *                                        ximax_dcns(nn),
     *                                        inda,iprint,nel,
     *                                        ityp1,isect1,impr2)


	              endif

                 endif



              endif
  206      continue
        endif









      k=k2




	enddo





















c  ====================================================================
c 8.0 calcul de la fct objectif avec les nouveaux xi c.à.d. s(i).
c     ------------------------------------------------------------	
      obj2=0.d00
	obj2multi=0.	!multi obj
	fact=0.			!multi obj
      rewind(302)		!extension neto	!fev2007
	nc=1
      
      if(itera.eq.iteram) then
	 itt=1
	 write(666,63)
	else
	 itt=0
	endif
   63 format(/' resultat final (gross thicknesses) :'/20(1h*)) 

	
	if(imulti.eq.1) then													!multi obj
	  
c	  if(iicout.eq.(-1)) then												!multi obj
c	    call objin2(obj2,sopt,itt)											!multi obj
c        endif																	!multi obj

c	  if(iicout.eq.0) then													!multi obj
c	    do nel=1,neto														!multi obj
c		  nbrxi=nvar(nel)													!multi obj
c		  call objpd2(ntot,sopt(nc),nxit(1,nel),nbrxi,obj2,width,			!multi obj
c     *                  iprint,nel,spec(nel),itt,itype(nel),isect(nel))		!multi obj
c	      nc=nc+nbrxi														!multi obj
c		enddo
c        endif																	!multi obj

c	  if(iicout.eq.1) then													!multi obj
c	    do nel=1,neto														!multi obj
c		  nbrxi=nvar(nel)													!multi obj
c	      call objct2(ntot,sopt(nc),nxit(1,nel),nbrxi,obj2,width,			!multi obj
c     *                  iprint,nel,spec(nel),itt,itype(nel),isect(nel))		!multi obj
c	      nc=nc+nbrxi														!multi obj
c		enddo																!multi obj
c        endif																	!multi obj
	  
c	  if(iicout.gt.1) then													!multi obj
c	    call sensibcout2(obj2,sopt,itt,width,ntot)							!multi obj
c	  endif																	!multi obj
	  
	  
c	  if(iicout.eq.(-2)) then												!multi obj
        
 	    if(w3.ne.0) then													!multi obj
		  call objin2(obj2multi,sopt,itt)									!multi obj
	      fact=fact+(w3*fk(3)/obj2multi)**rho								!multi obj
	      write (666,64) obj2multi											!multi obj
	      obj2multi=0.														!multi obj
		  rewind 302														!multi obj
		endif																!multi obj
  
	    if(w2.ne.0) then													!multi obj
	      do nel=1,neto														!multi obj
		    nbrxi=nvar(nel)													!multi obj
		    call objpd2(ntot,sopt(nc),nxit(1,nel),nbrxi,obj2multi,			!multi obj
     *                    width,iprint,nel,spec(nel),itt,itype(nel),			!multi obj
     *					isect(nel))											!multi obj	
     	        nc=nc+nbrxi														!multi obj
		  enddo																!multi obj
	      fact=fact+(w2*obj2multi/fk(2))**rho								!multi obj
	      write (666,62) obj2multi											!multi obj
		  obj2multi=0.														!multi obj
		  nc=1																!multi obj
		  rewind 302														!multi obj
		endif																!multi obj
	  
	    if(w1.ne.0) then													!multi obj

!   	      call spepite2(obj2,sopt,width)		                            !redressage !septembre 2007


		  if(icout.eq.1) then												!multi obj
	        do nel=1,neto													!multi obj
		      nbrxi=nvar(nel)												!multi obj
                call objct2(ntot,sopt(nc),nxit(1,nel),nbrxi,obj2multi,		!multi obj
     *                      width,iprint,nel,spec(nel),itt,itype(nel),		!multi obj
     *					  isect(nel))										!multi obj
     	          nc=nc+nbrxi													!multi obj
		    enddo															!multi obj
		    fact=fact+(w1*obj2multi/fk(1))**rho								!multi obj
			write (666,60) obj2multi										!multi obj
	      endif																!multi obj

	      if(icout.gt.1) then												!multi obj
	        call sensibcout2(obj2multi,sopt,itt,width,ntot)					!multi obj
		    fact=fact+(w1*obj2multi/fk(1))**rho								!multi obj
			write (666,60) obj2multi										!multi obj
	      endif																!multi obj
	    endif																!multi obj

	    obj2=fact**(1./rho)													!multi obj

c	  endif																	!multi obj

	  goto 31																!multi obj

	endif																	!multi obj


	if(icout.eq.(-1)) then	!inertie
        call objin2(obj2,sopt,itt)								!obj inertie	!fev2007
	  goto 31
	endif
	
	do 30 nel=1,neto
        nbrxi=nvar(nel)
	  
        if(icout.eq.0) !poids
     *  call objpd2(ntot,sopt(nc),nxit(1,nel),nbrxi,obj2,width,
     *              iprint,nel,spec(nel),itt,itype(nel),isect(nel))


        if(icout.eq.1)  ! cost
     *  call objct2(ntot,sopt(nc),nxit(1,nel),nbrxi,obj2,width,
     *              iprint,nel,spec(nel),itt,itype(nel),isect(nel))

        nc=nc+nbrxi
  30  continue

      if (icout.gt.1) then                                           !newcost
	  call sensibcout2(obj2,sopt,itt,width,ntot)                   !newcost		!18.03.04
	endif                                                          !newcost


	if ((icout.gt.0).and.(iredress.eq.1)) then                     !redressage !septembre 2007
	  call spepite2(obj2,sopt,width)		                       !redressage !septembre 2007
	endif														   !redressage !septembre 2007



   31 if (imulti.eq.1) then				!multi obj
c	  if(iicout.ge.1) then				!multi obj
c	    write (666,60) obj2				!multi obj
c          write (67,160) obj2				!multi obj
c          write (66,160) obj2				!multi obj
c          write (*,  60) obj2				!multi obj
c        endif								!multi obj
c	  if(iicout.eq.0) then				!multi obj
c          write (666,62) obj2				!multi obj
c          write (67,162) obj2				!multi obj
c          write (*,  62) obj2				!multi obj
c          write (66,162) obj2				!multi obj
c	  endif								!multi obj
c	  if(iicout.eq.(-1)) then			!multi obj
c          write (666,64) obj2				!multi obj
c          write (67,164) obj2				!multi obj
c          write (*,  64) obj2				!multi obj
c          write (66,164) obj2				!multi obj
c	  endif								!multi obj
c	  if(iicout.eq.(-2)) then			!multi obj
          write (666,65) obj2				!multi obj
          write (67,165) obj2				!multi obj
          write (*,  65) obj2				!multi obj
          write (6970,  65) obj2				!multi obj
          write (66,165) obj2				!multi obj
c	  endif								!multi obj
	else								!multi obj
	  if(icout.ge.1) then  ! cost
	    write (666,60) obj2
          write (67,160) obj2					!15.10.05
          write (66,160) obj2					!15.10.05
          write (*,  60) obj2
          write (6970,  60) obj2
        endif
	  if(icout.eq.0) then
          write (666,62) obj2
          write (67,162) obj2					!15.10.05
          write (*,  62) obj2
          write (6970,  62) obj2
          write (66,162) obj2					!15.10.05
	  endif
	  if(icout.eq.(-1)) then					!obj inertie
          write (666,64) obj2
          write (67,164) obj2					
          write (*,  64) obj2
          write (6970,  64) obj2
          write (66,164) obj2					
	  endif
	endif								!multi obj

c   31 if(icout.ge.1) then  ! cost
c       write (666,60) obj2
c       write (6,660) obj2/1000.d00
c  660 format(1x,'cout recalcule = ',e14.7, ' keuros ou k$')
c      else
c       write (666,62) obj2
c       write (*,662) obj2/10000.d00
c  662 format(1x,'poids recalcule = ',e14.7, ' tonnes')
c	endif



c  ====================================================================

	return

  900 write(29,*)' eof lors de la lecture de conlin.dat (subr opti)'
      pause 'stop'
      stop
c  ====================================================================
c 9.0 les formats
c     ************
   16 format('d fct /dx =  ',(t14,9(e10.3,1x)))
   17 format('d c(j)/dx =  ',(t14,9(e10.3,1x)))
   18 format(9(e10.3))
   20 format(///' entree dans le module optimisation (conlin-lbr5)'/
     *        50(1h*)//
     *       ' conlin optimizer                      ',
     *       ' logiciel des bordages raidis'/
     *       ' version 2.0                            ',
     *       ' version : l.b.r.- 5.0'/
     *       ' author: prof. c. fleury                ',
     *       ' author: dr. ph. rigo'/)
   32 format('panel nø ',i3,9(1x,a8,3x))
   33 format('bornes min:',9(e11.4,1x))
   34 format('variables_:',9(e11.4,1x))
   35 format('bornes max:',9(e11.4,1x))
   50 format(/t30,'iteration nø ',i3/t30,18(1h*))
   51 format('initialisation des données dans opti'/40(1h+)) 
   52 format('lecture des anciennes données'/40(1h=))
   53 format('les nouvelles donnees'/25(1h-))
   54 format('fct. objectif = ',e14.7/)
   55 format('restrictions  geometriques : panneau nø',i3/50(1h-))
   57 format('restrictions  structurelles: panneau nø',i3/50(1h-))
   58 format('restrictions  d''ensemble (hogg & sagg)'/50(1h-))
   59 format(/' les resultats (toutes les n variables) :'/20(1h-)) 
   60 format(/'fct objectif cout(recalculée)    = ',e14.7, 'euro or $'/)
   62 format(/'fct objectif poids (recalculée)    = ',e14.7, 'n'/)
   64 format(/'fct objectif inertie (recalculée)    = ',e14.7, 'm**4'/)		!obj inertie
   65 format(/'fct objectif combinée (recalculée)    = ',e14.7/)				!multi obj
   61 format('restrictions  sur le centre de gravite '/50(1h-))
  131 format('variables_:',9(e11.4))
  132 format('panel nø',i2,2x,9(1x,a8,2x))
  133 format('bornes min:',9(e11.4))
  135 format('bornes max:',9(e11.4))
  160 format(/'fct objectif cout(après optimisation)= ',e14.7, 'euro'/)
  162 format(/'fct objectif poids (après optimisation)= ',e14.7, 'n'/)
  164 format(/'fct objectif inertie (après optimisation)= ',
     *         e14.7, 'm**4'/)
  165 format(/'fct objectif combinée (après optimisation)= ',e14.7/)			!multi obj
      end
	
c     *****************************************************************
c     *****************************************************************
c     *****************************************************************

      subroutine red1d(x,xr,nd,nxi2,mega,negalt,neto,ngmax)
c     ==================
c
c     modif: 5-9-95                                   ! créer : 5-9-95
c
c     *****************************************************************
      implicit real*8 (a-h,o-z)
      dimension mega(ngmax,4),nxi2(9,neto),x(nd),xr(nd)
      do 3 i=1,nd
    3 xr(i)=x(i)
      do 1 i=1,negalt
        j=nxi2(mega(i,1),mega(i,2))
        jj=j-i+1
        do 2 k=1,nd-jj
          jk=jj+k-1
          xr(jk)=xr(jk+1)
   2  	continue
        xr(nd)=0.e+00
   1  continue
      return
      end





