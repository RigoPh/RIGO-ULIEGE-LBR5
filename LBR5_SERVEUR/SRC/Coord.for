      subroutine coord()!,isymx,isymy) !sept06		!r&d14	!fev2007	!flexion d'axe vert.

	use param_section
      
	implicit double precision (a-h,o-z)
	double precision mod

	dimension ds(9)
      character*30 text

	double precision, allocatable,save :: dkg(:)
	double precision, allocatable,save :: dc(:)
	double precision, allocatable,save :: nch(:)
	double precision, allocatable,save :: n1(:)

	allocate (dkg(9*neto))
	allocate (dc(9*neto))
	allocate (nch(neto))
	allocate (n1(neto+1))
c
c initialisation ..............................................................
	call annuld(dkg,9*neto)
	call annuld(dc,9*neto)
	call annuld(nch,neto)
	call annuld(n1,neto+1)

c***********************************************************************
c
c     calcul des coordonnees x et y des extremites des panneaux dans le repere utilisateur (kxy).
c     calcul des axes neutres et des inerties flexionnelles relatives a ces axes neutres.
c     calcul du centre de gravite (cg dans repere utilisateur) et
c                                 des inerties de masse relatives aux axes passant par le cg
c
c
c     delt = epaisseur moyenne (raidisseurs et traverses compris),
c            utilise pour le calcul des axes neutres et des inerties flexionnelles
c     delt2= epaisseur moyenne (raidisseurs, cadres et traverses compris),
c            utilise pour le calcul du centre de gravite et des inerties de masse
c
c   attention:
c   pour le calcul des inerties, centre de gravite, etc, les panneaux ortotopes sont
c   transformes en panneaux a epaisseur constante. cela signifie que les inerties
c   locales des paneaux autour de leur axe faible (oy) sont negligees. ce signifie aussi
c   que le centre de gravite d'un panneau est approximatif. pour une plaque il est toujours
c   situe au niveau du borde (z=0) et a mi-hauteur (y=h/2).
c
c
c     modifications:                       création: thèse ph. rigo, lbr-4 (1988)
c     ------------                                           ---------
c        7-10-99 :  version lbr 5.1
c        5-11-2000: definition d'un repere utilisateur
c       10-11-2000: restriction sur centre de gravite lbr-5.3c
c
c***********************************************************************
c
c   lecture des donnees sur le repere utilisateur et sur le centre de gravite
c   ==========================================================================
c le repere initi.eq.al de lbr-5 (oxgyg) a son origine a l'extremite du panneau 1.
c  - axe oxg est horizontal et positif vers la droite,
c  - axe oyg est vertical et positif vers le haut,
c
c le repere utilisateur de lbr-5 (oky) a son origine au point de k.
c  - le point k a les coord (xk,yk) dans le repere initial (fournies dans les donnees)
c          x = xg - xk
c          y = yg - yk
c  - axe ox est horizontal et positif vers la droite,
c  - axe oy est vertical et positif vers le haut,
      pi1=pi/180.d00                     !juillet2003
      read(iu_10(iboat),*) text                 ! titre
      read(iu_10(iboat),*) xk,yk                ! xk et yk = coord du centre k du repere utilisateur
	xk9=xk
	yk9=yk

      read(iu_10(iboat),*) igrav,xkgmin,xkgmax  ! kgmin=kg(min) et kgmax=kg(max)

	read(iu_10(iboat),*) text					!restri inertie		!sept2006
	read(iu_10(iboat),*)inert,imin			!restri inertie		!sept2006

	read(iu_10(iboat),*) text					!restri module		!sept2006
	read(iu_10(iboat),*) imod,modmin,ielt		!restri module		!sept2006

c
c  igrav              = 0                     pas de restriction sur la position du centre de gravite,
c                     = 1,2 ou 3  restriction sur le centre de gravite (si iopti>0)
c        igrav=1  kg > kg(min)  valeur minimale imposee (1 restriction)
c        igrav=2  kg < kg(min)  valeur maximale imposee (1 restriction)
c        igrav=3  kg > kg(min) et  (2 restrictions)
c                 kg < kg(min)
c
c            avec kg la position du centre de gravite (g)
c                    par rapport au point (k) , centre du repere utilisateur
c ---------------------------------------------------------------
c

	if(impr2.ge.1) then		!15.10.05		!sept2006
		if(langue==1) write(iu_11(iboat),121)xk,yk,ntn  ! xk et yk = coord du repere utilisateur
		if(langue==2) write(iu_11(iboat),221)xk,yk,ntn 
	endif					!15.10.05		!sept2006
c
c 1  calcul des aono, coord des ntn noeuds dans le repere initial (oxgyg)a l'ext. du panneau 1
c -------------------
      do 110 j2=1,ntn  ! ntn = nbre total de noeuds
        n1(j2)=0
        aono(j2,1)=0.0
        aono(j2,2)=0.0
 110  continue
c
      n2=1
      n1(n2)=1
      do 2000 kt=1,neto+1   ! neto+1 = nbre theorique max de noeuds
        if(kt.eq.n2) goto 118
        n6=kt+1
        do 500 jj=n6,n2
        do 500 kk=1,neto
          if(nch(kk).eq.1) goto 500
          ndd=noeud(kk,1)
          naa=noeud(kk,2)
          if((n1(kt).eq.ndd).and.(n1(jj).eq.naa)) nch(kk)=1
          if((n1(kt).eq.naa).and.(n1(jj).eq.ndd)) nch(kk)=1
  500   continue
  118   nn=n1(kt)
        do 200 j=1,neto  ! neto = nbre total de panneaux
          if(nch(j).eq.1) goto 200
          ndd=noeud(j,1)
          naa=noeud(j,2)
          x=+1.0
          nq=naa
          if(ndd.eq.nn) goto 190
          if(naa.eq.nn) goto 180
          goto 200
  180     x=-1.
          nq=ndd
  190     sph2=vsin(-panneau(j).phil/2.,0.d00)

          s2=vsin(panneau(j).tetas,-panneau(j).phil/2.)
          c29=vcos(panneau(j).tetas,-panneau(j).phil/2.)
          aono(nq,1)=aono(nn,1)+x*2.*panneau(j).q*sph2*s2
          aono(nq,2)=aono(nn,2)-x*2.*panneau(j).q*sph2*c29

          n2=n2+1
          n1(n2)=nq
          nch(j)=1
  200   continue
 2000 continue

c 2.0  repere utilisateur
c ------------------------
c 2.1 changement de repere, coord de aono dans repere utilisateur kxy
c

      do j2=1,ntn  ! ntn = nbre total de noeuds
        aono(j2,1)=aono(j2,1)-xk  ! coord de k =(xk,yk) dans repere initial
        aono(j2,2)=aono(j2,2)-yk
      enddo

c
c 2.2   z(kd,i) = coord (x,y) des pts origines et arrivees des panneaux (kd=1,neto)
c                           dans le repere utilisateur

      if(impr2.ge.0) then		!15.10.05	!sept2006
	  if(langue==1) write(iu_11(iboat),120)
        if(langue==2) write(iu_11(iboat),130)
	endif					!15.10.05	!sept2006
      do 600 kd=1,neto
        ndd=noeud(kd,1)
        naa=noeud(kd,2)
        z(kd,1)=aono(ndd,1)  ! coord  x du noeud depart
        z(kd,2)=aono(naa,1)  ! coord  x du noeud d'arrivee
        z(kd,3)=aono(ndd,2)  ! coord  y du noeud depart
        z(kd,4)=aono(naa,2)  ! coord  y du noeud d'arrivee
        if(impr2.ge.0) then	!15.10.05	!sept2006
	    write(iu_11(iboat),104) kd,z(kd,1),z(kd,3),z(kd,2),z(kd,4)
	  endif					!15.10.05	!sept2006
  600 continue

c     vérification de la longueur des panneaux                           !juillet2003
c     ----------------------------------------                           !juillet2003

      do nel=1,neto
        phi=-panneau(nel).phil
        corde = 2*dabs(panneau(nel).q*dsin(phi/2*pi1))
        if (phi.gt.0.) then
          teta = (90. - panneau(nel).tetas)*pi1
        else
          teta = (-90. - panneau(nel).tetas)*pi1
        endif
        x2 = corde*dcos(-teta + (phi/2)*pi1)
        y2 = corde*dsin(-teta + (phi/2)*pi1)

        erreur = dsqrt((z(nel,1)+x2-z(nel,2))**2
     *                 + (z(nel,3)+y2-z(nel,4))**2)
        if (erreur.gt.0.001) then
          write(*,*) 'attention : error dans le panneau ',nel
          write(*,*) '****************************************'
      write(*,*)'les coordonnées de ce panneau sont incohérentes avec la
     *longueur introduite par l''utilisateur'
      write(*,*)'ordre de grandeur de l''erreur (en m) :',erreur
      write(iu_14(iboat),*) 'attention : error dans le panneau ',nel				
	write(iu_14(iboat),*) '****************************************'			 
	write(iu_14(iboat),*)
     *'les coordonnées de ce panneau sont incohérentes avec	
     *la longueur introduite par l''utilisateur'						
	write(iu_14(iboat),*)
     *'ordre de grandeur de l''erreur (en m) :',erreur			
	write(iu_14(iboat),*)' '
	  endif
      enddo

c
c 3   calcul des inerties flexionnelles (aixx et aiyy)
c ---------------------------------------------------
c     autour des axes kx et ky (repere utilisateur)
c	Calcul de omesx, omesy,ometpart,omesypart,aixx2(),aiyy2(),omet2,omesx2,omesy2

      call momentstatiques(omesx,omesy)

c
c   position des axes neutres (dans le repere utilisateur)
c   ------------------------------------------------------
      ! sans cadre
      
	call axesneutres(nnoeud,nnoeud1,nnoeud2,omesx,omesy,x1,x2)


c   position du centre de gravite (kgx,kgy) (dans le repere utilisateur, kxy)
c   -------------------------------------------------------------------------
      ! avec cadre
    4	if(sym.eq.0) goto 19                                                                             !flexion d'axe vert.
c      if(k.eq.1) goto 19
      if(sym.eq.1) then
        if(iabs(idnint(x1)-idnint(x2)).lt.1) then
          akgy=aono(nnoeud,2)
          akgx=omesx2/omet2
        else
          akgx=aono(nnoeud,1)
          akgy=omesy2/omet2
        endif
        goto 20
      endif
      if(sym.eq.2) then
        if(iabs(idnint(x1)-idnint(x2)).lt.1) then
          akgy=aono(nnoeud1,2)
          akgx=aono(nnoeud2,1)
        else
          akgx=aono(nnoeud1,1)
          akgy=aono(nnoeud2,2)
        endif
        goto 20
      endif

   19 akgx=omesx2/omet2
      akgy=omesy2/omet2

   20 continue

c
c 4  restriction sur la position du centre de gravite kg (=kgy)
c ---------------------------------------------------
      if(iopti.eq.0) goto 14 !juil07	! pas d'optimisation

c     coordonnees dans les axes utilisateurs (kxy)
c
c     igrav       = 0                     pas de restriction sur la position du centre de gravite,
c                     = 1,2 ou 3  restriction sur le centre de gravite (si iopti>0)
c        igrav=1  kg > kg(min)  valeur minimale imposee (1 restriction)
c        igrav=2  kg < kg(min)  valeur maximale imposee (1 restriction)
c        igrav=3  kg > kg(min) et  (2 restrictions)
c                 kg < kg(min)
c
c            avec kg la position du centre de gravite (g)
c                    par rapport au point (k) , centre du repere utilisateur

	write(iu_31(iboat),*)'igrav ',igrav							!eugen (14.10.2007)
      if(igrav.eq.0) then ! pas de restriction sur le kg
        write(iu_11(iboat),'(/a/1x,55(1h*)/)')										!sept2006
     *       ' pas de restriction sur la position du centre de gravite'
        write(iu_31(iboat),'(/a/1x,50(1h*)/)')
     *       ' pas de restriction sur la position du centre de gravite'
        write(iu_31(iboat),*)'  kg actuel  =', akgy,' m'			!eugen (14.10.2007)
	  write(iu_14(iboat) ,'(/a/1x,55(1h*)/)')										!sept2006				!bug
     *	' pas de restriction sur la position du centre de gravite'		!sept2006				!bug
	  goto 14															!sept2006
	endif																!sept2006
	
	write(iu_11(iboat),'(/a/1x,48(1h*)/)')										!sept2006
     *    ' restriction sur la position du centre de gravite'				!sept2006
	write(iu_31(iboat),'(/a/1x,48(1h*)/)')										!sept2006
     *	' restriction sur la position du centre de gravite'				!sept2006
	write(iu_11(iboat) ,*)'  kg actuel  =', akgy,' m'
      write(iu_31(iboat),*)'  kg actuel  =', akgy,' m'   
		
	if(igrav.eq.1) then ! kgmin impose
	  write(iu_14(iboat),*)'  1 restriction est imposee : kgmin < kg'				!sept2006				!bug
        write(iu_11(iboat),*)'  1 restriction est imposee : kgmin < kg'				!sept2006
	  write(iu_11(iboat),*)'  kg minimum =', xkgmin,' m'							!sept2006
	  write(iu_31(iboat),*)'  1 restriction est imposee : kgmin < kg'
        write(iu_31(iboat),*)'  kg minimum =', xkgmin,' m'
      else if(igrav.eq.2) then
        write(iu_14(iboat),*)'  1 restriction est imposee : kg < kgmax'				!sept2006				!bug
	  write(iu_11(iboat),*)'  1 restriction est imposee : kg < kgmax'				!sept2006
	  write(iu_11(iboat),*)'  kg maximum =', xkgmax,' m'							!sept2006
	  write(iu_31(iboat),*)'  1 restriction est imposee : kg < kgmax'
        write(iu_31(iboat),*)'  kg maximum =', xkgmax,' m'
      else if(igrav.eq.3) then
        write(iu_14(iboat),*)
     *'  2 restrictions sont imposees kgmin < kg < kgmax'	!sept2006					!bug
	  write(iu_11(iboat),*)
     *'  2 restrictions sont imposees kgmin < kg < kgmax'	!sept2006
	  write(iu_11(iboat),*)'  kg maximum =', xkgmax,' m'							!sept2006
	  write(iu_11(iboat),*)'  kg minimum =', xkgmin,' m'							!sept2006
	  write(iu_31(iboat),*)
     *'  2 restr. sont imposees : kgmin < kg < kgmax'
        write(iu_31(iboat),*)'  kg maximum =', xkgmax,' m'
        write(iu_31(iboat),*)'  kg minimum =', xkgmin,' m'
      endif

!      rewind(iu_25(iboat))		!extension neto	!fev2007

      call annuld(dkg,9*neto)
	im=0

      do 11 nel=1,neto				!sept2006

	  epsa = panneau(nel).epsa
	  epsr = panneau(nel).epsr
	  delta = panneau(nel).delta
	  hya = panneau(nel).hya
	  dya = panneau(nel).dya
	  wya = panneau(nel).wya
	  tya = panneau(nel).tya
	  hxr = panneau(nel).hxr
	  dxr = panneau(nel).dxr
	  wxr = panneau(nel).wxr
	  txr = panneau(nel).txr
	  q = panneau(nel).q
	  epais = panneau(nel).epais

        nbrxi=nvar(nel,iboat)  ! nbre de variable de conception dans le pannneau nel
        if (nbrxi.eq.0) then
          goto 11						!sept2006
        else
c        ds = d[section(j)]/d xi(j) avec j= numero du panneau
	   phil=-panneau(nel).phil*pi/180
	   if(itype(nel).ne.5) then													!février 2004
	   ds(1)=dabs(q*phil)								! xi=1 epaisseur borde
	   ds(2)=dya/epsa*dabs(q*phil)						! xi=2 hauteur ame cadre
	   ds(3)=hya/epsa*dabs(q*phil)						! xi=3 epaiss ame cadre
	   ds(4)=tya/epsa*dabs(q*phil)						! xi=4 largeur sem cadre
	   ds(5)=-(hya*dya+wya*tya)/(epsa*epsa)*dabs(q*phil)! xi=5 entredistance cadre (epsa)
	   ds(6)=dxr/epsr*dabs(q*phil)						! xi=2 haut ame raid
	   ds(7)=hxr/epsr*dabs(q*phil)						! xi=3 epaiss ame raid
	   ds(8)=txr/epsr*dabs(q*phil)						! xi=4 larg sem raid
	   ds(9)=-(hxr*dxr+wxr*txr)/(epsr*epsr)*dabs(q*phil)! xi=5 entredistance raid (epsr)
		else																!février 2004
		if(isect(nel).eq.3) then					!+++ 26.07.05											!février 2004
	   ds(1)=2*dya/epsa*dabs(q*phil)						 ! xi=1 hauteur ame double t		!février 2004
	   ds(2)=2*hya/epsa*dabs(q*phil)						 ! xi=2 epaiss ame double t		!février 2004
	   ds(3)=2*tya/epsa*dabs(q*phil)						 ! xi=3 largeur sem double t		!février 2004
	   ds(5)=-(2*hya*dya+2*wya*tya)/(epsa*epsa)*dabs(q*phil) ! xi=5 entredistance épontille (epsa)	!février 2004		
		elseif(isect(nel).eq.1) then			!+++ 26.07.05											!février 2004
		ds(1)=(pi*epais)/epsa*dabs(q*phil)				       ! xi=1 diamètre extérieur		!février 2004
		ds(4)=(pi*(hya-2*epais))/epsa*dabs(q*phil)		       ! xi=4 épaisseur de paroi mince !février 2004
		ds(5)=-(pi*(hya-epais)*epais)/(epsa*epsa)*dabs(q*phil) ! xi=5 entredistance épontille !février 2004 
		elseif(isect(nel).eq.2) then				!+++ 26.07.05									!février 2004
       	ds(1)=(4*epais)/epsa*dabs(q*phil)				! xi=1 côté extérieur			!février 2004
		ds(4)=(4*(hya-2*epais))/epsa*dabs(q*phil)		! xi=4 épaisseur de paroi mince !février 2004
		ds(5)=-(4*(hya-epais)*epais)/(epsa*epsa)*dabs(q*phil) ! xi=5 entredistance épontille !février 2004 
		endif																!février 2004
	   endif																!février 2004
        endif

	  sph2=vsin(-panneau(nel).phil/2.,0.d00)						!oct2006
        s1=vsin(panneau(nel).tetas,0.d00)								!sept2006
        s2=vsin(panneau(nel).tetas,-panneau(nel).phil/2.)				!oct2006
	  d2=q*(s1-2.*sph2*s2/phil)										!sept2006
	  	  
	  do 12 j=1,nbrxi												!sept2006
          jj=nxit(j,nel,iboat)  ! no de ref de chaque var. de conception	!sept2006
          imm=im+j								!sept2006
		if(isymx.eq.1) then											!sept2006
		  dkg(imm)=0.												!sept2006
		else														!sept2006
	      dkg(imm)=ds(jj)*((z(nel,3)+d2)-akgy)/omet2				!oct2006
!		  dkg(imm)=(ds(jj)*((z(nel,3)+d2)) - omesy2*ds(jj))
!     *				/(omet2)
	    endif														!sept2006
	
   12  continue														!sept2006

        im=im+nbrxi													!sept2006

  11  continue														!sept2006


c     sauvetage restrictions et sensibilites dans 'file iu_26(iboat)'			!extension neto	!fev2007
c     ------------------------------------------------------
c     borne min : kgmin < kg avec kg > 0 et kgmax > 0
c     restriction : c(x)= kgmin-kg < 0
c        dc(x)/dxi= - dkg/dxi
      if((igrav.eq.1).or.(igrav.eq.3)) then
        text='kgmin < kg'
        c=xkgmin-akgy
        cm=0.0
        do i=1,ntot(iboat)
          dc(i)= - dkg(i)
        enddo

	  kcontr = kcontr+1
	  do i=1,ntot(iboat)
		cijopt(i,kcontr,iboat)=dc(i)
	  enddo
	  cjopt(kcontr,iboat) = c
	  cjmopt(kcontr,iboat) = cm
	  vnom2(kcontr,iboat) = text

!        write(iu_26(iboat),*) c
!	  write(iu_26(iboat),*) cm
!	  write(iu_26(iboat),*) text				!extension neto	!fev2007
!        write(iu_26(iboat),*) (dc(i),i=1,ntot(iboat))		!extension neto	!fev2007


        img=img+1  ! +1 au compteur des restrictions struc
        if(iopti.gt.2) then
		if(impr2.ne.-3) then		!15.10.05						!sept2006
          write(iu_31(iboat),*)
          write(iu_31(iboat),*) 'c,cm,text=',c,cm,'  ',text
          write(iu_31(iboat),*) 'dc dans coord après write iu_26(iboat):
     *'				!extension neto	!fev2007
          write(iu_31(iboat),'(9(e10.3,1x))') (dc(i),i=1,ntot(iboat))
		endif
        endif
      endif

c     borne max : kg < kgmax avec kg > 0 et kgmax > 0
c     restriction : c(x)= kg/kgmax < 1
c        dc(x)/dxi= +1/kgmax * dkg/dxi
      if((igrav.eq.2).or.(igrav.eq.3)) then
        text='kg < kgmax'
        c=akgy-xkgmax
        cm=0.0
        do i=1,ntot(iboat)
          dc(i)= dkg(i)
        enddo

	  kcontr = kcontr+1
	  do i=1,ntot(iboat)
		cijopt(i,kcontr,iboat)=dc(i)
	  enddo
	  cjopt(kcontr,iboat) = c
	  cjmopt(kcontr,iboat) = cm
	  vnom2(kcontr,iboat) = text

!        write(iu_26(iboat),*) c
!	  write(iu_26(iboat),*) cm
!	  write(iu_26(iboat),*) text				!extension neto	!fev2007
!        write(iu_26(iboat),*) (dc(i),i=1,ntot(iboat))		!extension neto	!fev2007

        img=img+1  ! +1 au compteur des restrictions struc
        if(iopti.gt.2) then
		if(impr2.ne.-3) then			!15.10.05			!sept2006
          write(iu_31(iboat),*)
          write(iu_31(iboat),*) 'c,cm,text=',c,cm,'  ',text
          write(iu_31(iboat),*) 'dc dans coord après write iu_26(iboat):
     *'		!extension neto	!fev2007
          write(iu_31(iboat),'(9(e10.3,1x))') (dc(i),i=1,ntot(iboat))
		endif			!15.10.05							!sept2006
        endif
      endif

c 5  calcul des inerties flexionnelles (aixx et aiyy)						
c -------------------------------------------------
c     autour des axes neutres passant pr le point (xneut,yneut)
c      
   14 write(iu_11(iboat),'(//a/1x,34(1h*))')													!sept2006
     *    ' inerties flexionnelles et de masse'		!restri inertie					!sept2006
	if(iopti.ne.0)																	!sept2006
     *write(iu_31(iboat),'(//a/1x,34(1h*))')													!sept2006
     *	' inerties flexionnelles et de masse'		!restri inertie					!sept2006
	
	call calcul_inerties_flexionnelles()

c     impressions
      if(langue==1) then																!sept2006
	  write(iu_11(iboat),124)xneut,yneut,aiyytot,aixxtot										!sept2006
	  if(sym.ne.0) write(iu_11(iboat),125)													!sept2006
	  write(iu_11(iboat),126) aiuutot															!sept2006
	  if(sym.ne.0) write(iu_11(iboat),127)													!sept2006
        write(iu_11(iboat),122)																	!sept2006
	else																			!sept2006
        write(iu_11(iboat),224)xneut,yneut,aiyytot,aixxtot										!sept2006
	  if(sym.ne.0) write(iu_11(iboat),225)													!sept2006
        write(iu_11(iboat),226) aiuutot															!sept2006
	  if(sym.ne.0) write(iu_11(iboat),227)													!sept2006
        write(iu_11(iboat),222)																	!extension neto	!fev2007
	endif																			!sept2006

      do 315 j=1,neto																	!sept2006
        write(iu_11(iboat),123) j,panneau(j).omega,panneau(j).xneu,
     *		panneau(j).yneu,panneau(j).delt,aiyy(j),aixx(j)							!sept2006
 315  continue																		!sept2006

c 6	restriction sur l'inertie ixx		!restri inertie
c ---------------------------------
	if(iopti.eq.0) goto 15															!sept2006
	if(inert.ne.0) then																!sept2006
        write(iu_11(iboat),'(/a/1x,27(1h.))')													!sept2006
     *    ' restriction sur inertie iyy'												!sept2006
	  write(iu_31(iboat),'(/a/1x,27(1h.))')													!sept2006
     *    ' restriction sur inertie iyy'												!sept2006
	  write(iu_11(iboat),'(a,t16,e14.7,t32,a)') ' iyy actuel  = ',							!sept2006
     *	aixxtotpart,'m**4'															!r&d13	!fev2007
	  write(iu_31(iboat),'(a,t16,e14.7,t32,a)') ' iyy actuel  = ',							!sept2006
     *	aixxtotpart,'m**4'															!r&d13	!fev2007
	  write(iu_11(iboat),'(a,t16,e14.7,t32,a)')
     * ' iyy minimum = ',imin,'m**4'					!sept2006
	  write(iu_31(iboat),'(a,t16,e14.7,t32,a)')
     * ' iyy minimum = ',imin,'m**4'				!sept2006
	  im=0																			!sept2006
	  call annuld(dc,9*neto)														!sept2006

	  do 13 nel=1,neto
	    nbrxi=nvar(nel,iboat)

	    if(nbrxi.eq.0) goto 13

	    if(itype(nel).ne.5) then
	  
		  do j=1,nbrxi
		    jj=nxit(j,nel,iboat) 	! no de ref de chaque var. de conception
		    imm=im+j
		    dc(imm) = deriveeixx(nel,jj)
		  enddo
	    else
		  do j=1,nbrxi
			jj=nxit(j,nel,iboat) 	! no de ref de chaque var. de conception
		    imm=im+j
		    dc(imm)=1.e-10
	  	  enddo
	    endif
	    im=im+nbrxi
   13   continue
   																						!sept2006
        text='iyy > imin'																				!sept2006
	  c=-aixxtotpart																				!r&d13	!fev2007
	  cm=-imin
	  
	  kcontr = kcontr+1
	  do i=1,ntot(iboat)
		cijopt(i,kcontr,iboat)=dc(i)
	  enddo
	  cjopt(kcontr,iboat) = c
	  cjmopt(kcontr,iboat) = cm
	  vnom2(kcontr,iboat) = text
	  																					!sept2006
!	  write(iu_26(iboat),*) c
!	  write(iu_26(iboat),*) cm
!	  write(iu_26(iboat),*) text																			!extension neto	!fev2007
!	  write(iu_26(iboat),*) (dc(i),i=1,ntot(iboat))																	!extension neto	!fev2007
	  
	  img=img+1					! +1 au compteur des restrictions struc								!sept2006
	  if(iopti.gt.2) then																			!sept2006
		if(impr2.ne.-3) then																		!sept2006
	    write(iu_31(iboat),*) 																				!sept2006
	    write(iu_31(iboat),*) 'c,cm,text=',c,cm,'  ',text													!sept2006
         write(iu_31(iboat),*) 'dc après write 303:'															!sept2006
          write(iu_31(iboat),'(9(e10.3,1x))') (dc(i),i=1,ntot(iboat))													!sept2006
		endif																						!sept2006
	  endif																							!sept2006
!	  rewind iu_25(iboat)	  																				!extension neto	!fev2007
	endif						! restri inertie													!sept2006																			!sept2006
																			

c6	restriction sur le module ixx/v		!restri module
c -----------------------------------
	if(imod.ne.0) then																				!sept2006
        v=max(dabs(z(ielt,3)-yneutpart),dabs(z(ielt,4)-yneutpart))									!r&d13	!fev2007
	  mod=aixxtotpart/v																				!r&d13	!fev2007
	  write(iu_11(iboat),'(/a/1x,28(1h.))')																	!sept2006
     *    ' restriction sur module iyy/v'																!sept2006
	  write(iu_31(iboat),'(/a/1x,28(1h.))')																	!sept2006
     *    ' restriction sur module iyy/v'																!sept2006
	  write(iu_11(iboat),'(a,t18,e14.7,t34,a)')
     * ' iyy/v actuel  = ',mod,'m**3'								!sept2006
	  write(iu_31(iboat),'(a,t18,e14.7,t34,a)')
     * ' iyy/v actuel  = ',mod,'m**3'								!sept2006
	  write(iu_11(iboat),'(a,t18,e14.7,t34,a)')																!sept2006
     *     ' iyy/v minimum = ',modmin,'m**3'															!sept2006
	  write(iu_31(iboat),'(a,t18,e14.7,t34,a)')																!sept2006														!sept2006
     *     ' iyy/v minimum = ',modmin,'m**3'															!sept2006
	  im=0																							!sept2006
	  call annuld(dc,9*neto)																		!sept2006
	  do 16 nel=1,neto																				!sept2006
	    
          nbrxi=nvar(nel,iboat)  ! nbre de variable de conception dans le pannneau nel						!sept2006																
	    
		if (nbrxi.eq.0) goto 16																		!sept2006
		
		if(itype(nel).ne.5) then
		  
		  do j=1,nbrxi																				!sept2006
	        jj=nxit(j,nel,iboat)		! no de ref de chaque var. de conception							!sept2006
	        imm=im+j
			
			derixx = -deriveeixx(nel,jj) !moins car c la der. de -ixx
	        
		    !Calcul de la dérivée de v
			if ((z(ielt,3)-yneutpart).gt.(z(ielt,4)-yneutpart)) then
				if (z(ielt,3).gt.yneutpart) then
					derv=-deriveeyneutpart(nel,jj)
				else
					derv=deriveeyneutpart(nel,jj)
				endif
			else
				if (z(ielt,4).gt.yneutpart) then
					derv=-deriveeyneutpart(nel,jj)
				else
					derv=deriveeyneutpart(nel,jj)
				endif 
			endif

			dc(imm) = -(derixx*v - aixxtotpart*derv)/v**2
																								!sept2006
	      enddo																						!sept2006
	    else																						!sept2006
	      do j=1,nbrxi																				!sept2006
	        jj=nxit(j,nel,iboat)		! no de ref de chaque var. de conception							!sept2006
	        imm=im+j																				!sept2006
			dc(imm)=1.e-10																			!sept2006
	      enddo																						!sept2006
	    endif																						!sept2006
	    im=im+nbrxi																					!sept2006
  16	  continue
  
        text='i/v > i/vmin'																			!sept2006
	  c=-mod																						!sept2006
	  cm=-modmin
	  
	  kcontr = kcontr+1
	  do i=1,ntot(iboat)
		cijopt(i,kcontr,iboat)=dc(i)
	  enddo
	  cjopt(kcontr,iboat) = c
	  cjmopt(kcontr,iboat) = cm
	  vnom2(kcontr,iboat) = text
	  																					!sept2006
!	  write(iu_26(iboat),*) c
!	  write(iu_26(iboat),*) cm
!	  write(iu_26(iboat),*) text																			!extension neto	!fev2007		
!	  write(iu_26(iboat),*) (dc(i),i=1,ntot(iboat))																	!extension neto	!fev2007
	  
	  																!sept2006	
	  img=img+1					! +1 au compteur des restrictions struc								!sept2006
	  if(iopti.gt.2) then																			!sept2006
		if(impr2.ne.-3) then																		!sept2006			
	    write(iu_31(iboat),*) 																				!sept2006
	    write(iu_31(iboat),*) 'c,cm,text=',c,cm,'  ',text													!sept2006
         write(iu_31(iboat),*) 'dc après write 303:'															!sept2006
          write(iu_31(iboat),'(9(e10.3,1x))') (dc(i),i=1,ntot(iboat))													!sept2006
		endif																						!sept2006
	  endif																							!sept2006
!	  rewind iu_25(iboat)	  																				!extension neto	!fev2007
	endif						! restri module														!sept2006

c
c 7  calcul des inerties de masses : ig(x) et ig(y)
cc -------------------------------------------------
cc     autour des axes passant par le centre de gravite en (kgx,kgy)
   15 aiyytot2=0.																						!sept2006
      aixxtot2=0.																						!sept2006
      aiuutot2=0.																						!sept2006
      do j=1,neto																						!sept2006
        aiyytot2=aiyytot2+aiyy2(j)+panneau(j).omega2*
     *			((panneau(j).xneu-akgx)**2)										!sept2006
        aiuutot2=aiuutot2+aiyy2(j)+panneau(j).omega2*
     *			((panneau(j).xneu)**2)										!sept2006
        aixxtot2=aixxtot2+aixx2(j)+panneau(j).omega2*
     *			((panneau(j).yneu-akgy)**2)										!sept2006
      enddo																							!sept2006


      if((ncondi.eq.0).or.(sym.eq.0)) then                            !flexion d'axe vert.			!sept2006
        goto 22																						!sept2006
      endif																							!sept2006
      if(sym.eq.1) then																				!sept2006
        aiyytot2=2*aiyytot2																			!sept2006
        aixxtot2=2*aixxtot2																			!sept2006
        goto 22																						!sept2006
      endif																							!sept2006
      if(sym.eq.2) then																				!sept2006
        aiyytot2=4*aiyytot2																			!sept2006
        aixxtot2=4*aixxtot2																			!sept2006
        goto 22																						!sept2006
      endif																							!sept2006

   22 continue                                                 !flexion d'axe vert.					!sept2006

c     impressions
      if(langue==1) then																				!sept2006
        write(iu_11(iboat),134)akgx,akgy,aiyytot2,aixxtot2														!sept2006
	  if(sym.ne.0) write(iu_11(iboat),135)																	!sept2006						!sept2006
	  write(iu_11(iboat),136) aiuutot2																		!sept2006
	  if(sym.ne.0) write(iu_11(iboat),137)																	!sept2006
        write(iu_11(iboat),122)																					!sept2006
	else																							!sept2006
	  write(iu_11(iboat),234)akgx,akgy,aiyytot2,aixxtot2														!sept2006
	  if(sym.ne.0) write(iu_11(iboat),235)																	!sept2006
	  write(iu_11(iboat),236) aiuutot2																		!sept2006
	  if(sym.ne.0) write(iu_11(iboat),237)																	!sept2006
        write(iu_11(iboat),222)																					!extension neto	!fev2007
	endif																							!sept2006
      do  j=1,neto																					!sept2006
       write(iu_11(iboat),123)j,panneau(j).omega2,panneau(j).xneu,
     *		panneau(j).yneu,panneau(j).delt2,aiyy2(j),aixx2(j)																	!sept2006
      enddo																							!sept2006

c	Deallocation des vecteurs
	deallocate (dkg)
	deallocate (dc)
	deallocate (nch)
	deallocate (n1)

c     format
c     ---------
  104 format(2x,i2,t8,4(e11.4,1x)/)																	!sept2006
  115 format(20i4)																					!sept2006

  120 format(t2,'panneau',t11,'y depart',t22,'z depart',t33,'y arrivee',								!sept2006
     *      t44,'z arrivee'/)																			!sept2006
  121 format(/' les axes globaux initiaux de la structure (horiz. ',									!sept2006
     * 'oy et vert. oz) ont leur origine'/' au point de depart du ',									!sept2006
     * 'panneau 1'/' dans ce repere, les coordonnees du repere ',										!sept2006
     * 'utilisateur sont:'/t4,'(yk,zk)= ',e11.4,1x,e11.4' m'/											!sept2006
     * ' c''est ce repere ',																			!sept2006
     * 'qui est utilise dans tous les resultats qui suivent!!'//										!sept2006
     * ' nbre de noeuds',i3//' coordonnees des noeuds dans le',										!sept2006
     * ' repere utilisateur:'/t2,50(1h-))										!flexion d'axe vert.	!sept2006
  122 format(/t2,'pour chaque panneau'/t2,19(1h.)/t2,													!sept2006
     *'panneau',t11,'surface(m**2)',t25,'coord.(y,z) centre',											!sept2006
     *' de gravite',t55,'epaiss equiv.',																!sept2006
     *  t70,'inerties izz et iyy (m**4)'/)															!sept2006
  123 format(t2,i3,t11,e11.4,t26,e11.4,t39,e11.4,t55,e11.4,t70,										!sept2006
     *          e11.4,t83,e11.4)																		!sept2006
  124 format(/' 1- inertie flexionnelle (bordé, raid et trav.)'/										!sept2006
     * t2,46(1h-)/' a utiliser dans le dimensionnement structurel'//									!sept2006
     *' pour toute la structure'/ 																	!sept2006
     * t2,23(1h.)/																					!sept2006
     *' yg = ',t8,e11.4,t24,'m',t30,'(position axe neutre vertical
     * suivant axe oy du repère utilisateur)'/														!sept2006
     *' zg = ',t8,e11.4,t24,'m',t30,'(position axe neutre horizontal
     * suivant axe oz du repère utilisateur)'//
     *' izz = ',t8,e14.7,t24,'m**4',t30,'(inertie autour axe neutre
     * vertical)'/
     *' iyy = ',t8,e14.7,t24,'m**4',t30,'(inertie autour axe neutre
     * horizontal)')																					!sept2006
  125 format(' !!! les axes de symétrie sont pris en compte dans
     * le calcul des inerties izz et iyy. les valeurs izz et'/t6,
     * 'iyy données ci-dessus correspondent ainsi à la structure
     * complète et pas à la structure modélisée !!!')													!sept2006
  126 format(/' iuu =',t8,e14.7,t24,'m**4',t30,'(inertie autour axe
     * vertical du repère utilisateur)')																!sept2006
  127 format(' !!! la valeur iuu donnée ci-dessus correspond
     * à la structure modélisée et pas à la structure complète !!!')			!flexion d'axe vert.	!sept2006
  130 format(t2,'panel',t11,'y depart',t22,'z depart',t33,'y arrival',								!sept2006
     *      t44,'z arrival'/)																			!sept2006
  134 format(/' 2- inertie de masse (bordé, raid, cadre et trav.)'/									!sept2006
     * t2,49(1h-)/' a utiliser dans la verification de la stabilite a la								!sept2006
     * flottaison'//' pour toute la structure'/ 														!sept2006
     * t2,23(1h.)/																					!sept2006
     *' yg = ',t9,e11.4,t24,'m',t30,'(position du centre de gravité
     * suivant axe oy du repère utilisateur)'/
     *' zg = ',t9,e11.4,t24,'m',t30,'(position du centre de gravité
     * suivant axe oz du repère utilisateur)'//
     * /' izz = ',t8,e14.7,t24,'m**4',t30,'(inertie autour axe vertical
     * passant par le centre de gravité (yg,zg))'/
     *' iyy = ',t8,e14.7,t24,'m**4',t30,'(inertie autour axe horizontal
     * passant par le centre de gravité (yg,zg))')													!sept2006
  135 format(' !!! les axes de symétrie sont pris en compte dans
     * le calcul des inerties izz et iyy. les valeurs izz et'/t6,
     * 'iyy données ci-dessus correspondent ainsi à la structure
     * complète et pas à la structure modélisée !!!')													!sept2006
  136 format(/' iuu =',t8,e14.7,t24,'m**4',t30,'(inertie autour axe
     * vertical du repère utilisateur)')
  137 format(' !!! la valeur iuu donnée ci-dessus correspond
     * à la structure modélisée et pas à la structure complète !!!')									!sept2006
  221 format(/' the basic frames (oy horiz. and oz vertic.)',											!sept2006
     * ' have their origine at the arriving node of panel 1'/											!sept2006
     * ' in this frame, the origin of the user coordinate-frame is:',									!sept2006
     *  /t4,'(yk,zk)= ',e11.4,1x,e11.4' m'/															!sept2006
     * ' any future coordinates will refer to this user-frames!!'//									!sept2006
     * ' nbre of nodes',i3//t2,'nodes coordinates :'/t2,45(1h-))										!sept2006
  222 format(/t2,'for each panel'/t2,14(1h.)/t2,														!sept2006
     *'panel',t11,'area (m**2)',t25,'coord.(y,z) centre',												!sept2006
     *' of gravity',t55,'equiv.thickness',															!sept2006
     *  t70,'inertia izz and iyy (m**4)'/)															!sept2006
  224 format(/' 1- flexional inertia (plate, stiff and long. girders)'/								!sept2006
     * t2,53(1h-)/' to use in the structural design'//												!sept2006
     *' for the structure'/ 																			!sept2006
     * t2,17(1h.)/																					!sept2006
     *' yg = ',t8,e11.4,t24,'m',t30,'(location of the vertical neutral								!sept2006
     * axis along the oy axis of the user-frames)'/													!sept2006
     *' zg = ',t8,e11.4,t24,'m',t30,'(location of the horizontal neutral								!sept2006
     * axis along the oz axis of the user-frames)'//													!sept2006
     *' izz = ',t8,e14.7,t24,'m**4',t30,'(inertia around the vertical									!sept2006
     * neutral axis)'/																				!sept2006
     *' iyy = ',t8,e14.7,t24,'m**4',t30,'(inertia around the horizontal								!sept2006
     * neutral axis)')																				!sept2006
  225 format(' !!! the symmetry axes are taken into account to determine								!sept2006
     * the inertia izz and iyy. the values izz'/t6,'and iyy											!sept2006
     * given above are then relative to the full structure and not									!sept2006
     * to the modeled structure !!!')																	!sept2006
  226 format(/' iuu =',t8,e14.7,t25,'(inertia around the vertical axis								!sept2006
     * of the user-frames)')																			!sept2006
  227 format(' !!! the value iuu given above is relative to the modeled								!sept2006
     * structure and not to the full structure !!!')													!sept2006
  234 format(/' 2- mass inertia (plate, stiff, frame and long. girders)'								!sept2006
     * /t2,55(1h-)/' to use in the flotation stability analysis'//									!sept2006
     * ' for the structure'/ 																			!sept2006
     * t2,17(1h.)/																					!sept2006
     *' yg = ',t9,e11.4,t24,'m',t30,'(location of the centre of gravity								!sept2006
     * along the oy axis of the user-frames)'/														!sept2006
     *' zg = ',t9,e11.4,t24,'m',t30,'(location of the centre of gravity								!sept2006
     * along the oz axis of the user-frames)'//														!sept2006
     * ' izz = ',t8,e14.7,t24,'m**4',t30,'(inertia around the vertical								!sept2006
     * axis going through the centre of gravity (yg,zg))'/											!sept2006
     *' iyy = ',t8,e14.7,t24,'m**4',t30,'(inertia around the horizontal								!sept2006
     * axis going through the centre of gravity (yg,zg))')											!sept2006
  235 format(' !!! the symmetry axes are taken into account to determine								!sept2006
     * the inertia izz and iyy. the values izz'/t6,'and iyy											!sept2006
     * given above are then relative to the full structure and not									!sept2006
     * to the modeled structure !!!')																	!sept2006
  236 format(/' iuu =',t8,e14.7,t25,'(inertia around the vertical axis								!sept2006
     * of the user-frames)')																			!sept2006
  237 format(' !!! the value iuu given above is relative to the modeled								!sept2006
     * structure and not to the full structure !!!')													!sept2006

      return
      end

	subroutine momentstatiques(omesx,omesy)
c	***************************************************************************
c	Calcul des inerties flexionnelles (aixx et aiyy)
c     ------------------------------------------------
c     autour des axes kx et ky (repere utilisateur)
c	***************************************************************************

	use param_section
      
	implicit double precision (a-h,o-z)
	akgy=0.             !bug2011
	akgx=0.             !bug2011
	d1=0.             !bug2011
	d2=0.             !bug2011
      omega=0.             !bug2011
      omet =0.
      omesx=0.
      omesy=0.
      omesx2=0.  ! WARNING remise à 0 car centre de gravite était erroné
      omesy2=0.  ! WARNING remise à 0 car centre de gravite était erroné
      omet2 =0.
	ometpart=0.		!r&d13	!fev2007
	omesypart=0.	!r&d13	!fev2007

      do 300 j=1,neto
        d1=0.             !bug2011
	  d2=0.             !bug2011   
        phi=-panneau(j).phil
        sph1=vsin(phi,0.d00)
        sph2=vsin(phi/2.d00,0.d00)
        c19=vcos(panneau(j).tetas,0.d00)
        c29=vcos(panneau(j).tetas,phi/2.d00)
        c39=vcos(2.*panneau(j).tetas,phi)
        s1=vsin(panneau(j).tetas,0.d00)
        s2=vsin(panneau(j).tetas,phi/2.d00)
	  delta =panneau(j).delt  ! epaisseur moyenne sans les cadres
        delta2=panneau(j).delt2 ! epaisseur moyenne avec les cadres
        q=panneau(j).q
        phil=phi*pi/180.
        d1=q*(c19-2.*sph2*c29/phil) ! (d1,d2)= coord du centre des axes neutres du panneau j
        panneau(j).d1 = d1
	  d2=q*(s1-2.*sph2*s2/phil) !          par rapport au pt de depart du panneau
	  panneau(j).d2 = d2
        omega =dabs(delta *q*phil) ! section du panneau j (sans cadre)
	  panneau(j).omega = omega
        omega2=dabs(delta2*q*phil) ! section du panneau j (avec cadre)
	  panneau(j).omega2 = omega2

	  if(dabs(phil).gt.0.00001) then
          temp =-d1+q*c19                    ! ** cas des coques
          temp2= d2-q*s1
          aiyy(j)=delta*q*dabs(q*q*(phil/2.+0.5*sph1*c39)    !inertie autour axe vertical
     *                  -4.*q*temp*sph2*c29+temp*temp*phil)
          aixx(j)=delta*q*dabs(q*q*(phil/2.-0.5*sph1*c39)    !inertie autour axe horizontal
     *                  +4.*q*temp2*sph2*s2+temp2*temp2*phil)
		aixxpart(j)=panneau(j).part*aixx(j)						   !inertie autour axe horizontal avec coef. part.	!r&d13	!fev2007
        else
          aiyy(j)=((s1*q*phil)**2+(c19*delta)**2)*delta*q*dabs(phil)/12. ! ** cas des plaques
          aixx(j)=((c19*q*phil)**2+(s1*delta)**2)*delta*q*dabs(phil)/12.
	    aixxpart(j)=((c19*q*phil)**2+(s1*panneau(j).part*delta)**2)*		   !r&d13	!fev2007
     *			    panneau(j).part*delta*q*dabs(phil)/12.					   !avec coef. part.        
        endif

      ! sans cadre
        omet =omet + omega              ! section totale
        omesx=omesx+ omega*(z(j,1)+d1)  ! moment statique horizontal
        omesy=omesy+ omega*(z(j,3)+d2)  ! moment statique vertical
	  ometpart=ometpart  +panneau(j).part*omega				! section totale avec coef. part.			!r&d13	!fev2007
	  omesypart=omesypart+panneau(j).part*omega*(z(j,3)+d2)	! moment statique vertical avec coef. part.	!r&d13	!fev2007
      ! avec cadre
	  aixx2(j)=aixx(j)*delta2/delta
        aiyy2(j)=aiyy(j)*delta2/delta
        omet2 =omet2 + omega2               ! section totale
        omesx2=omesx2 + omega2*(z(j,1)+d1)  ! moment statique horizontal
        omesy2=omesy2 + omega2*(z(j,3)+d2)  ! moment statique vertical
  300 continue

	
	return
	end

!	subroutine 

	subroutine axesneutres(nnoeud,nnoeud1,nnoeud2,omesx,omesy,x1,x2)
c	***************************************************************************
c	CALCUL DE LA POSITION DES AXES NEUTRES

c	***************************************************************************
     
	use param_section,XI_=>XI,XF_=>XF
	
	implicit double precision (a-h,o-z)
	dimension ino(2),ipan(2)

	isymx=0		!sept06
	isymy=0		!sept06
	sym=0
	if(ncondi.eq.0) then											!flexion d'axe vert.
	  goto 17
	else
	  do i=1,2
	    ino(i)=0
	    ipan(i)=0
	  enddo
	  do i=1,ntn
	    if(nno(i,2).eq.7) then
		  if(sym.eq.0) then
		    sym=sym+1
	        ino(sym)=i
	        do l=1,neto
		     if((noeud(l,1).eq.i).or.(noeud(l,2).eq.i)) ipan(sym)=l
		    enddo 
		  else
			xi=1000*aono(i,1)
			xk=1000*aono(ino(sym),1)
			yi=1000*aono(i,2)
			yk=1000*aono(ino(sym),2)
			if((dabs(xi-xk).gt.10.).and.(dabs(yi-yk).gt.10.)) then
			  sym=sym+1
		      ino(sym)=i
	          do l=1,neto
		        if((noeud(l,1).eq.i).or.(noeud(l,2).eq.i)) ipan(sym)=l
		      enddo 
	        endif
		  endif
	    endif
	  enddo
	endif
	if(sym.eq.0) goto 17												!flexion d'axe vert.
	if(sym.eq.1) then
	  nnoeud=ino(1)
	  noeud1=noeud(ipan(1),1)
	  noeud2=noeud(ipan(1),2)
	  x1=1000*aono(noeud1,1)
	  x2=1000*aono(noeud2,1)
	  if(iabs(idnint(x1)-idnint(x2)).lt.1) then
	    yneut=aono(nnoeud,2)
		yneutpart=aono(nnoeud,2)			!r&d13	!fev2007							
		isymx=1								!sept06
		xneut=omesx/omet
	  else
		xneut=aono(nnoeud,1)
		isymy=1								!sept06
		yneut=omesy/omet
		yneutpart=omesypart/ometpart		!r&d13	!fev2007
	  endif
	  goto 18
	endif
	if(sym.eq.2) then
	  nnoeud1=ino(1)
	  nnoeud2=ino(2)
	  noeud1=noeud(ipan(1),1)
	  noeud2=noeud(ipan(1),2)
	  x1=1000*aono(noeud1,1)
	  x2=1000*aono(noeud2,1)
	  isymx=1								!sept06
	  isymy=1								!sept06
	  if(iabs(idnint(x1)-idnint(x2)).lt.1) then
	    yneut=aono(nnoeud1,2)
	    yneutpart=aono(nnoeud1,2)			!r&d13	!fev2007
		xneut=aono(nnoeud2,1)
	  else
		xneut=aono(nnoeud1,1)
	    yneut=aono(nnoeud2,2)
		yneutpart=aono(nnoeud2,2)			!r&d13	!fev2007
	  endif
	  goto 18
	endif
	
   17	xneut=omesx/omet
      yneut=omesy/omet

	yneutpart=omesypart/ometpart			!r&d13	!fev2007

   18 continue

	return
	end

c	******************************************************************
	double precision function deriveeixx(nel,var)
c	CALCUL DES DERIVEES DE LA FONCTION INERTIE IXX par rapport à la 
c	variable var du panneau nel
c	******************************************************************

      
      
	use param_section
      

	implicit double precision (a-h,o-z)
	dimension ds(9),dixx(9)
	integer*4 var

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
	phil = panneau(nel).phil
	q = panneau(nel).q
	epais = panneau(nel).epais
	
	delt = panneau(nel).delt

	! Calcul des dérivées
	sph2=vsin(-panneau(nel).phil/2.,0.d00)
	c19=vcos(panneau(nel).tetas,0.d00)
	s1=vsin(panneau(nel).tetas,0.d00)
	s2=vsin(panneau(nel).tetas,-panneau(nel).phil/2.) 
	
	phil=-panneau(nel).phil*pi/180.

	dixx(1)=q*dabs(phil)/12.*									!r&d13	!fev2007
     *	(panneau(nel).part*((c19*q*phil)**2+							!r&d13	!fev2007
     *	(s1*panneau(nel).part*delt)**2)+								!r&d13	!fev2007
     *	panneau(nel).part*delt*											!r&d13	!fev2007
     *	(2.*(s1*panneau(nel).part*delt)*s1*panneau(nel).part))					!r&d13	!fev2007	! xi=1 épaisseur borde
	dixx(6)=q*dabs(phil)/12.*									!r&d13	!fev2007
     *	(panneau(nel).part*dxr/epsr*((c19*q*phil)**2+					!r&d13	!fev2007
     *	(s1*panneau(nel).part*delt)**2)+								!r&d13	!fev2007
     *	panneau(nel).part*delt*											!r&d13	!fev2007
     *   (2.*(s1*panneau(nel).part*delt)*s1*panneau(nel).part*dxr/epsr))			!r&d13	!fev2007	! xi=6 hauteur ame raidisseur
      dixx(7)=q*dabs(phil)/12.*									!r&d13	!fev2007	
     *	(panneau(nel).part*hxr/epsr*((c19*q*phil)**2+					!r&d13	!fev2007
     *	(s1*panneau(nel).part*delt)**2)+								!r&d13	!fev2007
     *	panneau(nel).part*delt*											!r&d13	!fev2007
     *   (2.*(s1*panneau(nel).part*delt)*s1*panneau(nel).part*hxr/epsr))			!r&d13	!fev2007	! xi=7 epaiss ame raidisseur
      dixx(8)=q*dabs(phil)/12.*									!r&d13	!fev2007				
     *	(panneau(nel).part*txr/epsr*((c19*q*phil)**2+					!r&d13	!fev2007
     *	(s1*panneau(nel).part*delt)**2)+								!r&d13	!fev2007
     *	panneau(nel).part*delt*											!r&d13	!fev2007
     *   (2.*(s1*panneau(nel).part*delt)*s1*panneau(nel).part*txr/epsr))			!r&d13	!fev2007	! xi=8 larg sem raidisseur
      dixx(9)=q*dabs(phil)/12.*									!r&d13	!fev2007
     *    ((-1.)*panneau(nel).part*(hxr*dxr+wxr*txr)/(epsr**2)*			!r&d13	!fev2007
     *    ((c19*q*phil)**2+(s1*panneau(nel).part*delt)**2)+				!r&d13	!fev2007
     *    panneau(nel).part*delt*(2.*(s1*panneau(nel).part*delt)*s1*				!r&d13	!fev2007
     *    (-1.)*panneau(nel).part*(hxr*dxr+wxr*txr)/(epsr**2)))			!r&d13	!fev2007	! xi=9 larg sem raidisseur
	dixx(2)=0 !dixx(6)/1.e3										! xi=2 hauteur ame cadre
	dixx(3)=0 !dixx(7)/1.e3										! xi=3 epaiss ame cadre
	dixx(4)=0 !dixx(8)/1.e3										! xi=4 largeur sem cadre
	dixx(5)=0 !dixx(9)/1.e3										! xi=5 entredistance cadre (epsa)
	
	ds(1)=dabs(q*phil)											! xi=1 epaisseur borde
	ds(6)=dxr/epsr*dabs(q*phil)									! xi=6 haut ame raid
	ds(7)=hxr/epsr*dabs(q*phil)									! xi=7 epaiss ame raid
	ds(8)=txr/epsr*dabs(q*phil)									! xi=8 larg sem raid
	ds(9)=-(hxr*dxr+wxr*txr)/(epsr*epsr)*dabs(q*phil)			! xi=9 entredistance raid (epsr)
	ds(2)=0 !ds(6)/1.e3											! xi=2 hauteur ame cadre
	ds(3)=0 !ds(7)/1.e3											! xi=3 epaiss ame cadre
	ds(4)=0 !ds(8)/1.e3											! xi=4 largeur sem cadre
	ds(5)=0 !ds(9)/1.e3

	domesypart = panneau(nel).part*ds(var)*(z(nel,3)+panneau(nel).d2)
	dometpart = panneau(nel).part*ds(var)
	deriveeixx = 0.

	do i=1,neto
	  if (i.eq.nel) then

		if(isymx.eq.0) then
		  deriveeixx = deriveeixx + (-1.)*(	
     *		dixx(var) + panneau(nel).part*ds(var)*
     *		((panneau(nel).yneu-yneutpart)**2)
     *		+ panneau(nel).part*panneau(nel).omega*2.*
     *		(panneau(nel).yneu-yneutpart)*
     *		(-1.)*(domesypart*ometpart - 
     *		omesypart*dometpart) / ometpart**2)

	    else
		  deriveeixx = (-1.)*(dixx(var)+panneau(nel).part*ds(var)*
     *		((panneau(nel).yneu-yneutpart)**2))
		endif

	  else
		if(isymx.eq.0) then
		  deriveeixx = deriveeixx + (-1.)*(
     *		panneau(i).part*panneau(i).omega*2.*
     *		(panneau(i).yneu-yneutpart)*(-1.)*(domesypart*ometpart - 
     *		omesypart*dometpart) / ometpart**2)
		else

		endif

	  endif
	enddo

	if(sym.eq.1) deriveeixx = 2.*deriveeixx
	if(sym.eq.2) deriveeixx = 4.*deriveeixx

	return
	end function

c	******************************************************************
	double precision function deriveeyneutpart(nel,var)
c	CALCUL DES DERIVEES DE LA FONCTION yneutpart par rapport à la 
c	variable var du panneau nel
c	yneutpart = omesypart/ometpart
c	******************************************************************

	use param_section

	implicit double precision (a-h,o-z)
	dimension ds(9)
	integer*4 var

	hxr = panneau(nel).hxr
	dxr = panneau(nel).dxr
	wxr = panneau(nel).wxr
	txr = panneau(nel).txr
	epsr = panneau(nel).epsr
	q = panneau(nel).q

	phil=-panneau(nel).phil*pi/180. !!!

	ds(1)=dabs(q*phil)											! xi=1 epaisseur borde
	ds(6)=dxr/epsr*dabs(q*phil)									! xi=6 haut ame raid
	ds(7)=hxr/epsr*dabs(q*phil)									! xi=7 epaiss ame raid
	ds(8)=txr/epsr*dabs(q*phil)									! xi=8 larg sem raid
	ds(9)=-(hxr*dxr+wxr*txr)/(epsr*epsr)*dabs(q*phil)			! xi=9 entredistance raid (epsr)
	ds(2)=0 !ds(6)/1.e3											! xi=2 hauteur ame cadre
	ds(3)=0 !ds(7)/1.e3											! xi=3 epaiss ame cadre
	ds(4)=0 !ds(8)/1.e3											! xi=4 largeur sem cadre
	ds(5)=0 !ds(9)/1.e3

	domesypart = panneau(nel).part*ds(var)*(z(nel,3)+panneau(nel).d2)
	dometpart = panneau(nel).part*ds(var)
	deriveeyneutpart = (domesypart*ometpart - omesypart*dometpart) / 
     *					ometpart**2

	return
	end function

c	******************************************************************
	subroutine calcul_inerties_flexionnelles()
c	Calcul des inerties flexionnelles (aixx et aiyy)						
c     ------------------------------------------------
c     autour des axes neutres passant pr le point (xneut,yneut)
c 
	use param_section

	aiyytot=0.d00																	!sept2006
      aixxtot=0.d00																	!sept2006
	aixxtotpart=0.d00																	!r&d13	!fev2007
      aiuutot=0.d00																		!sept2006
      do 301 j=1,neto																		!sept2006
       panneau(j).xneu=z(j,1)+panneau(j).d1 ! coord du centre de gravite d'un panneau dans repere utilisateur
       panneau(j).yneu=z(j,3)+panneau(j).d2
       aiyytot=aiyytot+aiyy(j)+panneau(j).omega*
     *		((panneau(j).xneu-xneut)**2)							
       aiuutot=aiuutot+aiyy(j)+panneau(j).omega*((panneau(j).xneu)**2)
       aixxtot=aixxtot+aixx(j)+panneau(j).omega*
     *			((panneau(j).yneu-yneut)**2)					
	 aixxtotpart=aixxtotpart+aixxpart(j)+panneau(j).part							!r&d13	!fev2007
     *			*panneau(j).omega*((panneau(j).yneu-yneutpart)**2)											!r&d13	!fev2007

  301 continue																		!sept2006


      if(sym.eq.0) goto 21				!flexion d'axe vert.						!sept2006		
	if(sym.eq.1) then																!sept2006
        aiyytot=2*aiyytot																!sept2006
        aixxtot=2*aixxtot																!sept2006
	  aixxtotpart=2*aixxtotpart														!r&d13	!fev2007			
        goto 21																		!sept2006
      endif																			!sept2006
      if(sym.eq.2) then																!sept2006
        aiyytot=4*aiyytot																!sept2006
        aixxtot=4*aixxtot																!sept2006
	  aixxtotpart=4*aixxtotpart														!r&d13	!fev2007		
        goto 21																		!sept2006
      endif																			!sept2006

   21 continue                                                 !flexion d'axe vert.	!sept2006

	return
	end
