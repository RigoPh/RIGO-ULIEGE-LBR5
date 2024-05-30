      subroutine coord(impr,img,ncondi)!,isymx,isymy) !sept06		!r&d14	!fev2007	!flexion d'axe vert.
      use sharedvar
      implicit real*8 (a-h,o-z)
	real *8   mod
      dimension ino(2),ipan(2)
      dimension aiyy(neto),aixx(neto),iyy2(neto),aiyy2(neto),aixx2(neto)
	dimension aixxpart(neto)											!r&d13	!fev2007
      dimension omega2(neto),dkg(9*neto),dc(9*neto),ds(9)
      dimension xneu(neto),yneu(neto),dd1(neto),dd2(neto),
     *          omega(neto),aono(neto+1,2)
      dimension nch(neto),n1(neto+1)
      character*30 text    !dad

c
c initialisation ..............................................................
      do i=1,neto
       nch(i)=0
       n1(i)=0
      enddo
      n1(neto+1)=0
      do i=1,9*neto
       dkg(i)=0.d00
      enddo

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

c
      pi1=pi/180.                     !juillet2003
      read(55,*) text                 ! titre
      read(55,*) xk,yk                ! xk et yk = coord du centre k du repere utilisateur
      read(55,*) igrav,xkgmin,xkgmax  ! kgmin=kg(min) et kgmax=kg(max)

	read(55,*) text					!restri inertie		!sept2006
	read(55,*)inert,imin			!restri inertie		!sept2006

	read(55,*) text					!restri module		!sept2006
	read(55,*) imod,modmin,ielt		!restri module		!sept2006

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
		if(langue==1) write(66,121)xk,yk,ntn  ! xk et yk = coord du repere utilisateur
		if(langue==2) write(66,221)xk,yk,ntn 
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
  190     sph2=vsin(-philn(j)/2.,0.d00)

          s2=vsin(tetas(j),-philn(j)/2.)
          c29=vcos(tetas(j),-philn(j)/2.)
          aono(nq,1)=aono(nn,1)+x*2.*qn(j)*sph2*s2
          aono(nq,2)=aono(nn,2)-x*2.*qn(j)*sph2*c29

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
	  if(langue==1) write(66,120)
        if(langue==2) write(66,130)
	endif					!15.10.05	!sept2006
      do 600 kd=1,neto
        ndd=noeud(kd,1)
        naa=noeud(kd,2)
        z(kd,1)=aono(ndd,1)  ! coord  x du noeud depart
        z(kd,2)=aono(naa,1)  ! coord  x du noeud d'arrivee
        z(kd,3)=aono(ndd,2)  ! coord  y du noeud depart
        z(kd,4)=aono(naa,2)  ! coord  y du noeud d'arrivee
        if(impr2.ge.0) then	!15.10.05	!sept2006
	    write(66,104) kd,z(kd,1),z(kd,3),z(kd,2),z(kd,4)
	  endif					!15.10.05	!sept2006
  600 continue


c     vérification de la longueur des panneaux                           !juillet2003
c     ----------------------------------------                           !juillet2003

      do nel=1,neto
        phi=-philn(nel)
        corde = 2*dabs(qn(nel)*dsin(phi/2*pi1))
        if (phi.gt.0.) then
          teta = (90. - tetas(nel))*pi1
        else
          teta = (-90. - tetas(nel))*pi1
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


      write(6970,*) 'attention : error dans le panneau ',nel
      write(6970,*) '****************************************'
      write(6970,*)
     *'les coordonnées de ce panneau sont incohérentes avec la
     *longueur introduite par l''utilisateur'
      write(6970,*)'ordre de grandeur de l''erreur (en m) :',erreur

      write(29,*) 'attention : error dans le panneau ',nel				!bug  !sept2006        
	write(29,*) '****************************************'				!bug  !sept2006       
	write(29,*)'les coordonnées de ce panneau sont incohérentes avec	!bug  !sept2006
     *la longueur introduite par l''utilisateur'							!bug  !sept2006
	write(29,*)'ordre de grandeur de l''erreur (en m) :',erreur			!bug  !sept2006
	write(29,*)' '
	  endif
      enddo

c
c 3   calcul des inerties flexionnelles (aixx et aiyy)
c ---------------------------------------------------
c     autour des axes kx et ky (repere utilisateur)

      omet =0.
      omesx=0.
      omesy=0.
      omet2 =0.
      omesx2=0.
      omesy2=0.
	ometpart=0.		!r&d13	!fev2007
	omesypart=0.	!r&d13	!fev2007

      do 300 j=1,neto
        phi=-philn(j)
        sph1=vsin(phi,0.d00)
        sph2=vsin(phi/2.,0.d00)
        c19=vcos(tetas(j),0.d00)
        c29=vcos(tetas(j),phi/2.)
        c39=vcos(2.*tetas(j),phi)
        s1=vsin(tetas(j),0.d00)
        s2=vsin(tetas(j),phi/2.)
        delta =delt(j)  ! epaisseur moyenne sans les cadres
        delta2=delt2(j) ! epaisseur moyenne avec les cadres
        q=qn(j)
        phil=phi*pi/180.
        d1=q*(c19-2.*sph2*c29/phil) ! (d1,d2)= coord du centre des axes neutres du panneau j
        d2=q*(s1-2.*sph2*s2/phil) !          par rapport au pt de depart du panneau
        dd1(j)=d1
        dd2(j)=d2
        omega (j)=dabs(delta *q*phil) ! section du panneau j (sans cadre)
        omega2(j)=dabs(delta2*q*phil) ! section du panneau j (avec cadre)
        if(dabs(phil).gt.0.00001) then
          temp =-d1+q*c19                    ! ** cas des coques
          temp2= d2-q*s1
          aiyy(j)=delta*q*dabs(q*q*(phil/2.+0.5*sph1*c39)    !inertie autour axe vertical
     *                  -4.*q*temp*sph2*c29+temp*temp*phil)
          aixx(j)=delta*q*dabs(q*q*(phil/2.-0.5*sph1*c39)    !inertie autour axe horizontal
     *                  +4.*q*temp2*sph2*s2+temp2*temp2*phil)
		aixxpart(j)=part(j)*aixx(j)						   !inertie autour axe horizontal avec coef. part.	!r&d13	!fev2007
        else
          aiyy(j)=((s1*q*phil)**2+(c19*delta)**2)*delta*q*dabs(phil)/12. ! ** cas des plaques
          aixx(j)=((c19*q*phil)**2+(s1*delta)**2)*delta*q*dabs(phil)/12.
	    aixxpart(j)=((c19*q*phil)**2+(s1*part(j)*delta)**2)*		   !r&d13	!fev2007
     *			    part(j)*delta*q*dabs(phil)/12.					   !avec coef. part.        
        endif
      ! sans cadre
        omet =omet +omega(j)               ! section totale
        omesx=omesx+omega(j) *(z(j,1)+d1)  ! moment statique horizontal
        omesy=omesy+omega(j) *(z(j,3)+d2)  ! moment statique vertical
	  ometpart=ometpart  +part(j)*omega(j)				! section totale avec coef. part.			!r&d13	!fev2007
	  omesypart=omesypart+part(j)*omega(j)*(z(j,3)+d2)	! moment statique vertical avec coef. part.	!r&d13	!fev2007
      ! avec cadre
        aixx2(j)=aixx(j)*delta2/delta
        aiyy2(j)=aiyy(j)*delta2/delta
        omet2 =omet2 +omega2(j)               ! section totale
        omesx2=omesx2+omega2(j) *(z(j,1)+d1)  ! moment statique horizontal
        omesy2=omesy2+omega2(j) *(z(j,3)+d2)  ! moment statique vertical
  300 continue
c
c   position des axes neutres (dans le repere utilisateur)
c   ------------------------------------------------------
      ! sans cadre
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

   20 continue                                                                                                  !flexion d'axe vert.

c
c 4  calcul des inerties flexionnelles (aixx et aiyy)
c -------------------------------------------------
c     autour des axes neutres passant pr le point (xneut,yneut)
c      aiyytot=0.
c      aixxtot=0.
c      aiuutot=0.
c      do 301 j=1,neto
c       xneu(j)=z(j,1)+dd1(j) ! coord du centre de gravite d'un panneau dans repere utilisateur
c       yneu(j)=z(j,3)+dd2(j)
c       aiyytot=aiyytot+aiyy(j)+omega(j)*((xneu(j)-xneut)**2)
c       aiuutot=aiuutot+aiyy(j)+omega(j)*((xneu(j)      )**2)
c       aixxtot=aixxtot+aixx(j)+omega(j)*((yneu(j)-yneut)**2)
c  301 continue
c
c      if((ncondi.eq.0).or.(k.eq.0)) goto 21                            !flexion d'axe vert.
c      if(k.eq.1) then
c        aiyytot=2*aiyytot
c        aixxtot=2*aixxtot
c        goto 21
c      endif
c      if(k.eq.2) then
c        aiyytot=4*aiyytot
c        aixxtot=4*aixxtot
c        goto 21
c      endif
c
c   21 continue                                                 !flexion d'axe vert.
c
cc     impressions
c      if(langue==1) then
c	  write(66,124)xneut,yneut,aiyytot,aixxtot,aiuutot
c        write(66,122)
c	else
c        write(66,224)xneut,yneut,aiyytot,aixxtot,aiuutot
c        write(66,222)
c	endif
c
c      do 315 j=1,neto
c        write(66,123)j,omega(j),xneu(j),yneu(j),delt(j),aiyy(j),aixx(j)
c 315  continue


c
c 5  calcul des inerties de masses : ig(x) et ig(y)
cc -------------------------------------------------
cc     autour des axes passant par le centre de gravite en (kgx,kgy)
c      aiyytot=0.
c      aixxtot=0.
c      aiuutot=0.
c      do j=1,neto
c        aiyytot=aiyytot+aiyy2(j)+omega2(j)*((xneu(j)-akgx)**2)
c        aiuutot=aiuutot+aiyy2(j)+omega2(j)*((xneu(j)    )**2)
c        aixxtot=aixxtot+aixx2(j)+omega2(j)*((yneu(j)-akgy)**2)
c      enddo
c
c
c     if((ncondi.eq.0).or.(k.eq.0)) then                            !flexion d'axe vert.
c        goto 22
c      endif
c      if(k.eq.1) then
c        aiyytot=2*aiyytot
c        aixxtot=2*aixxtot
c        goto 22
c      endif
c      if(k.eq.2) then
c        aiyytot=4*aiyytot
c        aixxtot=4*aixxtot
c        goto 22
c      endif
c
c   22 continue                                                 !flexion d'axe vert.

c     impressions
c      if(langue==1) then
c        write(66,134)akgx,akgy,aiyytot,aixxtot,aiuutot
c        write(66,122)
c	else
c	  write(66,234)akgx,akgy,aiyytot,aixxtot
c        write(66,222)
c	endif
c      do  j=1,neto
c       write(66,123)j,omega2(j),xneu(j),yneu(j),delt2(j),
c     *              aiyy2(j),aixx2(j)
c      enddo

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

	write(666,*)'igrav ',igrav							!eugen (14.10.2007)
      if(igrav.eq.0) then ! pas de restriction sur le kg
        write(66,'(/a/1x,55(1h*)/)')										!sept2006
     *       ' pas de restriction sur la position du centre de gravite'
c        return
c      endif

        write(666,'(/a/1x,50(1h*)/)')
     *       ' pas de restriction sur la position du centre de gravite'
        write(666,*)'  kg actuel  =', akgy,' m'			!eugen (14.10.2007)
	  write(29 ,'(/a/1x,55(1h*)/)')										!sept2006				!bug
     *	' pas de restriction sur la position du centre de gravite'		!sept2006				!bug
	  goto 14															!sept2006
	endif																!sept2006
	
	write(66,'(/a/1x,48(1h*)/)')										!sept2006
     *    ' restriction sur la position du centre de gravite'				!sept2006
	write(666,'(/a/1x,48(1h*)/)')										!sept2006
     *	' restriction sur la position du centre de gravite'				!sept2006
	write(66 ,*)'  kg actuel  =', akgy,' m'
      write(666,*)'  kg actuel  =', akgy,' m'   
		
	if(igrav.eq.1) then ! kgmin impose
	  write(29,*)'  1 restriction est imposee : kgmin < kg'				!sept2006				!bug
        write(66,*)'  1 restriction est imposee : kgmin < kg'				!sept2006
	  write(66,*)'  kg minimum =', xkgmin,' m'							!sept2006
	  write(666,*)'  1 restriction est imposee : kgmin < kg'
        write(666,*)'  kg minimum =', xkgmin,' m'
      else if(igrav.eq.2) then
        write(29,*)'  1 restriction est imposee : kg < kgmax'				!sept2006				!bug
	  write(66,*)'  1 restriction est imposee : kg < kgmax'				!sept2006
	  write(66,*)'  kg maximum =', xkgmax,' m'							!sept2006
	  write(666,*)'  1 restriction est imposee : kg < kgmax'
        write(666,*)'  kg maximum =', xkgmax,' m'
      else if(igrav.eq.3) then
        write(29,*)'  2 restrictions sont imposees kgmin < kg < kgmax'	!sept2006					!bug
	  write(66,*)'  2 restrictions sont imposees kgmin < kg < kgmax'	!sept2006
	  write(66,*)'  kg maximum =', xkgmax,' m'							!sept2006
	  write(66,*)'  kg minimum =', xkgmin,' m'							!sept2006
	  write(666,*)'  2 restr. sont imposees : kgmin < kg < kgmax'
        write(666,*)'  kg maximum =', xkgmax,' m'
        write(666,*)'  kg minimum =', xkgmin,' m'
      endif

      rewind(302)		!extension neto	!fev2007

      call annuld(dkg,9*neto)
	im=0

      do 11 nel=1,neto				!sept2006

        read(302) epsa,epsr,delta,hya,dya,wya,tya,hxr,dxr,wxr,txr,phil,		!extension neto	!fev2007
     *            q,epais                                                     !février 2004

        nbrxi=nvar(nel)  ! nbre de variable de conception dans le pannneau nel
        if (nbrxi.eq.0) then
          goto 11						!sept2006
        else
c        ds = d[section(j)]/d xi(j) avec j= numero du panneau
	   phil=-philn(nel)*pi/180
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

	  sph2=vsin(-philn(nel)/2.,0.d00)								!oct2006
        s1=vsin(tetas(nel),0.d00)										!sept2006
        s2=vsin(tetas(nel),-philn(nel)/2.)							!oct2006
	  d2=q*(s1-2.*sph2*s2/phil)										!sept2006
	  	  
	  do 12 j=1,nbrxi												!sept2006
          jj=nxit(j,nel)  ! no de ref de chaque var. de conception	!sept2006
          imm=im+j								!sept2006
c         write(666,*) 'k,kk,imm',k,kk,imm							!sept2006
		if(isymx.eq.1) then											!sept2006
		  dkg(imm)=0.												!sept2006
		else														!sept2006
	      dkg(imm)=ds(jj)*((z(nel,3)+d2)-akgy)/omet2				!oct2006
	    endif														!sept2006
c         write(666,*) 'dkg(imm)',dkg(imm)
	
   12  continue														!sept2006

        im=im+nbrxi													!sept2006

  11  continue														!sept2006

      rewind(302)		!extension neto	!fev2007

c     sauvetage restrictions et sensibilites dans 'file 303'			!extension neto	!fev2007
c     ------------------------------------------------------
c     borne min : kgmin < kg avec kg > 0 et kgmax > 0
c     restriction : c(x)= kgmin-kg < 0
c        dc(x)/dxi= - dkg/dxi
      if((igrav.eq.1).or.(igrav.eq.3)) then
        text='kgmin < kg'
        c=xkgmin-akgy
        cm=0.0
        do i=1,ntot
          dc(i)= - dkg(i)
        enddo
        write(303) c,cm,text				!extension neto	!fev2007
        write(303) (dc(i),i=1,ntot)		!extension neto	!fev2007
!        write(6969,*) c,cm,text				!extension neto	!fev2007
!        write(6969,*) (dc(i),i=1,ntot)		!extension neto	!fev2007

c		do ii=1,ntot
c		open(666661, file = 'dc-coord605-dyn.txt')
c		write (666661,*) dc(ii)
c		enddo
c		pause

        img=img+1  ! +1 au compteur des restrictions struc
        if(iopti.gt.2) then
		if(impr2.ne.-3) then		!15.10.05						!sept2006
          write(666,*)
          write(666,*) 'c,cm,text=',c,cm,'  ',text
          write(666,*) 'dc dans coord après write 303:'				!extension neto	!fev2007
          write(666,'(9(e10.3,1x))') (dc(i),i=1,ntot)
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
        do i=1,ntot
          dc(i)= dkg(i)
        enddo
        write(303) c,cm,text				!extension neto	!fev2007
        write(303) (dc(i),i=1,ntot)		!extension neto	!fev2007
!        write(6969,*) c,cm,text				!extension neto	!fev2007
!        write(6969,*) (dc(i),i=1,ntot)		!extension neto	!fev2007
		
c		do ii=1,ntot
c		open(666662, file = 'dc-coord-635-dyn.txt')
c		write (666662,*) dc(ii)
c		enddo
c		pause

        img=img+1  ! +1 au compteur des restrictions struc
        if(iopti.gt.2) then
		if(impr2.ne.-3) then			!15.10.05			!sept2006
          write(666,*)
          write(666,*) 'c,cm,text=',c,cm,'  ',text
          write(666,*) 'dc dans coord après write 303:'		!extension neto	!fev2007
          write(666,'(9(e10.3,1x))') (dc(i),i=1,ntot)
		endif			!15.10.05							!sept2006
        endif
      endif



c 5  calcul des inerties flexionnelles (aixx et aiyy)						
c -------------------------------------------------
c     autour des axes neutres passant pr le point (xneut,yneut)
c      
   14 write(66,'(//a/1x,34(1h*))')													!sept2006
     *    ' inerties flexionnelles et de masse'		!restri inertie					!sept2006
	if(iopti.ne.0)																	!sept2006
     *write(666,'(//a/1x,34(1h*))')													!sept2006
     *	' inerties flexionnelles et de masse'		!restri inertie					!sept2006

	aiyytot=0.																		!sept2006
      aixxtot=0.																		!sept2006
	aixxtotpart=0.																	!r&d13	!fev2007
      aiuutot=0.																		!sept2006
      do 301 j=1,neto																		!sept2006
       xneu(j)=z(j,1)+dd1(j) ! coord du centre de gravite d'un panneau dans repere utilisateur
       yneu(j)=z(j,3)+dd2(j)															!sept2006
       aiyytot=aiyytot+aiyy(j)+omega(j)*((xneu(j)-xneut)**2)							!sept2006
       aiuutot=aiuutot+aiyy(j)+omega(j)*((xneu(j)      )**2)							!sept2006
       aixxtot=aixxtot+aixx(j)+omega(j)*((yneu(j)-yneut)**2)							!sept2006
	 aixxtotpart=aixxtotpart+aixxpart(j)+part(j)*omega(j)*							!r&d13	!fev2007
     *			 ((yneu(j)-yneutpart)**2)											!r&d13	!fev2007

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

c     impressions
      if(langue==1) then																!sept2006
	  write(66,124)xneut,yneut,aiyytot,aixxtot										!sept2006
	  if(sym.ne.0) write(66,125)													!sept2006
	  write(66,126) aiuutot															!sept2006
	  if(sym.ne.0) write(66,127)													!sept2006
        write(66,122)																	!sept2006
	else																			!sept2006
        write(66,224)xneut,yneut,aiyytot,aixxtot										!sept2006
	  if(sym.ne.0) write(66,225)													!sept2006
        write(66,226) aiuutot															!sept2006
	  if(sym.ne.0) write(66,227)													!sept2006
        write(66,222)																	!extension neto	!fev2007
	endif																			!sept2006


      do 315 j=1,neto																	!sept2006
        write(66,123)j,omega(j),xneu(j),yneu(j),delt(j),aiyy(j),aixx(j)				!sept2006
 315  continue																		!sept2006

c 6	restriction sur l'inertie ixx		!restri inertie
c ---------------------------------
	if(iopti.eq.0) goto 15															!sept2006
	if(inert.ne.0) then																!sept2006
        write(66,'(/a/1x,27(1h.))')													!sept2006
     *    ' restriction sur inertie iyy'												!sept2006
	  write(666,'(/a/1x,27(1h.))')													!sept2006
     *    ' restriction sur inertie iyy'												!sept2006
	  write(66,'(a,t16,e14.7,t32,a)') ' iyy actuel  = ',							!sept2006
     *	aixxtotpart,'m**4'															!r&d13	!fev2007
	  write(666,'(a,t16,e14.7,t32,a)') ' iyy actuel  = ',							!sept2006
     *	aixxtotpart,'m**4'															!r&d13	!fev2007
	  write(66,'(a,t16,e14.7,t32,a)') ' iyy minimum = ',imin,'m**4'					!sept2006
	  write(666,'(a,t16,e14.7,t32,a)') ' iyy minimum = ',imin,'m**4'				!sept2006
	  im=0																			!sept2006
	  call annuld(dc,9*neto)														!sept2006
	  do 13 nel=1,neto																!sept2006
	    read(302) epsa,epsr,delta,hya,dya,wya,tya,hxr,dxr,wxr,txr,					!extension neto	!fev2007	
     *			  phil,q,epais														!sept2006
          nbrxi=nvar(nel)  ! nbre de variable de conception dans le pannneau nel																	
	    if (nbrxi.eq.0) goto 13														!sept2006
		phil=-philn(nel)*pi/180.													!oct2006
		c19=vcos(tetas(nel),0.d00)													!sept2006
		s1=vsin(tetas(nel),0.d00)													!sept2006
		if(itype(nel).ne.5) then													!sept2006
	      dixx(1)=q*dabs(phil)/12.*													!sept2006
     *	          (part(nel)*((c19*q*phil)**2+										!r&d13	!fev2007
     *			  (s1*part(nel)*delt(nel))**2)+										!r&d13	!fev2007
     *		      part(nel)*delt(nel)*												!r&d13	!fev2007
     *			  (2.*(s1*part(nel)*delt(nel))*s1*part(nel)))						!r&d13	!fev2007	! xi=1 épaisseur borde	!sept2006
		  dixx(6)=q*dabs(phil)/12.*													!sept2006
     *	          (part(nel)*dxr/epsr*((c19*q*phil)**2+								!r&d13	!fev2007
     *			  (s1*part(nel)*delt(nel))**2)+										!r&d13	!fev2007		
     *			  part(nel)*delt(nel)*												!r&d13	!fev2007
     *			  (2.*(s1*part(nel)*delt(nel))*s1*part(nel)*dxr/epsr))				!r&d13	!fev2007	! xi=6 hauteur ame raidisseur
            dixx(7)=q*dabs(phil)/12.*													!sept2006
     *	          (part(nel)*hxr/epsr*((c19*q*phil)**2+								!r&d13	!fev2007
     *			  (s1*part(nel)*delt(nel))**2)+										!r&d13	!fev2007	
     *			  part(nel)*delt(nel)*												!r&d13	!fev2007
     *			  (2.*(s1*part(nel)*delt(nel))*s1*part(nel)*hxr/epsr))				!r&d13	!fev2007	! xi=7 epaiss ame raidisseur!sept2006
            dixx(8)=q*dabs(phil)/12.*													!sept2006
     *	          (part(nel)*txr/epsr*((c19*q*phil)**2+								!r&d13	!fev2007
     *			  (s1*part(nel)*delt(nel))**2)+										!r&d13	!fev2007	
     *			  part(nel)*delt(nel)*												!r&d13	!fev2007
     *			  (2.*(s1*part(nel)*delt(nel))*s1*part(nel)*txr/epsr))				!r&d13	!fev2007	! xi=8 larg sem raidisseur	!sept2006
		  dixx(9)=q*dabs(phil)/12.*													!sept2006
     *	          ((-1.)*part(nel)*(hxr*dxr+wxr*txr)/(epsr**2)*						!r&d13	!fev2007
     *			  ((c19*q*phil)**2+(s1*part(nel)*delt(nel))**2)+					!r&d13	!fev2007
     *			  part(nel)*delt(nel)*(2.*(s1*part(nel)*delt(nel))*s1*				!r&d13	!fev2007
     *			  (-1.)*part(nel)*(hxr*dxr+wxr*txr)/(epsr**2)))						!r&d13	!fev2007	! xi=9 larg sem raidisseur		!sept2006
		  dixx(2)=dixx(6)/1.e3								! xi=2 hauteur ame cadre				!sept2006
		  dixx(3)=dixx(7)/1.e3								! xi=3 epaiss ame cadre					!sept2006
		  dixx(4)=dixx(8)/1.e3								! xi=4 largeur sem cadre				!sept2006
		  dixx(5)=dixx(9)/1.e3								! xi=5 entredistance cadre (epsa)		!sept2006
		  ds(1)=dabs(q*phil)								! xi=1 epaisseur borde					!sept2006
		  ds(6)=dxr/epsr*dabs(q*phil)						! xi=6 haut ame raid					!sept2006
		  ds(7)=hxr/epsr*dabs(q*phil)						! xi=7 epaiss ame raid					!sept2006
		  ds(8)=txr/epsr*dabs(q*phil)						! xi=8 larg sem raid					!sept2006
		  ds(9)=-(hxr*dxr+wxr*txr)/(epsr*epsr)*dabs(q*phil)	! xi=9 entredistance raid (epsr)		!sept2006
		  ds(2)=ds(6)/1.e3									! xi=2 hauteur ame cadre				!sept2006
		  ds(3)=ds(7)/1.e3									! xi=3 epaiss ame cadre					!sept2006
		  ds(4)=ds(8)/1.e3									! xi=4 largeur sem cadre				!sept2006
		  ds(5)=ds(9)/1.e3									! xi=5 entredistance cadre (epsa)		!sept2006
		  do j=1,nbrxi																				!sept2006
	        jj=nxit(j,nel)		! no de ref de chaque var. de conception							!sept2006
	        imm=im+j
			if(isymx.eq.0) then		!sept06																	!sept2006
			  dc(imm)=(-1.)*(dixx(jj)+part(nel)*ds(jj)*												!r&d13	!fev2007
     *				  ((yneu(nel)-yneutpart)**2)+													!r&d13	!fev2007
     *			      part(nel)*omega(nel)*(2.*(yneu(nel)-yneutpart)*								!r&d13	!fev2007
     *			      (-1.)*part(nel)*ds(jj)*(yneu(nel)-yneutpart)/									!r&d13	!fev2007
     *				  ometpart))																	!r&d13	!fev2007
		    else																					!sept2006
	          dc(imm)=(-1.)*(dixx(jj)+part(nel)*ds(jj)*												!r&d13	!fev2007
     *				  ((yneu(nel)-yneutpart)**2))													!r&d13	!fev2007
	        endif																					!sept2006
			if(sym.eq.1) dc(imm)=2.*dc(imm)															!sept2006
	        if(sym.eq.2) dc(imm)=4.*dc(imm)															!sept2006
	      enddo																						!sept2006
	    else																						!sept2006
	      do j=1,nbrxi																				!sept2006
	        jj=nxit(j,nel)		! no de ref de chaque var. de conception							!sept2006
	        imm=im+j																				!sept2006
			dc(imm)=1.e-10																			!sept2006
	      enddo																						!sept2006
	    endif																						!sept2006
	    im=im+nbrxi																					!sept2006
  13	  continue																						!sept2006
        text='iyy > imin'																				!sept2006
	  c=-aixxtotpart																				!r&d13	!fev2007
	  cm=-imin																						!sept2006
	  write(303) c,cm,text																			!extension neto	!fev2007
	  write(303) (dc(i),i=1,ntot)																	!extension neto	!fev2007
!	  write(6969,*) c,cm,text																			!extension neto	!fev2007
!	  write(6969,*) (dc(i),i=1,ntot)																	!extension neto	!fev2007
	  
c	    do ii=1,ntot
c		open(666663, file = 'dc-coord791-dyn.txt')
c		write (666663,*) dc(ii)
c		enddo
c		pause
	  																	!sept2006
	  img=img+1					! +1 au compteur des restrictions struc								!sept2006
	  if(iopti.gt.2) then																			!sept2006
		if(impr2.ne.-3) then																		!sept2006
	    write(666,*) 																				!sept2006
	    write(666,*) 'c,cm,text=',c,cm,'  ',text													!sept2006
         write(666,*) 'dc après write 303:'															!sept2006
          write(666,'(9(e10.3,1x))') (dc(i),i=1,ntot)													!sept2006
		endif																						!sept2006
	  endif																							!sept2006
	  rewind 302	  																				!extension neto	!fev2007
	endif						! restri inertie													!sept2006																			!sept2006
																			

c6	restriction sur le module ixx/v		!restri module
c -----------------------------------
	if(imod.ne.0) then																				!sept2006
        v=max(dabs(z(ielt,3)-yneutpart),dabs(z(ielt,4)-yneutpart))									!r&d13	!fev2007
	  mod=aixxtotpart/v																				!r&d13	!fev2007
	  write(66,'(/a/1x,28(1h.))')																	!sept2006
     *    ' restriction sur module iyy/v'																!sept2006
	  write(666,'(/a/1x,28(1h.))')																	!sept2006
     *    ' restriction sur module iyy/v'																!sept2006
	  write(66,'(a,t18,e14.7,t34,a)') ' iyy/v actuel  = ',mod,'m**3'								!sept2006
	  write(666,'(a,t18,e14.7,t34,a)') ' iyy/v actuel  = ',mod,'m**3'								!sept2006
	  write(66,'(a,t18,e14.7,t34,a)')																!sept2006
     *     ' iyy/v minimum = ',modmin,'m**3'															!sept2006
	  write(666,'(a,t18,e14.7,t34,a)')																!sept2006														!sept2006
     *     ' iyy/v minimum = ',modmin,'m**3'															!sept2006
	  im=0																							!sept2006
	  call annuld(dc,9*neto)																		!sept2006
	  do 16 nel=1,neto																				!sept2006
	    read(302) epsa,epsr,delta,hya,dya,wya,tya,hxr,dxr,wxr,txr,									!extension neto	!fev2007	
     *			  phil,q,epais																		!sept2006
          nbrxi=nvar(nel)  ! nbre de variable de conception dans le pannneau nel						!sept2006																
	    if (nbrxi.eq.0) goto 16																		!sept2006
		phil=-philn(nel)*pi/180.																	!oct2006
		c19=vcos(tetas(nel),0.d00)																	!sept2006
		s1=vsin(tetas(nel),0.d00)																	!sept2006
		if(itype(nel).ne.5) then																	!sept2006
	      dixx(1)=q*dabs(phil)/12.*													!sept2006
     *	          (part(nel)*((c19*q*phil)**2+										!r&d13	!fev2007
     *			  (s1*part(nel)*delt(nel))**2)+										!r&d13	!fev2007
     *		      part(nel)*delt(nel)*												!r&d13	!fev2007
     *			  (2.*(s1*part(nel)*delt(nel))*s1*part(nel)))						!r&d13	!fev2007	! xi=1 épaisseur borde	!sept2006
		  dixx(6)=q*dabs(phil)/12.*													!sept2006
     *	          (part(nel)*dxr/epsr*((c19*q*phil)**2+								!r&d13	!fev2007
     *			  (s1*part(nel)*delt(nel))**2)+										!r&d13	!fev2007		
     *			  part(nel)*delt(nel)*												!r&d13	!fev2007
     *			  (2.*(s1*part(nel)*delt(nel))*s1*part(nel)*dxr/epsr))				!r&d13	!fev2007	! xi=6 hauteur ame raidisseur
            dixx(7)=q*dabs(phil)/12.*													!sept2006
     *	          (part(nel)*hxr/epsr*((c19*q*phil)**2+								!r&d13	!fev2007
     *			  (s1*part(nel)*delt(nel))**2)+										!r&d13	!fev2007	
     *			  part(nel)*delt(nel)*												!r&d13	!fev2007
     *			  (2.*(s1*part(nel)*delt(nel))*s1*part(nel)*hxr/epsr))				!r&d13	!fev2007	! xi=7 epaiss ame raidisseur!sept2006
            dixx(8)=q*dabs(phil)/12.*													!sept2006
     *	          (part(nel)*txr/epsr*((c19*q*phil)**2+								!r&d13	!fev2007
     *			  (s1*part(nel)*delt(nel))**2)+										!r&d13	!fev2007	
     *			  part(nel)*delt(nel)*												!r&d13	!fev2007
     *			  (2.*(s1*part(nel)*delt(nel))*s1*part(nel)*txr/epsr))				!r&d13	!fev2007	! xi=8 larg sem raidisseur	!sept2006
		  dixx(9)=q*dabs(phil)/12.*													!sept2006
     *	          ((-1.)*part(nel)*(hxr*dxr+wxr*txr)/(epsr**2)*						!r&d13	!fev2007
     *			  ((c19*q*phil)**2+(s1*part(nel)*delt(nel))**2)+					!r&d13	!fev2007
     *			  part(nel)*delt(nel)*(2.*(s1*part(nel)*delt(nel))*s1*				!r&d13	!fev2007
     *			  (-1.)*part(nel)*(hxr*dxr+wxr*txr)/(epsr**2)))						!r&d13	!fev2007	! xi=9 larg sem raidisseur		!sept2006
		  dixx(2)=dixx(6)/1.e3										! xi=2 hauteur ame cadre		!sept2006
		  dixx(3)=dixx(7)/1.e3										! xi=3 epaiss ame cadre			!sept2006
		  dixx(4)=dixx(8)/1.e3										! xi=4 largeur sem cadre		!sept2006
		  dixx(5)=dixx(9)/1.e3										! xi=5 entredistance cadre (epsa)!sept2006
		  ds(1)=dabs(q*phil)										! xi=1 epaisseur borde			!sept2006
		  ds(6)=dxr/epsr*dabs(q*phil)								! xi=6 haut ame raid			!sept2006
		  ds(7)=hxr/epsr*dabs(q*phil)								! xi=7 epaiss ame raid			!sept2006
		  ds(8)=txr/epsr*dabs(q*phil)								! xi=8 larg sem raid			!sept2006
		  ds(9)=-(hxr*dxr+wxr*txr)/(epsr*epsr)*dabs(q*phil)			! xi=9 entredistance raid (epsr)!sept2006
		  ds(2)=ds(6)/1.e3											! xi=2 hauteur ame cadre		!sept2006
		  ds(3)=ds(7)/1.e3											! xi=3 epaiss ame cadre			!sept2006
		  ds(4)=ds(8)/1.e3											! xi=4 largeur sem cadre		!sept2006
		  ds(5)=ds(9)/1.e3											! xi=5 entredistance cadre (epsa)!sept2006
		  do j=1,nbrxi																				!sept2006
	        jj=nxit(j,nel)		! no de ref de chaque var. de conception							!sept2006
	        imm=im+j
			if(isymx.eq.0) then								!sept06																		!sept2006
			  dc(imm)=dixx(jj)+part(nel)*ds(jj)*													!r&d13	!fev2007
     *				  ((yneu(nel)-yneut)**2)+														!r&d13	!fev2007
     *			      part(nel)*omega(nel)*(2.*(yneu(nel)-yneutpart)*								!r&d13	!fev2007
     *			      (-1.)*part(nel)*ds(jj)*(yneu(nel)-yneutpart)/									!r&d13	!fev2007
     *				  ometpart)																		!r&d13	!fev2007
			  if(sym.eq.1) dc(imm)=2.*dc(imm)														!sept2006
	          if(sym.eq.2) dc(imm)=4.*dc(imm)														!sept2006
			  dc(imm)=(-1.)*(dc(imm)*v-aixxtotpart*													!r&d13	!fev2007
     *                  (-1.)*part(nel)*ds(jj)*(yneu(nel)-yneutpart)/									!r&d13	!fev2007
     *				  ometpart)/v**2																!r&d13	!fev2007
		    else																					!sept2006
	          dc(imm)=dixx(jj)+part(nel)*ds(jj)*													!r&d13	!fev2007
     *				  ((yneu(nel)-yneutpart)**2)													!r&d13	!fev2007
			  if(sym.eq.1) dc(imm)=2.*dc(imm)														!sept2006
	          if(sym.eq.2) dc(imm)=4.*dc(imm)														!sept2006
	          dc(imm)=(-1.)*dc(imm)/v																!sept2006
			endif																					!sept2006
	      enddo																						!sept2006
	    else																						!sept2006
	      do j=1,nbrxi																				!sept2006
	        jj=nxit(j,nel)		! no de ref de chaque var. de conception							!sept2006
	        imm=im+j																				!sept2006
			dc(imm)=1.e-10																			!sept2006
	      enddo																						!sept2006
	    endif																						!sept2006
	    im=im+nbrxi																					!sept2006
  16	  continue																						!sept2006
        text='i/v > i/vmin'																			!sept2006
	  c=-mod																						!sept2006
	  cm=-modmin																					!sept2006
	  write(303) c,cm,text																			!extension neto	!fev2007		
	  write(303) (dc(i),i=1,ntot)																	!extension neto	!fev2007
	  
!	  write(6969,*) c,cm,text																			!extension neto	!fev2007		
!	  write(6969,*) (dc(i),i=1,ntot)																	!extension neto	!fev2007
c	    do ii=1,ntot
c		open(666664, file = 'dc-coord900-dyn.txt')
c		write (666664,*) dc(ii)
c		enddo
c		pause
	  																!sept2006	
	  img=img+1					! +1 au compteur des restrictions struc								!sept2006
	  if(iopti.gt.2) then																			!sept2006
		if(impr2.ne.-3) then																		!sept2006			
	    write(666,*) 																				!sept2006
	    write(666,*) 'c,cm,text=',c,cm,'  ',text													!sept2006
         write(666,*) 'dc après write 303:'															!sept2006
          write(666,'(9(e10.3,1x))') (dc(i),i=1,ntot)													!sept2006
		endif																						!sept2006
	  endif																							!sept2006
	  rewind 302	  																				!extension neto	!fev2007
	endif						! restri module														!sept2006

c
c 7  calcul des inerties de masses : ig(x) et ig(y)
cc -------------------------------------------------
cc     autour des axes passant par le centre de gravite en (kgx,kgy)
   15 aiyytot2=0.																						!sept2006
      aixxtot2=0.																						!sept2006
      aiuutot2=0.																						!sept2006
      do j=1,neto																						!sept2006
        aiyytot2=aiyytot2+aiyy2(j)+omega2(j)*((xneu(j)-akgx)**2)										!sept2006
        aiuutot2=aiuutot2+aiyy2(j)+omega2(j)*((xneu(j)    )**2)										!sept2006
        aixxtot2=aixxtot2+aixx2(j)+omega2(j)*((yneu(j)-akgy)**2)										!sept2006
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
        write(66,134)akgx,akgy,aiyytot2,aixxtot2														!sept2006
	  if(sym.ne.0) write(66,135)																	!sept2006						!sept2006
	  write(66,136) aiuutot2																		!sept2006
	  if(sym.ne.0) write(66,137)																	!sept2006
        write(66,122)																					!sept2006
	else																							!sept2006
	  write(66,234)akgx,akgy,aiyytot2,aixxtot2														!sept2006
	  if(sym.ne.0) write(66,235)																	!sept2006
	  write(66,236) aiuutot2																		!sept2006
	  if(sym.ne.0) write(66,237)																	!sept2006
        write(66,222)																					!extension neto	!fev2007
	endif																							!sept2006
      do  j=1,neto																					!sept2006
       write(66,123)j,omega2(j),xneu(j),yneu(j),delt2(j),												!sept2006
     *              aiyy2(j),aixx2(j)																	!sept2006
      enddo																							!sept2006


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

c  104 format(2x,i2,t8,4(e11.4,1x)/)
c 115 format(20i4)
c
c  120 format(t2,'panneau',t11,'y depart',t22,'z depart',t33,'y arrivee',
c     *      t44,'y arrivee'/)
c  121 format(/' les axes globaux initiaux de la structure (horiz. ',
c     * 'oy et vert. oz) ont leur origine'/'  au point de depart du ',
c     * 'panneau 1'/' dans ce repere, les coordonnees du repere ',
c     * 'utilisateur sont:'/t4,'(yk,zk)= ',e11.4,1x,e11.4' m'/
c     * ' c''est ce repere ',
c     * 'qui est utilise dans tous les resultats qui suivent!!'//
c     * ' nbre de noeuds',i3//' coordonnees des noeuds dans le',
c     * ' repere utilisateur:'/t2,50(1h-))                                                               !flexion d'axe vert.
c  122 format(t2,'pour chaque panneau'/t2,19(1h.)/t2,
c     *'panneau',t11,'surface(m**2)',t25,'coord.(y,z) centre',
c     *' de gravite',t55,'epaiss equiv.',
c     *  t70,'inerties izz et iyy (m**4)'/)
c  123 format(t2,i3,t11,e11.4,t26,e11.4,t39,e11.4,t55,e11.4,t70,
c     *          e11.4,t83,e11.4)
c  124 format(/' pour la structure complete en tenant compte
c     * des symetries eventuelles (repère utilisateur)'/t2,
c     * 90(1h=)//' 1- inertie flexionnelle (bordé, raid et trav.)'/
c     *  45(1h-)/t5,'coord. des axes neutres',t42,'inerties (m**4) ',
c     * 'dans les axes neutres'/t2,' kx (horiz.)   et    ky (vert.)',
c     *  t42,'axe vert.iyy',t58,' axe horiz. ixx'//
c     * t2,e11.4,t20,e11.4,t42,e14.7,t60,e14.7//
c     * ' inertie autour de l''axe vertical du repère ',
c     * 'utilisateur = ',e14.7,' m**4'/)                                                               !flexion d'axe vert.
c  130 format(t2,'panel',t11,'x depart',t22,'y depart',t33,'x arrival',
c     *      t44,'y arrival'/)
c  134 format(/' 2- inertie de masse (borde, raid, cadre et trav.)'/
c     * 52(1h-)/t4,'coord. du centre de gravité',t41,'inerties (m**4) ',
c     * 'selon les axes'/t2,' xxg (horiz.)  et   yyg (vert.) ',t40,
c     *'vert. iyy',t58,' horiz. ixx'/t40' passant par le centre ',
c     *'de gravite (xg,yg)'//t2,e11.4,t20,e11.4,t42,e14.7,t60,e14.7//
c     * ' inertie de masse autour de l''axe vertical du repere ',
c     * 'utilisateur = ',e14.7,' m**4'/)

c  221 format(/' the basic frames (ox horiz. and oy vertic.)',
c     * ' have their origine at the arriving node of panel 1'/
c     * ' in this frame, the origin of the user coordinate-frame is:',
c     *  /t4,'(kx,ky)= ',e11.4,1x,e11.4' m'/
c     * ' any future coordinates will refer to this user-frames!!'//
c     * ' nbre of nodes',i3//t2,'nodes coordinates :'/t2,45(1h-))
c  222 format(t2,'for each panel'/t2,35(1h.)/t2,
c     *'panel',t11,'area (m**2)',t25,'coord.(x,y) centre',
c     *' of gravity',t55,'equiv.thickness',
c     *  t70,'inertia iyy and ixx (m**4)'/)
c  224 format(' for the complete structure by taking account of
c     *possible symmetries (user frame)'/t2,82(1h=)//
c     * ' 1- flexional inertia (plate, stiff and long. girders)'/
c     * 45(1h-)/t5,'coord. of neutral axis',
c     * t42,'inertia (m**4) around the neutral axis'/t2,
c     * ' kg (horiz.)  and    ky (vert.) ',t42,'iyy (vert. axis)',
c     * t58,'ixx (horiz. axis)'//t2,e11.4,t20,e11.4,t42,e14.7,
c     * t60,e14.7//' inertia around the vertical axis of the user-',
c     * 'frame= ',e14.7,' m**4'/)
c  234 format(/' 2- mass inertia (plate, stiff, frame and girder.)'/
c     * 52(1h-)/t4,'coord. of centre of gravity',t41,'inertia (m**4) ',
c     * 'around axis'/t2,' xxg (horiz.)  et   yyg (vert.) ',t40,
c     *'vert. iyy',t58,' horiz. ixx'/t40' centered at the centre ',
c     *'of gravity (xg,yg)'//t2,e11.4,t20,e11.4,t42,e14.7,t60,e14.7//
c     * ' mass inertia around the vertical axis of the user-frame = ',
c     * e14.7,' m**4'/)

c      return
c      end
