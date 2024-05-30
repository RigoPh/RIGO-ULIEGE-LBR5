      subroutine geom(it,nxi,nbrxi,itera,ic,coefk1,length,nel)									!r&d15
          
	use param_section, length_=>length,itera_=>itera

	implicit double precision (a-h,o-z)
	character*80 vnom    !dad
      dimension nxi(9),dcj(9)
	double precision delta_min,delta_max,hya_min,hya_max,dya_min,
     *				 dya_max,wya_min,wya_max,hxr_min,hxr_max,dxr_min,
     *				 dxr_max,wxr_min,wxr_max,length

c***********************************************************************
c     subroutine geom
c     ****************
c     cette subroutine calcule les restrictions géométriques
c         cj(xi) < cjmax  et les sensibilités = dcj(n) = d(cj)/dxi.
c
c     il y a aussi un test pour vérifier si les variables intervenans da
c     les restrictions choisies sont bien des variables de conception (s
c
c     création     : 4-6-96 (ph. rigo pour l.b.r.-5)
c     modifications:
c            1-7-96 restrictions 104,105, 204 et 205
c           13-3-98 restrictions géométriques de hughes
c
c     dernière modif.:4-4-99

c***********************************************************************
c   les variables de conception ; nbrxi = nvar(nel)
c   -------------------------------------------------
c       1       delta =  épaisseur du bordage (d=delta)
c       2       hya   =  hauteur de l'âme des cadres (dw)
c       3       dya   =  épaisseur de l'âme des cadres (tw)
c       4       wya   =  largeur des semelles des cadres (df)
c       5       epsa  =  entredistance entre cadres
c       6       hxr   =  hauteur de l'âme des raidisseurs  (dw)
c       7       dxr   =  épaisseur de l'âme des raidisseurs (tw)
c       8       wxr   =  largeur des semelles des raidisseurs (df)
c       9       epsr  =  entredistance entre raidisseurs


c ****************************************************************
c                restrictions geometriques des transversaux (cadres)
c                -------------------------------------------
c                set de rahman-caldwell
c                nø 101 :  df-dw     <0         (df = haut. ame)
c                nø 102 :  dw-2*df   <0         (dw = larg. semelle)
c                nø 103 :  0.0065+dw/170-tw<0   (tw = épaisseur ame)
c                set a:
c                nø 104 :  delta-2*tw<0         (delta = épaiss. bordé)
c                nø 105 :  3*delta-dw<0
c                set de hughes
c                        nø 109 :  0.625df-dw<0
c                        nø 110 :  dw-2.5*df <0
c                nø 111 :  dw-120*tw <0
c                nø 112 :  tw-2*delta<0  (complétaire à c104)
c                pour situation inverse (si raid/girder >> transv. frame
c                nø 113 :  dw-36*tw  <0  (complément set hughes = c211
c                nø 114 :  dw-40*tw  <0  (complément set rahman = c203
c                si profil en cornières (à la place de c 109 et c110)
c                        nø 115 :  1.25*df-dw<0
c                        nø 116 :  dw-5*df   <0
c
c            restrictions geometriques des raidisseurs  (longitudinaux)
c            ----------------------------------------------------------
c                set de rahman-caldwell
c                nø 201 :  df-dw     <0
c                nø 202 :  dw-2*df   <0
c                nø 203 :  dw-40*tw  <0
c                set a:
c                nø 204 :  delta-2*tw<0
c                nø 205 :  3*delta-dw<0

c                set de hughes
c                        nø 209 :  0.625df-dw<0
c                        nø 210 :  dw-2.5*df <0
c                        nø 211 :  dw-36*tw  <0
c                        nø 212 :  tw-2*delta<0  (complétaire à c204)
c                pour situation inverse (si raid/girder >> transv. frame
c                        nø 213 :  dw-120*tw <0         (complément set
c                        nø 214 :  0.0065+dw/170-tw<0   (complément set
c                si profil en cornières (à la place de c 209 et c210)
c                        nø 215 :  1.25*df-dw<0
c                        nø 216 :  dw-5*df   <0
c
c            restrictions croisees (transversaux - raidisseurs)
c            --------------------------------------------------
c                set de hughes
c                nø 301 :  dw(raid)-dw(aig)   <0
c                nø 302 :  tw(raid)-4*tw(aig) <0
c                nø 303 :  tw(aig) -4*tw(raid)<0
c                pour situation inverse (si raid/girder >> transv. frame
c                nø 304 :  dw(aig)-dw(raid)   <0

c ****************************************************************
c 1.    restrictions geometriques
c ****************************************************************
c     ii = nø local  de la restriction géométrique dans le panneau (i=1,
c     it = nø global de la restriction géométrique (pour la structure)
c     ic = référence de la restriction géométrique (cfr lm2)

      call annuld(dcj,9)
      ratio=0.

!	Ajout épaisseurs de corrosion
	delta = panneau(nel).delta + panneau(nel).corro(1)
	epsa  = panneau(nel).epsa
      hya   = panneau(nel).hya
	dya   = panneau(nel).dya + panneau(nel).corro(2)
	wya   = panneau(nel).wya
	tya   = panneau(nel).tya + panneau(nel).corro(2)
	hxr   = panneau(nel).hxr
	dxr   = panneau(nel).dxr + panneau(nel).corro(3)
	wxr   = panneau(nel).wxr
	txr   = panneau(nel).txr + panneau(nel).corro(3)
	epsr  = panneau(nel).epsr
!


c ****************************************************************
c 2.    restrictions geometriques des aiguilles - cadres
c ****************************************************************
c     restrictions nø 101 :  df-dw<0
c     -----------------------------------------------------------

      if(ic.eq.101) then
        vnom='df-dw<0    cadres'
        cj=wya-hya
        cjm=0.
	  
	  if ((wya.le.0.001.or.hya.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de cadre !!!'
	    !read(*,*)
	  endif

        if (hya.ne.0.) ratio=wya/hya

c     calcul de dcj = dérivées = sensibilités
        l=0
	  wya_min = wya
	  hya_max = hya
        do 101 ik=1,nbrxi
        if (nxi(ik).eq.2)  then
          dcj(ik)=-1.
          l=l+1
	    hya_max=panneau(nel).dvmax(nxi(ik))
        else if(nxi(ik).eq.4)  then
          dcj(ik)=+1.
          l=l+1
	    wya_min=panneau(nel).dvmin(nxi(ik))
        endif
  101   continue

	  if (wya_min.gt.hya_max) call errorgeom(ic,nel)
        call test(l,ic,nel)                                   !r&d15

c     restrictions nø 102 :  dw-2*df<0
c     -----------------------------------------------------------
      else if(ic.eq.102) then
        vnom='dw-2*df<0  cadres'
        cj=hya-2.*wya
        cjm=0.

	  if ((wya.le.0.001.or.hya.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de cadre !!!'
	    !read(*,*)
	  endif

        if (wya.ne.0.) ratio=hya/(2.*wya)
	  hya_min=hya
	  wya_max=wya
c      calcul de dcj = dérivées = sensibilités
        l=0
        do 102 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=1.
          l=l+1
	    hya_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.4)  then
          dcj(ik)=-2.
          l=l+1
	    wya_max=panneau(nel).dvmax(nxi(ik))
        endif
  102   continue

	  if (hya_min.gt.(2.d00*wya_max)) call errorgeom(ic,nel)
        call test(l,ic,nel)                                   !r&d15

c     restrictions nø 103 :  0.0065+dw/170-tw<0
c     -----------------------------------------------------------
      else if(ic.eq.103) then
        vnom='dw/a-tw<b  cadres'
        cj=hya/170-dya
        cjm=-0.0065

	  if ((dya.le.0.001.or.hya.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de cadre !!!'
	    !read(*,*)
	  endif

        if (dya.ne.0.) ratio=(hya/170)/dya
        hya_min=hya
	  dya_max=dya
c      calcul de dcj = dérivées = sensibilités
        l=0
        do 103 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=1./170.
          l=l+1
	    hya_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.3)  then
          dcj(ik)=-1.
          l=l+1
		dya_max=panneau(nel).dvmax(nxi(ik))
        endif
  103   continue

	  if (0.0065+hya_min/170.d00.gt.dya_max) call errorgeom(ic,nel)
        call test(l,ic,nel)                                   !r&d15

c     restrictions nø 104 :  d-2tw<0
c     -----------------------------------------------------------
      else if(ic.eq.104) then
        vnom='d-2tw<0    cadres'
        cj=delta-2.*dya
        cjm=0.

	  if (dya.le.0.001.and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de cadre !!!'
	    !read(*,*)
	  endif

        if (dya.ne.0.) ratio=delta/(2.*dya)
	  delta_min = delta
	  dya_max = dya
c      calcul de dcj = dérivées = sensibilités
        l=0
        do 104 ik=1,nbrxi
        if     (nxi(ik).eq.1)  then
          dcj(ik)=1.
          l=l+1
	    delta_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.3)  then
          dcj(ik)=-2.
          l=l+1
	    dya_max=panneau(nel).dvmax(nxi(ik))
        endif
  104 continue

	if (delta_min.gt.(2.d00*dya_max)) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15

c     restrictions nø 105 : 3d-dw<0
c     -----------------------------------------------------------
      else if(ic.eq.105) then
        vnom='3d-dw<0    cadres'
        cj=3.*delta-hya
        cjm=0.

	  if (hya.le.0.001.and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de cadre !!!'
	    !read(*,*)
	  endif	

        if (hya.ne.0.) ratio=3.*delta/hya
	  delta_min = delta
	  hya_max = hya
c      calcul de dcj = dérivées = sensibilités
        l=0
        do 105 ik=1,nbrxi
        if     (nxi(ik).eq.1)  then
          dcj(ik)=3.
          l=l+1
	    delta_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.2)  then
          dcj(ik)=-1.
          l=l+1
	    hya_max=panneau(nel).dvmax(nxi(ik))
        endif
  105 continue

	if ((3.d00*delta_min).gt.hya_max) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15

c     restrictions nø 109 : 0.625*df-dw<0
c     -----------------------------------------------------------
      else if(ic.eq.109) then
        vnom='0.6df-dw<0 cadres'
        cj=0.625*wya-hya
        cjm=0.

	  if ((hya.le.0.001.or.wya.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de cadre !!!'
	    !read(*,*)
	  endif

        if (hya.ne.0.) ratio=0.625*wya/hya
	  hya_max = hya
	  wya_min = wya
c      calcul de dcj = dérivées = sensibilités
        l=0
        do 109 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=-1.
          l=l+1
	    hya_max=panneau(nel).dvmax(nxi(ik))
        else if(nxi(ik).eq.4)  then
          dcj(ik)=0.625
          l=l+1
	    wya_min=panneau(nel).dvmin(nxi(ik))
        endif
  109 continue

	if ((0.625*wya_min).gt.hya_max) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15


c     restrictions nø 110 : dw-2.5*df   <0
c     -----------------------------------------------------------
      else if(ic.eq.110) then
        vnom='dw-2.5df<0 cadres'
        cj=hya-2.5*wya
        cjm=0.
 
 	  if ((hya.le.0.001.or.wya.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de cadre !!!'
	    !read(*,*)
	  endif

       if (wya.ne.0.) ratio=hya/(2.5*wya)
	  hya_min = hya
	  wya_max = wya
c      calcul de dcj = dérivées = sensibilités
        l=0
        do 110 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=1.
          l=l+1
	    hya_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.4)  then
          dcj(ik)=-2.5
          l=l+1
	    wya_max=panneau(nel).dvmax(nxi(ik))
        endif
  110 continue

	if (hya_min.gt.(2.5*wya_max)) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15


c     restrictions nø 111 : dw-120*tw <0
c     -----------------------------------------------------------
      else if(ic.eq.111) then
        vnom='dw-120tw<0 cadres'
        cj=hya-120.*dya
        cjm=0.

	  if ((hya.le.0.001.or.dya.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de cadre !!!'
	    !read(*,*)
	  endif

        if (dya.ne.0.) ratio=hya/(120.*dya)
	  hya_min = hya
	  dya_max = dya
c      calcul de dcj = dérivées = sensibilités
        l=0
        do 111 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=1.
          l=l+1
	    hya_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.3)  then
          dcj(ik)=-120.
          l=l+1
	    dya_max=panneau(nel).dvmax(nxi(ik))
        endif
  111 continue

	if (hya_min.gt.(120.d00*dya_max)) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15


c     restrictions nø 112 : tw-2*delta<0
c     -----------------------------------------------------------
      else if(ic.eq.112) then
        vnom='tw-2delt<0 cadres'
        cj=dya-2.*delta
        cjm=0.

	  if (dya.le.0.001.and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de cadre !!!'
	    !read(*,*)
	  endif

        if (delta.ne.0.) ratio=dya/(2.*delta)
	  delta_max = delta
	  dya_min = dya
c      calcul de dcj = dérivées = sensibilités
        l=0
        do 112 ik=1,nbrxi
        if     (nxi(ik).eq.1)  then
          dcj(ik)=-2.
          l=l+1
	    delta_max=panneau(nel).dvmax(nxi(ik))
        else if(nxi(ik).eq.3)  then
          dcj(ik)=1.
          l=l+1
	    dya_min=panneau(nel).dvmin(nxi(ik))
        endif
  112 continue

	if (dya_min.gt.(2.d00*delta_max)) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15


c     restrictions nø 113 : dw-36*tw  <0
c     -----------------------------------------------------------
      else if(ic.eq.113) then
        vnom='dw-36*tw<0 cadres'
        cj=hya-36.*dya
        cjm=0.

	  if ((hya.le.0.001.or.dya.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de cadre !!!'
	    !read(*,*)
	  endif

        if (dya.ne.0.) ratio=hya/(36.*dya)
        hya_min = hya
	  dya_max = dya
c      calcul de dcj = dérivées = sensibilités
        l=0
        do 113 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=1.
          l=l+1
	    hya_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.3)  then
          dcj(ik)=-36.
          l=l+1
	    dya_max=panneau(nel).dvmax(nxi(ik))
        endif
  113 continue

	if (hya_min.gt.(36.d00*dya_max)) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15


c     restrictions nø 114 : dw-40*tw  <0
c     -----------------------------------------------------------
      else if(ic.eq.114) then
        vnom='dw-40*tw<0 cadres'
        cj=hya-40.*dya
        cjm=0.

	  if ((hya.le.0.001.or.dya.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de cadre !!!'
	    !read(*,*)
	  endif

        if (dya.ne.0.) ratio=hya/(40.*dya)
	  hya_min = hya
	  dya_max = dya
c      calcul de dcj = dérivées = sensibilités
        l=0
        do 114 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=1.
          l=l+1
	    hya_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.3)  then
          dcj(ik)=-40.
          l=l+1
	    dya_max=panneau(nel).dvmax(nxi(ik))
        endif
  114 continue

	if (hya_min.gt.(40.d00*dya_max)) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15


c     restrictions nø 115 : 1.25*df-dw<0
c     -----------------------------------------------------------
      else if(ic.eq.115) then
        vnom='1.2df-dw<0 cadres'
        cj=1.25*wya-hya
        cjm=0.

	  if ((hya.le.0.001.or.wya.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de cadre !!!'
	    !read(*,*)
	  endif

        if (hya.ne.0.) ratio=(1.25*wya)/hya
        hya_max = hya
	  wya_min = wya
c      calcul de dcj = dérivées = sensibilités
        l=0
        do 115 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=-1.
          l=l+1
	    hya_max=panneau(nel).dvmax(nxi(ik))
        else if(nxi(ik).eq.4)  then
          dcj(ik)=1.25
          l=l+1
	    wya_min=panneau(nel).dvmin(nxi(ik))
        endif
  115 continue

	if ((1.25*wya_min).gt.hya_max) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15


c     restrictions nø 116 : dw-5*df   <0
c     -----------------------------------------------------------
      else if(ic.eq.116) then
        vnom='dw-5*df <0 cadres'
        cj=hya-5.*wya
        cjm=0.

	  if ((hya.le.0.001.or.wya.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de cadre !!!'
	    !read(*,*)
	  endif

        if (wya.ne.0.) ratio=hya/(5.*wya)
	  hya_min = hya
	  wya_max = wya
c      calcul de dcj = dérivées = sensibilités
        l=0
        do 116 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=1.
          l=l+1
	    hya_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.4)  then
          dcj(ik)=-5.
          l=l+1
	    wya_max=panneau(nel).dvmax(nxi(ik))
        endif
  116 continue

	if (hya_min.gt.(5.d00*wya_max)) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15

c     restrictions nø 120 : -df + 0,2.dw   < 0
c     -----------------------------------------------------------
      else if(ic.eq.120) then								!sept06
        vnom='df/dw>0.2  cadres'								!sept06
        cj=-wya + 0.2*hya									!sept06
        cjm=0.											!sept06

	  if ((hya.le.0.001.or.wya.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de cadre !!!'
	    !read(*,*)
	  endif

        if (hya.ne.0.) ratio=wya/(0.2*hya)
        hya_min = hya
	  wya_max = wya
c      calcul de dcj = dérivées = sensibilités			!sept06
        l=0												!sept06
	  do 120 ik=1,nbrxi									!sept06
        if     (nxi(ik).eq.2)  then						!sept06
          !dcj(ik)=wya/(hya**2)							!sept06
		dcj(ik)=0.2										!sept06
          l=l+1											!sept06
	    hya_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.4)  then						!sept06
          dcj(ik)=-1.										!sept06
          l=l+1											!sept06
	    wya_max=panneau(nel).dvmax(nxi(ik))
        endif												!sept06
  120 continue											!sept06

	if ((0.2*hya_min).gt.wya_max) call errorgeom(ic,nel)
      call test(l,ic,nel)										!r&d15	!sept06

c     restrictions nø 121 : df/dw   <0,5
c     -----------------------------------------------------------
      else if(ic.eq.121) then								!sept06
        vnom='df/dw<0.5  cadres'								!sept06
        cj=wya - 0.5*hya									!sept06
        cjm=0.											!sept06

	  if ((hya.le.0.001.or.wya.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de cadre !!!'
	    !read(*,*)
	  endif

        if (hya.ne.0.) ratio=wya/(0.5*hya)
	  hya_max = hya
	  wya_min = wya
c      calcul de dcj = dérivées = sensibilités			!sept06
        l=0
	  do 121 ik=1,nbrxi									!sept06
        if     (nxi(ik).eq.2)  then						!sept06
          dcj(ik)=-0.5									!sept06
          l=l+1											!sept06
	    hya_max=panneau(nel).dvmax(nxi(ik))
        else if(nxi(ik).eq.4)  then						!sept06
          dcj(ik)=1.										!sept06
          l=l+1											!sept06
	    wya_min=panneau(nel).dvmin(nxi(ik))
        endif												!sept06
  121 continue											!sept06

	if (wya_min.gt.(0.5*hya_max)) call errorgeom(ic,nel)
      call test(l,ic,nel)										!r&d15	!sept06

c     restrictions nø 122 : -dw/tw   <-28					!sept06
c     -----------------------------------------------------------
      else if(ic.eq.122) then								!sept06
        vnom='dw/tw>28   cadres'								!sept06
        cj=-hya + 28.*dya									!sept06
        cjm=0.									!sept06
        
 	  if ((hya.le.0.001.or.dya.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de cadre !!!'
	    !read(*,*)
	  endif

      if (hya.ne.0.) ratio=28.*dya/hya
	  hya_max = hya
	  dya_min = dya
c      calcul de dcj = dérivées = sensibilités
        l=0
	  do 122 ik=1,nbrxi									!sept06
        if     (nxi(ik).eq.2)  then
          dcj(ik)=-1.
          l=l+1
	    hya_max=panneau(nel).dvmax(nxi(ik))
        else if(nxi(ik).eq.3)  then
          dcj(ik)=28.
          l=l+1
	    dya_min=panneau(nel).dvmin(nxi(ik))
        endif
  122 continue

	if ((28.d00*dya_min).gt.hya_max) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15

c     restrictions nø 123 : dw/tw   <90
c     -----------------------------------------------------------
      else if(ic.eq.123) then
          vnom='dw/tw<90   cadres'
        cj=hya - 90.*dya
        cjm=0.

	  if ((hya.le.0.001.or.dya.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de cadre !!!'
	    !read(*,*)
	  endif

        if (hya.ne.0.) ratio=90.*hya/dya
	  hya_min = hya
	  dya_max = dya
c      calcul de dcj = dérivées = sensibilités
        l=0
	  do 123 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=1.
          l=l+1
	    hya_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.3)  then
          dcj(ik)=-90.
          l=l+1
	    dya_max=panneau(nel).dvmax(nxi(ik))
        endif
  123 continue

	if (dya_min.gt.(90.d00*hya_max)) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15

c     restrictions nø 126 : -tf/tw   <-1
c     -----------------------------------------------------------
c      else if(ic.eq.126) then
c          vnom='tf/tw>1    cadres'
c        cj=-tya + dya
c        cjm=0.

c      calcul de dcj = dérivées = sensibilités
c        l=0
c	  do 126 ik=1,nbrxi
c        if     (nxi(ik).eq.3)  then
c          dcj(ik)=1.
c          l=l+1
c        endif
c  126 continue

c      call test(l)


c ****************************************************************
c 3.    restrictions geometriques des raidisseurs
c ****************************************************************
c     restrictions nø 201 :  df-dw<0
c     -----------------------------------------------------------
      else if(ic.eq.201) then
        vnom='df-dw<0    raidisseurs'
        cj=wxr-hxr
        cjm=0.

	  if ((hxr.le.0.001.or.wxr.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (hxr.ne.0.) ratio=wxr/hxr
	  hxr_max = hxr
	  wxr_min = wxr
c      calcul de dcj = dérivées = sensibilités
        l=0
        do 201 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=-1.
          l=l+1
	    hxr_max=panneau(nel).dvmax(nxi(ik))
        else if(nxi(ik).eq.8)  then
          dcj(ik)=+1.
          l=l+1
	    wxr_min=panneau(nel).dvmin(nxi(ik))
        endif
  201 continue

	if (wxr_min.gt.hxr_max) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15


c     restrictions nø 202 :  dw-2*df<0
c     -----------------------------------------------------------
      else if(ic.eq.202) then
        vnom='dw-2*df<0  raidisseurs'
        cj=hxr-2.*wxr
        cjm=0.

	  if ((hxr.le.0.001.or.wxr.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (wxr.ne.0.) ratio=hxr/(2.*wxr)
	  hxr_min = hxr
	  wxr_max = wxr
c      calcul de dcj = dérivées = sensibilités
        l=0
        do 202 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=1.
          l=l+1
	    hxr_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.8)  then
          dcj(ik)=-2.
          l=l+1
	    wxr_max=panneau(nel).dvmax(nxi(ik))
        endif
  202 continue

	if (hxr_min.gt.(2.d00*wxr_max)) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15

c     restrictions nø 203 :  dw-40*tw<0
c     -----------------------------------------------------------
      else if(ic.eq.203) then
        vnom='dw-40*tw<0 raidisseurs'
        cj=hxr-40.*dxr
        cjm=0.

	  if ((hxr.le.0.001.or.dxr.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (dxr.ne.0.) ratio=hxr/(40.*dxr)
	  hxr_min = hxr
	  dxr_max = dxr
c      calcul de dcj = dérivées = sensibilités
        l=0
        do 203 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=1.
          l=l+1
	    hxr_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.7)  then
          dcj(ik)=-40.
          l=l+1
	    dxr_max=panneau(nel).dvmax(nxi(ik))
        endif
  203 continue

	if (hxr_min.gt.(40.d00*dxr_max)) then
c	     write(*,*) 'panneau(nel).dvmax(nxi(ik)) :',
c    *					panneau(nel).dvmax(nxi(ik))
c		 write(*,*) 'hxr_min =',hxr_min
c		 write(*,*) 'dxr_max =',dxr_max
		 call errorgeom(ic,nel)
	endif
      call test(l,ic,nel)                                   !r&d15

c     restrictions nø 204 :  d-2tw<0
c     -----------------------------------------------------------
      else if(ic.eq.204) then
        vnom='d-2tw<0    raidisseurs'
        cj=delta-2.*dxr
        cjm=0.
  
	  if (dxr.le.0.001.and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif
  
        if (dxr.ne.0.) ratio=delta/(2.*dxr)
	  delta_min = delta
	  dxr_max = dxr
c      calcul de dcj = dérivées = sensibilités
        l=0
        do 204 ik=1,nbrxi
        if     (nxi(ik).eq.1)  then
          dcj(ik)=1.
          l=l+1
	    delta_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.7)  then
          dcj(ik)=-2.
          l=l+1
	    dxr_max=panneau(nel).dvmax(nxi(ik))
        endif
  204 continue

	if (delta_min.gt.(2.d00*dxr_max)) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15

c     restrictions nø 205 : 3d-dw<0
c     -----------------------------------------------------------
      else if(ic.eq.205) then
          vnom='3d-dw<0    raidisseurs'
        cj=3.*delta-hxr
        cjm=0.

	  if (hxr.le.0.001.and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (hxr.ne.0.) ratio=3.*delta/hxr
	  delta_min = delta
	  hxr_max = hxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 205 ik=1,nbrxi
        if     (nxi(ik).eq.1)  then
          dcj(ik)=3.
          l=l+1
	    delta_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.6)  then
          dcj(ik)=-1.
          l=l+1
	    hxr_max=panneau(nel).dvmax(nxi(ik))
        endif
  205 continue

	if ((3.d00*delta_min).gt.hxr_max) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15


c     restrictions nø 206 : d-f(epsr)<0 (!!!passenger ship!!!)						!r&d15
c     -----------------------------------------------------------						!r&d15
      else if(ic.eq.206) then															!r&d15
	  vnom='f(epsr)-d<0'															!r&d15
	  if(panneau(nel).code.eq.1) then		!keel												!r&d15
	    deltamin=(3.8+0.040*length*coefk1**0.5+4.5*epsr)*0.001						!r&d15
	  elseif((panneau(nel).code.eq.2).or.(panneau(nel).code.eq.4))then	!bottom & bilge						!r&d15
		if(hxr.gt.0.010) then	!longitudinal framing								!r&d15
	      deltamin=(1.9+0.032*length*coefk1**0.5+4.5*epsr)*0.001					!r&d15
		else					!transverse framing									!r&d15
	      deltamin=(2.8+0.032*length*coefk1**0.5+4.5*epsr)*0.001					!r&d15
		endif																		!r&d15
	  elseif(panneau(nel).code.eq.3) then	!inner bottom - outside engine room					!r&d15
	    deltamin=(2.0+0.020*length*coefk1**0.5+4.5*epsr)*0.001						!r&d15
	  elseif(panneau(nel).code.eq.5) then	!side - below freeboard deck						!r&d15
	    deltamin=(2.1+0.028*length*coefk1**0.5+4.5*epsr)*0.001						!r&d15
	  elseif(panneau(nel).code.eq.6) then	!side - between freeboard & strength decks			!r&d15
	    deltamin=(2.1+0.013*length*coefk1**0.5+4.5*epsr)*0.001						!r&d15
	  elseif(panneau(nel).code.eq.7) then	!inner side											!r&d15
		if(length.lt.120.) then														!r&d15
	      deltamin=(1.7+0.013*length*coefk1**0.5+4.5*epsr)*0.001					!r&d15
		else																		!r&d15
	      deltamin=(3.6+2.2*coefk1**0.5+epsr)*0.001									!r&d15
		endif																		!r&d15
	  elseif(panneau(nel).code.eq.8) then	!weather strength deck								!r&d15
		deltamin=(2.1+2.2*coefk1**0.5+epsr)*0.001									!r&d15
	  elseif(panneau(nel).code.eq.9) then	!accomodation deck									!r&d15
	    if(length.lt.120.) then														!r&d15
	      deltamin=(1.3+0.004*length*coefk1**0.5+4.5*epsr)*0.001					!r&d15
		else																		!r&d15
	      deltamin=(2.1+2.2*coefk1**0.5+epsr)*0.001									!r&d15
		endif																		!r&d15
	  elseif(panneau(nel).code.eq.10) then	!bulkhead											!r&d15
	    if(length.lt.120.) then														!r&d15
	      deltamin=(1.7+0.013*length*coefk1**0.5+4.5*epsr)*0.001					!r&d15
		else																		!r&d15
	      deltamin=(3.6+2.2*coefk1**0.5+epsr)*0.001									!r&d15
		endif																		!r&d15
	  endif																			!r&d15

        cj=deltamin-delta																!r&d15
        cjm=0.																		!r&d15
        if (delta.ne.0.) ratio=deltamin/delta	

c     calcul de dcj = dérivées = sensibilités
        l=0																			!r&d15
        do 206 ik=1,nbrxi																!r&d15
          if(nxi(ik).eq.1)  then														!r&d15
            dcj(ik)=-1.																!r&d15
            l=l+1																		!r&d15
          else if(nxi(ik).eq.9)  then													!r&d15
            if(((panneau(nel).code.eq.7).or.(panneau(nel).code.eq.9).or.
     *			(panneau(nel).code.eq.10)).							!r&d15
     *	  and.(length.ge.120.)) then												!r&d15
		    dcj(ik)=0.001															!r&d15
		  elseif(panneau(nel).code.eq.8) then													!r&d15
	        dcj(ik)=0.001															!r&d15
	      else																		!r&d15
	        dcj(ik)=0.0045															!r&d15
		  endif																		!r&d15
            l=l+1																		!r&d15
          endif																		!r&d15
  206 continue																		!r&d15

      call test(l,ic,nel)                                   								!r&d15


c     restrictions nø 209 : 0.625*df-dw<0
c     -----------------------------------------------------------
      else if(ic.eq.209) then
          vnom='0.6df-dw<0 raidisseurs'
        cj=0.625*wxr-hxr
        cjm=0.
        
	  if ((hxr.le.0.001.or.wxr.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

	  if (hxr.ne.0.) ratio=0.625*wxr/hxr
	  hxr_max = hxr
	  wxr_min = wxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 209 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=-1.
          l=l+1
	    hxr_max=panneau(nel).dvmax(nxi(ik))
        else if(nxi(ik).eq.8)  then
          dcj(ik)=0.625
          l=l+1
		wxr_min=panneau(nel).dvmin(nxi(ik))
        endif
  209 continue

	if ((0.625*wxr_min).gt.hxr_max) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15


c     restrictions nø 210 : dw-2.5*df   <0
c     -----------------------------------------------------------
      else if(ic.eq.210) then
        vnom='dw-2.5df<0 raidisseurs'
        cj=hxr-2.5*wxr
        cjm=0.

	  if ((hxr.le.0.001.or.wxr.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (wxr.ne.0.) ratio=hxr/(2.5*wxr)
	  hxr_min = hxr
	  wxr_max = wxr
c      calcul de dcj = dérivées = sensibilités
        l=0
        do 210 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=1.
          l=l+1
	    hxr_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.8)  then
          dcj(ik)=-2.5
          l=l+1
	    wxr_max=panneau(nel).dvmax(nxi(ik))
        endif
  210 continue

	if (hxr_min.gt.(2.5*wxr_max)) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15


c     restrictions nø 211 : dw-36*tw <0
c     -----------------------------------------------------------
      else if(ic.eq.211) then
        vnom='dw-36*tw<0 raidisseurs'
        cj=hxr-36.*dxr
        cjm=0.

	  if ((hxr.le.0.001.or.dxr.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (dxr.ne.0.) ratio=hxr/(36.*dxr)
	  hxr_min = hxr
	  dxr_max = dxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 211 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=1.
          l=l+1
	    hxr_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.7)  then
          dcj(ik)=-36.
          l=l+1
	    dxr_max=panneau(nel).dvmax(nxi(ik))
        endif
  211 continue

	if (hxr_min.gt.(36.d00*dxr_max)) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15


c     restrictions nø 212 : tw-2*delta<0
c     ------------------------------------
      else if(ic.eq.212) then
        vnom='tw-2delt<0 raidisseurs'
        cj=dxr-2.*delta
        cjm=0.

	  if (dxr.le.0.001.and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (delta.ne.0.) ratio=dxr/(2.*delta)
	  delta_max = delta
	  dxr_min = dxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 212 ik=1,nbrxi
        if     (nxi(ik).eq.1)  then
          dcj(ik)=-2.
          l=l+1
	    delta_max=panneau(nel).dvmax(nxi(ik))
        else if(nxi(ik).eq.7)  then
          dcj(ik)=1.
          l=l+1
	    dxr_min=panneau(nel).dvmin(nxi(ik))
        endif
  212 continue

	if (dxr_min.gt.(2.d00*delta_max)) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15


c     restrictions nø 213 : dw-120*tw  <0
c     -------------------------------------
      else if(ic.eq.213) then
          vnom='dw-120tw<0 raidisseurs'
        cj=hxr-120.*dxr
        cjm=0.

	  if ((hxr.le.0.001.or.dxr.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (dxr.ne.0.) ratio=hxr/(120.*dxr)
	  hxr_min = hxr
	  dxr_max = dxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 213 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=1.
          l=l+1
	    hxr_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.7)  then
          dcj(ik)=-120.
          l=l+1
		dxr_max=panneau(nel).dvmax(nxi(ik))
        endif
  213 continue

	if (hxr_min.gt.(120.d00*dxr_max)) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15


c     restrictions nø 214 : 0.0065+dw/170-tw  <0
c     --------------------------------------------
      else if(ic.eq.214) then
        vnom='dw/a-tw<b  raidisseurs'
        cj=0.0065+hxr/170.-dxr
        cjm=0.

	  if ((hxr.le.0.001.or.dxr.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (dxr.ne.0.) ratio=(0.0065+hxr/170.)/dxr
	  hxr_min = hxr
	  dxr_max = dxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 214 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=1/170.
          l=l+1
	    hxr_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.7)  then
          dcj(ik)=-1.
          l=l+1
	    dxr_max=panneau(nel).dvmax(nxi(ik))
        endif
  214 continue

	if ((0.0065+hxr_min/170.d00).gt.dxr_max) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15


c     restrictions nø 215 : 1.25*df-dw<0
c     -----------------------------------------------------------
      else if(ic.eq.215) then
        vnom='1.2df-dw<0 raidisseurs'
        cj=1.25*wxr-hxr
        cjm=0.

	  if ((hxr.le.0.001.or.wxr.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (hxr.ne.0.) ratio=1.25*wxr/hxr
	  hxr_max = hxr
	  wxr_min = wxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 215 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=-1.
          l=l+1
	    hxr_max=panneau(nel).dvmax(nxi(ik))
        else if(nxi(ik).eq.8)  then
          dcj(ik)=1.25
          l=l+1
	    wxr_min=panneau(nel).dvmin(nxi(ik))
        endif
  215 continue

	if ((1.25*wxr_min).gt.hxr_max) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15


c     restrictions nø 216 : dw-5*df   <0
c     -----------------------------------------------------------
      else if(ic.eq.216) then
        vnom='dw-5*df<0  raidisseurs'
        cj=hxr-5.*wxr
        cjm=0.

	  if ((hxr.le.0.001.or.wxr.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (wxr.ne.0.) ratio=hxr/(5.*wxr)
	  hxr_min = hxr
	  wxr_max = wxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 216 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=1.
          l=l+1
		hxr_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.8)  then
          dcj(ik)=-5.
          l=l+1
		wxr_max=panneau(nel).dvmax(nxi(ik))
        endif
  216 continue

	if (hxr_min.gt.(5.d00*wxr_max)) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15

c     restrictions nø 220 : -dw/tw   <-8
c     -----------------------------------------------------------
      else if(ic.eq.220) then
        vnom='dw/tw>8    raidisseurs'
        cj=-hxr + 8.*dxr
        cjm=0.

	  if ((hxr.le.0.001.or.dxr.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (hxr.ne.0.) ratio=8.*dxr/hxr
	  hxr_max = hxr
	  dxr_min = dxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 220 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=-1.
          l=l+1
	    hxr_max=panneau(nel).dvmax(nxi(ik))
        else if(nxi(ik).eq.7)  then
          dcj(ik)=-8.
          l=l+1
		dxr_min=panneau(nel).dvmin(nxi(ik))
        endif
  220 continue

	if ((8.d00*dxr_min).gt.hxr_max) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15

c     restrictions nø 221 : dw/tw   <50
c     -----------------------------------------------------------
      else if(ic.eq.221) then
        vnom='dw/tw<50   raidisseurs'
        cj=hxr - 50.*dxr
        cjm=0.

	  if ((hxr.le.0.001.or.dxr.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (dxr.ne.0.) ratio=hxr/(50.*dxr)
	  hxr_min = hxr
	  dxr_max = dxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 221 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=1.
          l=l+1
	    hxr_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.7)  then
          dcj(ik)=-50.
          l=l+1
	    dxr_max=panneau(nel).dvmax(nxi(ik))
        endif
  221 continue

	if (hxr_min.gt.(50.d00*dxr_max)) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15

c     restrictions nø 230 : -df/dw   <-0,2
c     -----------------------------------------------------------
      else if(ic.eq.230) then
        vnom='df/dw>0.2  raidisseurs'
        cj=-wxr + 0.2*hxr
        cjm=0.

	  if ((hxr.le.0.001.or.wxr.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (wxr.ne.0.) ratio=0.2*hxr/wxr
	  hxr_min = hxr
	  wxr_max = wxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 230 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=0.2
          l=l+1
		hxr_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.8)  then
          dcj(ik)=-1.
          l=l+1
		wxr_max=panneau(nel).dvmax(nxi(ik))
        endif
  230 continue

	if ((0.2*hxr_min).gt.wxr_max) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15

c     restrictions nø 231 : df/dw   <0,5
c     -----------------------------------------------------------
      else if(ic.eq.231) then
        vnom='df/dw<0.5  raidisseurs'
        cj=wxr - 0.5*hxr
        cjm=0.

	  if ((hxr.le.0.001.or.wxr.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (hxr.ne.0.) ratio=wxr/(0.5*hxr)
	  hxr_max = hxr
	  wxr_min = wxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 231 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=-0.5
          l=l+1
		hxr_max=panneau(nel).dvmax(nxi(ik))
        else if(nxi(ik).eq.8)  then
          dcj(ik)=1.
          l=l+1
		wxr_min=panneau(nel).dvmin(nxi(ik))
        endif
  231 continue

	if (wxr_min.gt.(0.5*wxr_max)) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15

c     restrictions nø 232 : -dw/tw   <-28
c     -----------------------------------------------------------
      else if(ic.eq.232) then
          vnom='dw/tw>28   raidisseurs'
        cj=-hxr + 28.*dxr
        cjm=0.

	  if ((hxr.le.0.001.or.dxr.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (hxr.ne.0.) ratio=28.*dxr/hxr
	  hxr_max = hxr
	  dxr_min = dxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 232 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=-1.
          l=l+1
		hxr_max=panneau(nel).dvmax(nxi(ik))
        else if(nxi(ik).eq.7)  then
          dcj(ik)=28.
          l=l+1
		dxr_min=panneau(nel).dvmin(nxi(ik))
        endif
  232 continue

	if ((28.d00*dxr_min).gt.hxr_max) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15

c     restrictions nø 233 : dw/tw   <90
c     -----------------------------------------------------------
      else if(ic.eq.233) then
        vnom='dw/tw<90   raidisseurs'
        cj=hxr - 90.*dxr
        cjm=0.

	  if ((hxr.le.0.001.or.dxr.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (dxr.ne.0.) ratio=hxr/90.*dxr
	  hxr_min = hxr
	  dxr_max = dxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 233 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=1.
          l=l+1
		hxr_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.7)  then
          dcj(ik)=-90.
          l=l+1
		dxr_max=panneau(nel).dvmax(nxi(ik))
        endif
  233 continue

	if (hxr_min.gt.(90.d00*dxr_max)) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15

c     restrictions nø 234 : -tw   <-8
c     -----------------------------------------------------------
      else if(ic.eq.234) then
        vnom='tw>8       raidisseurs'
        cj=-dxr
        cjm=-0.008 !unité

	  if (hxr.le.0.001.and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (dxr.ne.0.) ratio=0.008/dxr
	  dxr_max = dxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 234 ik=1,nbrxi
        if     (nxi(ik).eq.7)  then
          dcj(ik)=-1.
          l=l+1
		dxr_max=panneau(nel).dvmax(nxi(ik))
	  endif
  234 continue

	if (0.008.gt.dxr_max) call errorgeom(ic,nel)
      if (l.eq.0) then
	  write(*,1)
	endif

c     restrictions nø 235 : tw   <20
c     -----------------------------------------------------------
      else if(ic.eq.235) then
          vnom='tw<20      raidisseurs'
        cj=dxr
        cjm=0.020  !unité
        
	  if (dxr.le.0.001.and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif
	  
	  ratio=dxr/0.020
	  dxr_min = dxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 235 ik=1,nbrxi
        if     (nxi(ik).eq.7)  then
          dcj(ik)=1.
          l=l+1
		dxr_min=panneau(nel).dvmin(nxi(ik))
	  endif
  235 continue

	if (dxr_min.gt.0.020) call errorgeom(ic,nel)
      if (l.eq.0) then
	  write(*,1)
	endif

c     restrictions nø 236 : -df/dw   <-0,1
c     -----------------------------------------------------------
      else if(ic.eq.236) then
        vnom='df/dw>0.1  raidisseurs'
        cj=-wxr + 0.1*hxr
        cjm=0.

	  if ((hxr.le.0.001.or.wxr.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif
        
	  if (wxr.ne.0.) ratio=0.1*hxr/wxr
	  hxr_min = hxr
	  wxr_max = wxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 236 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=0.1
          l=l+1
	    hxr_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.8)  then
          dcj(ik)=-1.
          l=l+1
	    wxr_max=panneau(nel).dvmax(nxi(ik))
        endif
  236 continue

	if ((0.1*hxr_min).gt.wxr_max) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15

c     restrictions nø 237 : dw/tw   <130
c     -----------------------------------------------------------
      else if(ic.eq.237) then
        vnom='dw/tw<130  raidisseurs'
        cj=hxr - 130.*dxr
        cjm=0.

	  if ((hxr.le.0.001.or.dxr.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (dxr.ne.0.) ratio=hxr/(130.*dxr)
	  hxr_min = hxr
	  dxr_max = dxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 237 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=1.
          l=l+1
	    hxr_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.7)  then
          dcj(ik)=-130.
          l=l+1
		dxr_max=panneau(nel).dvmax(nxi(ik))
        endif
  237 continue

	if (hxr_min.gt.(130.d00*dxr_max)) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15

c     restrictions nø 240 : -df/dw   <-0,1 (lq on simule traverse sur axe sym)
c     -----------------------------------------------------------
      else if(ic.eq.240) then
          vnom='2df/dw>0.2 raidisseurs'
        cj=-wxr + 0.1*hxr
        cjm=0.

	  if ((hxr.le.0.001.or.wxr.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (wxr.ne.0.) ratio=0.1*hxr/wxr
	  hxr_min = hxr
	  wxr_max = wxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 240 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=0.1
          l=l+1
		hxr_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.8)  then
          dcj(ik)=-1.
          l=l+1
		wxr_max=panneau(nel).dvmax(nxi(ik))
        endif
  240 continue
	
	if ((0.1*hxr_min).gt.wxr_max) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15

c     restrictions nø 241 : df/dw   <0,25 (lq on simule traverse sur axe sym)
c     -----------------------------------------------------------
      else if(ic.eq.241) then
          vnom='2df/dw<0.5 raidisseurs'
        cj=wxr - 0.25*hxr
        cjm=0.

	  if ((hxr.le.0.001.or.wxr.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (hxr.ne.0.) ratio=wxr/(0.25*hxr)
	  hxr_max = hxr
	  wxr_min = wxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 241 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=-0.25
          l=l+1
	    hxr_max=panneau(nel).dvmax(nxi(ik))
        else if(nxi(ik).eq.8)  then
          dcj(ik)=1.
          l=l+1
		wxr_min=panneau(nel).dvmin(nxi(ik))
        endif
  241 continue

	if (wxr_min.gt.(0.25*hxr_max)) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15

c     restrictions nø 242 : -dw/tw   <-56 (lq on simule traverse sur axe sym)
c     -----------------------------------------------------------
      else if(ic.eq.242) then
        vnom='dw/2.tw>28 raidisseurs'
        cj=-hxr + 56.*dxr
        cjm=0.

	  if ((hxr.le.0.001.or.dxr.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (hxr.ne.0.) ratio=56.*dxr/hxr
	  hxr_max = hxr
	  dxr_min = dxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 242 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=-1.
          l=l+1
		hxr_max=panneau(nel).dvmax(nxi(ik))
        else if(nxi(ik).eq.7)  then
          dcj(ik)=56.
          l=l+1
		dxr_min=panneau(nel).dvmin(nxi(ik))
        endif
  242 continue

	if ((56.d00*dxr_min).gt.hxr_max) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15

c     restrictions nø 243 : dw/tw   <180 (lq on simule traverse sur axe sym)
c     -----------------------------------------------------------
      else if(ic.eq.243) then
          vnom='dw/2.tw<90 raidisseurs'
        cj=hxr - 180.*dxr
        cjm=0.

	  if ((hxr.le.0.001.or.dxr.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (dxr.ne.0.) ratio=hxr/(180.*dxr)
	  hxr_min = hxr
	  dxr_max = dxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 243 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=1.
          l=l+1
	    hxr_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.7)  then
          dcj(ik)=-180.
          l=l+1
	    dxr_max=panneau(nel).dvmax(nxi(ik))
        endif
  243 continue

	if (hxr_min.gt.(180.d00*hxr_max)) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15

c     restrictions nø 244 : -tw   <-4 (lq on simule traverse sur axe sym)
c     -----------------------------------------------------------
      else if(ic.eq.244) then
        vnom='2.tw>8     raidisseurs'
        cj=-dxr
        cjm=-0.004 !unité

	  if (dxr.le.0.001.and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (dxr.ne.0.) ratio=0.004/dxr
	  dxr_max = dxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 244 ik=1,nbrxi
        if     (nxi(ik).eq.7)  then
          dcj(ik)=-1.
          l=l+1
		dxr_max=panneau(nel).dvmax(nxi(ik))
	  endif
  244 continue

      if (0.004.gt.dxr_max) call errorgeom(ic,nel)
	if (l.eq.0) then
	  write(*,1)
	endif

c     restrictions nø 245 : tw   <10 (lq on simule traverse sur axe sym)
c     -----------------------------------------------------------
      else if(ic.eq.245) then
        vnom='2.tw<20    raidisseurs'
        cj=dxr
        cjm=0.010  !unité

	  if (dxr.le.0.001.and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        ratio=dxr/0.010
	  dxr_min = dxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 245 ik=1,nbrxi
        if     (nxi(ik).eq.7)  then
          dcj(ik)=1.
          l=l+1
		dxr_min=panneau(nel).dvmin(nxi(ik))
	  endif
  245 continue

	if (dxr_min.gt.0.010) call errorgeom(ic,nel)
      if (l.eq.0) then
	  write(*,1)
	endif

c     restrictions nø 246 : -df/dw   <-0,05 (lq on simule traverse sur axe sym)
c     -----------------------------------------------------------
      else if(ic.eq.246) then
        vnom='2df/dw>0.1 raidisseurs'
        cj=-wxr + 0.05*hxr
        cjm=0.

	  if ((hxr.le.0.001.or.wxr.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (wxr.ne.0.) ratio=0.05*hxr/wxr
	  hxr_min = hxr
	  wxr_max = wxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 246 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=0.05
          l=l+1
		hxr_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.8)  then
          dcj(ik)=-1.
          l=l+1
		wxr_max=panneau(nel).dvmax(nxi(ik))
        endif
  246 continue

	if ((0.05*hxr_min).gt.wxr_max) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15

c     restrictions nø 247 : dw/tw   <260 (lq on simule traverse sur axe sym)
c     -----------------------------------------------------------
      else if(ic.eq.247) then
        vnom='dw/2tw<130 raidisseurs'
        cj=hxr - 260.*dxr
        cjm=0.

	  if ((hxr.le.0.001.or.dxr.le.0.001).and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

        if (dxr.ne.0.) ratio=hxr/(260.*dxr)
	  hxr_min = hxr
	  dxr_max = dxr
c     calcul de dcj = dérivées = sensibilités
        l=0
        do 247 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=1.
          l=l+1
		hxr_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.7)  then
          dcj(ik)=-260.
          l=l+1
		dxr_max=panneau(nel).dvmax(nxi(ik))
        endif
  247 continue

	if (hxr_min.gt.(260.d00*wxr_max)) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15



c ****************************************************************
c 4.    restrictions croisees entre cadres et raidisseurs
c ****************************************************************

c     restrictions nø 301 : dw(r)-dw(a)<0
c     -----------------------------------------------------------
      else if(ic.eq.301) then
        vnom='dw(r) <dw(a)'
        cj=hxr-hya
        cjm=0.

	  if (hxr.le.0.001.and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif
	  if (hya.le.0.001.and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de cadre !'
	    !read(*,*)
	  endif

        if (hya.ne.0.) ratio=hxr/hya
	  hya_max = hya
	  hxr_min = hxr
c      calcul de dcj = dérivées = sensibilités
        l=0
        do 301 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=-1.
          l=l+1
		hya_max=panneau(nel).dvmax(nxi(ik))
        else if(nxi(ik).eq.6)  then
          dcj(ik)=1.
          l=l+1
		hxr_min=panneau(nel).dvmin(nxi(ik))
        endif
  301 continue

	if (hxr_min.gt.hya_max) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15

c     restrictions nø 302 : tw(r)-4*tw(a)<0
c     -----------------------------------------------------------
      else if(ic.eq.302) then
        vnom='tw(r)<4tw(a)'
        cj=dxr-4.*dya
        cjm=0.

	  if (dxr.le.0.001.and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif
	  if (dya.le.0.001.and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de cadre !'
	    !read(*,*)
	  endif

        if (dya.ne.0.) ratio=dxr/(4.*dya)
	  dya_max = dya
	  dxr_min = dxr
c      calcul de dcj = dérivées = sensibilités
        l=0
        do 302 ik=1,nbrxi
        if     (nxi(ik).eq.3)  then
          dcj(ik)=1.
          l=l+1
		dya_max=panneau(nel).dvmax(nxi(ik))
        else if(nxi(ik).eq.7)  then
          dcj(ik)=-1.
          l=l+1
		dxr_min=panneau(nel).dvmin(nxi(ik))
        endif
  302 continue

	if (dxr_min.gt.(4.d00*dya_max)) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15

c     restrictions nø 303 : tw(a)-4*tw(r)<0
c     -----------------------------------------------------------
      else if(ic.eq.303) then
          vnom='tw(a)<4tw(r)'
        cj=dya-4.*dxr
        cjm=0.
        
  	  if (dxr.le.0.001.and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif
		  if (dya.le.0.001.and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif

	  if (dxr.ne.0.) ratio=dya/(4.*dxr)
	  dya_min = dya
	  dxr_max = dxr
c      calcul de dcj = dérivées = sensibilités
        l=0
        do 303 ik=1,nbrxi
        if     (nxi(ik).eq.3)  then
          dcj(ik)=1.
          l=l+1
		dya_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.7)  then
          dcj(ik)=-1.
          l=l+1
		dxr_max=panneau(nel).dvmax(nxi(ik))
        endif
  303 continue

	if (dya_min.gt.(4.d00*dxr_max)) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15


c     restrictions nø 304 : dw(a)-dw(r)<0
c     -----------------------------------------------------------
      else if(ic.eq.304) then
        vnom='dw(a) <dw(r)'
        cj=hya-hxr
        cjm=0.

	  if (hxr.le.0.001.and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif
	  if (hya.le.0.001.and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de cadre !'
	    !read(*,*)
	  endif

        if (hxr.ne.0.) ratio=hya/hxr
	  hya_min = hya
	  hxr_max = hxr
c      calcul de dcj = dérivées = sensibilités
        l=0
        do 304 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=1.
          l=l+1
		hya_min=panneau(nel).dvmin(nxi(ik))
        else if(nxi(ik).eq.6)  then
          dcj(ik)=-1.
          l=l+1
		hxr_max=panneau(nel).dvmax(nxi(ik))
        endif
  304 continue

	if (hya_min.gt.hxr_max) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15

c     restrictions nø 320 : -dw(a)/dw(r)<-2
c     -----------------------------------------------------------
      else if(ic.eq.320) then
        vnom='dwa/dw(r) >2'
        cj=-hya +2.*hxr
        cjm=0.

	  if (hxr.le.0.001.and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de raidisseur !'
	    !read(*,*)
	  endif
	  if (hya.le.0.001.and.itera.eq.0) then
	    write(*,*) 'Attention panneau :',nel,' Restriction geom ',ic,
     *	' utilisee alors qu''il semble ne pas y avoir de cadre !'
	    !read(*,*)
	  endif

        if (hya.ne.0.) ratio=2.*hxr/hya
	  hya_max = hya
	  hxr_min = hxr
c      calcul de dcj = dérivées = sensibilités
        l=0
        do 320 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=-1.
          l=l+1
		hya_max=panneau(nel).dvmax(nxi(ik))
        else if(nxi(ik).eq.6)  then
          dcj(ik)=2.
          l=l+1
		hxr_min=panneau(nel).dvmin(nxi(ik))
        endif
  320 continue

	if ((2.d00*hxr_min).gt.hya_max) call errorgeom(ic,nel)
      call test(l,ic,nel)                                   !r&d15

c ****************************************************************
c 5.    autres restrictions
c ****************************************************************
      else
       write(*,*)  ' nø de la restr. géométrique ',ic,'est incorrecte'
       write(iu_31(iboat),*)
     *' nø de la restr. géométrique ',ic,'est incorrecte'
       write(iu_14(iboat),*)
     *' nø de la restr. géométrique ',ic,'est incorrecte'		!sept06		!bug
       write(*,*) 'stop'
	 read(*,*)
       stop
      end if

c     sauvetage sur file iu_26(iboat) et impressions sur file iu_31(iboat)		!extension neto	!fev2007
c     ---------------------------------------------------

      if((ic.ne.150).and.
     *   (ic.ne.151).and.
     *   (ic.ne.152).and.
     *   (ic.ne.153).and.
     *   (ic.ne.250).and.
     *   (ic.ne.251).and.
     *   (ic.ne.252).and.
     *   (ic.ne.253)) then

	  
	  kcontr = kcontr+1
	  do i=1,nbrxi
		cijopt(i+ncont,kcontr,iboat)=dcj(i)
	  enddo
	  cjopt(kcontr,iboat) = cj
	  cjmopt(kcontr,iboat) = cjm
	  vnom2(kcontr,iboat) = vnom
	  ic_rest(kcontr,iboat) = ic
	  ratio_rest(kcontr,iboat) = ratio
	  !ncont = ncont + nbrxi

!        write(iu_26(iboat),*) ntot										!extension neto	!fev2007
!        write(iu_26(iboat),*) (dcj(i),i=1,nbrxi)
!	  write(iu_26(iboat),*) cj
!	  write(iu_26(iboat),*) cjm
!	  write(iu_26(iboat),'(a)') vnom
!	  write(iu_26(iboat),*) ic
!	  write(iu_26(iboat),*) ratio    !extension neto	!fev2007 dad

      
	if((itera.eq.0).and.(impr2.ge.-1)) then
        write(iu_31(iboat),427) it,cj,cjm,ic,vnom
        endif

      endif


      return

   1  format(' error'/'********'/
     *       ' les variables liées à cette restriction ne sont pas'
     *       ' définies comme variables de conception !!!')

  100 format(' restr .nø',i3,' de type ',i3,'; c=',e14.7,' < ',e11.5,
     *       ' (',a12,')'/'dc=',(t4,9(e11.4,1x)))
  427 format('c(',i3,')= ',e11.5,' < ',e11.5,' de type nø',i3,
     *       ' (',a12,')')
      end


c **********************************************************************
      subroutine test(l,ic,nel)                                   !r&d15
c     -------------------
c     error si les variables (hauteur, épaisseur, etc.) n'interviennent
c     comme variables de conception.
	use param_section

      implicit double precision (a-h,o-z)

      if(l.eq.0) then
          write(*,1)
          write(iu_31(iboat),1)
	  	write(iu_14(iboat),1)		!sept06
		write(*,*) 'Panneau:',nel,' Restriction geom.:',ic
          write(*,*) 'stop'
	    read(*,*)
          return
      else if ((l.eq.1).and.(ic.ne.206)) then !pq pas 206 ???
          write(*,2)
          write(iu_31(iboat),2)
		write(iu_14(iboat),2)		!sept06	
		write(*,*) 'Panneau:',nel,' Restriction geom.:',ic
      endif

      return
   1  format(' error'/'********'/
     *       ' les variables liees a cette restriction ne sont pas'
     *       ' definies comme variables de conception !!!')
   2  format(' attention'/'**********'/
     *       ' une seule variable liee e cette restriction est'
     *       ' definie comme variable de conception !!!')
      end
c **********************************************************************
	subroutine errorgeom(i,nel)

	use param_section, ONLY : langue

	integer*4 i,nel

	if (langue.eq.1) then
	write(*,'(a,i4,a,i3)') 'Erreur dans la restriction geometrique',i,
     * ' du panneau ',nel
	 write(*,'(a)') 'Impossible de satisfaire la restriction avec les 
     *variables selectionnees et les bornes definies!'
	 write(*,'(a)') 'Attention, bornes ont peut-etre ete mod.ds Opti'
	 write(*,'(a)') 'Veuillez corriger vos donnees - appuyer sur ENTER 
     *pour poursuivre'
	elseif (langue.eq.2) then
	 write(*,'(a,i4,a,i3)') 'Error in the geometrical constraint',i,
     *' of the panel ',nel
	 write(*,'(a)') 'Impossible to satisfy constraint with selected va
     *riable and bounds defined!'
	 write(*,'(a)') 'Attention, limits could have been mod.in opti'
	 write(*,'(a)') 'Please correct your data - press ENTER to resume'
	endif
	read(*,*)
	stop

	return
	end
