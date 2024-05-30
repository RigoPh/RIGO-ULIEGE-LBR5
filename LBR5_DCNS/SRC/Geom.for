      subroutine geom(epsa,epsr,delta,hya,dya,wya,tya,hxr,dxr,wxr,txr,
     *                ii,it,nxi,nbrxi,itera,iprint,ic,m2max,ntot,impr2,	!sept2006       ! on envoit ic directement a la place de lm2
     *				code,coefk1,length,isect1,ityp1,ximin,ximax,
     *                xicou,neto)									!r&d15
      implicit real *8 (a-h,o-z)
      integer*2 code
	real*8 length
	character*30 vnom    !dad
      dimension nxi(9),dcj(9)
	dimension ximin(neto*9)
	dimension ximax(neto*9)
	dimension xicou(neto*9)

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

c ****************************************************************
c 2.    restrictions geometriques des aiguilles - cadres
c ****************************************************************
c     restrictions nø 101 :  df-dw<0
c     -----------------------------------------------------------

      if(ic.eq.101) then
        vnom='df-dw<0    cadres'
        cj=wya-hya
        cjm=0.
        if (hya.ne.0.) ratio=wya/hya

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 101 ik=1,nbrxi
        if (nxi(ik).eq.2)  then
          dcj(ik)=-1.
          l=l+1
        else if(nxi(ik).eq.4)  then
          dcj(ik)=+1.
          l=l+1
        endif
  101   continue

        call test(l,ic)                                   !r&d15


c     restrictions nø 102 :  dw-2*df<0
c     -----------------------------------------------------------
      else if(ic.eq.102) then
        vnom='dw-2*df<0  cadres'
        cj=hya-2.*wya
        cjm=0.
        if (wya.ne.0.) ratio=hya/(2.*wya)

c      calcul de dcj = dérivées = sensibilités
        l=0
        do 102 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=1.
          l=l+1
        else if(nxi(ik).eq.4)  then
          dcj(ik)=-2.
          l=l+1
        endif
  102   continue

        call test(l,ic)                                   !r&d15

c     restrictions nø 103 :  0.0065+dw/170-tw<0
c     -----------------------------------------------------------
      else if(ic.eq.103) then
        vnom='dw/a-tw<b  cadres'
        cj=hya/170-dya
        cjm=-0.0065
        if (dya.ne.0.) ratio=(hya/170)/dya

c      calcul de dcj = dérivées = sensibilités
        l=0
        do 103 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=1./170.
          l=l+1
        else if(nxi(ik).eq.3)  then
          dcj(ik)=-1.
          l=l+1
        endif
  103   continue

        call test(l,ic)                                   !r&d15

c     restrictions nø 104 :  d-2tw<0
c     -----------------------------------------------------------
      else if(ic.eq.104) then
        vnom='d-2tw<0    cadres'
        cj=delta-2.*dya
        cjm=0.
        if (dya.ne.0.) ratio=delta/(2.*dya)

c      calcul de dcj = dérivées = sensibilités
        l=0
        do 104 ik=1,nbrxi
        if     (nxi(ik).eq.1)  then
          dcj(ik)=1.
          l=l+1
        else if(nxi(ik).eq.3)  then
          dcj(ik)=-2.
          l=l+1
        endif
  104 continue

      call test(l,ic)                                   !r&d15

c     restrictions nø 105 : 3d-dw<0
c     -----------------------------------------------------------
      else if(ic.eq.105) then
          vnom='3d-dw<0    cadres'
        cj=3.*delta-hya
        cjm=0.
        if (hya.ne.0.) ratio=3.*delta/hya

c      calcul de dcj = dérivées = sensibilités
        l=0
        do 105 ik=1,nbrxi
        if     (nxi(ik).eq.1)  then
          dcj(ik)=3.
          l=l+1
        else if(nxi(ik).eq.2)  then
          dcj(ik)=-1.
          l=l+1
        endif
  105 continue

      call test(l,ic)                                   !r&d15

c     restrictions nø 109 : 0.625*df-dw<0
c     -----------------------------------------------------------
      else if(ic.eq.109) then
          vnom='0.6df-dw<0 cadres'
        cj=0.625*wya-hya
        cjm=0.
        if (hya.ne.0.) ratio=0.625*wya/hya

c      calcul de dcj = dérivées = sensibilités
        l=0
        do 109 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=-1.
          l=l+1
        else if(nxi(ik).eq.4)  then
          dcj(ik)=0.625
          l=l+1
        endif
  109 continue

      call test(l,ic)                                   !r&d15


c     restrictions nø 110 : dw-2.5*df   <0
c     -----------------------------------------------------------
      else if(ic.eq.110) then
          vnom='dw-2.5df<0 cadres'
        cj=hya-2.5*wya
        cjm=0.
        if (wya.ne.0.) ratio=hya/(2.5*wya)

c      calcul de dcj = dérivées = sensibilités
        l=0
        do 110 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=1.
          l=l+1
        else if(nxi(ik).eq.4)  then
          dcj(ik)=-2.5
          l=l+1
        endif
  110 continue

      call test(l,ic)                                   !r&d15


c     restrictions nø 111 : dw-120*tw <0
c     -----------------------------------------------------------
      else if(ic.eq.111) then
          vnom='dw-120tw<0 cadres'
        cj=hya-120.*dya
        cjm=0.
        if (dya.ne.0.) ratio=hya/(120.*dya)

c      calcul de dcj = dérivées = sensibilités
        l=0
        do 111 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=1.
          l=l+1
        else if(nxi(ik).eq.3)  then
          dcj(ik)=-120.
          l=l+1
        endif
  111 continue

      call test(l,ic)                                   !r&d15


c     restrictions nø 112 : tw-2*delta<0
c     -----------------------------------------------------------
      else if(ic.eq.112) then
          vnom='tw-2delt<0 cadres'
        cj=dya-2.*delta
        cjm=0.
        if (delta.ne.0.) ratio=dya/(2.*delta)

c      calcul de dcj = dérivées = sensibilités
        l=0
        do 112 ik=1,nbrxi
        if     (nxi(ik).eq.1)  then
          dcj(ik)=-2.
          l=l+1
        else if(nxi(ik).eq.3)  then
          dcj(ik)=1.
          l=l+1
        endif
  112 continue

      call test(l,ic)                                   !r&d15


c     restrictions nø 113 : dw-36*tw  <0
c     -----------------------------------------------------------
      else if(ic.eq.113) then
          vnom='dw-36*tw<0 cadres'
        cj=hya-36.*dya
        cjm=0.
        if (dya.ne.0.) ratio=hya/(36.*dya)

c      calcul de dcj = dérivées = sensibilités
        l=0
        do 113 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=1.
          l=l+1
        else if(nxi(ik).eq.3)  then
          dcj(ik)=-36.
          l=l+1
        endif
  113 continue

      call test(l,ic)                                   !r&d15


c     restrictions nø 114 : dw-40*tw  <0
c     -----------------------------------------------------------
      else if(ic.eq.114) then
          vnom='dw-40*tw<0 cadres'
        cj=hya-40.*dya
        cjm=0.
        if (dya.ne.0.) ratio=hya/(40.*dya)

c      calcul de dcj = dérivées = sensibilités
        l=0
        do 114 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=1.
          l=l+1
        else if(nxi(ik).eq.3)  then
          dcj(ik)=-40.
          l=l+1
        endif
  114 continue

      call test(l,ic)                                   !r&d15


c     restrictions nø 115 : 1.25*df-dw<0
c     -----------------------------------------------------------
      else if(ic.eq.115) then
          vnom='1.2df-dw<0 cadres'
        cj=1.25*wya-hya
        cjm=0.
        if (hya.ne.0.) ratio=(1.25*wya)/hya

c      calcul de dcj = dérivées = sensibilités
        l=0
        do 115 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=-1.
          l=l+1
        else if(nxi(ik).eq.4)  then
          dcj(ik)=1.25
          l=l+1
        endif
  115 continue

      call test(l,ic)                                   !r&d15


c     restrictions nø 116 : dw-5*df   <0
c     -----------------------------------------------------------
      else if(ic.eq.116) then
          vnom='dw-5*df <0 cadres'
        cj=hya-5.*wya
        cjm=0.
        if (wya.ne.0.) ratio=hya/(5.*wya)

c      calcul de dcj = dérivées = sensibilités
        l=0
        do 116 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=1.
          l=l+1
        else if(nxi(ik).eq.4)  then
          dcj(ik)=-5.
          l=l+1
        endif
  116 continue

      call test(l,ic)                                   !r&d15

c     restrictions nø 120 : -df + 0,2.dw   < 0
c     -----------------------------------------------------------
      else if(ic.eq.120) then								!sept06
          vnom='df/dw>0.2  cadres'								!sept06
        cj=-wya + 0.2*hya									!sept06
        cjm=0.											!sept06
        if (hya.ne.0.) ratio=wya/(0.2*hya)

c      calcul de dcj = dérivées = sensibilités			!sept06
        l=0												!sept06
	  do 120 ik=1,nbrxi									!sept06
        if     (nxi(ik).eq.2)  then						!sept06
          !dcj(ik)=wya/(hya**2)							!sept06
		dcj(ik)=0.2										!sept06
          l=l+1											!sept06
        else if(nxi(ik).eq.4)  then						!sept06
          dcj(ik)=-1.										!sept06
          l=l+1											!sept06
        endif												!sept06
  120 continue											!sept06

      call test(l,ic)										!r&d15	!sept06

c     restrictions nø 121 : df/dw   <0,5
c     -----------------------------------------------------------
      else if(ic.eq.121) then								!sept06
          vnom='df/dw<0.5  cadres'								!sept06
        cj=wya - 0.5*hya									!sept06
        cjm=0.											!sept06
        if (hya.ne.0.) ratio=wya/(0.5*hya)

c      calcul de dcj = dérivées = sensibilités			!sept06
        l=0
	  do 121 ik=1,nbrxi									!sept06
        if     (nxi(ik).eq.2)  then						!sept06
          dcj(ik)=-0.5									!sept06
          l=l+1											!sept06
        else if(nxi(ik).eq.4)  then						!sept06
          dcj(ik)=1.										!sept06
          l=l+1											!sept06
        endif												!sept06
  121 continue											!sept06

      call test(l,ic)										!r&d15	!sept06

c     restrictions nø 122 : -dw/tw   <-28					!sept06
c     -----------------------------------------------------------
      else if(ic.eq.122) then								!sept06
          vnom='dw/tw>28   cadres'								!sept06
        cj=-hya + 28.*dya									!sept06
        cjm=0.									!sept06
        if (hya.ne.0.) ratio=28.*dya/hya

c      calcul de dcj = dérivées = sensibilités
        l=0
	  do 122 ik=1,nbrxi									!sept06
        if     (nxi(ik).eq.2)  then
          dcj(ik)=-1.
          l=l+1
        else if(nxi(ik).eq.3)  then
          dcj(ik)=28.
          l=l+1
        endif
  122 continue

      call test(l,ic)                                   !r&d15

c     restrictions nø 123 : dw/tw   <90
c     -----------------------------------------------------------
      else if(ic.eq.123) then
          vnom='dw/tw<90   cadres'
        cj=hya - 90.*dya
        cjm=0.
        if (hya.ne.0.) ratio=90.*hya/dya

c      calcul de dcj = dérivées = sensibilités
        l=0
	  do 123 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=1.
          l=l+1
        else if(nxi(ik).eq.3)  then
          dcj(ik)=-90.
          l=l+1
        endif
  123 continue

      call test(l,ic)                                   !r&d15

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


c     restrictions nø 150 : tw/tf   <1
c     -----------------------------------------------------------
!      else if(ic.eq.150) then
!        vnom='tw/tf<1   cadres'
!        if (tya.ne.0.) ratio=dya/tya

!        do 150 i=1,nbrxi  !on vérifie si l'on a bien sélectionné 
		                  !comme variable de conception wya
!           if ((ityp1.ne.5).and.(nxi(i).eq.4))then   
!              nn=i+ntot
!              call dcns_geom_n50(hya,dya,wya,tya,ximin(nn),ximax(nn),              
!     *                   inda,1,iprint,nel,ityp1,isect1,
!     *				   impr2)    		
!              xicou(nn)=wya
!            endif
!  150   continue



c     restrictions nø 151 : tf/(2*tw)   <1
c     -----------------------------------------------------------
!      else if(ic.eq.151) then
!        vnom='tf/(2*tw)<1   cadres'
!        if (dya.ne.0.) ratio=tya/(2.*dya)

!        do 151 i=1,nbrxi  !on vérifie si l'on a bien sélectionné 
		                  !comme variable de conception wya
!           if ((ityp1.ne.5).and.(nxi(i).eq.4))then   
!              nn=i+ntot
!              call dcns_geom_n51(hya,dya,wya,tya,ximin(nn),ximax(nn),              
!     *                   inda,1,iprint,nel,ityp1,isect1,
!     *				   impr2)    		
!              xicou(nn)=wya
!            endif
!  151   continue


c     restrictions nø 152 : 8*tf/df   <1
c     -----------------------------------------------------------
!      else if(ic.eq.152) then
!        vnom='8*tf/df<1   cadres'
!        if (wya.ne.0.) ratio=8*tya/wya

!        do 152 i=1,nbrxi  !on vérifie si l'on a bien sélectionné 
		                  !comme variable de conception wya
!           if ((ityp1.ne.5).and.(nxi(i).eq.4))then   
!              nn=i+ntot
!              call dcns_geom_n52(hya,dya,wya,tya,ximin(nn),ximax(nn),              
!     *                   inda,1,iprint,nel,ityp1,isect1,
!     *				   impr2)    		
!              xicou(nn)=wya
!            endif
!  152   continue


c     restrictions nø 153 : df/(16*tf) <1
c     -----------------------------------------------------------
!      else if(ic.eq.153) then
!        vnom='df/(16*tf)<1   cadres'
!        if (tya.ne.0.) ratio=wya/(16*tya)

!        do 153 i=1,nbrxi  !on vérifie si l'on a bien sélectionné 
		                  !comme variable de conception wya
!           if ((ityp1.ne.5).and.(nxi(i).eq.4))then   
!              nn=i+ntot
!              call dcns_geom_n53(hya,dya,wya,tya,ximin(nn),ximax(nn),              
!     *                   inda,1,iprint,nel,ityp1,isect1,
!     *				   impr2)    		
!              xicou(nn)=wya
!            endif
!  153   continue


c ****************************************************************
c 3.    restrictions geometriques des raidisseurs
c ****************************************************************
c     restrictions nø 201 :  df-dw<0
c     -----------------------------------------------------------
      else if(ic.eq.201) then
          vnom='df-dw<0    raidisseurs'
        cj=wxr-hxr
        cjm=0.
        if (hxr.ne.0.) ratio=wxr/hxr

c      calcul de dcj = dérivées = sensibilités
        l=0
        do 201 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=-1.
          l=l+1
        else if(nxi(ik).eq.8)  then
          dcj(ik)=+1.
          l=l+1
        endif
  201 continue

      call test(l,ic)                                   !r&d15


c     restrictions nø 202 :  dw-2*df<0
c     -----------------------------------------------------------
      else if(ic.eq.202) then
          vnom='dw-2*df<0  raidisseurs'
        cj=hxr-2.*wxr
        cjm=0.
        if (wxr.ne.0.) ratio=hxr/(2.*wxr)

c      calcul de dcj = dérivées = sensibilités
        l=0
        do 202 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=1.
          l=l+1
        else if(nxi(ik).eq.8)  then
          dcj(ik)=-2.
          l=l+1
        endif
  202 continue

      call test(l,ic)                                   !r&d15

c     restrictions nø 203 :  dw-40*tw<0
c     -----------------------------------------------------------
      else if(ic.eq.203) then
          vnom='dw-40*tw<0 raidisseurs'
        cj=hxr-40.*dxr
        cjm=0.
        if (dxr.ne.0.) ratio=hxr/(40.*dxr)

c      calcul de dcj = dérivées = sensibilités
        l=0
        do 203 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=1.
          l=l+1
        else if(nxi(ik).eq.7)  then
          dcj(ik)=-40.
          l=l+1
        endif
  203 continue

      call test(l,ic)                                   !r&d15

c     restrictions nø 204 :  d-2tw<0
c     -----------------------------------------------------------
      else if(ic.eq.204) then
          vnom='d-2tw<0    raidisseurs'
        cj=delta-2.*dxr
        cjm=0.
        if (dxr.ne.0.) ratio=delta/(2.*dxr)

c      calcul de dcj = dérivées = sensibilités
        l=0
        do 204 ik=1,nbrxi
        if     (nxi(ik).eq.1)  then
          dcj(ik)=1.
          l=l+1
        else if(nxi(ik).eq.7)  then
          dcj(ik)=-2.
          l=l+1
        endif
  204 continue

      call test(l,ic)                                   !r&d15

c     restrictions nø 205 : 3d-dw<0
c     -----------------------------------------------------------
      else if(ic.eq.205) then
          vnom='3d-dw<0    raidisseurs'
        cj=3.*delta-hxr
        cjm=0.
        if (hxr.ne.0.) ratio=3.*delta/hxr

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 205 ik=1,nbrxi
        if     (nxi(ik).eq.1)  then
          dcj(ik)=3.
          l=l+1
        else if(nxi(ik).eq.6)  then
          dcj(ik)=-1.
          l=l+1
        endif
  205 continue

      call test(l,ic)                                   !r&d15


c     restrictions nø 206 : d-f(epsr)<0 (!!!passenger ship!!!)						!r&d15
c     -----------------------------------------------------------						!r&d15
      else if(ic.eq.206) then															!r&d15
	  vnom='f(epsr)-d<0'															!r&d15
	  if(code.eq.1) then		!keel												!r&d15
	    deltamin=(3.8+0.040*length*coefk1**0.5+4.5*epsr)*0.001						!r&d15
	  elseif((code.eq.2).or.(code.eq.4)) then	!bottom & bilge						!r&d15
		if(hxr.gt.0.010) then	!longitudinal framing								!r&d15
	      deltamin=(1.9+0.032*length*coefk1**0.5+4.5*epsr)*0.001					!r&d15
		else					!transverse framing									!r&d15
	      deltamin=(2.8+0.032*length*coefk1**0.5+4.5*epsr)*0.001					!r&d15
		endif																		!r&d15
	  elseif(code.eq.3) then	!inner bottom - outside engine room					!r&d15
	    deltamin=(2.0+0.020*length*coefk1**0.5+4.5*epsr)*0.001						!r&d15
	  elseif(code.eq.5) then	!side - below freeboard deck						!r&d15
	    deltamin=(2.1+0.028*length*coefk1**0.5+4.5*epsr)*0.001						!r&d15
	  elseif(code.eq.6) then	!side - between freeboard & strength decks			!r&d15
	    deltamin=(2.1+0.013*length*coefk1**0.5+4.5*epsr)*0.001						!r&d15
	  elseif(code.eq.7) then	!inner side											!r&d15
		if(length.lt.120.) then														!r&d15
	      deltamin=(1.7+0.013*length*coefk1**0.5+4.5*epsr)*0.001					!r&d15
		else																		!r&d15
	      deltamin=(3.6+2.2*coefk1**0.5+epsr)*0.001									!r&d15
		endif																		!r&d15
	  elseif(code.eq.8) then	!weather strength deck								!r&d15
		deltamin=(2.1+2.2*coefk1**0.5+epsr)*0.001									!r&d15
	  elseif(code.eq.9) then	!accomodation deck									!r&d15
	    if(length.lt.120.) then														!r&d15
	      deltamin=(1.3+0.004*length*coefk1**0.5+4.5*epsr)*0.001					!r&d15
		else																		!r&d15
	      deltamin=(2.1+2.2*coefk1**0.5+epsr)*0.001									!r&d15
		endif																		!r&d15
	  elseif(code.eq.10) then	!bulkhead											!r&d15
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
            if(((code.eq.7).or.(code.eq.9).or.(code.eq.10)).							!r&d15
     *	  and.(length.ge.120.)) then												!r&d15
		    dcj(ik)=0.001															!r&d15
		  elseif(code.eq.8) then													!r&d15
	        dcj(ik)=0.001															!r&d15
	      else																		!r&d15
	        dcj(ik)=0.0045															!r&d15
		  endif																		!r&d15
            l=l+1																		!r&d15
          endif																		!r&d15
  206 continue																		!r&d15

      call test(l,ic)                                   								!r&d15


c     restrictions nø 209 : 0.625*df-dw<0
c     -----------------------------------------------------------
      else if(ic.eq.209) then
          vnom='0.6df-dw<0 raidisseurs'
        cj=0.625*wxr-hxr
        cjm=0.
        if (hxr.ne.0.) ratio=0.625*wxr/hxr

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 209 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=-1.
          l=l+1
        else if(nxi(ik).eq.8)  then
          dcj(ik)=0.625
          l=l+1
        endif
  209 continue

      call test(l,ic)                                   !r&d15


c     restrictions nø 210 : dw-2.5*df   <0
c     -----------------------------------------------------------
      else if(ic.eq.210) then
          vnom='dw-2.5df<0 raidisseurs'
        cj=hxr-2.5*wxr
        cjm=0.
        if (wxr.ne.0.) ratio=hxr/(2.5*wxr)

c      calcul de dcj = dérivées = sensibilités
        l=0
        do 210 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=1.
          l=l+1
        else if(nxi(ik).eq.8)  then
          dcj(ik)=-2.5
          l=l+1
        endif
  210 continue

      call test(l,ic)                                   !r&d15


c     restrictions nø 211 : dw-36*tw <0
c     -----------------------------------------------------------
      else if(ic.eq.211) then
          vnom='dw-36*tw<0 raidisseurs'
        cj=hxr-36.*dxr
        cjm=0.
        if (dxr.ne.0.) ratio=hxr/(36.*dxr)

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 211 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=1.
          l=l+1
        else if(nxi(ik).eq.7)  then
          dcj(ik)=-36.
          l=l+1
        endif
  211 continue

      call test(l,ic)                                   !r&d15


c     restrictions nø 212 : tw-2*delta<0
c     ------------------------------------
      else if(ic.eq.212) then
          vnom='tw-2delt<0 raidisseurs'
        cj=dxr-2.*delta
        cjm=0.
        if (delta.ne.0.) ratio=dxr/(2.*delta)

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 212 ik=1,nbrxi
        if     (nxi(ik).eq.1)  then
          dcj(ik)=-2.
          l=l+1
        else if(nxi(ik).eq.7)  then
          dcj(ik)=1.
          l=l+1
        endif
  212 continue

      call test(l,ic)                                   !r&d15


c     restrictions nø 213 : dw-120*tw  <0
c     -------------------------------------
      else if(ic.eq.213) then
          vnom='dw-120tw<0 raidisseurs'
        cj=hxr-120.*dxr
        cjm=0.
        if (dxr.ne.0.) ratio=hxr/(120.*dxr)

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 213 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=1.
          l=l+1
        else if(nxi(ik).eq.7)  then
          dcj(ik)=-120.
          l=l+1
        endif
  213 continue

      call test(l,ic)                                   !r&d15


c     restrictions nø 214 : 0.0065+dw/170-tw  <0
c     --------------------------------------------
      else if(ic.eq.214) then
          vnom='dw/a-tw<b  raidisseurs'
        cj=0.0065+hxr/170.-dxr
        cjm=0.
        if (dxr.ne.0.) ratio=(0.0065+hxr/170.)/dxr

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 214 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=1/170.
          l=l+1
        else if(nxi(ik).eq.7)  then
          dcj(ik)=-1.
          l=l+1
        endif
  214 continue

      call test(l,ic)                                   !r&d15


c     restrictions nø 215 : 1.25*df-dw<0
c     -----------------------------------------------------------
      else if(ic.eq.215) then
          vnom='1.2df-dw<0 raidisseurs'
        cj=1.25*wxr-hxr
        cjm=0.
        if (hxr.ne.0.) ratio=1.25*wxr/hxr

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 215 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=-1.
          l=l+1
        else if(nxi(ik).eq.8)  then
          dcj(ik)=1.25
          l=l+1
        endif
  215 continue

      call test(l,ic)                                   !r&d15


c     restrictions nø 216 : dw-5*df   <0
c     -----------------------------------------------------------
      else if(ic.eq.216) then
          vnom='dw-5*df<0  raidisseurs'
        cj=hxr-5.*wxr
        cjm=0.
        if (wxr.ne.0.) ratio=hxr/(5.*wxr)

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 216 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=1.
          l=l+1
        else if(nxi(ik).eq.8)  then
          dcj(ik)=-5.
          l=l+1
        endif
  216 continue

      call test(l,ic)                                   !r&d15

c     restrictions nø 220 : -dw/tw   <-8
c     -----------------------------------------------------------
      else if(ic.eq.220) then
          vnom='dw/tw>8    raidisseurs'
        cj=-hxr + 8.*dxr
        cjm=0.
        if (hxr.ne.0.) ratio=8.*dxr/hxr

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 220 ik=1,nbrxi
        if     (nxi(ik).eq.7)  then
          dcj(ik)=8.
          l=l+1
        else if(nxi(ik).eq.6)  then
          dcj(ik)=-1.
          l=l+1
        endif
  220 continue

      call test(l,ic)                                   !r&d15

c     restrictions nø 221 : dw/tw   <50
c     -----------------------------------------------------------
      else if(ic.eq.221) then
          vnom='dw/tw<50   raidisseurs'
        cj=hxr - 50.*dxr
        cjm=0.
        if (dxr.ne.0.) ratio=hxr/(50.*dxr)

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 221 ik=1,nbrxi
        if     (nxi(ik).eq.7)  then
          dcj(ik)=-50.
          l=l+1
        else if(nxi(ik).eq.6)  then
          dcj(ik)=1.
          l=l+1
        endif
  221 continue

      call test(l,ic)                                   !r&d15

c     restrictions nø 230 : -df/dw   <-0,2
c     -----------------------------------------------------------
      else if(ic.eq.230) then
          vnom='df/dw>0.2  raidisseurs'
        cj=-wxr + 0.2*hxr
        cjm=0.
        if (wxr.ne.0.) ratio=0.2*hxr/wxr

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 230 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=0.2
          l=l+1
        else if(nxi(ik).eq.8)  then
          dcj(ik)=-1.
          l=l+1
        endif
  230 continue

      call test(l,ic)                                   !r&d15

c     restrictions nø 231 : df/dw   <0,5
c     -----------------------------------------------------------
      else if(ic.eq.231) then
          vnom='df/dw<0.5  raidisseurs'
        cj=wxr - 0.5*hxr
        cjm=0.
        if (hxr.ne.0.) ratio=wxr/(0.5*hxr)

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 231 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=-0.5
          l=l+1
        else if(nxi(ik).eq.8)  then
          dcj(ik)=1.
          l=l+1
        endif
  231 continue

      call test(l,ic)                                   !r&d15

c     restrictions nø 232 : -dw/tw   <-28
c     -----------------------------------------------------------
      else if(ic.eq.232) then
          vnom='dw/tw>28   raidisseurs'
        cj=-hxr + 28.*dxr
        cjm=0.
        if (hxr.ne.0.) ratio=28.*dxr/hxr

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 232 ik=1,nbrxi
        if     (nxi(ik).eq.7)  then
          dcj(ik)=28.
          l=l+1
        else if(nxi(ik).eq.6)  then
          dcj(ik)=-1.
          l=l+1
        endif
  232 continue

      call test(l,ic)                                   !r&d15

c     restrictions nø 233 : dw/tw   <90
c     -----------------------------------------------------------
      else if(ic.eq.233) then
          vnom='dw/tw<90   raidisseurs'
        cj=hxr - 90.*dxr
        cjm=0.
        if (dxr.ne.0.) ratio=hxr/90.*dxr

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 233 ik=1,nbrxi
        if     (nxi(ik).eq.7)  then
          dcj(ik)=-90.
          l=l+1
        else if(nxi(ik).eq.6)  then
          dcj(ik)=1.
          l=l+1
        endif
  233 continue

      call test(l,ic)                                   !r&d15

c     restrictions nø 234 : -tw   <-8
c     -----------------------------------------------------------
      else if(ic.eq.234) then
          vnom='tw>8       raidisseurs'
        cj=-dxr
        cjm=-0.008 !unité
        if (dxr.ne.0.) ratio=0.008/dxr

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 234 ik=1,nbrxi
        if     (nxi(ik).eq.7)  then
          dcj(ik)=-1.
          l=l+1
	  endif
  234 continue

      if (l.eq.0) then
	  write(*,1)
	  write(6970,1)
	endif

c     restrictions nø 235 : tw   <20
c     -----------------------------------------------------------
      else if(ic.eq.235) then
          vnom='tw<20      raidisseurs'
        cj=dxr
        cjm=0.020  !unité
        ratio=dxr/0.020

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 235 ik=1,nbrxi
        if     (nxi(ik).eq.7)  then
          dcj(ik)=1.
          l=l+1
	  endif
  235 continue

      if (l.eq.0) then
	  write(*,1)
	  write(6970,1)
	endif

c     restrictions nø 236 : -df/dw   <-0,1
c     -----------------------------------------------------------
      else if(ic.eq.236) then
          vnom='df/dw>0.1  raidisseurs'
        cj=-wxr + 0.1*hxr
        cjm=0.
        if (wxr.ne.0.) ratio=0.1*hxr/wxr

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 236 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=0.1
          l=l+1
        else if(nxi(ik).eq.8)  then
          dcj(ik)=-1.
          l=l+1
        endif
  236 continue

      call test(l,ic)                                   !r&d15

c     restrictions nø 237 : dw/tw   <130
c     -----------------------------------------------------------
      else if(ic.eq.237) then
          vnom='dw/tw<130  raidisseurs'
        cj=hxr - 130.*dxr
        cjm=0.
        if (dxr.ne.0.) ratio=hxr/(130.*dxr)

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 237 ik=1,nbrxi
        if     (nxi(ik).eq.7)  then
          dcj(ik)=-130.
          l=l+1
        else if(nxi(ik).eq.6)  then
          dcj(ik)=1.
          l=l+1
        endif
  237 continue

      call test(l,ic)                                   !r&d15

c     restrictions nø 240 : -df/dw   <-0,1 (lq on simule traverse sur axe sym)
c     -----------------------------------------------------------
      else if(ic.eq.240) then
          vnom='2df/dw>0.2 raidisseurs'
        cj=-wxr + 0.1*hxr
        cjm=0.
        if (wxr.ne.0.) ratio=0.1*hxr/wxr

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 240 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=0.1
          l=l+1
        else if(nxi(ik).eq.8)  then
          dcj(ik)=-1.
          l=l+1
        endif
  240 continue

      call test(l,ic)                                   !r&d15

c     restrictions nø 241 : df/dw   <0,25 (lq on simule traverse sur axe sym)
c     -----------------------------------------------------------
      else if(ic.eq.241) then
          vnom='2df/dw<0.5 raidisseurs'
        cj=wxr - 0.25*hxr
        cjm=0.
        if (hxr.ne.0.) ratio=wxr/(0.25*hxr)

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 241 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=-0.25
          l=l+1
        else if(nxi(ik).eq.8)  then
          dcj(ik)=1.
          l=l+1
        endif
  241 continue

      call test(l,ic)                                   !r&d15

c     restrictions nø 242 : -dw/tw   <-56 (lq on simule traverse sur axe sym)
c     -----------------------------------------------------------
      else if(ic.eq.242) then
          vnom='dw/2.tw>28 raidisseurs'
        cj=-hxr + 56.*dxr
        cjm=0.
        if (hxr.ne.0.) ratio=56.*dxr/hxr

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 242 ik=1,nbrxi
        if     (nxi(ik).eq.7)  then
          dcj(ik)=56.
          l=l+1
        else if(nxi(ik).eq.6)  then
          dcj(ik)=-1.
          l=l+1
        endif
  242 continue

      call test(l,ic)                                   !r&d15

c     restrictions nø 243 : dw/tw   <180 (lq on simule traverse sur axe sym)
c     -----------------------------------------------------------
      else if(ic.eq.243) then
          vnom='dw/2.tw<90 raidisseurs'
        cj=hxr - 180.*dxr
        cjm=0.
        if (dxr.ne.0.) ratio=hxr/(180.*dxr)

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 243 ik=1,nbrxi
        if     (nxi(ik).eq.7)  then
          dcj(ik)=-180.
          l=l+1
        else if(nxi(ik).eq.6)  then
          dcj(ik)=1.
          l=l+1
        endif
  243 continue

      call test(l,ic)                                   !r&d15

c     restrictions nø 244 : -tw   <-4 (lq on simule traverse sur axe sym)
c     -----------------------------------------------------------
      else if(ic.eq.244) then
          vnom='2.tw>8     raidisseurs'
        cj=-dxr
        cjm=-0.004 !unité
        if (dxr.ne.0.) ratio=0.004/dxr

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 244 ik=1,nbrxi
        if     (nxi(ik).eq.7)  then
          dcj(ik)=-1.
          l=l+1
	  endif
  244 continue

      if (l.eq.0) then
	  write(*,1)
	  write(6970,1)
	endif

c     restrictions nø 245 : tw   <10 (lq on simule traverse sur axe sym)
c     -----------------------------------------------------------
      else if(ic.eq.245) then
          vnom='2.tw<20    raidisseurs'
        cj=dxr
        cjm=0.010  !unité
        ratio=dxr/0.010

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 245 ik=1,nbrxi
        if     (nxi(ik).eq.7)  then
          dcj(ik)=1.
          l=l+1
	  endif
  245 continue

      if (l.eq.0) then
	  write(*,1)
	  write(6970,1)
	endif

c     restrictions nø 246 : -df/dw   <-0,05 (lq on simule traverse sur axe sym)
c     -----------------------------------------------------------
      else if(ic.eq.246) then
          vnom='2df/dw>0.1 raidisseurs'
        cj=-wxr + 0.05*hxr
        cjm=0.
        if (wxr.ne.0.) ratio=0.05*hxr/wxr

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 246 ik=1,nbrxi
        if     (nxi(ik).eq.6)  then
          dcj(ik)=0.05
          l=l+1
        else if(nxi(ik).eq.8)  then
          dcj(ik)=-1.
          l=l+1
        endif
  246 continue

      call test(l,ic)                                   !r&d15

c     restrictions nø 247 : dw/tw   <260 (lq on simule traverse sur axe sym)
c     -----------------------------------------------------------
      else if(ic.eq.247) then
          vnom='dw/2tw<130 raidisseurs'
        cj=hxr - 260.*dxr
        cjm=0.
        if (dxr.ne.0.) ratio=hxr/(260.*dxr)

c     calcul de dcj = dérivées = sensibilités
        l=0
        do 247 ik=1,nbrxi
        if     (nxi(ik).eq.7)  then
          dcj(ik)=-260.
          l=l+1
        else if(nxi(ik).eq.6)  then
          dcj(ik)=1.
          l=l+1
        endif
  247 continue

      call test(l,ic)                                   !r&d15


c     restrictions nø 250 : tw/tf   <1
c     -----------------------------------------------------------
!      else if(ic.eq.250) then
!          vnom='tw/tf<1   raidisseurs'
!        if (txr.ne.0.) ratio=dxr/txr



!        do 250 i=1,nbrxi  !on vérifie si l'on a bien sélectionné 
		                  !comme variable de conception wxr
!           if ((ityp1.ne.5).and.(nxi(i).eq.8))then   
!              nn=i+ntot
!              call dcns_geom_n50(hxr,dxr,wxr,txr,ximin(nn),ximax(nn),             
!     *                   indr,2,iprint,nel,ityp1,isect1,
!     *				   impr2)    		
!              xicou(nn)=wxr
!            endif
!  250   continue



c     restrictions nø 251 : tf/(2*tw)   <1
c     -----------------------------------------------------------
!      else if(ic.eq.251) then
!        vnom='tf/(2*tw)<1   raidisseurs'
!        if (dxr.ne.0.) ratio=txr/(2.*dxr)

!        do 251 i=1,nbrxi  !on vérifie si l'on a bien sélectionné 
		                  !comme variable de conception wxr
!           if ((ityp1.ne.5).and.(nxi(i).eq.8))then   
!              nn=i+ntot
!              call dcns_geom_n51(hxr,dxr,wxr,txr,ximin(nn),ximax(nn),              
!     *                   indr,2,iprint,nel,ityp1,isect1,
!     *				   impr2)    		
!              xicou(nn)=wxr
!            endif
!  251   continue


c     restrictions nø 252 : 8*tf/df   <1
c     -----------------------------------------------------------
!      else if(ic.eq.252) then
!        vnom='8*tf/df<1   raidisseurs'
!        if (wxr.ne.0.) ratio=8*txr/wxr

!        do 252 i=1,nbrxi  !on vérifie si l'on a bien sélectionné 
		                  !comme variable de conception wxr
!           if ((ityp1.ne.5).and.(nxi(i).eq.8))then   
!              nn=i+ntot
!              call dcns_geom_n52(hxr,dxr,wxr,txr,ximin(nn),ximax(nn),              
!     *                   indr,2,iprint,nel,ityp1,isect1,
!     *				   impr2)    		
!              xicou(nn)=wxr
!            endif
!  252   continue


c     restrictions nø 253 : df/(16*tf) <1
c     -----------------------------------------------------------
!      else if(ic.eq.253) then
!        vnom='df/(16*tf)<1   raidisseurs'
!        if (txr.ne.0.) ratio=wxr/(16*txr)

!        do 253 i=1,nbrxi  !on vérifie si l'on a bien sélectionné 
		                  !comme variable de conception wxr
!           if ((ityp1.ne.5).and.(nxi(i).eq.8))then   
!              nn=i+ntot
!              call dcns_geom_n53(hxr,dxr,wxr,txr,ximin(nn),ximax(nn),              
!     *                   indr,2,iprint,nel,ityp1,isect1,
!     *				   impr2)    		
!              xicou(nn)=wxr
!            endif
!  253   continue

c ****************************************************************
c 4.    restrictions croisees entre cadres et raidisseurs
c ****************************************************************

c     restrictions nø 301 : dw(r)-dw(a)<0
c     -----------------------------------------------------------
      else if(ic.eq.301) then
          vnom='dw(r) <dw(a)'
        cj=hxr-hya
        cjm=0.
        if (hya.ne.0.) ratio=hxr/hya

c      calcul de dcj = dérivées = sensibilités
        l=0
        do 301 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=-1.
          l=l+1
        else if(nxi(ik).eq.6)  then
          dcj(ik)=1.
          l=l+1
        endif
  301 continue

      call test(l,ic)                                   !r&d15

c     restrictions nø 302 : tw(r)-4*tw(a)<0
c     -----------------------------------------------------------
      else if(ic.eq.302) then
          vnom='tw(r)<4tw(a)'
        cj=dxr-4.*dya
        cjm=0.
        if (dya.ne.0.) ratio=dxr/(4.*dya)

c      calcul de dcj = dérivées = sensibilités
        l=0
        do 302 ik=1,nbrxi
        if     (nxi(ik).eq.3)  then
          dcj(ik)=1.
          l=l+1
        else if(nxi(ik).eq.7)  then
          dcj(ik)=-1.
          l=l+1
        endif
  302 continue

      call test(l,ic)                                   !r&d15

c     restrictions nø 303 : tw(a)-4*tw(r)<0
c     -----------------------------------------------------------
      else if(ic.eq.303) then
          vnom='tw(a)<4tw(r)'
        cj=dya-4.*dxr
        cjm=0.
        if (dxr.ne.0.) ratio=dya/(4.*dxr)

c      calcul de dcj = dérivées = sensibilités
        l=0
        do 303 ik=1,nbrxi
        if     (nxi(ik).eq.3)  then
          dcj(ik)=1.
          l=l+1
        else if(nxi(ik).eq.7)  then
          dcj(ik)=-1.
          l=l+1
        endif
  303 continue

      call test(l,ic)                                   !r&d15


c     restrictions nø 304 : dw(a)-dw(r)<0
c     -----------------------------------------------------------
      else if(ic.eq.304) then
          vnom='dw(a) <dw(r)'
        cj=hya-hxr
        cjm=0.
        if (hxr.ne.0.) ratio=hya/hxr

c      calcul de dcj = dérivées = sensibilités
        l=0
        do 304 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=1.
          l=l+1
        else if(nxi(ik).eq.6)  then
          dcj(ik)=-1.
          l=l+1
        endif
  304 continue

      call test(l,ic)                                   !r&d15

c     restrictions nø 320 : -dw(a)/dw(r)<-2
c     -----------------------------------------------------------
      else if(ic.eq.320) then
          vnom='dwa/dw(r) >2'
        cj=-hya +2.*hxr
        cjm=0.
        if (hya.ne.0.) ratio=2.*hxr/hya

c      calcul de dcj = dérivées = sensibilités
        l=0
        do 320 ik=1,nbrxi
        if     (nxi(ik).eq.2)  then
          dcj(ik)=-1.
          l=l+1
        else if(nxi(ik).eq.6)  then
          dcj(ik)=2.
          l=l+1
        endif
  320 continue

      call test(l,ic)                                   !r&d15

c ****************************************************************
c 5.    autres restrictions
c ****************************************************************
      else
       write(*,*)  ' nø de la restr. géométrique ',ic,'est incorrecte'
       write(6970,*)
     *  ' nø de la restr. géométrique ',ic,'est incorrecte'
       write(666,*)' nø de la restr. géométrique ',ic,'est incorrecte'
       write(29,*)' nø de la restr. géométrique ',ic,'est incorrecte'		!sept06		!bug
	 pause 'stop'
         stop
      end if

c     sauvetage sur file 303 et impressions sur file 666		!extension neto	!fev2007
c     ---------------------------------------------------

      if((ic.ne.150).and.
     *   (ic.ne.151).and.
     *   (ic.ne.152).and.
     *   (ic.ne.153).and.
     *   (ic.ne.250).and.
     *   (ic.ne.251).and.
     *   (ic.ne.252).and.
     *   (ic.ne.253)) then

        write(303) ntot										!extension neto	!fev2007
        write(303) (dcj(i),i=1,nbrxi),cj,cjm,vnom,ic,ratio    !extension neto	!fev2007 dad

!        write(6969,*) ntot										!extension neto	!fev2007
!        write(6969,*) (dcj(i),i=1,nbrxi),cj,cjm,vnom,ic,ratio    !extension neto	!fev2007 dad
c		do jj=1,nbrxi
c		open(666665, file = 'dcj-geom1276-dyn.txt')
c		write (666665,*) dcj(jj)
c		enddo
c		pause

      
	if((itera.eq.0).and.(impr2.ge.-1)) then
        write(666,427) it,cj,cjm,ic,vnom
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
      subroutine test(l,ic)                                   !r&d15
c     -------------------
c     error si les variables (hauteur, épaisseur, etc.) n'interviennent
c     comme variables de conception.

      implicit real *8 (a-h,o-z)

      if(l.eq.0) then
          write(*,1)
          write(6970,1)
          write(666,1)
		write(29,1)		!sept06
          pause 'stop'
          return
      else if ((l.eq.1).and.(ic.ne.206)) then
          write(*,2)
          write(6970,2)
          write(666,2)
		write(29,1)		!sept06	
      endif

      return
   1  format(' error'/'********'/
     *       ' les variables liées à cette restriction ne sont pas'
     *       ' définies comme variables de conception !!!')
   2  format(' attention'/'**********'/
     *       ' une seule variable liée à cette restriction est'
     *       ' définie comme variable de conception !!!')
      end
c **********************************************************************
