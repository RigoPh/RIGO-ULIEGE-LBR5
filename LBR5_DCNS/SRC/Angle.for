     cdec$ attributes dllexport::angle
      subroutine angle(nel,phil,nvsmb,
     *                 angl,noeud,nno,nsign,neto)
c
c**********************************************************************
c**********************************************************************
c
c angle
c
c la subroutine "angle"  calcule les relations de tranformations
c pour permettre l'assemblage automatique des panneaux.
c
c     modifié : 27-11-95 (module optimisation)      créer: thèse de ph. rigo
c
c***********************************************************************
c**********************************************************************

      implicit real*8 (a-h,o-z)

      common a,b
      common/py/pi
      dimension a(400),b(360)

c     common/ang/angl,noh,noeud,mcomp,nno,nc,nsign
      dimension angl(neto),noeud(neto,2),nno(neto+1,2),nsign(neto)

      ns =nsign(nel)
      an =angl(nel)*pi/180.
      an2=an-phil*pi/180.

      if(dabs(angl(nel)).ne.180.) goto 4
      c=-1.
      s=0.
      c2=-dcos(-phil*pi/180.)
      s2=-dsin(-phil*pi/180.)
      goto 6

   4  if(dabs(angl(nel)).ne.90.) goto 5
      ii=1
      if(angl(nel).eq.-90.) ii=-1
      c=0.
      s=ii
      c2=-dsin(-phil*pi/180.)*ii
      s2=+dcos(-phil*pi/180.)*ii
      goto 6

  5   c =dcos(an)
      s =dsin(an)
      c2=dcos(an2)
      s2=dsin(an2)

  6   kk=0
      nd=noeud(nel,1)
      na=noeud(nel,2)
      iii=0
      if(ns.eq.-1) iii=1
      if(nno(nd,2).ne.0) kk=1+iii
      if(nno(na,2).ne.0) kk=2-iii
      if(kk.eq.1) goto 30
c
c changement de repere des grandeurs relatives au depart du panneau
c
      do 10 i=1,8
        lr=(i-1)*20
        a(lr+201)=a(lr+ 1)*s-a(lr+13)*c
        a(lr+207)=a(lr+ 7)*s-a(lr+11)*c
        a(lr+211)=a(lr+11)*s+a(lr+ 7)*c
        a(lr+213)=a(lr+13)*s+a(lr+ 1)*c
        a(lr+ 1)=a(lr+201)
        a(lr+ 7)=a(lr+207)*ns
        a(lr+11)=a(lr+211)*ns
        a(lr+13)=a(lr+213)
        a(lr+ 5)=a(lr+5)*ns
        a(lr+ 9)=a(lr+9)*ns
   10 continue
      l=0
      do 101 i=1,nvsmb
        l=(i-1)*16
        b(201)=b( 1+l)*s-b(13+l)*c
        b(207)=b( 7+l)*s-b(11+l)*c
        b(211)=b(11+l)*s+b( 7+l)*c
        b(213)=b(13+l)*s+b( 1+l)*c
        b( 1+l)=b(201)
        b( 7+l)=b(207)*ns
        b(11+l)=b(211)*ns
        b(13+l)=b(213)
        b( 5+l)=b(5+l)*ns
        b( 9+l)=b(9+l)*ns
  101 continue
      if(kk.eq.2) return
c
c changement de repere des grandeurs relatives a l'arrivee du panneau
c
   30 do 25 i=1,8
        lr=(i-1)*20
        a(lr+202)=a(lr+ 2)*s2-a(lr+14)*c2
        a(lr+208)=a(lr+ 8)*s2-a(lr+12)*c2
        a(lr+212)=a(lr+12)*s2+a(lr+ 8)*c2
        a(lr+214)=a(lr+14)*s2+a(lr+ 2)*c2
        a(lr+ 2)=a(lr+202)
        a(lr+ 8)=a(lr+208)*ns
        a(lr+12)=a(lr+212)*ns
        a(lr+14)=a(lr+214)
        a(lr+ 6)=a(lr+ 6)*ns
        a(lr+10)=a(lr+10)*ns
   25 continue

      l=0
      do 103 i=1,nvsmb
        l=(i-1)*16
        b(202)=b(2+l)*s2-b(14+l)*c2
        b(208)=b(8+l)*s2-b(12+l)*c2
        b(212)=b(12+l)*s2+b(8+l)*c2
        b(214)=b(14+l)*s2+b(2+l)*c2
        b( 2+l)=b(202)
        b( 8+l)=b(208)*ns
        b(12+l)=b(212)*ns
        b(14+l)=b(214)
        b( 6+l)=b( 6+l)
        b(10+l)=b(10+l)
 103  continue

  40  return
      end
