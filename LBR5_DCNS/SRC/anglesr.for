      subroutine anglesr(nel,phil,nvsmb,aux,zb)
      use sharedvar
      implicit real*8 (a-h,o-z)
      dimension aux(400),zb(360)
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
      ns =nsign(nel)
      an =angle(nel)*pi/180.
      an2=an-phil*pi/180.

      if(dabs(angle(nel)).ne.180.) goto 4
      c=-1.
      s=0.
      c29=-dcos(-phil*pi/180.)
      s2=-dsin(-phil*pi/180.)
      goto 6

   4  if(dabs(angle(nel)).ne.90.) goto 5
      ii=1
      if(angle(nel).eq.-90.) ii=-1
      c=0.
      s=ii
      c29=-dsin(-phil*pi/180.)*ii
      s2=+dcos(-phil*pi/180.)*ii
      goto 6

  5   c =dcos(an)
      s =dsin(an)
      c29=dcos(an2)
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
        aux(lr+201)=aux(lr+ 1)*s-aux(lr+13)*c
        aux(lr+207)=aux(lr+ 7)*s-aux(lr+11)*c
        aux(lr+211)=aux(lr+11)*s+aux(lr+ 7)*c
        aux(lr+213)=aux(lr+13)*s+aux(lr+ 1)*c
        aux(lr+ 1)=aux(lr+201)
        aux(lr+ 7)=aux(lr+207)*ns
        aux(lr+11)=aux(lr+211)*ns
        aux(lr+13)=aux(lr+213)
        aux(lr+ 5)=aux(lr+5)*ns
        aux(lr+ 9)=aux(lr+9)*ns
   10 continue
      l=0
      do 101 i=1,nvsmb
        l=(i-1)*16
        zb(201)=zb( 1+l)*s-zb(13+l)*c
        zb(207)=zb( 7+l)*s-zb(11+l)*c
        zb(211)=zb(11+l)*s+zb( 7+l)*c
        zb(213)=zb(13+l)*s+zb( 1+l)*c
        zb( 1+l)=zb(201)
        zb( 7+l)=zb(207)*ns
        zb(11+l)=zb(211)*ns
        zb(13+l)=zb(213)
        zb( 5+l)=zb(5+l)*ns
        zb( 9+l)=zb(9+l)*ns
  101 continue
      if(kk.eq.2) return
c
c changement de repere des grandeurs relatives a l'arrivee du panneau
c
   30 do 25 i=1,8
        lr=(i-1)*20
        aux(lr+202)=aux(lr+ 2)*s2-aux(lr+14)*c29
        aux(lr+208)=aux(lr+ 8)*s2-aux(lr+12)*c29
        aux(lr+212)=aux(lr+12)*s2+aux(lr+ 8)*c29
        aux(lr+214)=aux(lr+14)*s2+aux(lr+ 2)*c29
        aux(lr+ 2)=aux(lr+202)
        aux(lr+ 8)=aux(lr+208)*ns
        aux(lr+12)=aux(lr+212)*ns
        aux(lr+14)=aux(lr+214)
        aux(lr+ 6)=aux(lr+ 6)*ns
        aux(lr+10)=aux(lr+10)*ns
   25 continue

      l=0
      do 103 i=1,nvsmb
        l=(i-1)*16
        zb(202)=zb(2+l)*s2-zb(14+l)*c29
        zb(208)=zb(8+l)*s2-zb(12+l)*c29
        zb(212)=zb(12+l)*s2+zb(8+l)*c29
        zb(214)=zb(14+l)*s2+zb(2+l)*c29
        zb( 2+l)=zb(202)
        zb( 8+l)=zb(208)*ns
        zb(12+l)=zb(212)*ns
        zb(14+l)=zb(214)
        zb( 6+l)=zb( 6+l)
        zb(10+l)=zb(10+l)
 103  continue

  40  return
      end
