      subroutine system(a,c,n,ns,x,na)                                  psy00010
      implicit real *8(a-h,o-z)                                         psy00020
      dimension d(20),ipos(20),x(360)    !x(ns*n)  juin2006             psy00030
      dimension a(400),c(360)

c***********************************************************************psy00050
c    system est utilisé par linea mais aussi par bateau !!!!!!!!!!!
c***********************************************************************psy00050
c
c     subroutine system (n,ns,x,na) + common a,c
c     subroutine de resolution d'un systeme de n equations a n inconnues
c     par la methode des pivots.

c     soit le systeme [a] x = c
c        a la matrice,  c le vect indep. et x les inconnues (solutions)
c        c peut contenir ns vecteurs indépendants

c     n  = nbr d'inconnues  (taille du système)
c     na = dimension de la matrice [a] que l'on forme hors du vecteur
c          situe dans le common (na=20)
c     ns = nbre de vecteurs independants (ici "ns" max vaut 340/20=17)

c     dimensions nécessaires : a(na,na);x(na*ns);c(na*ns)
c     nb: les vecteurs d et ipos sont des vecteurs de travail.

c     modif: 21-8-94                         création : thèse de ph. rigo
c            avril 99 (lbr5.1)
c***********************************************************************psy00160
      do 10 k=1,n                                                       psy00170
      ipos(k)=0                                                         psy00180
   10 d(k)=a(k)                                                         psy00190
      call pmaxar(d,1,n,umax,ip,na)                                     psy00200
      a(1)=umax                                                         psy00210
      ipos(1)=ip                                                        psy00220
      d(ip)=d(1)                                                        psy00230
      do 15 i=2,n                                                       psy00240
   15 a(i)=d(i)/a(1)                                                    psy00250
      do 100 i=2,n                                                      psy00260
      do 20 j=1,n                                                       psy00270
      ia=j+(i-1)*na                                                     psy00280
   20 d(j)=a(ia)                                                        psy00290
      jaux=i-1                                                          psy00300
      do 50 k=1,jaux                                                    psy00310
      ip=ipos(k)                                                        psy00320
      ia=k+(i-1)*na                                                     psy00330
      a(ia)=d(ip)                                                       psy00340
      d(ip)=d(k)                                                        psy00350
      jauxp=k+1                                                         psy00360
      do 50 kl=jauxp,n                                                  psy00370
      ial=kl+(k-1)*na                                                   psy00380
   50 d(kl)=d(kl)-a(ial)*a(ia)                                          psy00390
      call pmaxar(d,i,n,umax,ip,na)                                        psy00400
      ii=i+(i-1)*na                                                     psy00410
      a(ii)=umax                                                        psy00420
      ipos(i)=ip                                                        psy00430
      d(ip)=d(i)                                                        psy00440
      ix=i+1                                                            psy00450
      if(ix-n) 31,31,100                                                psy00460
   31 do 60 km=ix,n                                                     psy00470
      iam=km+(i-1)*na                                                   psy00480
   60 a(iam)=d(km)/a(ii)                                                psy00490
  100 continue                                                          psy00500
      do 505 ij=1,ns                                                    psy00510
      do 110 i=1,n                                                      psy00520
      ip=i+(ij-1)*na                                                    psy00530
  110 d(i)=c(ip)                                                        psy00540
      nm=n-1                                                            psy00550
      do 200 i=1,nm                                                     psy00560
      ip=ipos(i)                                                        psy00570
      ir=i+(ij-1)*na                                                    psy00580
      c(ir)=d(ip)                                                       psy00590
      d(ip)=d(i)                                                        psy00600
      ijk=i+1                                                           psy00610
      do 200 j=ijk,n                                                    psy00620
      ji=j+(i-1)*na                                                     psy00630
  200 d(j)=d(j)-a(ji)*c(ir)                                             psy00640
      nn=n+(n-1)*na                                                     psy00650
      nx=n+(ij-1)*na                                                    psy00660
      x(nx)=d(n)/a(nn)                                                  psy00670
      naux=n-1                                                          psy00680
      do 500 i=1,naux                                                   psy00690
      iz=n-i                                                            psy00700
      izp=iz+1                                                          psy00710
      izc=iz+(ij-1)*na                                                  psy00720
      d(iz)=c(izc)                                                      psy00730
      do 501 j=izp,n                                                    psy00740
      izj=iz+(j-1)*na                                                   psy00750
      izx=j+(ij-1)*na                                                   psy00760
  501 d(iz)=d(iz)-a(izj)*x(izx)                                         psy00770
      izi=iz+(iz-1)*na                                                  psy00780
      izx=iz+(ij-1)*na                                                  psy00790
  500 x(izx)=d(iz)/a(izi)                                               psy00800
  505 continue                                                          psy00810
   51 return                                                            psy00820
      end                                                               psy00830


      subroutine pmaxar(x,ib,ie,umax,is,na)                             psy00840
      implicit real *8(a-h,o-z)                                         psy00850
      dimension x(na)                                                   psy00860
c***********************************************************************psy00870
c                                                                       psy00880
c     subroutine maxar                                                  psy00890
c     subroutine de recherche d'une valeur maximum en valeur absolue    psy00900
c     dans un groupe de valeurs donnees.                                psy00910
c                                                                       psy00920
c***********************************************************************psy00930
      dumax=x(ib)                                                       psy00940
      is=ib                                                             psy00950

      if(ib-ie) 15,20,20                                                psy00960

   15 ibp=ib+1                                                          psy00970
      do 10 i=ibp,ie                                                    psy00980
        if(dabs(dumax)-dabs(x(i))) 11,10,10                               psy00990
   11   dumax=x(i)                                                        psy01000
        is=i                                                              psy01010
   10 continue                                                          psy01020

   20 umax=dumax                                                        psy01030

      return                                                            psy01040
      end                                                               psy01050
