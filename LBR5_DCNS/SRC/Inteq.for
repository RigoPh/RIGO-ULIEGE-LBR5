      subroutine inteq(a1,mt,m,lr,kc,aux)
      use sharedvar
      implicit real*8(a-h,o-z)
      dimension a1(360),aux(2*mt,2*mt)

c***********************************************************************
c     subroutine intequ
c     ------------------
c     subroutine de remplissage des coefficients de la matrice des
c     inconnues hyperstatiques des traverses, dans la matrice [ aux ].
c
c     modifié: le 22-6-95 (ph. rigo)                    créer : thèse
c***********************************************************************
c       avec kc indice sur les sollicitations: kc = 1  disb et kc = 2  d
c       avec lr indice sur les forces        : kc = 1  xo   et kc = 2  z

      pi1=pi/180.
      pi2=2.*pi

      ki=(kc-1)*mt+1
      kf=kc*mt
      li=1+(lr-1)*mt
      lf=lr*mt

      do 15 j=ki,kf

      do 15 i=li,lf
      k1=j-ki+1
      l1=i-li+1
      z1=dabs(abtr(l1)-abtr(k1))*pi1

      do 16 k=1,m
      kh=4*k+lr*16+32*(i-(lr-1)*mt)-3
      zs=argq(2*k-1)*z1
      zr=argq(2*k)*  z1
      zss=argq(2*k-1)*(pi2-z1)
      zrr=argq(2*k)*  (pi2-z1)

      aux(i,j)=aux(i,j)+( a1(kh)*dcos(zr) +a1(kh+1)*dsin(zr) )*expo(zs)
     *            +( a1(kh+2)*dcos(zrr)+a1(kh+3)*dsin(zrr))*expo(zss)
 16   continue

      aux(i,j)=-aux(i,j)/10000.
      if(i.eq.j)aux(i,j)=aux(i,j)+1.

 15   continue

      return
      end
