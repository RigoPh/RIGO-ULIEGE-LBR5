subroutine inteq(array,m,lr,kc,aux,nel)

use param_section

implicit double precision(a-h,o-z)
dimension array(2*mt_max*(8+nsolmax)),aux(2*panneau(nel).mt,2*panneau(nel).mt)
      
!***********************************************************************
!     subroutine intequ
!     ------------------
!     subroutine de remplissage des coefficients de la matrice des
!     inconnues hyperstatiques des traverses, dans la matrice [ aux ].
!
!     modifié: le 22-6-95 (ph. rigo)                    créer : thèse
!***********************************************************************
!       avec kc indice sur les sollicitations: kc = 1  disb et kc = 2  d
!       avec lr indice sur les forces        : kc = 1  xo   et kc = 2  z


pi1=pi/180.d00
pi2=2.d00*pi

ki=(kc-1)*panneau(nel).mt+1
kf=kc*panneau(nel).mt
li=1+(lr-1)*panneau(nel).mt
lf=lr*panneau(nel).mt

do j=ki,kf

      do i=li,lf
      k1=j-ki+1
      l1=i-li+1
      z1=dabs(panneau(nel).abtr(l1)-panneau(nel).abtr(k1))*pi1

      do k=1,m
      kh=4*k+lr*16+32*(i-(lr-1)*panneau(nel).mt)-3
      zs=argq(2*k-1)*z1
      zr=argq(2*k)*  z1
      zss=argq(2*k-1)*(pi2-z1)
      zrr=argq(2*k)*  (pi2-z1)

      aux(i,j)=aux(i,j)+( array(kh)  *dcos(zr) +array(kh+1)*dsin(zr) )*expo(zs)  &
                       +( array(kh+2)*dcos(zrr)+array(kh+3)*dsin(zrr))*expo(zss)
      enddo

      aux(i,j)=-aux(i,j)/10000.d00
      if(i.eq.j)aux(i,j)=aux(i,j)+1.d00

      end do
end do

return
end
