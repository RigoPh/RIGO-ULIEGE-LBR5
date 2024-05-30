      subroutine pconti(a,b,c,d,e,k,i,s,x)
      implicit real*8(a-h,o-z)
      dimension a(720),b(720),c(720),d(720),e(8),s(1710)

c***********************************************************************
c
c     subroutine conti
c     la subroutine ajoute aux coefficients dish les coefficients dus
c     aux charges unitaires de bords du panneau multipliees par les
c     inconnues hyperstatiques des charges unitaires de bords.
c     l'ensemble est place dans dish
c
c***********************************************************************

      kh=(i-1)*38+8*k-1
      ij=(i-1)*16+4*k-3
      s(kh)=s(kh)+e(1)*c(ij)+e(3)*d(ij)+e(5)*a(ij)+e(7)*b(ij)
      s(kh+1)=s(kh+1)+e(1)*c(ij+1)+e(3)*d(ij+1)
     *               +e(5)*a(ij+1)+e(7)*b(ij+1)
      s(kh+2)=s(kh+2)+(e(2)*c(ij)-e(4)*d(ij)-e(6)*a(ij)+e(8)*b(ij))*x
      s(kh+3)=s(kh+3)+(e(2)*c(ij+1)-e(4)*d(ij+1)-e(6)*a(ij+1)
     *                                          +e(8)*b(ij+1))*x
      s(kh+4)=s(kh+4)+e(1)*c(ij+2)+e(3)*d(ij+2)+e(5)*a(ij+2)
     *               +e(7)*b(ij+2)
      s(kh+5)=s(kh+5)+e(1)*c(ij+3)+e(3)*d(ij+3)
     *               +e(5)*a(ij+3)+e(7)*b(ij+3)
      s(kh+6)=s(kh+6)+x*(e(2)*c(ij+2)-e(4)*d(ij+2)
     *                  -e(6)*a(ij+2)+e(8)*b(ij+2))
      s(kh+7)=s(kh+7)+x*(e(2)*c(ij+3)-e(4)*d(ij+3)
     *                  -e(6)*a(ij+3)+e(8)*b(ij+3))
      return
      end
