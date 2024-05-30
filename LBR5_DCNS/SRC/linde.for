      subroutine linde(k,m,l,a,c,n)
      implicit real*8 (a-h,o-z)
      dimension a(720),c(8)
c***********************************************************************
c
c     subroutine linde
c
c     calcul des wר,wרר,wררר, etc.  des disa, b,c et d
c     calcul des coefficients des fonctions derivees
c     secondes et troisiemes pour u,v et w.
c
c***********************************************************************
      do 10 i=1,k
      ij=16*(n+i-2)
      jm=16*(l+i-2)
      do 10 j=1,m
      kx=ij+4*j-3
      nx=jm+4*j-3
      a(kx)=-c(2*j-1)*a(nx)+c(2*j)*a(nx+1)
      a(kx+1)=-c(2*j)*a(nx)-c(2*j-1)*a(nx+1)
      a(kx+2)=c(2*j-1)*a(nx+2)-c(2*j)*a(nx+3)
  10  a(kx+3)=c(2*j)*a(nx+2)+c(2*j-1)*a(nx+3)
      return
      end
