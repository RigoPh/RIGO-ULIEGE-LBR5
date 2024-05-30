      subroutine deriv(k,m,l,a,b,n,q)                                   pde00010
      implicit double precision(a-h,o-z)                                          pde00020
      dimension a(1710),b(8)                                            pde00030

c***********************************************************************pde00040
c                                                                       pde00050
c     subroutine deriv                                                  pde00060
c     subroutine de calcul des coefficients des fonctions derivees      pde00070
c     secondes et troisiemes pour la forme dish.                        pde00080
c                                                                       pde00090
c***********************************************************************pde00100
      do 10 i=1,k                                                       pde00110
      ij=38*(n+i-2)                                                     pde00120
      jm=38*(i+l-2)                                                     pde00130
      a(ij+1)=a(jm+2)/q                                                 pde00140
      a(ij+2)=a(jm+3)*2/q                                               pde00150
      a(ij+3)=a(jm+4)*3/q                                               pde00160
      a(ij+4)=0.                                                        pde00170
      temp=a(jm+5)                                                      pde00180
      a(ij+5)=a(jm+6)/q                                                 pde00190
      a(ij+6)=-temp/q                                                   pde00200
      do 10 j=1,m                                                       pde00210
      kx=ij+8*j-1                                                       pde00220
      nx=jm+8*j-1                                                       pde00230
      a(kx)=-b(2*j-1)*a(nx)+b(2*j)*a(nx+1)                              pde00240
      a(kx+1)=-b(2*j)*a(nx)-b(2*j-1)*a(nx+1)                            pde00250
      a(kx+2)=b(2*j-1)*a(nx+2)-b(2*j)*a(nx+3)                           pde00260
      a(kx+3)=b(2*j)*a(nx+2)+b(2*j-1)*a(nx+3)                           pde00270
      a(kx+4)=b(2*j-1)*a(nx+4)-b(2*j)*a(nx+5)                           pde00280
      a(kx+5)=b(2*j)*a(nx+4)+b(2*j-1)*a(nx+5)                           pde00290
      a(kx+6)=-b(2*j-1)*a(nx+6)+b(2*j)*a(nx+7)                          pde00300
  10  a(kx+7)=-b(2*j)*a(nx+6)-b(2*j-1)*a(nx+7)                          pde00310
      return                                                            pde00320
      end                                                               pde00330
