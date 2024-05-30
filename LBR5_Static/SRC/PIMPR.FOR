      SUBROUTINE PIMPR(D,MS,M)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION D(1710)
C***********************************************************************
C
C    IMPRESSION DES COEFFICIENTS CONTENUS
C        DANS DISH ( CHARGE HYDRAULIQUE )
C        DANS DISL ( FORCES DE BORD     )
C        DANS DISM ( MOMENTS DE BORD    )
C
C***********************************************************************
      DO 1 I=1,(3+MS)
          J2=(I-1)*38+1
 1    WRITE(66,3)(D(J),J=J2,(J2+5+8*M))
      DO 2   I=24,45
          J2=(I-1)*38+1
 2    WRITE(66,3)(D(J),J=J2,(J2+5+8*M))
 3    FORMAT(6(2X,E14.7)/4(8(2X,E14.7)/))
      RETURN
      END
