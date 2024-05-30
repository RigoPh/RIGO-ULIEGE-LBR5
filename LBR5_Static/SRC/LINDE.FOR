      SUBROUTINE LINDE(K,M,L,A,C,N)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(720),C(8)
C***********************************************************************
C
C     SUBROUTINE LINDE
C
C     Calcul des wר,wרר,wררר, etc.  des DISA, B,C et D
C     CALCUL DES COEFFICIENTS DES FONCTIONS DERIVEES
C     SECONDES ET TROISIEMES pour U,V et W.
C
C***********************************************************************
      DO 10 I=1,K
      IJ=16*(N+I-2)
      JM=16*(L+I-2)
      DO 10 J=1,M
      KX=IJ+4*J-3
      NX=JM+4*J-3
      A(KX)=-C(2*J-1)*A(NX)+C(2*J)*A(NX+1)
      A(KX+1)=-C(2*J)*A(NX)-C(2*J-1)*A(NX+1)
      A(KX+2)=C(2*J-1)*A(NX+2)-C(2*J)*A(NX+3)
  10  A(KX+3)=C(2*J)*A(NX+2)+C(2*J-1)*A(NX+3)
      RETURN
      END
