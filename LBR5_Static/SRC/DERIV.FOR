      SUBROUTINE DERIV(K,M,L,A,B,N,Q)                                   PDE00010
      IMPLICIT REAL*8(A-H,O-Z)                                          PDE00020
      DIMENSION A(1710),B(8)                                            PDE00030
C***********************************************************************PDE00040
C                                                                       PDE00050
C     SUBROUTINE DERIV                                                  PDE00060
C     SUBROUTINE DE CALCUL DES COEFFICIENTS DES FONCTIONS DERIVEES      PDE00070
C     SECONDES ET TROISIEMES POUR LA FORME DISH.                        PDE00080
C                                                                       PDE00090
C***********************************************************************PDE00100
      DO 10 I=1,K                                                       PDE00110
      IJ=38*(N+I-2)                                                     PDE00120
      JM=38*(I+L-2)                                                     PDE00130
      A(IJ+1)=A(JM+2)/Q                                                 PDE00140
      A(IJ+2)=A(JM+3)*2/Q                                               PDE00150
      A(IJ+3)=A(JM+4)*3/Q                                               PDE00160
      A(IJ+4)=0.                                                        PDE00170
      TEMP=A(JM+5)                                                      PDE00180
      A(IJ+5)=A(JM+6)/Q                                                 PDE00190
      A(IJ+6)=-TEMP/Q                                                   PDE00200
      DO 10 J=1,M                                                       PDE00210
      KX=IJ+8*J-1                                                       PDE00220
      NX=JM+8*J-1                                                       PDE00230
      A(KX)=-B(2*J-1)*A(NX)+B(2*J)*A(NX+1)                              PDE00240
      A(KX+1)=-B(2*J)*A(NX)-B(2*J-1)*A(NX+1)                            PDE00250
      A(KX+2)=B(2*J-1)*A(NX+2)-B(2*J)*A(NX+3)                           PDE00260
      A(KX+3)=B(2*J)*A(NX+2)+B(2*J-1)*A(NX+3)                           PDE00270
      A(KX+4)=B(2*J-1)*A(NX+4)-B(2*J)*A(NX+5)                           PDE00280
      A(KX+5)=B(2*J)*A(NX+4)+B(2*J-1)*A(NX+5)                           PDE00290
      A(KX+6)=-B(2*J-1)*A(NX+6)+B(2*J)*A(NX+7)                          PDE00300
  10  A(KX+7)=-B(2*J)*A(NX+6)-B(2*J-1)*A(NX+7)                          PDE00310
      RETURN                                                            PDE00320
      END                                                               PDE00330
