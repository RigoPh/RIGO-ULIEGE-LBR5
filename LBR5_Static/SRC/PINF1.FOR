      SUBROUTINE PINF1(SUM,ALI,ZB,HYP,KK,K,MS)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION ALI(360),HYP(360),ZB(360)
C***********************************************************************
C
C    CALCUL DES TERMES INDEPENDANTS POUR LA DETERMINATION DES
C    INCONNUES DE BORDS LORS DE L'ASSEMBLAGE DES PANNEAUX DANS MDR.
C    ET CELA POUR LA CHARGE EXTERIEURE.
C
C    INF1 EST RELATIF A LA PRESENCE DES TRAVERSES.
C
C***********************************************************************
      DO 1 I=1,16
      SUM=0.
      DO 2 J=1,MS ! MS = 2* nbre de traverses
      IJ=J+I*20
   2  SUM=SUM+ALI(IJ)*HYP(J+KK)
   1  ZB(I+K)=-SUM/10000.
      RETURN
      END
