      SUBROUTINE PINF2(ZB,D,S1,S2,C1,C2,PH,PH2,PH3,K)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION D(1710),ZB(360)
C***********************************************************************
C
C    CALCUL DES TERMES INDEPENDANTS POUR LA DETERMINATION DES
C    INCONNUES DE BORDS LORS DE L'ASSEMBLAGE DES PANNEAUX DANS MDR.
C    ET CELA POUR LA CHARGE EXTERIEURE.
C
C    INF2 EST RELATIF AUX TERMES INDEP., Y, Y**2 , Y**3
C
C***********************************************************************
      ZB(1+K)=ZB(1+K)-D(77)-D(82)*S1
      ZB(2+K)=ZB(2+K)-D(77)-D(78)*PH-D(79)*PH2-D(80)*PH3-D(82)*S2
      ZB(3+K)=ZB(3+K)-D(39)-D(44)*S1
      ZB(4+K)=ZB(4+K)-D(39)-D(40)*PH-D(41)*PH2-D(42)*PH3-D(44)*S2
      ZB(5+K)=ZB(5+K)-D(875)-D(880)*S1
      ZB(6+K)=ZB(6+K)-D(875)-D(876)*PH-D(877)*PH2-D(878)*PH3-D(880)*S2
      ZB(7+K)=ZB(7+K)-D(913)-D(918)*S1
      ZB(8+K)=ZB(8+K)-D(913)-D(914)*PH-D(915)*PH2-D(916)*PH3-D(918)*S2
      ZB(9+K)=ZB(9+K)-D(989)-D(993)*C1
      ZB(10+K)=ZB(10+K)-D(989)-D(990)*PH-D(991)*PH2-D(993)*C2
      ZB(11+K)=ZB(11+K)-D(951)-D(955)*C1
      ZB(12+K)=ZB(12+K)-D(951)-D(952)*PH-D(953)*PH2-D(955)*C2
      ZB(13+K)=ZB(13+K)-D(1)-D(5)*C1
      ZB(14+K)=ZB(14+K)-D(1)-D(2)*PH-D(3)*PH2-D(5)*C2
      ZB(15+K)=ZB(15+K)-D(1065)-D(1069)*C1
      ZB(16+K)=ZB(16+K)-D(1065)-D(1066)*PH-D(1067)*PH2-D(1069)*C2
      RETURN
      END
