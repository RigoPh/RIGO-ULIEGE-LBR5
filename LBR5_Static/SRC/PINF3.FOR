      SUBROUTINE PINF3(A,C2,S2,C3,S3,C4,S4,K,KK)
      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION A(1710),AUX(400),ZB(360)
      COMMON AUX,ZB
C***********************************************************************
C
C    CALCUL DES TERMES INDEPENDANTS POUR LA DETERMINATION DES
C    INCONNUES DE BORDS LORS DE L'ASSEMBLAGE DES PANNEAUX DANS MDR.
C    ET CELA POUR LA CHARGE EXTERIEURE.
C
C    INF3 EST RELATIF AUX TERMES EN EXP(-ALPHA*Y)*(..COS..+..SIN..)
C
C***********************************************************************
      ZB(1+KK)=ZB(1+KK)-A(8*K+75)-A(8*K+77)*C2-A(8*K+78)*S2
     *      -A(8*K+79)*C3-A(8*K+80)*S3-A(8*K+81)*C4-A(8*K+82)*S4
      ZB(2+KK)=ZB(2+KK)-A(8*K+75)*C2-A(8*K+76)*S2-A(8*K+77)
     *      -A(8*K+79)*C4-A(8*K+80)*S4-A(8*K+81)*C3-A(8*K+82)*S3
      ZB(3+KK)=ZB(3+KK)-A(8*K+37)-A(8*K+39)*C2-A(8*K+40)*S2
     *      -A(8*K+41)*C3-A(8*K+42)*S3-A(8*K+43)*C4-A(8*K+44)*S4
      ZB(4+KK)=ZB(4+KK)-A(8*K+37)*C2-A(8*K+38)*S2-A(8*K+39)
     *      -A(8*K+41)*C4-A(8*K+42)*S4-A(8*K+43)*C3-A(8*K+44)*S3
      ZB(5+KK)=ZB(5+KK)-A(8*K+873)-A(8*K+875)*C2-A(8*K+876)*S2
     *      -A(8*K+877)*C3-A(8*K+878)*S3-A(8*K+879)*C4-A(8*K+880)*S4
      ZB(6+KK)=ZB(6+KK)-A(8*K+873)*C2-A(8*K+874)*S2-A(8*K+875)
     *      -A(8*K+877)*C4-A(8*K+878)*S4-A(8*K+879)*C3-A(8*K+880)*S3
      ZB(7+KK)=ZB(7+KK)-A(8*K+911)-A(8*K+913)*C2-A(8*K+914)*S2
     *      -A(8*K+915)*C3-A(8*K+916)*S3-A(8*K+917)*C4-A(8*K+918)*S4
      ZB(8+KK)=ZB(8+KK)-A(8*K+911)*C2-A(8*K+912)*S2-A(8*K+913)
     *      -A(8*K+915)*C4-A(8*K+916)*S4-A(8*K+917)*C3-A(8*K+918)*S3
      ZB( 9+KK)=ZB( 9+KK)-A(8*K+989)*C2-A(8*K+990)*S2-A(8*K+987)
     *      -A(8*K+991)*C3-A(8*K+992)*S3-A(8*K+993)*C4-A(8*K+994)*S4
      ZB(10+KK)=ZB(10+KK)-A(8*K+987)*C2-A(8*K+988)*S2-A(8*K+989)
     *      -A(8*K+991)*C4-A(8*K+992)*S4-A(8*K+993)*C3-A(8*K+994)*S3
      ZB(11+KK)=ZB(11+KK)-A(8*K+951)*C2-A(8*K+952)*S2-A(8*K+949)
     *      -A(8*K+953)*C3-A(8*K+954)*S3-A(8*K+955)*C4-A(8*K+956)*S4
      ZB(12+KK)=ZB(12+KK)-A(8*K+949)*C2-A(8*K+950)*S2-A(8*K+951)
     *      -A(8*K+953)*C4-A(8*K+954)*S4-A(8*K+955)*C3-A(8*K+956)*S3
      ZB(13+KK)=ZB(13+KK)-A(8*K+  1)*C2-A(8*K+  2)*S2-A(8*K- 1)
     *      -A(8*K+  3)*C3-A(8*K+  4)*S3-A(8*K+  5)*C4-A(8*K+  6)*S4
      ZB(14+KK)=ZB(14+KK)-A(8*K-  1)*C2-A(8*K-  0)*S2-A(8*K+ 1)
     *      -A(8*K+  3)*C4-A(8*K+  4)*S4-A(8*K+  5)*C3-A(8*K+  6)*S3
      ZB(15+KK)=ZB(15+KK)-A(8*K+1065)*C2-A(8*K+1066)*S2-A(8*K+1063)
     *      -A(8*K+1067)*C3-A(8*K+1068)*S3-A(8*K+1069)*C4-A(8*K+1070)*S4
      ZB(16+KK)=ZB(16+KK)-A(8*K+1063)*C2-A(8*K+1064)*S2-A(8*K+1065)
     *      -A(8*K+1067)*C4-A(8*K+1068)*S4-A(8*K+1069)*C3-A(8*K+1070)*S3
      RETURN
      END
