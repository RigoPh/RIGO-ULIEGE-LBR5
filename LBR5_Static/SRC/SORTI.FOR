      SUBROUTINE SORTI(LAMB,DIS,MT,EFF,Z2)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 LAMB
      DIMENSION DIS(5),EFF(9690),Z2(2295)

c     COMMON/DI/EFF(9690),EFF2(4280),CONC(750),Z2(2295)

C***********************************************************************
C
C     SUBROUTINE RESULT(FORME REDUITE U,V,W)
C     SUBROUTINE DE MISE EN FORME DES RESULTATS POUR 5 POINTS DE L'AXE
C     DES X A PARTIR DU VECTEUR Z2,RESULTAT DU CALCUL SUR LES 31+20 DE
C     L'AXE DES Y(PHI).
C     LES RESULTATS SONT CONTENUS DANS LES VECTEURS EFF
C
C     Modifié : 23-8-94 (MODULE OPTIMISATION)      Créer: Thèse de Ph. Rigo
C               24-3-99 (suppression du common/DI)
C                                                                       
C***********************************************************************
      KH=31+2*MT

      DO 114 K=1,5
      SP=DSIN(LAMB*DIS(K))
      SQ=DCOS(LAMB*DIS(K))
C
C     CALCUL DE V,U,W
C     EN 5 PTS SELON X ET 31+20(MAX) SELON Y
C
      DO 112 I=1,KH
      IJ=I+(K-1)*51
      EFF(IJ)=EFF(IJ)+Z2(I)*SP
      EFF(IJ+255)=EFF(IJ+255)+Z2(I+51)*SQ
      EFF(IJ+510)=EFF(IJ+510)+Z2(I+102)*SP
  112 CONTINUE
  114 CONTINUE

      RETURN
      END
