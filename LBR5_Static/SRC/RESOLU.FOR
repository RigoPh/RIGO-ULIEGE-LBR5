      SUBROUTINE RESOLU(MS,NBREQU,HYP,AUX,ZB,KLI,BLOKA,IMPR)

      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER *2 KLI
      DIMENSION AUX(MS,MS),ZB(MS,NBREQU),BLOKA(MS,MS),
     *          KLI(MS,2),HYP(360)

C***********************************************************************
C
C     SUBROUTINE RESOLU (remplacement de la Sub. System)
C     ================
C     SUBROUTINE de résolution du système de calcul des traverses.
C           (AUX) * HYP = ZB
C     Soit AUX la matrice ; AUX(2*MT,2*MT)
C          ZB  le vecteur indépendant (NBREQU cas) ; ZB(2*MT,NBREQU)
C          HYP le vecteur solution (NBREQU cas) ; HYP(340=17*20)
C
C     Modifié : 21-6-95 (MODULE OPTIMISATION)              Créer: 21-6-95
C                                                                      
C***********************************************************************
C     DETERMINATION DE L'INVERSE DE LA MATRICE AUX.
c     ============================================
C     BLOKA et KLI sont des matrices de travail
C     Input  : AUX est la matrice à inverser
C     Output : AUX est la matrice inversée

c      IF(IMPR.EQ.1) THEN
c      WRITE(66,*)' La matrice AUX à inverser est :'
c      DO 554 J=1,MS                                                     
c 554  WRITE(66,401)J,(AUX(J,I),I=1,MS)                        
c      WRITE(66,*)' '
c      WRITE(66,*)' Terme indépendant :'
c      DO 555 J=1,9                                                     
c 555  WRITE(66,401)J,(ZB(I,J),I=1,MS)                        
c      ENDIF

      CALL RECIP(MS,AUX,BLOKA,KLI(1,1),KLI(1,2))
C     =============

c      IF(IMPR.EQ.1) THEN
c      WRITE(66,*)' La matrice inverse est :'
c      DO 553 J=1,MS                                                     
c      WRITE(66,*)'Ligne nø',J
c 553  WRITE(66,401)(AUX(J,I),I=1,MS)
c      ENDIF
                                        
C     Calcul des vecteurs solutions (HYP)
c     ============================================
c      IF(IMPR.EQ.1) WRITE(66,*)' Les vecteurs solutions (HYP) sont '
      DO 121 K=1,NBREQU
        K1=(K-1)*20
        DO 2003 I=1,MS
          IK=I+K1
          DO 2004 J=1,MS
 2004     HYP(IK)=HYP(IK)+AUX(I,J)*ZB(J,K)
 2003   CONTINUE
c        IF(IMPR.EQ.1) WRITE(66,401) (HYP(I+K1),I=1,MS)
 121  CONTINUE

  401 FORMAT(I2,1X,8(E11.4,1X))                  

      RETURN
      END
