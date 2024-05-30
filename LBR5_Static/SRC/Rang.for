      SUBROUTINE RANG(NEL,NE,A,B,NVSMB,NETO,NSOL,
     *                NOEUD,MCOMP,NNO,NC,NSIGN)

C***********************************************************************
C
C     SUBROUTINE RANG ,ASSEMBLAGE AUTOMATIQUE
C     ----------------------------------------
C
C     Modifié : 29-11-95 (MODULE OPTIMISATION)      Créer: Thèse de Ph. Rigo
C     =========                                    
C
C Les equations sont classees comme suit:
c  - d'abord les 4NC equations de compatibilite (soit NC equ de Compatibilite)
c  - ensuite les equations d'equilibre de chaque noeud
C***********************************************************************

      IMPLICIT REAL*8 (A-H,O-Z)

	DIMENSION NOEUD(NETO,2),MCOMP(2*NETO,2),NNO(NETO+1,2),NSIGN(NETO)
      DIMENSION A(NE,NE),B(NE,NVSMB)

      COMMON X(400),Y(360)
c     COMMON/ANG/ANGLE,NOH,NOEUD,MCOMP,NNO,NC,NSIGN
      COMMON/DEDE/MDP(15,4),MAR(15,4)

      II=0
      IF(NSIGN(NEL).EQ.1) GOTO 9
      II=1
  9   ND=NOEUD(NEL,1)
      NA=NOEUD(NEL,2)
      I1=(NC+ND-1)*4+1
      I2=(NC+NA-1)*4+1
      J1=(NEL-1)*8+1
      J2=J1+7
      IB=NSOL+1+(NEL-1)*8

c      WRITE(66,*)'NSIGN(NEL),II,I1,I2,J1,J2'
c      WRITE(66,*)NSIGN(NEL),II,I1,I2,J1,J2
c      WRITE(66,*)'ND,NA,NNO(ND,2),NNO(NA,2)'
c      WRITE(66,*)ND,NA,NNO(ND,2),NNO(NA,2)
C
C RANGEMENT DES COEFFICIENTS RELATIFS AUX EQUATIONS D'EQUILIBRE ET AUX
C CONDITIONS D'APPUIS EVENTUELS
C --------------------------------------------------------------------
      M=1
      N=1
      IF(NNO(ND,2).NE.0) M=NNO(ND,2)
      IF(NNO(NA,2).NE.0) N=NNO(NA,2)
      DO 100 J=J1,J2
        K=20*(J-J1)
        DO 101 I=1,4
          A(I1+I-1,J)=-X(MDP(M,I)+K+II)
          A(I2+I-1,J)= X(MAR(N,I)+K-II)
  101   CONTINUE
  100 CONTINUE
C
      DO 80 I=1,4
        DO 90 IS=1,NSOL
          IS1=(IS-1)*16
          B(I1+I-1,IS)=B(I1+I-1,IS)-Y(MDP(M,I)+II+IS1)
          B(I2+I-1,IS)=B(I2+I-1,IS)+Y(MAR(N,I)-II+IS1)
   90   CONTINUE
        IF(NVSMB.EQ.NSOL) GOTO 80
        KK=0
        DO 70 J=IB,IB+7
          KK=KK+16
          B(I1+I-1,J)=B(I1+I-1,J)-Y(MDP(M,I)+II+KK)
          B(I2+I-1,J)=B(I2+I-1,J)+Y(MAR(N,I)-II+KK)
   70   CONTINUE
   80 CONTINUE
C
C RANGEMENT DES COEFFICIENTS RELATIFS AUX EQUATIONS DE COMPATIBILITE
C ------------------------------------------------------------------
      IF (NC.EQ.0) GOTO 300

      DO 200 J=1,NC
      M=(J-1)*4+1
      IF (MCOMP(J,1).EQ.NEL) GOTO 210
      IF (MCOMP(J,2).EQ.NEL) GOTO 220
      GOTO 200
  210 DO 201 I=J1,J2
        K=20*(I-J1)
        A(M+0,I)=X( 2+K-II)
        A(M+1,I)=X( 4+K-II)
        A(M+2,I)=X(14+K-II)
        A(M+3,I)=X(16+K-II)
  201 CONTINUE
      DO 91 IS=1,NSOL
        IS1=(IS-1)*16
        B(M+0,IS)=B(M+0,IS)+Y( 2-II+IS1)
        B(M+1,IS)=B(M+1,IS)+Y( 4-II+IS1)
        B(M+2,IS)=B(M+2,IS)+Y(14-II+IS1)
        B(M+3,IS)=B(M+3,IS)+Y(16-II+IS1)
   91 CONTINUE
      IF(NVSMB.EQ.NSOL) GOTO 200
      KK=0
      DO 81 I=IB,IB+7
        KK=KK+16
        B(M+0,I)=B(M+0,I)+Y( 2-II+KK)
        B(M+1,I)=B(M+1,I)+Y( 4-II+KK)
        B(M+2,I)=B(M+2,I)+Y(14-II+KK)
   81   B(M+3,I)=B(M+3,I)+Y(16-II+KK)
      GOTO  200

  220 DO 202 I=J1,J2
        K=20*(I-J1)
        A(M  ,I)=-X( 1+K+II)
        A(M+1,I)=-X( 3+K+II)
        A(M+2,I)=-X(13+K+II)
        A(M+3,I)=-X(15+K+II)
  202 CONTINUE
      DO 92 IS=1,NSOL
        IS1=(IS-1)*16
        B(M+0,IS)=B(M+0,IS)-Y( 1+II+IS1)
        B(M+1,IS)=B(M+1,IS)-Y( 3+II+IS1)
        B(M+2,IS)=B(M+2,IS)-Y(13+II+IS1)
        B(M+3,IS)=B(M+3,IS)-Y(15+II+IS1)
   92 CONTINUE
      IF(NVSMB.EQ.NSOL) GOTO 200
      KK=0
      DO 82 I=IB,IB+7
        KK=KK+16
        B(M  ,I)=B(M  ,I)-Y( 1+II+KK)
        B(M+1,I)=B(M+1,I)-Y( 3+II+KK)
        B(M+2,I)=B(M+2,I)-Y(13+II+KK)
   82   B(M+3,I)=B(M+3,I)-Y(15+II+KK)
  200 CONTINUE

  300 RETURN
      END
