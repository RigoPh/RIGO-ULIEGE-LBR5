      SUBROUTINE RANG2(NEL,NE,A,B,NETO,NSOL,
     *                 NOEUD,MCOMP,NNO,NC,NSIGN)

      IMPLICIT REAL*8 (A-H,O-Z)                                         PRA00020

	DIMENSION NOEUD(NETO,2),MCOMP(2*NETO,2),NNO(NETO+1,2),NSIGN(NETO)

      DIMENSION A(NE,8),B(NE,NSOL)              

C***********************************************************************PRA00050
C                                                                       
C     SUBROUTINE RANG2, (ASSEMBLAGE AUTOMATIQUE)                           
C     *****************
C
C     Modifié : 18- 8-94 									Créer: 18-8-94
C                4-12-95 (NSOL cas de charge)
C                                                                                                                                            
C***********************************************************************PRA00090
      COMMON X(400),Y(360)                                              PRA00100
c     COMMON/ANG/ANGLE,NOH,NOEUD,MCOMP,NNO,NC,NSIGN                     PRA00110
      COMMON/DEDE/MDP(15,4),MAR(15,4)                                   PRA00120

      II=1                                                              PRA00130
      IF(NSIGN(NEL).EQ.1) II=0                                          PRA00140
      
      ND=NOEUD(NEL,1)                                                   PRA00160
      NA=NOEUD(NEL,2)                                                   PRA00170
      I1=(NC+ND-1)*4+1                                                  PRA00180
      I2=(NC+NA-1)*4+1                                                  PRA00190

c      WRITE(66,*)'NSIGN(NEL),II,I1,I2'
c      WRITE(66,*)NSIGN(NEL),II,I1,I2
c      WRITE(66,*)'ND,NA,NNO(ND,2),NNO(NA,2)'
c      WRITE(66,*)ND,NA,NNO(ND,2),NNO(NA,2)
C                                                                       PRA00230
C RANGEMENT DES COEFFICIENTS RELATIFS AUX EQUATIONS D'EQUILIBRE ET AUX  PRA00240
C CONDITIONS D'APPUIS EVENTUELS                                         PRA00250
C --------------------------------------------------------------------  PRA00260
      M=1                                                               PRA00270
      N=1                                                               PRA00280
      IF(NNO(ND,2).NE.0) M=NNO(ND,2)                                    PRA00290
      IF(NNO(NA,2).NE.0) N=NNO(NA,2)                                    PRA00300

      DO 100 J=1,8                                                      PRA00310
      K=20*(J-1)                                                        PRA00320
      DO 100 I=1,4                                                      PRA00330
      A(I1+I-1,J)=-X(MDP(M,I)+K+II)                                     PRA00340
      A(I2+I-1,J)= X(MAR(N,I)+K-II)                                     PRA00350
  100 CONTINUE                                                          PRA00360

      DO 80 IS=1,NSOL                                                   PRA00380
      KK=(IS-1)*16
      DO 80 I=1,4                                                       PRA00380
      B(I1+I-1,IS)=B(I1+I-1,IS)-Y(MDP(M,I)+II+KK)                            
      B(I2+I-1,IS)=B(I2+I-1,IS)+Y(MAR(N,I)-II+KK)                            
   80 CONTINUE                                                          PRA00470
C                                                                       PRA00480
C RANGEMENT DES COEFFICIENTS RELATIFS AUX EQUATIONS DE COMPATIBILITE    PRA00490
C ------------------------------------------------------------------    PRA00500
      IF (NC.EQ.0) GOTO 300                                             PRA00510
      DO 200 J=1,NC                                                     PRA00520
      M=(J-1)*4+1                                                       PRA00530
      IF (MCOMP(J,1).EQ.NEL) GOTO 210                                   PRA00540
      IF (MCOMP(J,2).EQ.NEL) GOTO 220                                   PRA00550
      GOTO 200                                                          PRA00560

C  Le panneau arrive au noeud considéré (c'est son extrémité qui compte)
  210 DO 201 I=1,8                                                      PRA00570
      K=20*(I-1)                                                        PRA00580
      A(M,I  )=X(2+K-II)                                                PRA00590
      A(M+1,I)=X(4+K-II)                                                PRA00600
      A(M+2,I)=X(14+K-II)                                               PRA00610
      A(M+3,I)=X(16+K-II)                                               PRA00620
  201 CONTINUE                                                          PRA00630
      DO 81 IS=1,NSOL                                                   PRA00380
      KK=(IS-1)*16
      B(M  ,IS)=B(M  ,IS)+Y( 2-II+KK)                                   PRA00640
      B(M+1,IS)=B(M+1,IS)+Y( 4-II+KK)                                   PRA00650
      B(M+2,IS)=B(M+2,IS)+Y(14-II+KK)                                   PRA00660
      B(M+3,IS)=B(M+3,IS)+Y(16-II+KK)                                   PRA00670
   81 CONTINUE
      GOTO  200                                                         PRA00760

C  Le panneau part du noeud considéré (c'est son noeud de départ qui compte)
  220 DO 202 I=1,8                                                      PRA00770
      K=20*(I-1)                                                        PRA00780
      A(M,I  )=-X(1+K+II)                                               PRA00790
      A(M+1,I)=-X(3+K+II)                                               PRA00800
      A(M+2,I)=-X(13+K+II)                                              PRA00810
      A(M+3,I)=-X(15+K+II)                                              PRA00820
  202 CONTINUE                                                          PRA00830
      DO 82 IS=1,NSOL                                                   PRA00380
      KK=(IS-1)*16
      B(M  ,IS)=B(M  ,IS)-Y( 1+II+KK)                                   PRA00840
      B(M+1,IS)=B(M+1,IS)-Y( 3+II+KK)                                   PRA00850
      B(M+2,IS)=B(M+2,IS)-Y(13+II+KK)                                   PRA00860
      B(M+3,IS)=B(M+3,IS)-Y(15+II+KK)                                   PRA00870
   82 CONTINUE
  200 CONTINUE                                                          PRA00960
  300 CONTINUE
  
c      WRITE(66,*) 'dA/dx) et (dB/dx) dans RANG2'
c      DO 102 J=1,NE                                                     
c      WRITE(66,6)(A(J,I),I=1,8)                        
c 102  WRITE(66,6)B(J)                                        
c    6 FORMAT(8E11.4)                  

      RETURN                                                            PRA00970
      END                                                               PRA00980
