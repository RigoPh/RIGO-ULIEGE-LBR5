      SUBROUTINE PEDGE(C1,S1,C2,S2,C3,S3,C4,S4,K,LR,IS,DA,DB,DC,DD,
     *               ALD,ALZ)
      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION AUX(400),U(200),DA(720),DB(720),DC(720),DD(720)
      COMMON AUX,U
C***********************************************************************
C
C     SUBROUTINE EDGE
C     SUBROUTINE DE CALCUL DES EFFETS DE BORDS SOUS L'ENSEMBLE DES 8
C     FORCES AUX COUPURES EN X=0 ET X=H,COEFFICIENTS DES INCONNUES
C     HYPERSTATIQUES DE LA MATRICE DE JONCTION.LA PLAQUE EST CONSIDEREE
C     COMME LISSE.
C
C***********************************************************************
      KA=4*K+16*IS-19
      AUX(LR)=AUX(LR)+DC(KA)*C1+DC(KA+1)*S1
     *                     +DC(KA+2)*C3+DC(KA+3)*S3
      AUX(LR+20)=AUX(LR+20)+(DC(KA)*C2+DC(KA+1)*S2
     *                     +DC(KA+2)*C4+DC(KA+3)*S4)*ALD
      AUX(LR+40)=AUX(LR+40)+DD(KA)*C1+DD(KA+1)*S1
     *                     +DD(KA+2)*C3+DD(KA+3)*S3
      AUX(LR+60)=AUX(LR+60)+(DD(KA)*C2+DD(KA+1)*S2
     *                     +DD(KA+2)*C4+DD(KA+3)*S4)*ALZ
      AUX(LR+80)=AUX(LR+80)+DA(KA)*C1+DA(KA+1)*S1
     *                     +DA(KA+2)*C3+DA(KA+3)*S3
      AUX(LR+100)=AUX(LR+100)+(DA(KA)*C2+DA(KA+1)*S2
     *                       +DA(KA+2)*C4+DA(KA+3)*S4)*ALZ
      AUX(LR+120)=AUX(LR+120)+DB(KA)*C1+DB(KA+1)*S1
     *                       +DB(KA+2)*C3+DB(KA+3)*S3
      AUX(LR+140)=AUX(LR+140)+(DB(KA)*C2+DB(KA+1)*S2
     *                       +DB(KA+2)*C4+DB(KA+3)*S4)*ALD
      RETURN
      END
