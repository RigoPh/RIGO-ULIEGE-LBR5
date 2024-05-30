      SUBROUTINE CARAC2(E,ETA,A,B,C,D,V,F,G,H,O,P,Q,Z,SH,FAM,
     *                  KSR,KSA,SPEC,PO,PHIL,RAYON,W,
     *                  DELT,DELT2,IMPR,CONST2)
      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION Z(74),FAM(6),CONST2(6,9)
      COMMON /PY/PI
C***********************************************************************
C
C     SUBROUTINE CARACT2
C     ==================
C     CALCUL DES COEFFICIENTS DE RAIDEUR DU PANNEAU
C      pour la couche complementaire de raidisseurs et de cadres
C
C       C=DELTA,  W=WIDTH

C       A=EPSA2  B=EPSR2
C       D=HYA2,  V=DYA2,      F=WYA2,  G=TYA2,  (cadres)
C       H=HXR2,  O=DXR2,      P=WXR2,  Q=TXR2,  (raidisseurs)

C   Solution Vecteur CONST = Z  et CONST2 (derivee de CONST)
C
C     Modif: 8-06-95                              Creation :21-8-95
c           20-11-2000 : calcul de DELT2
C***********************************************************************
c      CONST(1)= D       CONST( 2)= K
c      CONST(3)= Oy      CONST( 9)= Ox
c      CONST(4)= Hy      CONST(10)= Hx
c      CONST(5)= Sy      CONST(11)= Sx
c      CONST(6)= Ry      CONST(12)= Rx
c      CONST(7)= Ty      CONST(13)= Tx
c      CONST(8)= Ly      CONST(14)= Lx

      KKSA=(-1)**KSA
      KKSR=(-1)**KSR

C     CADRES
      IF(A.GE.0.00001) THEN
      TEMP=E*(D*V+F*G)/A
      CONST2(3,1)=CONST2(3,1)+KKSA*TEMP/2.
      Z(3)=Z(3)+TEMP
      Z(4)=Z(4)+E*(D*V*(C/2.+D/2.)+F*G*(C/2.+D+G/2.))*KKSA/A
      CONST2(4,1)=DABS(Z(4))
      TEMP=SH*5.*F*G/(6.*A)
      Z(5)=Z(5)+TEMP*FAM(1)
      Z(6)=Z(6)+E*(V*D**3/12.+V*D*(C/2.+D/2.)**2+F*G**3/12.+
     *                   F*G*(C/2.+D+G/2.)**2)/A
      Z(7)=Z(7)+SH*(D*V**3+F*G**3)/(3.*A)
      Z(8)=Z(8)-(C/2.+D+G/2.)*TEMP*FAM(2)*KKSA
      ENDIF

C     RAIDISSEURS
      IF(B.GE.0.00001) THEN
      TEMP=E*(H*O+P*Q)/B
      CONST2(5,1)=CONST2(5,1)+KKSR*TEMP/2.
      Z(9)=Z(9)+TEMP
      Z(10)=Z(10)+E*(H*O*(C/2.+H/2.)+P*Q*(C/2.+H+Q/2.))/B*KKSR
      CONST2(6,1)=DABS(Z(10))
      TEMP=SH*5.*P*Q/(6.*B)
      Z(11)=Z(11)+TEMP*FAM(3)
      Z(12)=Z(12)+E*(O*H**3/12.+O*H*(C/2.+H/2.)**2+P*Q**3/12.+
     *                    P*Q*(C/2.+H+Q/2.)**2)/B
      Z(13)=Z(13)+SH*(H*O**3+P*Q**3)/(3.*B)
      Z(14)=Z(14)-(C/2.+H+Q/2.)*TEMP*FAM(4)*KKSR
      ENDIF

C     CALCUL des Poids du panneau
      SPECC=SPEC*W
      IF(A.GE.0.00001) THEN
        PAIG2=((D*V+F*G)/A) * (RAYON+KKSA*D/2.)*PHIL*(PI/180.) *SPECC
	ELSE
	  PAIG2=0.0
	ENDIF
      IF(B.GE.0.00001) THEN
        PRAID2=((H*O+P*Q)/B) * RAYON*PHIL*(PI/180.) *SPECC
	ELSE
	  PRAID2=0.0
	ENDIF
      POTEL=PO*W+PAIG2+PRAID2

      PO=POTEL/W
      WRITE(66,12)PAIG2,PRAID2,POTEL

C     Impressions de CONST
	IF (IMPR.GE.-1) THEN			!15.10.05
        WRITE(66,15)
        WRITE(66,11)(Z(I),I=3,14)
	ENDIF						!15.10.05

C
C    CALCUL DE L'EPAISSEUR MOYENNE COMPTE TENU DES RAID. Complementaires

      IF(B.GE.0.00001) DELT=DELT + (H*O+P*Q)/B

C    CALCUL DE L'EPAISSEUR MOYENNE COMPTE TENU DES Raid. et Cadres Complementaires
      
	IF(A.GE.0.00001) DELT2= DELT2 + (D*V+F*G)/A
	IF(B.GE.0.00001) DELT2= DELT2 + (H*O+P*Q)/B 

      RETURN
   11 FORMAT(6(1X,E11.4))
   12 FORMAT(//' POIDS HORS DE l''EAU (non dejauge)'/
     *         ' Pour les nervures complementaires'/
     *         '   Poids des cadres      = ',E14.7, 'N.'/
     *         '   Poids des raidisseurs = ',E14.7, 'N.'//
     *         ' POIDS TOTAL (avec nervure second)  = ',E14.7, 'N.'/)
   15 FORMAT(/'COEFFICIENTS DE RAIDEUR DU PANNEAU',/
     * '(avec la seconde couche de nervures: aig. et raid)'/
     * T10,'OMYR HYR SYR RYR TYR LYR'/T10,'OMXR HXR SXR RXR TXR LXR '/)
      END
