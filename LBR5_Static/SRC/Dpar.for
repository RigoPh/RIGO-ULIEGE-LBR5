      SUBROUTINE DPAR(M,PX,COEFF,DCOEFF,DPX,DVAR,ISYM,NXI,NBRXI,MT)
C     ****************
      IMPLICIT REAL *8(A-H,O-Z)

      DIMENSION COEFF(32,41),DCOEFF(32,33,9),DPX(8,9),DVAR(33,9,16),
     *          PX(8),NXI(9)

C***********************************************************************
C
C    Expression des dérivées des DISA, DISB, DISC et DISD.
C    -----------------------------------------------------
C   DVAR(13,9,16) avec 13 variables (U V Wø Ny Nyx My Ry W Wøø Wøøø Uø V
C                       9 variables de conception,
C                   et 16 termes (pour DISC, DISA, DISB et DISD).

C       DVAR = DVARA,DVARB,DVARC,DVARD

C     modif: 14-6-95                                     Créer : 28-2-94
C***********************************************************************
c     Ordre des termes
C       1 : EXP(-Alpha Q Y) * COS (Béta Q Y)
C       2 : EXP(-Alpha Q Y * SIN (Béta Q Y)
C       3 : EXP(-Alpha Q (2pi-Y)) * COS (Béta Q (2pi-Y))
C       4 : EXP(-Alpha Q (2pi-Y)) * SIN (Béta Q (2pi-Y))

      CALL TEMP(COEFF,DCOEFF,PX,DPX,DVAR, 1,M,ISYM,1, 1,NXI,NBRXI)
      CALL TEMP(COEFF,DCOEFF,PX,DPX,DVAR, 2,M,ISYM,2,-1,NXI,NBRXI)
      CALL TEMP(COEFF,DCOEFF,PX,DPX,DVAR, 3,M,ISYM,3,-1,NXI,NBRXI)
      CALL TEMP(COEFF,DCOEFF,PX,DPX,DVAR, 4,M,ISYM,4, 1,NXI,NBRXI)
      CALL TEMP(COEFF,DCOEFF,PX,DPX,DVAR, 6,M,ISYM,5,-1,NXI,NBRXI)
      CALL TEMP(COEFF,DCOEFF,PX,DPX,DVAR, 8,M,ISYM,6, 1,NXI,NBRXI)
      CALL TEMP(COEFF,DCOEFF,PX,DPX,DVAR,14,M,ISYM,7,-1,NXI,NBRXI)
      CALL TEMP(COEFF,DCOEFF,PX,DPX,DVAR,16,M,ISYM,8, 1,NXI,NBRXI)
      CALL TEMP(COEFF,DCOEFF,PX,DPX,DVAR,37,M,ISYM,9, 1,NXI,NBRXI)
      CALL TEMP(COEFF,DCOEFF,PX,DPX,DVAR,38,M,ISYM,10,-1,NXI,NBRXI)
      CALL TEMP(COEFF,DCOEFF,PX,DPX,DVAR,39,M,ISYM,11,-1,NXI,NBRXI)
      CALL TEMP(COEFF,DCOEFF,PX,DPX,DVAR,40,M,ISYM,12, 1,NXI,NBRXI)
      CALL TEMP(COEFF,DCOEFF,PX,DPX,DVAR,41,M,ISYM,13,-1,NXI,NBRXI)

      DO 1 J=1,MT
       K1=15+J*2
       K2=12+J*2
       CALL TEMP(COEFF,DCOEFF,PX,DPX,DVAR,K1,M,ISYM,K2,1,NXI,NBRXI)
       K1=K1+1
       K2=K2+1
       CALL TEMP(COEFF,DCOEFF,PX,DPX,DVAR,K1,M,ISYM,K2,1,NXI,NBRXI)
    1 CONTINUE

      RETURN
      END

C *************************************************************************
C *************************************************************************

      SUBROUTINE TEMP(COEFF,DCOEFF,PX,DPX,DVAR,I,M,ISYM,II,ISYM2,
     *                NXI,NBRXI)

      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION COEFF(32,41),DCOEFF(32,33,9),DPX(8,9),DVAR(33,9,16),
     *          PX(8),NXI(9)

C     I =  nø des var.: U=1 V=2 Wø=3 Ny=4 Nyx=6 My=8 Ry=14 W=16    dans
C                       Wøø=37  Wøøø=38   Uø=39 Vø=40      Vøø=41
C     II=  nø des var.: U=1 V=2 Wø=3 Ny=4 Nyx=5 My=6 Ry=7  W=8     dans
C                       Wøø=9   Wøøø=10   Uø=11 Vø=12      Vøø=13
C     ISYM  est relatif au cas de sollicitation (DISA,DISB, etc), symétr
C     ISYM2 est relatif aux variables (U,V,X, etc), symétrique ou non

      DO 2 J=1,M
        JJ=(J-1)*4
        J2=J*2
        J8=(J-1)*8

      DO 1 KK=1,NBRXI
      K=NXI(KK)
      DVAR(II,K,JJ+1)=
     *   DCOEFF(J8+1,II,K) * PX(J2-1)  +  DCOEFF(J8+2,II,K) * PX(J2)
     * +  COEFF(J8+1,I)   *DPX(J2-1,K)+   COEFF(J8+2,I)   *DPX(J2,K)
      DVAR(II,K,JJ+2)=
     *   DCOEFF(J8+3,II,K) * PX(J2-1)  +  DCOEFF(J8+4,II,K) * PX(J2)
     * +  COEFF(J8+3,I)   *DPX(J2-1,K)+   COEFF(J8+4,I)   *DPX(J2,K)
      DVAR(II,K,JJ+3)=DVAR(II,K,JJ+1)*ISYM*ISYM2
      DVAR(II,K,JJ+4)=DVAR(II,K,JJ+2)*ISYM*ISYM2
    1 CONTINUE

    2 CONTINUE

      RETURN
      END
