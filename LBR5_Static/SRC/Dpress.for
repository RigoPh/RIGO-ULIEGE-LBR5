      SUBROUTINE DPRESS(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DVARH,BKSE,IJK,
     *                  NXI,NBRXI,MT)

      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION ARG(8),DISC(720),DVARC(33,9,16),DVARH(33,9,38),NXI(9)

      COMMON /PY/PI,SI(4),CO(4),EX(4)
      COMMON/OPTI3/ BID1(54),BID2(20),DARG(8,9)
C***********************************************************************
C
C    Expression (partiel) des dérivées de DISH (cfr subr PRESS)
C    -----------------------------------------------------------
C    DVARH(13,9,38) avec 13 variables (U V Wø Ny Nyx My Ry W)Wøø Wøøø Uø
C                         9 variables de conception,
C                     et 38 termes (cfr DISH).
C
C    DVARC = contient les dérivées de DISC
C
C     Modif: 14-6-95                                     Créer : 28-2-94
C***********************************************************************
c     Ordre des termes indép. (1 à 6)
C       1 : const       2 : y           3 : y**2         4 : y**3
C       5 : COS(téta+y) 6 : SIN(téta+y)
c     Ordre des termes EXP(y) (7 à 38 = 4 x 8 termes)
C       1 : EXP(-Alpha Q Y)            * COS (Béta Q Y)
C       2 : EXP(-Alpha Q Y)            * SIN (Béta Q Y)
C       3 : EXP(-Alpha Q (Yo-Y))       * COS (Béta Q (Yo-Y))
C       4 : EXP(-Alpha Q (Yo-Y))       * SIN (Béta Q (Yo-Y))
C       5 : EXP(-Alpha Q (2pi-Y))      * COS (Béta Q (2pi-Y))
C       6 : EXP(-Alpha Q (2pi-Y))      * SIN (Béta Q (2pi-Y))
C       7 : EXP(-Alpha Q (2pi-(Yo-Y))) * COS (Béta Q (2pi-(Yo-Y)))
C       8 : EXP(-Alpha Q (2pi-(Yo-Y))) * SIN (Béta Q (2pi-(Yo-Y)))

      CALL TT(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DVARH, 2,1,NXI,NBRXI)
      CALL TT(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DVARH, 1,2,NXI,NBRXI)
      CALL TT(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DVARH,29,3,NXI,NBRXI)
      CALL TT(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DVARH,25,4,NXI,NBRXI)
      CALL TT(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DVARH,27,5,NXI,NBRXI)
      CALL TT(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DVARH,24,6,NXI,NBRXI)
      CALL TT(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DVARH,26,7,NXI,NBRXI)
      CALL TT(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DVARH, 3,8,NXI,NBRXI)
      CALL TT(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DVARH,35,9,NXI,NBRXI)
      CALL TT(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DVARH,38,10,NXI,NBRXI)
      CALL TT(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DVARH,31,11,NXI,NBRXI)
      CALL TT(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DVARH,30,12,NXI,NBRXI)
      CALL TT(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DVARH,33,13,NXI,NBRXI)


      DO 2 J=1,MT
      K1=2+J*2
      K2=12+J*2
      CALL TT(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DVARH,K1,K2,NXI,NBRXI)
      K1=K1+1
      K2=K2+1
      CALL TT(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DVARH,K1,K2,NXI,NBRXI)
    2 CONTINUE

      E=BKSE*4.*0.981/(PI*IJK)

      J1=6+8*M
      DO 1 II=1,13+2*MT
      DO 1 KK=1,NBRXI
      K=NXI(KK)
      DO 1 J=1,J1
   1  DVARH(II,K,J)=E*DVARH(II,K,J)

      RETURN
      END

C    *********************************************************************
C    *********************************************************************

      SUBROUTINE TT(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DVARH,I,II,NXI,NBRXI)
C     ==============
      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION ARG(8),DISC(720),DVARC(33,9,16),DVARH(33,9,38),NXI(9)

      COMMON /PY/PI,SI(4),CO(4),EX(4)
      COMMON/OPTI3/ BID1(54),BID2(20),DARG(8,9)

C  I = class. des var.: U=2 V=1 Wø=29 Ny=25 Nyx=27 My=24 Ry=26 W=3 dans
C                       Wøø=35  Wøøø=38   Uø=31 Vø=30      Vøø=33
C                       Xo,Zo de 4 à 23
C  II= class. des var.: U=1 V=2 Wø=3  Ny=4  Nyx=5  My=6  Ry=7  W=8 dans
C                       Wøø=9   Wøøø=10   Uø=11 Vø=12      Vøø=13
C                       Xo,Zo de 14 à 33
C  K = indice des 9 variables de conception  ; K=1,NBRXI
C  J = indice sur Alpha(i) et Béta(i) ; J=1,M

      PIQ2=2.*PI*Q

      DO 4 KK=1,NBRXI
         K=NXI(KK)
c        DO 6 J=1,38
c   6    DVARH(II,K,J)=0.
      DO 5 J=1,M
      L=4*(J-1)
      JK=(I-1)*16+L
      JJ=8*J-2

      J2=J*2
      J1=J2-1
      AL=ARG(J1)
      BE=ARG(J2)
      DAL=DARG(J1,K)
      DBE=DARG(J2,K)
      DAL2=PIQ2*DAL
      DBE2=PIQ2*DBE
      U1= AL*AL+BE*BE
      U2= U1*U1
      U3= U2*U1
      R1=-AL*AL+BE*BE
      S1=2.*AL*BE
      DU1=2.*( AL*DAL+BE*DBE)
      DR1=2.*(-AL*DAL+BE*DBE)
      DS1=2.*( AL*DBE+BE*DAL)

      P1=XF-XI
      P2=P1/(PHIL*PI/180.)
      P3=P2/Q

      COS1=EX(J)*CO(J)
      SIN1=EX(J)*SI(J)

c      WRITE(66,*)' EX=',EX
c      WRITE(66,*)' CO=',CO
c      WRITE(66,*)' SI=',SI
c      WRITE(66,*)' COS1  SIN1=',COS1,SIN1

      AA=DISC(JK+1)
      BB=DISC(JK+2)
      DAA=DVARC(II,K,L+1)
      DBB=DVARC(II,K,L+2)
      T1=( AL*AA+BE*BB)
      T2=(-BE*AA+AL*BB)
      T3=( R1*AA-S1*BB)
      T4=( S1*AA+R1*BB)
      DT1=( DAL*AA+AL*DAA+DBE*BB+BE*DBB)
      DT2=(-DBE*AA-BE*DAA+DAL*BB+AL*DBB)
      DT3=( DR1*AA+R1*DAA-DS1*BB-S1*DBB)
      DT4=( DS1*AA+S1*DAA+DR1*BB+R1*DBB)
      T11=(U1*DT1-DU1*T1)/U2
      T22=(U1*DT2-DU1*T2)/U2
      T33=P3*(U1*DT3-2.*T3*DU1)/U3
      T44=P3*(U1*DT4-2.*T4*DU1)/U3

c      WRITE(66,*)' T1,T2,T3,T4=', T1,T2,T3,T4

      IF(II.GE.14) GOTO 1
      GOTO(1,2,2,1,2,1,2,1,1,2,2,1,2),II

C     Termes U, Ny, My ,W + Wøø, Vø et Xo Zo  (c.à.d. 1,4,6,8+9,12 et 14
C     ------------------------------------------------------------------
   1  TEMP=T1*COS1+T2*SIN1
      TEMP2= 2.* (  DU1*(-T1+TEMP)
     *   + U1*( DT1 + DAL2*TEMP - COS1*(DT1+DBE2*T2)
     *                          - SIN1*(DT2-DBE2*T1) )  )/U2
      DVARH(II,K,1)=DVARH(II,K,1) + TEMP2 * XI
      DVARH(II,K,2)=DVARH(II,K,2) + TEMP2 * P2

c      WRITE(66,*)' TEMP=',TEMP
c      WRITE(66,*)' TEMP2=',TEMP2
c      WRITE(66,*)' XI=',XI
c      WRITE(66,*)' DVARH(II,K,1)=',DVARH(II,K,1)
c      WRITE(66,*)' '

      DVARH(II,K,JJ+1)=-XI*T11 - T33
      DVARH(II,K,JJ+2)=-XI*T22 - T44
      DVARH(II,K,JJ+3)=-XF*T11 + T33
      DVARH(II,K,JJ+4)=-XF*T22 + T44
      DVARH(II,K,JJ+5)= XI*T11 - T33
      DVARH(II,K,JJ+6)= XI*T22 - T44
      DVARH(II,K,JJ+7)= XF*T11 + T33
      DVARH(II,K,JJ+8)= XF*T22 + T44
      GOTO 3

C     Termes V, Wø, Nyx, Ry + Wøøø,Uø etVøø (c.à.d. 2,3,5,7 +10,11,13)
C     ------------------------------------------
   2  TEMP=T3*COS1+T4*SIN1
c      WRITE(66,*)'TEMP=',TEMP
      DVARH(II,K,1)=DVARH(II,K,1) + 2.*P3*(-2.*DU1*(T3-TEMP)
     *   + U1*( DT3 + DAL2*TEMP - COS1*(DT3+DBE2*T4)
     *                          - SIN1*(DT4-DBE2*T3) )  )/U3
      DVARH(II,K,JJ+1)=-XI*T11 - T33
      DVARH(II,K,JJ+2)=-XI*T22 - T44
      DVARH(II,K,JJ+3)= XF*T11 - T33
      DVARH(II,K,JJ+4)= XF*T22 - T44
      DVARH(II,K,JJ+5)=-XI*T11 + T33
      DVARH(II,K,JJ+6)=-XI*T22 + T44
      DVARH(II,K,JJ+7)= XF*T11 + T33
      DVARH(II,K,JJ+8)= XF*T22 + T44
    3 CONTINUE
    5 CONTINUE

    4 CONTINUE
      RETURN
      END
