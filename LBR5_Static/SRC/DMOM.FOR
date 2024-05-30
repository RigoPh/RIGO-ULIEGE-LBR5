      SUBROUTINE DMOM(ARG,M,PHIL,DISB,DVARB,DTEMP,DVARH,TETA,Q,
     *                IMPR,NXI,NBRXI,MT,A,B,C,D)

      IMPLICIT REAL *8(A-H,O-Z)
      REAL*8 LAMB
      DIMENSION ARG(8),NXI(9),
     *  DISB(720),DVARB(33,9,16),DTEMP(33,9,38),DVARH(33,9,38)
      COMMON /PY/PI,SI(4),CO(4),EX(4)
      COMMON/OPTI3/ BID1(54),BID2(20),DARG(8,9)
C***********************************************************************
C
C    Expression (partiel) des dérivées de DISH (cfr subr MOM)
C    -----------------------------------------------------------
C    DVARH(13,9,38) avec 13 variables (U V Wø Ny Nyx My Ry W)Wøø Wøøø Uø
C                         9 variables de conception,
C                     et 38 termes (cfr DISH).

C     LES DONNEES SONT CONTENUES  DANS DVARB.
C              DVARB = contient les dérivées de DISB

C     LES RESULTATS SONT PLACES DANS "DTEMP" (puis additionner à "DVARH"
C       (LE VECTEUR "DTEMP" EST UN VECTEUR DE TRAVAIL TEMPORAIRE)

C     modif: 29-3-96                                     Créer : 20 mars
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

      IF(IMPR.NE.0) WRITE(*,*) ' ENTREE DANS  DMOM  '

      CALL CR(ARG,M,A,B,C,D,PHIL,Q,DISB,DVARB,DTEMP, 2,1,NXI,NBRXI)
      CALL CR(ARG,M,A,B,C,D,PHIL,Q,DISB,DVARB,DTEMP, 1,2,NXI,NBRXI)
      CALL CR(ARG,M,A,B,C,D,PHIL,Q,DISB,DVARB,DTEMP,29,3,NXI,NBRXI)
      CALL CR(ARG,M,A,B,C,D,PHIL,Q,DISB,DVARB,DTEMP,25,4,NXI,NBRXI)
      CALL CR(ARG,M,A,B,C,D,PHIL,Q,DISB,DVARB,DTEMP,27,5,NXI,NBRXI)
      CALL CR(ARG,M,A,B,C,D,PHIL,Q,DISB,DVARB,DTEMP,24,6,NXI,NBRXI)
      CALL CR(ARG,M,A,B,C,D,PHIL,Q,DISB,DVARB,DTEMP,26,7,NXI,NBRXI)
      CALL CR(ARG,M,A,B,C,D,PHIL,Q,DISB,DVARB,DTEMP, 3,8,NXI,NBRXI)
      CALL CR(ARG,M,A,B,C,D,PHIL,Q,DISB,DVARB,DTEMP,35,9,NXI,NBRXI)
      CALL CR(ARG,M,A,B,C,D,PHIL,Q,DISB,DVARB,DTEMP,38,10,NXI,NBRXI)
      CALL CR(ARG,M,A,B,C,D,PHIL,Q,DISB,DVARB,DTEMP,31,11,NXI,NBRXI)
      CALL CR(ARG,M,A,B,C,D,PHIL,Q,DISB,DVARB,DTEMP,30,12,NXI,NBRXI)
      CALL CR(ARG,M,A,B,C,D,PHIL,Q,DISB,DVARB,DTEMP,33,13,NXI,NBRXI)

      DO 2 J=1,MT
      K1=2+J*2
      K2=12+J*2
      CALL CR(ARG,M,A,B,C,D,PHIL,Q,DISB,DVARB,DTEMP,K1,K2,NXI,NBRXI)
      K1=K1+1
      K2=K2+1
      CALL CR(ARG,M,A,B,C,D,PHIL,Q,DISB,DVARB,DTEMP,K1,K2,NXI,NBRXI)
    2 CONTINUE

      J1=6+8*M
      DO 1 II=1,13+2*MT
      DO 1 KK=1,NBRXI
      K=NXI(KK)
      DO 1 J=1,J1
      DVARH(II,K,J)=DVARH(II,K,J)+DTEMP(II,K,J)
      DTEMP(II,K,J)=0.
   1  CONTINUE


      RETURN
      END

      SUBROUTINE CR(ARG,M,A,B,C,D,PHIL,Q,DISB,DVARB,DTEMP,
     *              I,II,NXI,NBRXI)
C     ==============
      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION ARG(8),DISB(720),DVARB(33,9,16),DTEMP(33,9,38),NXI(9)
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
      P1=Q*PHIL*PI/180.
      P2=P1*P1
      P3=P2*P1

c      WRITE(66,*)' Variable nø',II
c      WRITE(66,*)' ---------------'
c      WRITE(66,*)' A,B,C,D=',A,B,C,D
c      WRITE(66,*)' Q=',Q

      DO 4 KK=1,NBRXI
         K=NXI(KK)

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

      U1=AL*AL+BE*BE
      U2=U1*U1
      U3=U2*U1
      U4=U2*U2
      U5=U2*U3
      DU1=2.*( AL*DAL+BE*DBE)

      ALU=AL/U1
      BEU=BE/U1
      R1=-AL*AL+BE*BE
      S1=2.*AL*BE
      R1U=R1/U2
      S1U=S1/U2

      DALU= R1U*DAL-S1U*DBE
      DBEU=-R1U*DBE-S1U*DAL
      DR1=2.*(-AL*DAL+BE*DBE)
      DS1=2.*( AL*DBE+BE*DAL)
      DR1U=(U1*DR1-2.*R1*DU1)/U3
      DS1U=(U1*DS1-2.*S1*DU1)/U3

      R2=(-AL*AL+3.*BE*BE)*2.*AL
      S2=(-3.*AL*AL+BE*BE)*2.*BE
      R2U=R2/U3
      S2U=S2/U3
      DR2U=(6.*U1*(R1*DAL+S1*DBE)-3.*R2*DU1)/U4
      DS2U=(6.*U1*(R1*DBE-S1*DAL)-3.*S2*DU1)/U4

      R3=6.*(AL**4+BE**4-6.*((AL*BE)**2))
      S3=24.*AL*BE*(-R1)
      R3U=R3/U4
      S3U=S3/U4
      DR3U=( 12.*U1*(-R2*DAL+S2*DBE)-4.*R3*DU1)/U5
      DS3U=(-12.*U1*( S2*DAL+R2*DBE)-4.*S3*DU1)/U5

      COS1=EX(J)*CO(J)
      SIN1=EX(J)*SI(J)

c      WRITE(66,*)' EX=',EX
c      WRITE(66,*)' CO=',CO
c      WRITE(66,*)' SI=',SI
c      WRITE(66,*)' COS1, SIN1=',COS1,SIN1

      AA=DISB(JK+1)
      BB=DISB(JK+2)
      DAA=DVARB(II,K,L+1)
      DBB=DVARB(II,K,L+2)

      T1U= ALU*AA+BEU*BB
      T2U=-BEU*AA+ALU*BB
      T3U= R1U*AA-S1U*BB
      T4U= S1U*AA+R1U*BB
      T5U= R2U*AA+S2U*BB
      T6U=-S2U*AA+R2U*BB
      T7U= R3U*AA+S3U*BB
      T8U=-S3U*AA+R3U*BB

      DT1U= DALU*AA+ALU*DAA+DBEU*BB+BEU*DBB
      DT2U=-DBEU*AA-BEU*DAA+DALU*BB+ALU*DBB
      DT3U= DR1U*AA+R1U*DAA-DS1U*BB-S1U*DBB
      DT4U= DS1U*AA+S1U*DAA+DR1U*BB+R1U*DBB
      DT5U= DR2U*AA+R2U*DAA+DS2U*BB+S2U*DBB
      DT6U=-DS2U*AA-S2U*DAA+DR2U*BB+R2U*DBB
      DT7U= DR3U*AA+R3U*DAA+DS3U*BB+S3U*DBB
      DT8U=-DS3U*AA-S3U*DAA+DR3U*BB+R3U*DBB

c      WRITE(66,*)'AL,BE,DAL,DBE'
c      WRITE(66,*) AL,BE,DAL,DBE
c      WRITE(66,*)'R1,R2,R3'
c      WRITE(66,*) R1,R2,R3
c      WRITE(66,*)'S1,S2,S3'
c      WRITE(66,*) S1,S2,S3
c      WRITE(66,*)'R1U,R2U,R3U'
c      WRITE(66,*) R1U,R2U,R3U
c      WRITE(66,*)'S1U,S2U,S3U'
c      WRITE(66,*) S1U,S2U,S3U
c      WRITE(66,*)'DR1U,DR2U,DR3U'
c      WRITE(66,*) DR1U,DR2U,DR3U
c      WRITE(66,*)'DS1U,DS2U,DS3U'
c      WRITE(66,*) DS1U,DS2U,DS3U
c      WRITE(66,*)'T1,T2,T3,T4,T5,T6,T7,T8'
c      WRITE(66,*) T1,T2,T3,T4,T5,T6,T7,T8
c      WRITE(66,*)'T1U,T2U,T3U,T4U,T5U,T6U,T7U,T8U'
c      WRITE(66,*) T1U,T2U,T3U,T4U,T5U,T6U,T7U,T8U
c      WRITE(66,*)'DT1U,DT2U,DT3U,DT4U,DT5U,DT6U,DT7U,DT8U'
c      WRITE(66,*) DT1U,DT2U,DT3U,DT4U,DT5U,DT6U,DT7U,DT8U

      IF(II.GE.14) GOTO 1
      GOTO(1,2,2,1,2,1,2,1,1,2,2,1,2),II

C     Termes U, Ny, My ,W + Wøø, Vø et Xo Zo  (c.à.d. 1,4,6,8+9,12 et 14
C     ------------------------------------------------------------------
   1  CONTINUE

c      TEMP11=((AL*AA+BE*BB)*(1-EX(I)*CO(I))
c     *                          -EX(I)*SI(I)*(-BE*AA+AL*BB))/U1
c      TEMP22=(-(R2*AA+S2*BB)*(1-EX(I)*CO(I))
c     *                        -EX(I)*SI(I)*(S2*AA-R2*BB))/U3
c      WRITE(66,*)' TEMP1 =',TEMP11,' TEMP2 =',TEMP22
c      TEMP11=( T1U*(1-COS1)  -SIN1*T2U)
c      TEMP22=(-T5U*(1-COS1)  -SIN1*T6U)
c      WRITE(66,*)' TEMP1 =',TEMP11,' TEMP2 =',TEMP22


      TEMP1=PIQ2*(DAL*COS1+DBE*SIN1)*T1U
     *     +PIQ2*(DAL*SIN1-DBE*COS1)*T2U
     *     + (1.-COS1)*DT1U - SIN1*DT2U
      TEMP2=-PIQ2*(DAL*COS1+DBE*SIN1)*T5U
     *      -PIQ2*(DAL*SIN1-DBE*COS1)*T6U
     *      -(1.-COS1)*DT5U + SIN1*DT6U

      DTEMP(II,K,1)=DTEMP(II,K,1)+2.*     (TEMP1*D+TEMP2*   B)
      DTEMP(II,K,2)=DTEMP(II,K,2)+2.*Q*   (TEMP1*C+TEMP2*3.*A)
      DTEMP(II,K,3)=DTEMP(II,K,3)+2.*Q*Q*  TEMP1*B
      DTEMP(II,K,4)=DTEMP(II,K,4)+2.*Q*Q*Q*TEMP1*A

c      WRITE(66,*)' DTEMP1=',TEMP1 ,' DTEMP2= ',TEMP2
c      WRITE(66,*)' DTEMP(II,K,1)=',DTEMP(II,K,1)

      DTEMP(II,K,JJ+1)=-DT1U*D-DT3U*C+DT5U*B+DT7U*A
      DTEMP(II,K,JJ+2)=-DT2U*D-DT4U*C+DT6U*B+DT8U*A
      DTEMP(II,K,JJ+5)=+DT1U*D-DT3U*C-DT5U*B+DT7U*A
      DTEMP(II,K,JJ+6)=+DT2U*D-DT4U*C-DT6U*B+DT8U*A

      DTEMP(II,K,JJ+3)=-DTEMP(II,K,JJ+5)
     *     + P3* ( -DT1U*A )
     *     + P2* ( -DT1U*B + DT3U*3.*A )
     *     + P1* ( -DT1U*C + DT3U*2.*B + DT5U*3.*A )
      DTEMP(II,K,JJ+4)=-DTEMP(II,K,JJ+6)
     *     + P3* ( -DT2U*A )
     *     + P2* ( -DT2U*B + DT4U*3.*A )
     *     + P1* ( -DT2U*C + DT4U*2.*B + DT6U*3.*A )
      DTEMP(II,K,JJ+7)=-DTEMP(II,K,JJ+1)
     *     + P3* (  DT1U*A )
     *     + P2* (  DT1U*B + DT3U*3.*A )
     *     + P1* (  DT1U*C + DT3U*2.*B - DT5U*3.*A )
      DTEMP(II,K,JJ+8)=-DTEMP(II,K,JJ+2)
     *     + P3* (  DT2U*A )
     *     + P2* (  DT2U*B + DT4U*3.*A )
     *     + P1* (  DT2U*C + DT4U*2.*B - DT6U*3.*A )
      GOTO 3

C     Termes V, Wø, Nyx, Ry + Wøøø,Uø etVøø (c.à.d. 2,3,5,7 +10,11,13)
C     ------------------------------------------
   2  TEMP1=PIQ2*(DAL*COS1+DBE*SIN1)*T3U
     *     +PIQ2*(DAL*SIN1-DBE*COS1)*T4U
     *     + (1.-COS1)*DT3U - SIN1*DT4U
      TEMP2=-PIQ2*(DAL*COS1+DBE*SIN1)*T7U
     *      -PIQ2*(DAL*SIN1-DBE*COS1)*T8U
     *      -(1.-COS1)*DT7U + SIN1*DT8U

      DTEMP(II,K,1)=DTEMP(II,K,1)+2.*     (TEMP1*C+TEMP2*   A)
      DTEMP(II,K,2)=DTEMP(II,K,2)+4.*Q*    TEMP1*B
      DTEMP(II,K,3)=DTEMP(II,K,3)+6.*Q*Q*  TEMP1*A

c      WRITE(66,*)' TEMP1=',TEMP1
c      WRITE(66,*)' TEMP2=',TEMP2
c      WRITE(66,*)' DTEMP(II,K,1)=',DTEMP(II,K,1)
c      WRITE(66,*)' '

      DTEMP(II,K,JJ+1)=-DT1U*D-DT3U*C+DT5U*B+DT7U*A
      DTEMP(II,K,JJ+2)=-DT2U*D-DT4U*C+DT6U*B+DT8U*A
      DTEMP(II,K,JJ+5)=-DT1U*D+DT3U*C+DT5U*B-DT7U*A
      DTEMP(II,K,JJ+6)=-DT2U*D+DT4U*C+DT6U*B-DT8U*A

      DTEMP(II,K,JJ+3)=-DTEMP(II,K,JJ+5)
     *     - P3* ( -DT1U*A )
     *     - P2* ( -DT1U*B + DT3U*3.*A )
     *     - P1* ( -DT1U*C + DT3U*2.*B + DT5U*3.*A )
      DTEMP(II,K,JJ+4)=-DTEMP(II,K,JJ+6)
     *     - P3* ( -DT2U*A )
     *     - P2* ( -DT2U*B + DT4U*3.*A )
     *     - P1* ( -DT2U*C + DT4U*2.*B + DT6U*3.*A )
      DTEMP(II,K,JJ+7)=-DTEMP(II,K,JJ+1)
     *     + P3* (  DT1U*A )
     *     + P2* (  DT1U*B + DT3U*3.*A )
     *     + P1* (  DT1U*C + DT3U*2.*B - DT5U*3.*A )
      DTEMP(II,K,JJ+8)=-DTEMP(II,K,JJ+2)
     *     + P3* (  DT2U*A )
     *     + P2* (  DT2U*B + DT4U*3.*A )
     *     + P1* (  DT2U*C + DT4U*2.*B - DT6U*3.*A )
    3 CONTINUE
    5 CONTINUE

    4 CONTINUE
      RETURN
      END
