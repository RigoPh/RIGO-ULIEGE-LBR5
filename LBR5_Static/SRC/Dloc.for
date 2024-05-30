      SUBROUTINE DLOC(ARG,M,PHIL,DISC,DVARC,DTEMP,DVARH,TETA,Q,IJK,
     *             LAMB,WIDTH,IMPR,CHA,NPTS,BKSE,NXI,NBRXI,MT,IS,NSOL)

      IMPLICIT REAL *8(A-H,O-Z)
      REAL*8 LAMB
      DIMENSION ARG(8),NXI(9),Y(21),CHA(100,3,NSOL),NPTS(NSOL),
     *  DISC(720),DVARC(33,9,16),DTEMP(33,9,38),DVARH(33,9,38)
      COMMON /PY/PI,SI(4),CO(4),EX(4)
      COMMON/OPTI3/ BID1(54),BID2(20),DARG(8,9)
C***********************************************************************
C
C    Expression (partiel) des d�riv�es de DISH (cfr subr LOC)
C    -----------------------------------------------------------
C    DVARH(13,9,38) avec 13 variables (U V W� Ny Nyx My Ry W W�� W��� U� ...
C                         9 variables de conception,
C                     et 38 termes (cfr DISH).

C     LES DONNEES SONT CONTENUES  DANS DVARC.
C              DVARC = contient les d�riv�es de DISC

C     LES RESULTATS SONT PLACES DANS "DTEMP" (puis additionner � "DVARH"
C       (LE VECTEUR "DTEMP" EST UN VECTEUR DE TRAVAIL TEMPORAIRE)

C     modif: 14-2-96                                     Cr�er : Fev. 96
C***********************************************************************
c     Ordre des termes ind�p. (1 � 6)
C       1 : const       2 : y           3 : y**2         4 : y**3
C       5 : COS(t�ta+y) 6 : SIN(t�ta+y)
c     Ordre des termes EXP(y) (7 � 38 = 4 x 8 termes)
C       1 : EXP(-Alpha Q Y)            * COS (B�ta Q Y)
C       2 : EXP(-Alpha Q Y)            * SIN (B�ta Q Y)
C       3 : EXP(-Alpha Q (Yo-Y))       * COS (B�ta Q (Yo-Y))
C       4 : EXP(-Alpha Q (Yo-Y))       * SIN (B�ta Q (Yo-Y))
C       5 : EXP(-Alpha Q (2pi-Y))      * COS (B�ta Q (2pi-Y))
C       6 : EXP(-Alpha Q (2pi-Y))      * SIN (B�ta Q (2pi-Y))
C       7 : EXP(-Alpha Q (2pi-(Yo-Y))) * COS (B�ta Q (2pi-(Yo-Y)))
C       8 : EXP(-Alpha Q (2pi-(Yo-Y))) * SIN (B�ta Q (2pi-(Yo-Y)))

c      IF(IMPR.NE.0) WRITE(*,*) ' ENTREE DANS  DBLOC  '

      Y(1)=0.
      IPT=NPTS(IS)
      DW=WIDTH/FLOAT(IPT)
      DO 155 I=1,IPT
 155  Y(I+1)=Y(I)+DW

      QPHIL=Q*PHIL*PI/180.
      C=-2.*BKSE*0.981/(PI*IJK)

c      WRITE(66,*) 'Intervalle Y=',(Y(I),I=1,IPT)
c      WRITE(66,*) 'LAMB=',LAMB


      DO 156 LL=1,IPT
      XI=CHA(LL,2,IS)
      XF=CHA(LL,3,IS)

c      IF(IMPR.NE.0) WRITE(*,*) 'Intervalle ',LL,' CHI et CHF= ',XI,XF


      CALL TR(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DTEMP, 2,1,NXI,NBRXI)
      CALL TR(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DTEMP, 1,2,NXI,NBRXI)
      CALL TR(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DTEMP,29,3,NXI,NBRXI)
      CALL TR(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DTEMP,25,4,NXI,NBRXI)
      CALL TR(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DTEMP,27,5,NXI,NBRXI)
      CALL TR(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DTEMP,24,6,NXI,NBRXI)
      CALL TR(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DTEMP,26,7,NXI,NBRXI)
      CALL TR(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DTEMP, 3,8,NXI,NBRXI)
      CALL TR(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DTEMP,35,9,NXI,NBRXI)
      CALL TR(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DTEMP,38,10,NXI,NBRXI)
      CALL TR(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DTEMP,31,11,NXI,NBRXI)
      CALL TR(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DTEMP,30,12,NXI,NBRXI)
      CALL TR(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DTEMP,33,13,NXI,NBRXI)

      DO 2 J=1,MT
      K1=2+J*2
      K2=12+J*2
      CALL TR(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DTEMP,K1,K2,NXI,NBRXI)
      K1=K1+1
      K2=K2+1
      CALL TR(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DTEMP,K1,K2,NXI,NBRXI)
    2 CONTINUE

      E=C* (DCOS(LAMB*Y(LL+1))-DCOS(LAMB*Y(LL)) )

      J1=6+8*M
      DO 1 II=1,13+2*MT
      DO 1 KK=1,NBRXI
      K=NXI(KK)
      DO 1 J=1,J1
      DVARH(II,K,J)=DVARH(II,K,J)+E*DTEMP(II,K,J)
      DTEMP(II,K,J)=0.
   1  CONTINUE

156   CONTINUE

      RETURN
      END

      SUBROUTINE TR(ARG,M,XI,XF,PHIL,Q,DISC,DVARC,DTEMP,I,II,NXI,NBRXI)
C     ==============
      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION ARG(8),DISC(720),DVARC(33,9,16),DTEMP(33,9,38),NXI(9)
      COMMON /PY/PI,SI(4),CO(4),EX(4)
      COMMON/OPTI3/ BID1(54),BID2(20),DARG(8,9)

C  I = class. des var.: U=2 V=1 W�=29 Ny=25 Nyx=27 My=24 Ry=26 W=3 dans
C                       W��=35  W���=38   U�=31 V�=30      V��=33
C                       Xo,Zo de 4 � 23
C  II= class. des var.: U=1 V=2 W�=3  Ny=4  Nyx=5  My=6  Ry=7  W=8 dans
C                       W��=9   W���=10   U�=11 V�=12      V��=13
C                       Xo,Zo de 14 � 33
C  K = indice des 9 variables de conception  ; K=1,NBRXI
C  J = indice sur Alpha(i) et B�ta(i) ; J=1,M

      PIQ2=2.*PI*Q

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

C     Termes U, Ny, My ,W + W��, V� et Xo Zo  (c.�.d. 1,4,6,8+9,12 et 14
C     ------------------------------------------------------------------
   1  TEMP=T1*COS1+T2*SIN1
      TEMP2= 2.* (  DU1*(-T1+TEMP)
     *   + U1*( DT1 + DAL2*TEMP - COS1*(DT1+DBE2*T2)
     *                          - SIN1*(DT2-DBE2*T1) )  )/U2
      DTEMP(II,K,1)=DTEMP(II,K,1)+TEMP2 * XI
      DTEMP(II,K,2)=DTEMP(II,K,2)+ TEMP2 * P2

c      WRITE(66,*)' TEMP=',TEMP
c      WRITE(66,*)' TEMP2=',TEMP2
c      WRITE(66,*)' XI=',XI
c      WRITE(66,*)' DTEMP(II,K,1)=',DTEMP(II,K,1)
c      WRITE(66,*)' '

      DTEMP(II,K,JJ+1)=-XI*T11 - T33
      DTEMP(II,K,JJ+2)=-XI*T22 - T44
      DTEMP(II,K,JJ+3)=-XF*T11 + T33
      DTEMP(II,K,JJ+4)=-XF*T22 + T44
      DTEMP(II,K,JJ+5)= XI*T11 - T33
      DTEMP(II,K,JJ+6)= XI*T22 - T44
      DTEMP(II,K,JJ+7)= XF*T11 + T33
      DTEMP(II,K,JJ+8)= XF*T22 + T44
      GOTO 3

C     Termes V, W�, Nyx, Ry + W���,U� etV�� (c.�.d. 2,3,5,7 +10,11,13)
C     ------------------------------------------
   2  TEMP=T3*COS1+T4*SIN1
c      WRITE(66,*)'TEMP=',TEMP
      DTEMP(II,K,1)=DTEMP(II,K,1)+ 2.*P3*(-2.*DU1*(T3-TEMP)
     *   + U1*( DT3 + DAL2*TEMP - COS1*(DT3+DBE2*T4)
     *                          - SIN1*(DT4-DBE2*T3) )  )/U3
      DTEMP(II,K,JJ+1)=-XI*T11 - T33
      DTEMP(II,K,JJ+2)=-XI*T22 - T44
      DTEMP(II,K,JJ+3)= XF*T11 - T33
      DTEMP(II,K,JJ+4)= XF*T22 - T44
      DTEMP(II,K,JJ+5)=-XI*T11 + T33
      DTEMP(II,K,JJ+6)=-XI*T22 + T44
      DTEMP(II,K,JJ+7)= XF*T11 + T33
      DTEMP(II,K,JJ+8)= XF*T22 + T44
    3 CONTINUE
    5 CONTINUE

    4 CONTINUE
      RETURN
      END
