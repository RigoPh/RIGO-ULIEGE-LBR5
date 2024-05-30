      SUBROUTINE DLOCZ(ARG,M,XI,PHIL,DISC,DVARC,DTEMP,DVARH,TETA,Q,IJK,
     *                 LAMB,WIDTH,IMPR,CHA,dCHA,NPTS,NXI,NBRXI,MT,IS,     !!!aout04
     *                 NSOL,BZ)											!!!aout04

      IMPLICIT REAL *8(A-H,O-Z)
      REAL*8 LAMB
      DIMENSION ARG(8),NXI(9),Y(21),CHA(100,3,NSOL),NPTS(NSOL),
     *  DISC(720),DVARC(33,9,16),DTEMP(33,9,38),DVARH(33,9,38)

	DIMENSION dCHA(100,NSOL,9),dE(9),BZ(1710),TRANSP(13)  !!!aout04

      COMMON /PY/PI,SI(4),CO(4),EX(4)
      COMMON/OPTI3/ BID1(54),BID2(20),DARG(8,9)

	DATA TRANSP/2,1,29,25,27,24,26,3,35,38,31,30,33/     !!!aout04
c     (permet de passer le l'indice II à l'indice I)       !!!aout04

C***********************************************************************
C
C    Expression (partiel) des dérivées de DISH (cfr subr LOCZ)
C    -----------------------------------------------------------
C     DVARH(13,9,38) avec 13 variables (U V Wø Ny Nyx My Ry W Wøø Wøøø U
C                          9 variables de conception,
C                      et 38 termes (cfr DISH).

C       DVARC = contient les dérivées de DISC

C     modif: 19-3-96                                     Créer : 18-3-96
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

      PI1=PI/180
      PHIL1=PHIL*PI1
      TETA1=TETA*PI1

c      WRITE(66,*)' TETA et TETA1=',TETE,TETA1
c      WRITE(66,*)' PHIL  PHIL1=',  PHIL,PHIL1

      IF(DABS(TETA).NE.180.) GOTO 4
      COST=-1.
      SINT=0.
      COSTT=-DCOS(PHIL1)
      SINTT=-DSIN(PHIL1)
      GOTO 6
   4  IF(DABS(TETA).NE.90.) GOTO 5
      II=1
      IF(TETA.EQ.-90.) II=-1
      COST=0.
      SINT=II
      COSTT=-DSIN(PHIL1)*II
      SINTT=DCOS(PHIL1)*II
      GOTO 6
    5 COST=DCOS(TETA1)
      SINT=DSIN(TETA1)
      COSTT=DCOS(TETA1+PHIL1)
      SINTT=DSIN(TETA1+PHIL1)
    6 CONTINUE

c      WRITE(66,*)' II=',II
c      WRITE(66,*)' COST    SINT=',COST,SINT
c      WRITE(66,*)' COSTT  SINTT=',COSTT,SINTT

      CALL AR(ARG,M,PHIL,Q,DISC,DVARC,DTEMP, 2,1,
     *        SINT,COST,SINTT,COSTT,NXI,NBRXI)
      CALL AR(ARG,M,PHIL,Q,DISC,DVARC,DTEMP, 1,2,
     *        SINT,COST,SINTT,COSTT,NXI,NBRXI)
      CALL AR(ARG,M,PHIL,Q,DISC,DVARC,DTEMP,29,3,
     *        SINT,COST,SINTT,COSTT,NXI,NBRXI)
      CALL AR(ARG,M,PHIL,Q,DISC,DVARC,DTEMP,25,4,
     *        SINT,COST,SINTT,COSTT,NXI,NBRXI)
      CALL AR(ARG,M,PHIL,Q,DISC,DVARC,DTEMP,27,5,
     *        SINT,COST,SINTT,COSTT,NXI,NBRXI)
      CALL AR(ARG,M,PHIL,Q,DISC,DVARC,DTEMP,24,6,
     *        SINT,COST,SINTT,COSTT,NXI,NBRXI)
      CALL AR(ARG,M,PHIL,Q,DISC,DVARC,DTEMP,26,7,
     *        SINT,COST,SINTT,COSTT,NXI,NBRXI)
      CALL AR(ARG,M,PHIL,Q,DISC,DVARC,DTEMP, 3,8,
     *        SINT,COST,SINTT,COSTT,NXI,NBRXI)
      CALL AR(ARG,M,PHIL,Q,DISC,DVARC,DTEMP,35,9,
     *        SINT,COST,SINTT,COSTT,NXI,NBRXI)
      CALL AR(ARG,M,PHIL,Q,DISC,DVARC,DTEMP,38,10,
     *        SINT,COST,SINTT,COSTT,NXI,NBRXI)
      CALL AR(ARG,M,PHIL,Q,DISC,DVARC,DTEMP,31,11,
     *        SINT,COST,SINTT,COSTT,NXI,NBRXI)
      CALL AR(ARG,M,PHIL,Q,DISC,DVARC,DTEMP,30,12,
     *        SINT,COST,SINTT,COSTT,NXI,NBRXI)
      CALL AR(ARG,M,PHIL,Q,DISC,DVARC,DTEMP,33,13,
     *        SINT,COST,SINTT,COSTT,NXI,NBRXI)
      DO 2 J=1,MT
      K1=2+J*2
      K2=12+J*2
      CALL AR(ARG,M,PHIL,Q,DISC,DVARC,DTEMP,K1,K2,
     *        SINT,COST,SINTT,COSTT,NXI,NBRXI)
      K1=K1+1
      K2=K2+1
      CALL AR(ARG,M,PHIL,Q,DISC,DVARC,DTEMP,K1,K2,
     *        SINT,COST,SINTT,COSTT,NXI,NBRXI)
    2 CONTINUE

      C=-2./(PI*IJK*10000.)
      E=0.
	CALL ANNULD(dE,9)		!!!aout04
      Y(1)=0.
      IPT=NPTS(IS)
      DW=WIDTH/FLOAT(IPT)
      DO 155 I=1,IPT
 155  Y(I+1)=Y(I)+DW
      DO 156 I=1,IPT
	E=E+CHA(I,1,IS)*(DCOS(LAMB*Y(I+1))-DCOS(LAMB*Y(I)))				!!!aout04
      DO K=1,9														!!!aout04
      dE(K)=dE(K) + 
     *            dCHA(I,IS,K)*(DCOS(LAMB*Y(I+1))-DCOS(LAMB*Y(I)))	!!!aout04
 	ENDDO															!!!aout04
 156	CONTINUE
      E=E*C*XI
	DO K=1,9                !!!aout04
	dE(K)=dE(K)*C*XI        !!!aout04
	ENDDO                   !!!aout04

      J1=6+8*M
      DO 1 II=1,13+2*MT
      DO 1 KC=1,NBRXI
      K=NXI(KC)
      DO 1 J=1,J1
	JJ=J-1													!!!aout04
	I=(TRANSP(II)-1)*38 + 1 + JJ							!!!aout04
	B=BZ(I)													!!!aout04
      DVARH(II,K,J)=DVARH(II,K,J)+E*DTEMP(II,K,J)+dE(K)*B		!!!aout04
   1  DTEMP(II,K,J)=0.

C     IF(IGH.EQ.0) STOP
      RETURN
      END

      SUBROUTINE AR(ARG,M,PHIL,Q,DISC,DVARC,DTEMP,I,II,
     *              SINT,COST,SINTT,COSTT,NXI,NBRXI)
C     ==============
      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION ARG(8),DISC(720),DVARC(33,9,16),DTEMP(33,9,38),NXI(9)
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

C  BOUCLE SUR LES VARIABLES (1 à NBRXI)
      DO 4 KC=1,NBRXI
        K=NXI(KC)

      DO 5 J=1,M
      L=4*(J-1)
      JK=(I-1)*16+L
      IJ=8*J-2

      J2=J*2
      J1=J2-1
      AL=ARG(J1)
      BE=ARG(J2)
      DAL=DARG(J1,K)
      DBE=DARG(J2,K)
      DAL2=PIQ2*DAL
      DBE2=PIQ2*DBE

c      WRITE(66,*)'AL,BE,DAL,DBE'
c      WRITE(66,'(6E14.7)')AL,BE,DAL,DBE

      U1= AL*AL+BE*BE
      U2= U1*U1
      DU1=2.*( AL*DAL+BE*DBE)
      DR1=2.*(-AL*DAL+BE*DBE)
      S1=2.*AL*BE
      DS1=2.*( AL*DBE+BE*DAL)

c      WRITE(66,*)'U1,DU1,S1,DS1,DR1'
c      WRITE(66,'(6E14.7)')U1,DU1,S1,DS1,DR1

      UP=U1+1./(Q*Q)
      UM=U1-1./(Q*Q)
      DUP=2.*(UP*DAL+AL*DU1)
      DUM=2.*(UM*DBE+BE*DU1)
      BP=BE+1./Q
      BM=BE-1./Q

c      WRITE(66,*)'UP,UM,DUP,DUM,BP,BM'
c      WRITE(66,'(6E12.5)')UP,UM,DUP,DUM,BP,BM

      USP=UP-2.*BE*BE
      Z1=AL*AL+BP*BP
      Z2=AL*AL+BM*BM
      Z11=Z1*Z1
      Z22=Z2*Z2
      Z3=Z1*Z2
      Z4=Z3*Z3
      DZ1=2.*(AL*DAL+BP*DBE)
      DZ2=2.*(AL*DAL+BM*DBE)

c      WRITE(66,*)'USP,Z1,Z2,DZ1,DZ2'
c      WRITE(66,'(6E14.7)')USP,Z1,Z2,DZ1,DZ2

      BPP=BP/(2.*Z1)
      BMM=BM/(2.*Z2)
      AP =AL/(2.*Z1)
      AM =AL/(2.*Z2)
      DBMM=(-DZ2*BM + DBE*Z2)/(2.*Z22)
      DBPP=(-DZ1*BP + DBE*Z1)/(2.*Z11)
      DAP =(-DZ1*AL + DAL*Z1)/(2.*Z11)
      DAM =(-DZ2*AL + DAL*Z2)/(2.*Z22)

c      WRITE(66,*)'BPP,BMM,AP,AM'
c      WRITE(66,'(6E14.7)')BPP,BMM,AP,AM
c      WRITE(66,*)'DBPP,DBMM,DAP,DAM'
c      WRITE(66,'(6E14.7)')DBPP,DBMM,DAP,DAM

      COS1=EX(J)*CO(J)
      SIN1=EX(J)*SI(J)

c      WRITE(66,*)' COS1    SIN1=',COS1,SIN1
c      WRITE(66,*)' COST    SINT=',COST,SINT
c      WRITE(66,*)' COSTT  SINTT=',COSTT,SINTT

      AA=DISC(JK+1)
      BB=DISC(JK+2)
      DAA=DVARC(II,K,L+1)
      DBB=DVARC(II,K,L+2)
      T3=-2.*(USP*AA+S1 *BB)
      T4= 2.*(S1 *AA-USP*BB)
      DT3=-2.*(-AA*DR1+BB *DS1+USP*DAA+S1*DBB)
      DT4= 2.*( S1*DAA-USP*DBB+DS1* AA+DR1*BB)

      TEMP3= 2.*(BE*UM*SIN1-AL*UP*COS1)
      TEMP4=-2.*(AL*UP*SIN1+BE*UM*COS1)
      DNOM=2.*UP*DU1-8.*BE*DBE/(Q*Q)

c      WRITE(66,*)' AA,BB,DAA,DBB'
c      WRITE(66,'(6E14.7)')AA,BB,DAA,DBB
c      WRITE(66,*)'T3,T4'
c      WRITE(66,'(6E14.7)')T3,T4
c      WRITE(66,*)'DT3,DT4'
c      WRITE(66,'(6E14.7)')DT3,DT4

      IF(II.GE.14) GOTO 1
      GOTO(1,2,2,1,2,1,2,1,1,2,2,1,2),II

C     Termes U, Ny, My ,W + Wøø, Vø et Xo Zo  (c.à.d. 1,4,6,8+9,12 et 14
C     ------------------------------------------------------------------
   1  TEMP= 2.*(DAA*AL*UP+DBB*BE*UM) + AA*DUP+BB*DUM
     *  + DAA*TEMP3 + DBB* TEMP4
     *  + PIQ2*( -DAL*(AA*TEMP3+BB*TEMP4)+DBE*(-AA*TEMP4+BB*TEMP3) )
     *  + DUP*(AA*SIN1-BB*COS1) - DUM*(AA*COS1+BB*SIN1)
      TEMP2 =  (AA*(TEMP3+2.*AL*UP)+BB*(TEMP4+2.*BE*UM)) * DNOM
      DTEMP(II,K,6)=DTEMP(II,K,6) + (Z3*TEMP-TEMP2)/ Z4

c      WRITE(66,*)'Z3 et DZ3=DNOM'
c      WRITE(66,'(6E14.7)')Z3,DNOM
c      FCTIE=(AA*(TEMP3+2.*AL*UP)+BB*(TEMP4+2.*BE*UM))
c      WRITE(66,*)'FCT et DFCT'
c      WRITE(66,'(6E14.7)')FCTIE,TEMP

c      WRITE(66,*)' Z4=',Z4
c      WRITE(66,*)' Q=',Q
c      WRITE(66,*)' DTEMP(II,K,6)=',DTEMP(II,K,6)
c      WRITE(66,*)' '

      DTEMP(II,K,IJ+1)=COST*( AA*(-DBMM+DBPP)+ BB*(DAM-DAP)
     *                      +DAA*(- BMM+ BPP)+DBB*( AM- AP))
     *              +  SINT*( AA*(-DAM-DAP)  + BB*(-DBMM-DBPP)
     *                      +DAA*(- AM- AP)  +DBB*(- BMM- BPP) )
      DTEMP(II,K,IJ+2)=COST*( AA*(-DAM+DAP)+ BB*(-DBMM+DBPP)
     *                      +DAA*(- AM+ AP)+DBB*( -BMM+ BPP))
     *  +  SINT* (  AA*(DBMM+DBPP)+ BB*(-DAM-DAP)
     *               +DAA*( BMM+ BPP)+DBB*(- AM- AP) )

      DTEMP(II,K,IJ+3)=COSTT*( AA*(DBMM-DBPP)+ BB*(-DAM+DAP)
     *                       +DAA*( BMM- BPP)+DBB*(- AM+ AP))
     *  +  SINTT* (  AA*(-DAM-DAP)+ BB*(-DBMM-DBPP)
     *                +DAA*(- AM- AP)+DBB*(- BMM- BPP) )
      DTEMP(II,K,IJ+4)=COSTT*( AA*(DAM-DAP)+ BB*(DBMM-DBPP)
     *                       +DAA*( AM- AP)+DBB*( BMM- BPP))
     *  +  SINTT* (  AA*(DBMM+DBPP)+ BB*(-DAM-DAP)
     *                +DAA*( BMM+ BPP)+DBB*(- AM- AP) )

      DTEMP(II,K,IJ+5)=-COST*( AA*(DBMM-DBPP)+ BB*(-DAM+DAP)
     *                      +DAA*( BMM- BPP)+DBB*(- AM+ AP))
     *  -  SINT* (  AA*(-DAM-DAP)+ BB*(-DBMM-DBPP)
     *               +DAA*(- AM- AP)+DBB*(- BMM- BPP) )
      DTEMP(II,K,IJ+6)=-COST*( AA*(DAM-DAP)+ BB*(DBMM-DBPP)
     *                      +DAA*( AM- AP)+DBB*( BMM- BPP))
     *  -  SINT* (  AA*(DBMM+DBPP)+ BB*(-DAM-DAP)
     *               +DAA*( BMM+ BPP)+DBB*(- AM- AP) )

      DTEMP(II,K,IJ+7)=-COSTT*( AA*(-DBMM+DBPP)+ BB*(DAM-DAP)
     *                      +DAA*(- BMM+ BPP)+DBB*( AM- AP))
     *  -  SINTT* (  AA*(-DAM-DAP)+ BB*(-DBMM-DBPP)
     *               +DAA*(- AM- AP)+DBB*(- BMM- BPP) )
      DTEMP(II,K,IJ+8)=-COSTT*( AA*(-DAM+DAP)+ BB*(-DBMM+DBPP)
     *                       +DAA*(- AM+ AP)+DBB*( -BMM+ BPP))
     *  -  SINTT* (  AA*(DBMM+DBPP)+ BB*(-DAM-DAP)
     *                +DAA*( BMM+ BPP)+DBB*(- AM- AP) )

      GOTO 3

C     Termes V, Wø, Nyx, Ry + Wøøø, Uø et Vøø (c.à.d. 2,3,5,7 +10,11,13)
C     ------------------------------------------------------------------
   2  TEMP=T3*COS1+T4*SIN1
      DTEMP(II,K,5)=DTEMP(II,K,5) + (
     * Z3*(DT3 + DAL2*TEMP - COS1*(DT3+DBE2*T4) - SIN1*(DT4-DBE2*T3) )
     *  - (T3-TEMP)*DNOM      )/(Z4*Q)
c      WRITE(66,*)'TEMP=',TEMP

      DTEMP(II,K,IJ+1)=COST*( AA*(-DBMM+DBPP)+ BB*(DAM-DAP)
     *                      +DAA*(- BMM+ BPP)+DBB*( AM- AP))
     *  +  SINT* (  AA*(-DAM-DAP)+ BB*(-DBMM-DBPP)
     *               +DAA*(- AM- AP)+DBB*(- BMM- BPP) )
      DTEMP(II,K,IJ+2)=COST*( AA*(-DAM+DAP)+ BB*(-DBMM+DBPP)
     *                      +DAA*(- AM+ AP)+DBB*( -BMM+ BPP))
     *  +  SINT* (  AA*(DBMM+DBPP)+ BB*(-DAM-DAP)
     *               +DAA*( BMM+ BPP)+DBB*(- AM- AP) )

      DTEMP(II,K,IJ+3)=-COSTT*( AA*(DBMM-DBPP)+ BB*(-DAM+DAP)
     *                       +DAA*( BMM- BPP)+DBB*(- AM+ AP))
     *  -  SINTT* (  AA*(-DAM-DAP)+ BB*(-DBMM-DBPP)
     *                +DAA*(- AM- AP)+DBB*(- BMM- BPP) )
      DTEMP(II,K,IJ+4)=-COSTT*( AA*(DAM-DAP)+ BB*(DBMM-DBPP)
     *                       +DAA*( AM- AP)+DBB*( BMM- BPP))
     *  -  SINTT* (  AA*(DBMM+DBPP)+ BB*(-DAM-DAP)
     *                +DAA*( BMM+ BPP)+DBB*(- AM- AP) )

      DTEMP(II,K,IJ+5)=COST*( AA*(DBMM-DBPP)+ BB*(-DAM+DAP)
     *                      +DAA*( BMM- BPP)+DBB*(- AM+ AP))
     *  +  SINT* (  AA*(-DAM-DAP)+ BB*(-DBMM-DBPP)
     *               +DAA*(- AM- AP)+DBB*(- BMM- BPP) )
      DTEMP(II,K,IJ+6)=COST*( AA*(DAM-DAP)+ BB*(DBMM-DBPP)
     *                      +DAA*( AM- AP)+DBB*( BMM- BPP))
     *  +  SINT* (  AA*(DBMM+DBPP)+ BB*(-DAM-DAP)
     *               +DAA*( BMM+ BPP)+DBB*(- AM- AP) )

      DTEMP(II,K,IJ+7)=-COSTT*( AA*(-DBMM+DBPP)+ BB*(DAM-DAP)
     *                      +DAA*(- BMM+ BPP)+DBB*( AM- AP))
     *  -  SINTT* (  AA*(-DAM-DAP)+ BB*(-DBMM-DBPP)
     *               +DAA*(- AM- AP)+DBB*(- BMM- BPP) )
      DTEMP(II,K,IJ+8)=-COSTT*( AA*(-DAM+DAP)+ BB*(-DBMM+DBPP)
     *                       +DAA*(- AM+ AP)+DBB*( -BMM+ BPP))
     *  -  SINTT* (  AA*(DBMM+DBPP)+ BB*(-DAM-DAP)
     *                +DAA*( BMM+ BPP)+DBB*(- AM- AP) )

    3 CONTINUE
    5 CONTINUE

    4 CONTINUE
      RETURN
      END
