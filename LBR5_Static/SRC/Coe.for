      SUBROUTINE COE(CONST,ARG,COEFF,LAM,ETA,ETA2,MT,M,Z,X,W,A,B,C,F,
     *               H,G,IMPR,Q,NBRXI,NXI,DCOEFF,DARG,CONST2,CONST3,XX,
     *               IOPTI,ITYPE)											!février 2004

      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 LAM(8),LAMB

      DIMENSION CONST(74),ARG(8),COEFF(32,41),DARG(8,9),
     *  DCOEFF(32,33,9),CONST2(6,9),CONST3(2,10),NXI(9),XX(1)


      CALL COEE(CONST,ARG,COEFF,LAM,ETA,ETA2,MT,M,Z,X,W,A,B,C,F,
     *          H,G,IMPR,Q,NBRXI,NXI,DCOEFF,DARG,CONST2,CONST3,IOPTI,
     *  XX(1) ,XX(5), XX(9),XX(13),XX(17),XX(21),XX(25),XX(29), ! AU1 à AU8
     *  XX(33),XX(41),XX(49),XX(57),                            !ARG2 à ARG5
     *  XX(65),XX(66),XX(67),XX(68),XX(69),XX(70),XX(71),XX(72),XX(73),
     *  XX(74),XX(75),XX(76),XX(77),XX(78),XX(79),XX(80),XX(81),XX(82),
     *  XX(83),XX(84),XX(85),XX(86),XX(87),XX(88),XX(89),XX(90),XX(91),
     *  XX(92),XX(93),XX(94),XX(95),XX(96),XX(97),XX(98),XX(99),XX(100),
     *  XX(101),XX(102),XX(103),XX(104),XX(105),XX(106),XX(107),XX(108),
     *  XX(109),XX(110),XX(111),XX(112),XX(113),XX(114),XX(115),XX(116),
     *  XX(117),XX(118),XX(119),XX(120),XX(121),XX(122),XX(123),XX(124),
     *  XX(125),XX(126),XX(127),XX(128),XX(129),XX(130),XX(131),XX(132),
     *  XX(133),XX(134),XX(135),XX(136),XX(137),XX(138),XX(139),XX(140),
     *  XX(141),XX(142),XX(143),XX(144),XX(145),XX(146),ITYPE)			!février 2004

	RETURN
	END


C***********************************************************************
C***********************************************************************


      SUBROUTINE COEE(CONST,ARG,COEFF,LAM,ETA,ETA2,MT,M,Z,X,W,A,B,C,F,
     *           H,G,IMPR,Q,NBRXI,NXI,DCOEFF,DARG,CONST2,CONST3,IOPTI,
     *  AU1,AU2,AU3,AU4,AU5,AU6,AU7,AU8,ARG2,ARG3,ARG4,ARG5,
     *  QB,QC,QD,QE,QF,QG,QH,QI,QJ,QJJ,QK,QK3,QL,QL2,QM,SS,TT,LAMB,P,		!février 2004
     *  PP,R,RR,DP,DPP,DR,DRR,AUX1,AUX2,AUX3,AUX4,AUX5,AUX6,AUX7,AN1,
     *  AM1,AA,AAA,DAA,DAAA,BB,BBB,DBB,DBBB,SA,SB,SC,SD,SE,SF,SG,SL,
     *  R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R14,R15,R16,R17,
     *  S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,ITYPE)			!février 2004

      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 LAM(8),LAMB

      DIMENSION CONST(74),ARG(8),COEFF(32,41),ISIGN(41),DARG(8,9),
     *  DCOEFF(32,33,9),CONST2(6,9),CONST3(2,10),ISIGN2(33),NXI(9)

      DIMENSION AU1(4),AU2(4),AU3(4),AU4(4),AU5(4),AU6(4),AU7(4),
     *          AU8(4),ARG2(8),ARG3(8),ARG4(8),ARG5(8)

c     COMMON/ALLONG/AU1(4),AU2(4),AU3(4),AU4(4),AU5(4),AU6(4),AU7(4),
c     *  AU8(4),ARG2(8),ARG3(8),ARG4(8),ARG5(8),
c     *  QB,QC,QD,QE,QF,QG,QH,QI,QJ,QJJ,QK,QK3,QL,QL2,QM,LAMB,P,
c     *  PP,R,RR,DP,DPP,DR,DRR,AUX1,AUX2,AUX3,AUX4,AUX5,AUX6,AUX7,AN1,
c     *  AM1,AA,AAA,DAA,DAAA,BB,BBB,DBB,DBBB,SA,SB,SC,SD,SE,SF,SG,SL,
c     *  R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R14,R15,R16,R17,
c     *  S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12

      DATA ISIGN/1,-1,-1,1,1,-1,-1,1,1,-1,-1,-1,1,-1,1,1,
     *           1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
     *           1,-1,-1,1,-1/
      DATA ISIGN2/1,-1,-1,1,-1,1,-1,1,1,-1,-1,1,-1,
     *            1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1/
C***********************************************************************
C     SUBROUTINE COE
C     SUBROUTINE DE CALCUL DES COEFFICIENTS DES ELEMENTS DE REDUCTION
C     PAR COMBINAISON DES COEFFICIENTS DE L'EQUATION DE DEPLACEMENT W.
C                                     Créer (LBR3): Thèse (Ph. Rigo)
C     Modif : 12-6-95                       (LBR5): Janvier 94
C             13-4-99						  (LBR5.1):Avril  99
C***********************************************************************
C  INPUT:
C      Var. de concept. = Epaisseur du bordage (K=1)
C      ---------------------------------------------
c        CONST(1) = D  -> CONST2(1,1)=d(D)/d(épaiss)
c        CONST(2) = K  -> CONST2(2,1)=d(K)/d(épaiss)
c        CONST(4) = Hy -> CONST2(3,1)=d(Hy)/d(épaiss)   CADRE
c        CONST(6) = Ry -> CONST2(4,1)=d(Ry)/d(épaiss)   CADRE
c        CONST(10)= Hx -> CONST2(5,1)=d(Hx)/d(épaiss)   raidisseur
c        CONST(12)= Rx -> CONST2(6,1)=d(Rx)/d(épaiss)   raidisseur

c        Traverses  Hx -> CONST3(1,MT)=d(H)/d(épaiss)
c                   Rx -> CONST3(2,MT)=d(R)/d(épaiss)

C      Var. de concept. relative aux Aig.(k=2,5) et aux Raid.(k=6,9).
C      --------------------------------------------------------------
C           CADRES                         RAIDISSEURS
C      Constante      Dérivée(k=2,5)   Constante       Dérivée(k=6,9)
C    1 CONST(3)= Oy -> CONST2(1,k)     CONST( 9)= Ox -> CONST2(1,k)
C    2 CONST(4)= Hy -> CONST2(2,k)     CONST(10)= Hx -> CONST2(2,k)
C    3 CONST(5)= Sy -> CONST2(3,k)     CONST(11)= Sx -> CONST2(3,k)
C    4 CONST(6)= Ry -> CONST2(4,k)     CONST(12)= Rx -> CONST2(4,k)
C    5 CONST(7)= Ty -> CONST2(5,k)     CONST(13)= Tx -> CONST2(5,k)
C    6 CONST(8)= Ly -> CONST2(6,k)     CONST(14)= Lx -> CONST2(6,k)

C      A= D (1+ETA)/2+Sx+Sy         Z= D (1-ETA)/2+(Sx+Sy)
C      B= (K+Rx)                    G= D*ETA
C      C= (D+Ox)                    H= D + 2(Sx+Sy)
C      X= (K+Ry)                    F= Lx + Ly
C      W= (D+Oy)                    ETA2 = (1-ETA)/2
C                                   ETA3 = (1+ETA)/2

c      AU1(I)=2 A B           avec A=alpha=ARG(2I-1) et B=béta=ARG(2I)
c      AU2(I)=A**2-B**2
c      AU3(I)=-4(A**3 B - A B**3)
c      AU4(I)=A**4+B**4-6 A**2 B**2
c      AU5(I)=+(A**3 - 3 A B**2)
c      AU6(I)= -(3 A**2 B - B**3)
c      AU7(I)= A**5-10 A**3 B**2 + 5 A B**4
c      AU8(I)= B**5-10 B**3 A**2 + 5 B A**4

C  OUTPUT: (Les coef. COEFF correspondent à :
C     1 2 3  4  5   6   7  8  9  10  11  12 13 14 15 16 17   18  37  38   39 40  41
C     U V Wø NY NX NYX NXY MY MX MYX MXY QY QX RY RX W X0DY Z0DY Wøø Wøøø Uø Uøø Vø

C     Dérivées de COEFF (càd DCOEFF) sont dans l'ordre suivant:
C     1 2 3  4   5  6  7   8  9   10   11 12 13      14 15  à  32  33
C     U V Wø NY NYX MY RY  W  Wøø Wøøø Uø Vø  Vøø    Xo  Zo    Xo  Zo

	IF(ITYPE.EQ.5)THEN												!février 2004
	ETA=0.000														!février 2004
	CONST(1)=0														!février 2004
	CONST(2)=0														!février 2004
	ENDIF															!février 2004

      LAMB=LAM(1)                                                       
      ETA3=(1.+ETA)/2.                                 ! (1+u)/2
      QB=CONST(2)+CONST(13)                            ! K+Tx
      QC=CONST(2)+CONST(7)                             ! K+Ty
      QD=CONST(2)*(2.-ETA)+CONST(13)+CONST(7)          ! K(2-u)+Tx+Ty
      QE=CONST(2)*(1.-ETA)+CONST(13)                   ! K(1-u)+Tx
      QF=CONST(2)*(1.-ETA)+CONST(7)                    ! K(1-u)+Ty
      QG=(Z*LAMB/Q-LAM(3)*CONST(10))                   ! Z*L/q - Hx L**3
      QH=LAM(5)*CONST(10)+LAM(3)*G/Q                   ! Hx L**5 + L**3 D*ETA/q
      QI=W*C-G*H      
      QJJ=-CONST(10)*LAM(4)-G*LAM(2)/Q
      QJ=QJJ*A + W*C*LAM(2)/Q
      QK=C*CONST(4)*LAM(2)+W*Z/Q
      QK3=3.*QK
      QL=W*QG
      QL2=2.*QL
      QM=Z*QH
	SS=CONST(5)+CONST(11)						!Sx+Sy		!février 2004
	TT=CONST(7)+CONST(13)					    !Tx+Ty		!février 2004

      S1=C*ETA2+Z
      S2=W*ETA2+Z 		 	
      S3=C+W-G-ETA*H  
      S4=4.*W*Z
      S5=2.*LAM(2)*QI
      S6=LAMB*CONST(4)*A
      S7=W*ETA2*LAMB/Q + QG
      S8=LAMB*CONST(4)*ETA3
      S9=5.*CONST(4)*Z 
      S10=QJJ*ETA3 - (ETA*A-(W+C))*LAM(2)/Q
      S11=CONST(4)*LAM(2)+S2/Q 
      S12=Z*LAM(3)*ETA/Q+ETA2*QH 
	S13=CONST(9)*SS*LAM(4)								!février 2004
	S14=CONST(3)*CONST(9)*LAM(2)						!février 2004
	S15=CONST(3)*SS										!février 2004

      DO 13 I=1,M 
      IF(NBRXI.EQ.0) THEN
        DO 1 J=2*I-1,2*I
        ARG2(J)=ARG(J)*ARG(J)
        ARG3(J)=ARG2(J)*ARG(J)
        ARG4(J)=ARG2(J)*ARG2(J)
   1    ARG5(J)=ARG3(J)*ARG2(J)
        AU1(I)=2.*ARG(2*I-1)*ARG(2*I)                    ! 2 A B
        AU2(I)=ARG2(2*I-1)-ARG2(2*I)                     ! A**2-B**2
        AU3(I)=-2.*AU1(I)*AU2(I)                         ! -4(A**3 B - A B**3)
        AU4(I)=ARG4(2*I-1)+ARG4(2*I)-6.*ARG2(2*I-1)*ARG2(2*I) ! A**4+B**4-6 A**2 B**2
        AU5(I)=ARG3(2*I-1)-3.*ARG(2*I-1)*ARG2(2*I)            ! +(A**3 - 3 A B**2)
        AU6(I)=ARG3(2*I)-3.*ARG(2*I)*ARG2(2*I-1)              ! -(3 A**2 B - B**3)
        AU7(I)=ARG5(2*I-1)-10.*ARG3(2*I-1)*ARG2(2*I)  !
     *                  +5.*ARG(2*I-1)*ARG4(2*I)      ! A**5-10 A**3 B**2 + 5 A B**4
        AU8(I)=ARG5(2*I)-10.*ARG3(2*I)*ARG2(2*I-1)    !
     *                  +5.*ARG(2*I)*ARG4(2*I-1)      ! B**5-10 B**3 A**2 + 5 B A**4
      ELSE
C       Ces coéf. sont déjà calculés dans Subr COMPLEX (voir COMMON/ALLONG/)
        IF(IMPR.NE.0) THEN
        WRITE(66,*) ' '
        WRITE(66,*) 'Solution nø= ',I
        WRITE(66,*) '*****************'
        ENDIF
      ENDIF
      IA=2*I-1
      IB=2*I
      I1=8*I-7
      I2=8*I-6
      R1=AU2(I)*W*LAMB/Q - AU4(I)*CONST(4)*LAMB + QH
      R2=-QJJ*ARG(IA) + AU5(I)*W/Q - AU7(I)*CONST(4)
      R3= QJJ*ARG(IB)   + AU6(I)*W/Q + AU8(I)*CONST(4)
      R4= LAM(4)*C + AU4(I)*W + AU2(I)*2.*LAM(2)*G 
      R5= AU4(I)*Z - AU2(I)*LAM(2)*C
      R6=AU3(I)*Z + AU1(I)*LAM(2)*C
      R7=-AU1(I)*2.*LAM(2)*G + AU3(I)*W
      R8=AU1(I)*W*LAMB/Q + AU3(I)*LAMB*CONST(4)
      R9=-ARG(IA)*C*LAM(2)/Q + AU5(I)*Z/Q
      R10=AU5(I)*C*LAM(2) - AU7(I)*Z
      R11= ARG(IB)*C*LAM(2)/Q + AU6(I)*Z/Q 
      R12= AU6(I)*C*LAM(2) + AU8(I)*Z
      R14=-AU2(I)*LAM(2)*W + LAM(4)*Z 
      R15=R14*LAMB
      R16=-ARG(IA)*W*LAM(2)/Q + AU5(I)*CONST(4)*LAM(2)
      R17= ARG(IB)  *W*LAM(2)/Q + AU6(I)*CONST(4)*LAM(2)
      R18=-W*LAM(3)*AU2(I)+Z*LAM(5)
      R19=LAM(2)*C*AU5(I)-Z*AU7(I)
      R20=LAM(2)*C*AU6(I)+Z*AU8(I)

 	IF(ITYPE.NE.5)THEN												!février 2004
      AM1=Z* (W*AU4(I)+C*LAM(4)) - QI*LAM(2)*AU2(I)
      AN1=W*Z*AU3(I) + QI*LAM(2)*AU1(I)
        AUX1=AM1*AM1+AN1*AN1
        AUX2=AUX1*AUX1                     !   Calcul U
      P= QL*AU2(I) - AU4(I)*S6 + QM        !   ********
      R=-QL*AU1(I) - AU3(I)*S6
        AUX3=P*AM1+R*AN1
        AUX4=R*AM1-P*AN1
      COEFF(I1,1)=AUX3/AUX1
      COEFF(I2,1)=AUX4/AUX1
      AA=COEFF(I1,1)
      BB=COEFF(I2,1)

c      WRITE(66,*) 'AM1= ',AM1,' AN1= ',AN1
c      WRITE(66,*) 'P= ',P,' R= ',R
c      WRITE(66,*) 'AA= ',AA,' BB= ',BB

      PP=-ARG(IA)*QJ  + AU5(I)*QK - AU7(I)*CONST(4)*Z    !   Calcul V
      RR= ARG(IB)*QJ  + AU6(I)*QK + AU8(I)*CONST(4)*Z    !   ********
        AUX5=PP*AM1+RR*AN1
        AUX6=RR*AM1-PP*AN1                                              
      COEFF(I1,2)=AUX5/AUX1
      COEFF(I2,2)=AUX6/AUX1
      AAA=COEFF(I1,2)
      BBB=COEFF(I2,2)
	ELSE																	!février 2004			
	AM1=S13-(S14*AU2(I))+(S15*AU4(I))										!février 2004
      AN1=CONST(3)*SS*AU3(I)+CONST(3)*CONST(9)*LAM(2)*AU1(I)					!février 2004
        AUX1=AM1*AM1+AN1*AN1													!février 2004
        AUX2=AUX1*AUX1					                    !   Calcul U	!février 2004
      P=-CONST(10)*CONST(3)*LAM(3)*AU2(I)+CONST(10)*SS*LAM(5) !   ********	!février 2004
     *	+((CONST(3)*SS*LAMB*AU2(I))/Q)-CONST(4)*SS*LAMB*AU4(I)				!février 2004
      R=CONST(10)*CONST(3)*LAM(3)*AU1(I)										!février 2004
     *	-((CONST(3)*SS*LAMB*AU1(I))/Q)-CONST(4)*SS*LAMB*AU3(I)				!février 2004
        AUX3=P*AM1+R*AN1														!février 2004
        AUX4=R*AM1-P*AN1														!février 2004
      COEFF(I1,1)=AUX3/AUX1													!février 2004
      COEFF(I2,1)=AUX4/AUX1													!février 2004
      AA=COEFF(I1,1)															!février 2004
      BB=COEFF(I2,1)															!février 2004
      PP=CONST(10)*SS*LAM(4)*ARG(IA)								!  Calcul V	!février 2004
     *		+CONST(9)*CONST(4)*LAM(2)*AU5(I)+((CONST(3)*SS*AU5(I))/Q)	    !février 2004
     *		-CONST(4)*SS*AU7(I)												!février 2004
      RR=-CONST(10)*SS*LAM(4)*ARG(IB)											!février 2004
     *		+((CONST(3)*CONST(9)*LAM(2)*ARG(IB))/Q)							!février 2004
     *		+CONST(9)*CONST(4)*LAM(2)*AU6(I)+((CONST(3)*SS*AU6(I))/Q)		!février 2004  
     *		+CONST(4)*SS*AU8(I)												!février 2004

        AUX5=PP*AM1+RR*AN1													!février 2004
        AUX6=RR*AM1-PP*AN1													!février 2004                                 
      COEFF(I1,2)=AUX5/AUX1													!février 2004
      COEFF(I2,2)=AUX6/AUX1													!février 2004
      AAA=COEFF(I1,2)															!février 2004
      BBB=COEFF(I2,2)															!février 2004
	ENDIF																	!février 2004

c      WRITE(66,*) 'PP= ',PP,' RR= ',RR
c      WRITE(66,*) 'AAA= ',AAA,' BBB= ',BBB

      SA=-ARG(IA)*AAA - ARG(IB)*BBB
      SB=-ARG(IA)*BBB + ARG(IB)*AAA
      SC=-ARG(IA)*AA  - ARG(IB)*BB
      SD=-ARG(IA)*BB  + ARG(IB)*AA
      SE= AU2(I) *AAA + AU1(I) *BBB
      SF= AU2(I) *BBB - AU1(I) *AAA

      IF(IOPTI.EQ.0) GOTO 567
      DO 15 KK=1,NBRXI
      K=NXI(KK)
	IF(ITYPE.NE.5) THEN														!février 2004
      GOTO(16,17,17,17,17,18,18,18,18),K										!février 2004
	ELSE																	!février 2004
	GOTO(25,25,25,25,25),K													!février 2004
	ENDIF																	!février 2004

C     EPAISSEUR DU BORDAGE (K=1)
  16  DAM1=CONST2(1,1)*  ( LAM(4)*S1+AU4(I)*S2-LAM(2)*AU2(I)*S3 )
     *  + S4* (AU5(I) *DARG(IA,1)+AU6(I) *DARG(IB,1))
     *  - S5* (ARG(IA)*DARG(IA,1)-ARG(IB)*DARG(IB,1))
      DAN1=S4* (AU6(I) *DARG(IA,1)-AU5(I) *DARG(IB,1))
     *   + S5* (ARG(IB)*DARG(IA,1)+ARG(IA)*DARG(IB,1))
     *   + CONST2(1,1) * ( S2*AU3(I)+LAM(2)*AU1(I)*S3 )
      DP= QL2*  (ARG(IA)*DARG(IA,1)-ARG(IB)*DARG(IB,1))
     *  - 4.*S6*(AU5(I) *DARG(IA,1)+AU6(I) *DARG(IB,1))
     *  + CONST2(1,1) * (AU2(I)*S7-AU4(I)*S8+S12)
     *  + R18*CONST2(5,1)-LAMB*A*AU4(I)*CONST2(3,1)

c      WRITE(66,*) 'DP1=',DP1
c      WRITE(66,*) 'DP2=',DP2
c      WRITE(66,*) 'DP3=',DP3
c      DP=DP1+DP2+DP3
c      WRITE(66,*) 'DP TOTAL=',DP

      DR= - QL2 *  (ARG(IA)*DARG(IB,1)+ARG(IB)*DARG(IA,1))
     *    -4.*S6 * (AU6(I) *DARG(IA,1)- AU5(I)*DARG(IB,1))
     *    - CONST2(1,1)*( AU1(I)*S7 + AU3(I)*S8 )
     *    + W*LAM(3)*AU1(I)*CONST2(5,1)-LAMB*A*AU3(I)*CONST2(3,1)
      DPP= - QJ*DARG(IA,1) + QK3*(AU2(I)*DARG(IA,1)-AU1(I)*DARG(IB,1))
     *                     - S9 *(AU4(I)*DARG(IA,1)+AU3(I)*DARG(IB,1))
     * +CONST2(1,1)* (-ARG(IA)*S10 + AU5(I)*S11 - AU7(I)*CONST(4)*ETA2)
     * + LAM(4)*A*ARG(IA)*CONST2(5,1)+R19*CONST2(3,1)
      DRR= QJ*DARG(IB,1) + QK3 *(-AU2(I)*DARG(IB,1)-AU1(I)*DARG(IA,1))
     *                   + S9  *( AU4(I)*DARG(IB,1)-AU3(I)*DARG(IA,1))
     * +CONST2(1,1)* (ARG(IB)*S10 + AU6(I)*S11 + AU8(I)*CONST(4)*ETA2)
     * - LAM(4)*A*ARG(IB)*CONST2(5,1)+R20*CONST2(3,1)
      GOTO 19

C     CADRE - ame - semelle - entredistance (K=2,3,4,5)
   17 DAM1= CONST2(3,K)* R4 + CONST2(1,K)* R5
     *  + S4*(AU5(I) *DARG(IA,K)+ AU6(I) *DARG(IB,K))
     *  - S5*(ARG(IA)*DARG(IA,K)- ARG(IB)*DARG(IB,K))
      DAN1=S4* ( AU6(I) *DARG(IA,K)-AU5(I) *DARG(IB,K))
     *   + S5* ( ARG(IB)*DARG(IA,K)+ARG(IA)*DARG(IB,K))
     *   + CONST2(1,K)*R6   -  CONST2(3,K)*R7
      DP= QL2 * (ARG(IA)*DARG(IA,K)-ARG(IB)*DARG(IB,K))
     *  - 4.*S6*(AU5(I) *DARG(IA,K)+AU6(I) *DARG(IB,K))
     *+CONST2(3,K)*R1 +CONST2(1,K)*AU2(I)*QG -CONST2(2,K)*AU4(I)*LAMB*A
      DR= - QL2 * (ARG(IA)*DARG(IB,K)+ARG(IB)*DARG(IA,K))
     *    - 4.*S6*(AU6(I) *DARG(IA,K)-AU5(I) *DARG(IB,K))
     *-CONST2(1,K)*QG*AU1(I) -CONST2(2,K)*AU3(I)*LAMB*A -CONST2(3,K)*R8
      DPP=-DARG(IA,K) * QJ + QK3* (AU2(I)*DARG(IA,K)-AU1(I)*DARG(IB,K))
     *                     - S9 * (AU4(I)*DARG(IA,K)+AU3(I)*DARG(IB,K))
     *  + CONST2(1,K)*R9 + CONST2(2,K)*R10 + CONST2(3,K)*R2
      DRR= DARG(IB,K)*QJ + QK3* (-AU2(I)*DARG(IB,K)-AU1(I)*DARG(IA,K))
     *                   + S9 * ( AU4(I)*DARG(IB,K)-AU3(I)*DARG(IA,K))
     *  + CONST2(1,K)*R11  + CONST2(2,K)*R12 + CONST2(3,K)*R3
      GOTO 19

C     RAIDISSEUR - ame - semelle - entredistance (K=6,7,8,9)
  18  DAM1= CONST2(3,K)*R4    +CONST2(1,K)*R14
     *  + S4*( AU5(I)   *DARG(IA,K)+ AU6(I) *DARG(IB,K))
     *  - S5*(ARG(IA)*DARG(IA,K)-ARG(IB)*DARG(IB,K))
      DAN1=S4* ( AU6(I) *DARG(IA,K)-  AU5(I)  *DARG(IB,K))
     *   + S5* (ARG(IB)*DARG(IA,K)+ARG(IA)*DARG(IB,K))
     *    + CONST2(1,K)* AU1(I)*LAM(2)*W  + CONST2(3,K)*R7
      DP= QL2 * (ARG(IA)*DARG(IA,K)-ARG(IB)*DARG(IB,K))
     *  - 4.*S6*(AU5(I) *DARG(IA,K)+AU6(I) *DARG(IB,K))
     *  + CONST2(3,K)*R1 + CONST2(2,K)*R15
      DR= - QL2 * (ARG(IA)*DARG(IB,K)+ARG(IB)*DARG(IA,K))
     *    - 4.*S6*(AU6(I) *DARG(IA,K)-AU5(I) *DARG(IB,K))
     *   + CONST2(2,K)* W*LAM(3)*AU1(I) - CONST2(3,K)*R8
      DPP=-DARG(IA,K) * QJ + QK3* (AU2(I)*DARG(IA,K)-AU1(I)*DARG(IB,K))
     *                     - S9 * (AU4(I)*DARG(IA,K)+AU3(I)*DARG(IB,K))
     *+ CONST2(1,K)*R16 +CONST2(2,K)*ARG(IA)*LAM(4)*A  +CONST2(3,K)*R2
      DRR= DARG(IB,K)*QJ + QK3* (-AU2(I)*DARG(IB,K)-AU1(I)*DARG(IA,K))
     *                   + S9 * ( AU4(I)*DARG(IB,K)-AU3(I)*DARG(IA,K))
     *+ CONST2(1,K)*R17 -CONST2(2,K)*ARG(IB)*LAM(4)*A  +CONST2(3,K)*R3

	GOTO 19 !juillet04
C
C     EPONTILLE - ame - semelle - entredistance (K=1,2,3,4,5)				!février 2004
c
  25  DAM1= CONST2(3,K)* (LAM(4)*CONST(9)+AU4(I)*CONST(3))				!février 2004
     *  + CONST2(1,K)* (AU4(I)*SS - AU2(I)*LAM(2)*CONST(9))				!février 2004
     *  + 4.*CONST(3)*SS*(AU5(I) *DARG(IA,K)+ AU6(I) *DARG(IB,K))			!février 2004
     *  - 2.*LAM(2)*CONST(9)*CONST(3)*(ARG(IA)*DARG(IA,K)					!février 2004
     *  - ARG(IB)*DARG(IB,K))												!février 2004
      DAN1=(4.*CONST(3)*SS* ( AU6(I) *DARG(IA,K)-AU5(I) *DARG(IB,K))		!février 2004
     *   + 2.*LAM(2)*CONST(9)*CONST(3)* ( ARG(IB)*DARG(IA,K)				!février 2004
     *   +ARG(IA)*DARG(IB,K))+ CONST2(1,K)*(AU3(I)*SS						!février 2004
     *   +AU1(I)*LAM(2)*CONST(9))+CONST2(3,K)* AU3(I)*CONST(3))			!février 2004
      DP=+2*CONST(3)*(SS*(LAMB/Q)-CONST(10)*LAM(3)) *(ARG(IA)*DARG(IA,K)	!février 2004
     *  -ARG(IB)*DARG(IB,K))												!février 2004
     *  +CONST2(3,K)*(AU2(I)*CONST(3)*LAMB/Q +LAM(5)*CONST(10))			!février 2004
     *  +CONST2(1,K)*AU2(I)*(SS*LAMB/Q - CONST(10)*LAM(3))				!février 2004
     *  -CONST2(2,K)*SS*LAMB*AU4(I)-CONST(4)*CONST2(3,K)*LAMB*AU4(I)		!février 2004
     *  -CONST(4)*SS*LAMB*4.*(AU5(I)*DARG(IA,K)+AU6(I)*DARG(IB,K))		!février 2004	
      DR=-2.*CONST(3)*(SS*(LAMB/Q)-CONST(10)*LAM(3))*(ARG(IA)*DARG(IB,K)	!février 2004
     *  +ARG(IB)*DARG(IA,K))-CONST2(3,K)*AU1(I)*CONST(3)*LAMB/Q			!février 2004
     *  -CONST2(1,K)*(SS*LAMB/Q - CONST(10)*LAM(3))*AU1(I)				!février 2004
     *  -CONST2(2,K)*SS*LAMB*AU3(I)-CONST(4)*CONST2(3,K)*LAMB*AU4(I)		!février 2004
     *  -CONST(4)*SS*LAMB*4.*(AU6(I)*DARG(IA,K)-AU5(I)*DARG(IB,K))		!février 2004									
      DPP=-DARG(IA,K)*(-SS*CONST(10)*LAM(4)+CONST(3)*CONST(9)*LAM(2)/Q)	!février 2004
     *  +(3.*CONST(3)*SS/Q)* (AU2(I)*DARG(IA,K)-AU1(I)*DARG(IB,K))		!février 2004	
     *  + CONST2(1,K)*(-ARG(IA)*CONST(9)*LAM(2)/Q+AU5(I)*SS/Q)			!février 2004
     *  + CONST2(3,K)*(CONST(10)*LAM(4)*ARG(IA)+AU5(I)*CONST(3)/Q)		!février 2004
     *  + CONST(4)*(CONST(9)*LAM(2)*3.*(AU2(I)*DARG(IA,K)					!février 2004
     *  -AU1(I)*DARG(IB,K))-CONST2(3,K)*AU7(I)-SS*5.*(AU4(I)*DARG(IA,K)	!février 2004	
     *  -AU3(I)*DARG(IB,K)))+CONST2(2,K)*(CONST(9)*LAM(2)*AU5(I)			!février 2004
     *  -SS*AU7(I))														!février 2004
      DRR= DARG(IB,K)*(-SS*CONST(10)*LAM(4)+CONST(3)*CONST(9)*LAM(2)/Q)	!février 2004
     *  + (3.*CONST(3)*SS/Q)* (-AU2(I)*DARG(IB,K)-AU1(I)*DARG(IA,K))		!février 2004
     *  + CONST2(1,K)*(ARG(IB)*CONST(9)*LAM(2)/Q + AU6(I)*SS/Q)			!février 2004
     *  + CONST2(3,K)*(-CONST(10)*LAM(4)*ARG(IB)+AU6(I)*CONST(3)/Q)		!février 2004
     *  +CONST(4)*(-CONST(9)*LAM(2)*3.*(AU2(I)*DARG(IB,K)					!février 2004
     *  +AU1(I)*DARG(IA,K))+CONST2(3,K)*AU8(I)+SS*5.*(AU4(I)*DARG(IB,K)	!février 2004
     *   -AU3(I)*DARG(IA,K)))+CONST2(2,K)*(CONST(9)*LAM(2)*AU6(I)			!février 2004
     *  +SS*AU8(I))														!février 2004

   19 AUX7=AM1*DAM1+AN1*DAN1
      DAA=( AUX1*(P*DAM1+AM1*DP+R*DAN1+AN1*DR) - 2.*AUX3*AUX7 )/AUX2
      DBB=( AUX1*(R*DAM1+AM1*DR-P*DAN1-AN1*DP) - 2.*AUX4*AUX7 )/AUX2
      DCOEFF(I1,1,K)=DAA                                            ! d(U)/dXI
      DCOEFF(I2,1,K)=DBB
      DAAA=(AUX1*(PP*DAM1+AM1*DPP+RR*DAN1+AN1*DRR)-2.*AUX5*AUX7)/AUX2
      DBBB=(AUX1*(RR*DAM1+AM1*DRR-PP*DAN1-AN1*DPP)-2.*AUX6*AUX7)/AUX2
      DCOEFF(I1,2,K)=DAAA                                           ! d(V)/dXI
      DCOEFF(I2,2,K)=DBBB
      
      DCOEFF(I1,3,K)=-DARG(IA,K)                                    ! d(Wø)/dXI
      DCOEFF(I2,3,K)= DARG(IB,K)
      DCOEFF(I1,8,K)=0.                                             ! d(W)/dXI
      DCOEFF(I2,8,K)=0.
      DCOEFF(I1,9,K)= 2.*(ARG(IA)*DARG(IA,K)-ARG(IB)*DARG(IB,K))    ! d(Wøø)/dXI
      DCOEFF(I2,9,K)=-2.*(ARG(IA)*DARG(IB,K)+ARG(IB)*DARG(IA,K))
      DCOEFF(I1,10,K)= 3.*(-AU2(I) *DARG(IA,K)+AU1(I) *DARG(IB,K))  ! d(Wøøø)/dXI
      DCOEFF(I2,10,K)= 3.*( AU2(I) *DARG(IB,K)+AU1(I) *DARG(IA,K))
      DCOEFF(I1,11,K)=-( ARG(IA)  *DAA  +  ARG(IB)  *DBB +          ! d(Uø)/dXI
     *                  DARG(IA,K)* AA  + DARG(IB,K)* BB    )
      DCOEFF(I2,11,K)=+(-ARG(IA)  *DBB  +  ARG(IB)  *DAA -
     *                  DARG(IA,K)* BB  + DARG(IB,K)* AA    )
      DCOEFF(I1,12,K)=-( ARG(IA)  *DAAA +  ARG(IB)  *DBBB +         ! d(Vø)/dXI
     *                  DARG(IA,K)* AAA + DARG(IB,K)* BBB   )
      DCOEFF(I2,12,K)=+(-ARG(IA)  *DBBB +  ARG(IB)  *DAAA -
     *                  DARG(IA,K)* BBB + DARG(IB,K)* AAA   )
      DCOEFF(I1,13,K)=  AU2(I)*DAAA +  AU1(I)*DBBB +                ! d(Vøø)/dXI
     *         2.*( ARG(IA)*DARG(IA,K)-ARG(IB)*DARG(IB,K) )*AAA +
     *         2.*( ARG(IA)*DARG(IB,K)+ARG(IB)*DARG(IA,K) )*BBB 
      DCOEFF(I2,13,K)=  AU2(I)*DBBB -  AU1(I)*DAAA +
     *         2.*( ARG(IA)*DARG(IA,K)-ARG(IB)*DARG(IB,K) )*BBB -
     *         2.*( ARG(IA)*DARG(IB,K)+ARG(IB)*DARG(IA,K) )*AAA 

c      WRITE(66,*) 'DAM1= ',DAM1,' DAN1= ',DAN1
c      WRITE(66,*) 'DP= ',DP,' DR= ',DR
c      WRITE(66,*) 'DAA= ',DAA,' DBB= ',DBB
c      WRITE(66,*) '  '
c      WRITE(66,*) 'DPP= ',DPP,' DRR= ',DRR
c      WRITE(66,*) 'DAAA= ',DAAA,' DBBB= ',DBBB

      IF(MT.GT.0) THEN
      DU2A=  AU2(I)*DAA +  AU1(I)*DBB +                           ! d(Uøø)/dXI
     *         2.*( ARG(IA)*DARG(IA,K)-ARG(IB)*DARG(IB,K) )*AA +
     *         2.*( ARG(IA)*DARG(IB,K)+ARG(IB)*DARG(IA,K) )*BB 
      DU2B=  AU2(I)*DBB -  AU1(I)*DAA +
     *         2.*( ARG(IA)*DARG(IA,K)-ARG(IB)*DARG(IB,K) )*BB -
     *         2.*( ARG(IA)*DARG(IB,K)+ARG(IB)*DARG(IA,K) )*AA 
      DHTR=0.
      DRTR=0.
      DO 1401 ITRA=1,MT
      JTRAV=6*(ITRA-1)
      JJTRAV=2*(ITRA-1)
      IF(K.EQ.1) THEN
        DHTR=CONST3(1,ITRA)
        DRTR=CONST3(2,ITRA)
      ENDIF
      DCOEFF(I1,14+JJTRAV,K)=-DAA*LAM(2)*CONST(15+JTRAV)            !  d(Xo)/dXI
     *        + LAM(3)*DHTR
     *          + CONST(17+JTRAV)*(LAMB*DCOEFF(I1,12,K)+DU2A)
      DCOEFF(I2,14+JJTRAV,K)=-DBB*LAM(2)*CONST(15+JTRAV)            !  d(Xo)/dXI
     *          + CONST(17+JTRAV)*(LAMB*DCOEFF(I2,12,K)+DU2B)
      DCOEFF(I1,15+JJTRAV,K)=-LAM(2)*CONST(19+JTRAV)*DCOEFF(I1,9,K) !  d(Zo)/dXI
     *         - LAM(3)*(DHTR*AA+CONST(16+JTRAV)*DAA) + LAM(4)*DRTR   	
     *         - LAMB*CONST(20+JTRAV) * (LAMB*DCOEFF(I1,12,K)+DU2A)
      DCOEFF(I2,15+JJTRAV,K)= -LAM(2)*CONST(19+JTRAV)*DCOEFF(I2,9,K)
     *   - LAM(3)*(DHTR*BB+CONST(16+JTRAV)*DBB)
     *   - LAMB*CONST(20+JTRAV)*(LAMB*DCOEFF(I2,12,K)+DU2B)
 1401 CONTINUE
      ENDIF


	IF(ITYPE.NE.5) THEN														!février 2004
      GOTO(20,21,21,21,21,22,22,22,22),K
	ELSE																	!février 2004
	GOTO(26,26,26,26,26),K													!février 2004
	ENDIF																	!février 2004
C     BORDAGE
   20 DCOEFF(I1,4,K)=-W*( DARG(IA,K)*AAA + ARG(IA)*DAAA         !  d(Ny)/dXI
     *                  + DARG(IB,K)*BBB + ARG(IB)*DBBB )
     * - CONST(4)*2*( ARG(IA)*DARG(IA,K) - ARG(IB)*DARG(IB,K) ) 
     * + CONST2(1,K)*(1./Q+SA) - LAMB*ETA*(CONST(1)*DAA+AA*CONST2(1,K))
     * - CONST2(3,K)*AU2(I)
      DCOEFF(I2,4,K)=-W*( DARG(IA,K)*BBB + ARG(IA)*DBBB 	
     *                  - DARG(IB,K)*AAA - ARG(IB)*DAAA )
     * + CONST(4)*2*( ARG(IA)*DARG(IB,K) + ARG(IB)*DARG(IA,K) )
     * + CONST2(1,K) * SB - LAMB*ETA*(CONST(1)*DBB+BB*CONST2(1,K))
     * + CONST2(3,K)*AU1(I)
      DCOEFF(I1,5,K)= CONST2(1,K)*ETA2*(SC+LAMB*AAA)           !  d(Nxy)/dXI
     * + Z*( -ARG(IA)*DAA - DARG(IA,K)*AA
     *       -ARG(IB)*DBB - DARG(IB,K)*BB + LAMB*DAAA)
      DCOEFF(I2,5,K)= CONST2(1,K)*ETA2*(SD+LAMB*BBB)  
     * + Z*(  ARG(IB)*DAA + DARG(IB,K)*AA
     *       -ARG(IA)*DBB - DARG(IA,K)*BB +LAMB*DBBB)
      DCOEFF(I1,6,K)= (ARG(IA)*DAAA + DARG(IA,K)*AAA           ! d(My)/dXI
     *               + ARG(IB)*DBBB + DARG(IB,K)*BBB ) * CONST(4)
     * + X*2.* ( ARG(IA)*DARG(IA,K) - ARG(IB)*DARG(IB,K) ) 
     * + CONST2(2,K) * (AU2(I)-LAM(2)*ETA)
     * + CONST2(4,K)*AU2(I) - CONST2(3,K)*(1./Q+SA)
      DCOEFF(I2,6,K)=-(  ARG(IB)*DAAA + DARG(IB,K)*AAA 	
     *                 - ARG(IA)*DBBB - DARG(IA,K)*BBB ) * CONST(4)
     * - X*2.* ( ARG(IA)*DARG(IB,K) + ARG(IB)*DARG(IA,K) ) 
     * - (CONST2(2,K)+CONST2(4,K)) * AU1(I) - CONST2(3,K)*SB
      DCOEFF(I1,7,K)= CONST2(2,K) * (LAM(2)*(2.-ETA)*ARG(IA)-AU5(I))  ! d(Ry)/dXI
     * + CONST(4)* ( -2.*AAA* (ARG(IA)*DARG(IA,K)-ARG(IB)*DARG(IB,K))    
     *              - 2.*BBB* (ARG(IA)*DARG(IB,K)+ARG(IB)*DARG(IA,K)) 
     *              - DAAA*AU2(I) - DBBB*AU1(I) + DARG(IA,K)/Q   )
     * + LAMB*F* (-LAMB*DAAA + ARG(IA)*DAA + DARG(IA,K)*AA 
     *                       + ARG(IB)*DBB + DARG(IB,K)*BB )
     * + LAM(2)*QD*ARG(IA) -3.*X* (DARG(IA,K)*AU2(I)-DARG(IB,K)*AU1(I))
     * + CONST2(3,K)*(ARG(IA)/Q-SE)  - CONST2(4,K)*AU5(I)
      DCOEFF(I2,7,K)= CONST2(2,K) * (-LAM(2)*(2.-ETA)*ARG(IB)-AU6(I))
     * - CONST(4)* (  2.*BBB* (ARG(IA)*DARG(IA,K)-ARG(IB)*DARG(IB,K))
     *              - 2.*AAA* (ARG(IA)*DARG(IB,K)+ARG(IB)*DARG(IA,K))
     *              + DBBB*AU2(I) - DAAA*AU1(I) + DARG(IB,K)/Q   )
     * + LAMB*F* (-LAMB*DBBB + ARG(IA)*DBB + DARG(IA,K)*BB
     *                       - ARG(IB)*DAA - DARG(IB,K)*AA )
     * - LAM(2)*QD*ARG(IB) +3.*X* (DARG(IB,K)*AU2(I)+DARG(IA,K)*AU1(I))
     * - CONST2(3,K)*(ARG(IB)/Q+SF)  - CONST2(4,K)*AU6(I)

      GOTO 23
C     CADRES
   21 DCOEFF(I1,4,K)=-W*( DARG(IA,K)*AAA + ARG(IA)*DAAA    ! d(Ny)/dXI
     *                  + DARG(IB,K)*BBB + ARG(IB)*DBBB )
     * - CONST(4)*2* ( ARG(IA)*DARG(IA,K) - ARG(IB)*DARG(IB,K) )	
     * + CONST2(1,K)*(1./Q+SA) - DAA*LAMB*G - CONST2(2,K)*AU2(I)
      DCOEFF(I2,4,K)=-W*( DARG(IA,K)*BBB + ARG(IA)*DBBB 	
     *                  - DARG(IB,K)*AAA - ARG(IB)*DAAA )
     * + CONST(4)*2* ( ARG(IA)*DARG(IB,K) + ARG(IB)*DARG(IA,K) )
     * + CONST2(1,K)*SB - DBB*LAMB*G + CONST2(2,K)*AU1(I)
      DCOEFF(I1,5,K)= CONST2(3,K)* (SC+LAMB*AAA)             ! d(Nxy)/dXI
     * + Z*( -ARG(IA)*DAA - DARG(IA,K)*AA
     *       -ARG(IB)*DBB - DARG(IB,K)*BB + LAMB*DAAA)
      DCOEFF(I2,5,K)= CONST2(3,K)* (SD+LAMB*BBB)  
     * + Z*(  ARG(IB)*DAA + DARG(IB,K)*AA
     *       -ARG(IA)*DBB - DARG(IA,K)*BB +LAMB*DBBB)
      DCOEFF(I1,6,K)= ( ARG(IA)*DAAA + DARG(IA,K)*AAA        ! d(My)/dXI
     *                + ARG(IB)*DBBB + DARG(IB,K)*BBB ) * CONST(4)
     * + X*2.* ( ARG(IA)*DARG(IA,K) - ARG(IB)*DARG(IB,K) ) 
     * + CONST2(4,K)*AU2(I) - CONST2(2,K)*(1./Q+SA)
      DCOEFF(I2,6,K)=-(  ARG(IB)*DAAA + DARG(IB,K)*AAA 	
     *                 - ARG(IA)*DBBB - DARG(IA,K)*BBB ) * CONST(4)
     * - X*2.* ( ARG(IA)*DARG(IB,K) + ARG(IB)*DARG(IA,K) ) 
     * - CONST2(4,K)*AU1(I) - CONST2(2,K)*SB
      DCOEFF(I1,7,K)= CONST2(5,K)*LAM(2)*ARG(IA) - CONST2(4,K)*AU5(I) ! d(Ry)/dXI
     * + CONST2(2,K)*(ARG(IA)/Q-SE) + CONST2(6,K)*LAMB*(SC+LAMB*AAA)
     * + CONST(4)* ( -2.*AAA* (ARG(IA)*DARG(IA,K)-ARG(IB)*DARG(IB,K))    
     *              - 2.*BBB* (ARG(IA)*DARG(IB,K)+ARG(IB)*DARG(IA,K)) 
     *              - DAAA*AU2(I) - DBBB*AU1(I) + DARG(IA,K)/Q   )
     * + LAMB*F* (-LAMB*DAAA + ARG(IA)*DAA + DARG(IA,K)*AA 
     *                       + ARG(IB)*DBB + DARG(IB,K)*BB )
     * + LAM(2)*QD*ARG(IA) -3.*X* (DARG(IA,K)*AU2(I)-DARG(IB,K)*AU1(I))
      DCOEFF(I2,7,K)=-CONST2(5,K)*LAM(2)*ARG(IB)  -  CONST2(4,K)*AU6(I)
     * - CONST2(2,K)*(ARG(IB)/Q+SF) - CONST2(6,K)*LAMB*(SD+LAMB*BBB)
     * - CONST(4)* (  2.*BBB* (ARG(IA)*DARG(IA,K)-ARG(IB)*DARG(IB,K))
     *              - 2.*AAA* (ARG(IA)*DARG(IB,K)+ARG(IB)*DARG(IA,K))
     *              + DBBB*AU2(I) - DAAA*AU1(I) + DARG(IB,K)/Q   )
     * + LAMB*F* (-LAMB*DBBB + ARG(IA)*DBB + DARG(IA,K)*BB
     *                       - ARG(IB)*DAA - DARG(IB,K)*AA )
     * - LAM(2)*QD*ARG(IB) +3.*X* (DARG(IB,K)*AU2(I)+DARG(IA,K)*AU1(I))

      GOTO 23
C     RAIDISSEURS
   22 DCOEFF(I1,4,K)=-W* ( DARG(IA,K)*AAA + ARG(IA)*DAAA     ! d(Ny)/dXI
     *                   + DARG(IB,K)*BBB + ARG(IB)*DBBB )
     * -CONST(4)*2* (ARG(IA)*DARG(IA,K)-ARG(IB)*DARG(IB,K)) -DAA*LAMB*G
      DCOEFF(I2,4,K)=-W*( DARG(IA,K)*BBB + ARG(IA)*DBBB 	
     *                  - DARG(IB,K)*AAA - ARG(IB)*DAAA )
     * +CONST(4)*2* (ARG(IA)*DARG(IB,K)+ARG(IB)*DARG(IA,K)) -DBB*LAMB*G
      DCOEFF(I1,5,K)= CONST2(3,K)*(SC+LAMB*AAA)                ! d(Nxy)/dXI
     *        + Z*( -ARG(IA)*DAA - DARG(IA,K)*AA
     *              -ARG(IB)*DBB - DARG(IB,K)*BB + LAMB*DAAA)
      DCOEFF(I2,5,K)= CONST2(3,K)*(SD+LAMB*BBB)  
     *        + Z*(  ARG(IB)*DAA + DARG(IB,K)*AA
     *              -ARG(IA)*DBB - DARG(IA,K)*BB +LAMB*DBBB)
      DCOEFF(I1,6,K)= ( ARG(IA)*DAAA + DARG(IA,K)*AAA          ! d(My)/dXI
     *                + ARG(IB)*DBBB + DARG(IB,K)*BBB ) * CONST(4)
     *   + X*2.* ( ARG(IA)*DARG(IA,K) - ARG(IB)*DARG(IB,K) ) 
      DCOEFF(I2,6,K)=-(  ARG(IB)*DAAA + DARG(IB,K)*AAA 	
     *                 - ARG(IA)*DBBB - DARG(IA,K)*BBB ) * CONST(4)
     *   - X*2.* ( ARG(IA)*DARG(IB,K) + ARG(IB)*DARG(IA,K) ) 
      DCOEFF(I1,7,K)= CONST2(5,K)* LAM(2)*ARG(IA)              ! d(Ry)/dXI
     *              - CONST2(6,K)* LAMB*(SC+LAMB*AAA)
     * + CONST(4)* (- 2.*AAA* (ARG(IA)*DARG(IA,K)-ARG(IB)*DARG(IB,K))    
     *              - 2.*BBB* (ARG(IA)*DARG(IB,K)+ARG(IB)*DARG(IA,K)) 
     *              - DAAA*AU2(I) - DBBB*AU1(I) + DARG(IA,K)/Q   )
     * + LAMB*F* (-LAMB*DAAA + ARG(IA)*DAA + DARG(IA,K)*AA 
     *                       + ARG(IB)*DBB + DARG(IB,K)*BB )
     * + LAM(2)*QD*ARG(IA) -3.*X* (DARG(IA,K)*AU2(I)-DARG(IB,K)*AU1(I))
      DCOEFF(I2,7,K)= - CONST2(5,K)*LAM(2)*ARG(IB)
     *                - CONST2(6,K)*LAMB*(SD+LAMB*BBB)
     * - CONST(4)* (  2.*BBB* (ARG(IA)*DARG(IA,K)-ARG(IB)*DARG(IB,K))    
     *              - 2.*AAA* (ARG(IA)*DARG(IB,K)+ARG(IB)*DARG(IA,K)) 
     *              + DBBB*AU2(I) - DAAA*AU1(I) + DARG(IB,K)/Q   )
     * + LAMB*F* (-LAMB*DBBB + ARG(IA)*DBB + DARG(IA,K)*BB 
     *                       - ARG(IB)*DAA - DARG(IB,K)*AA )
     * - LAM(2)*QD*ARG(IB) +3.*X* (DARG(IB,K)*AU2(I)+DARG(IA,K)*AU1(I))
	GOTO 23						
C
C	EPONTILLES																		!février 2004
 26	DCOEFF(I1,4,K)=-CONST(3)*( DARG(IA,K)*AAA + ARG(IA)*DAAA      ! d(Ny)/dXI		!février 2004
     *                  + DARG(IB,K)*BBB + ARG(IB)*DBBB )								!février 2004
     * - CONST(4)*2.* ( ARG(IA)*DARG(IA,K) - ARG(IB)*DARG(IB,K) )						!février 2004
     * + CONST2(1,K)*(1./Q+SA) - CONST2(2,K)*AU2(I)									!février 2004		
      DCOEFF(I2,4,K)=-CONST(3)*( DARG(IA,K)*BBB + ARG(IA)*DBBB 						!février 2004
     *                  - DARG(IB,K)*AAA - ARG(IB)*DAAA )								!février 2004
     * + CONST(4)*2.* ( ARG(IA)*DARG(IB,K) + ARG(IB)*DARG(IA,K) )						!février 2004
     * + CONST2(1,K)*SB + CONST2(2,K)*AU1(I)											!février 2004
      DCOEFF(I1,5,K)= CONST2(3,K)* (SC+LAMB*AAA)						! d(Nxy)/dXI	!février 2004
     * + SS*( -ARG(IA)*DAA - DARG(IA,K)*AA											!février 2004
     *       -ARG(IB)*DBB - DARG(IB,K)*BB + LAMB*DAAA)								!février 2004
      DCOEFF(I2,5,K)= CONST2(3,K)* (SD+LAMB*BBB)										!février 2004
     * + SS*(  ARG(IB)*DAA + DARG(IB,K)*AA											!février 2004
     *       -ARG(IA)*DBB - DARG(IA,K)*BB +LAMB*DBBB)									!février 2004
      DCOEFF(I1,6,K)= ( ARG(IA)*DAAA + DARG(IA,K)*AAA				  ! d(My)/dXI		!février 2004
     *                + ARG(IB)*DBBB + DARG(IB,K)*BBB ) * CONST(4)					!février 2004
     * + CONST(6)*2.* ( ARG(IA)*DARG(IA,K) - ARG(IB)*DARG(IB,K) )						!février 2004			
     * + CONST2(4,K)*AU2(I) - CONST2(2,K)*(1./Q+SA)									!février 2004
      DCOEFF(I2,6,K)=-(  ARG(IB)*DAAA + DARG(IB,K)*AAA 								!février 2004
     *                 - ARG(IA)*DBBB - DARG(IA,K)*BBB ) * CONST(4)					!février 2004
     * - CONST(6)*2.* ( ARG(IA)*DARG(IB,K) + ARG(IB)*DARG(IA,K) )						!février 2004
     * - CONST2(4,K)*AU1(I) - CONST2(2,K)*SB											!février 2004
      DCOEFF(I1,7,K)= CONST2(5,K)*LAM(2)*ARG(IA) - CONST2(4,K)*AU5(I) 	! d(Ry)/dXI	!février 2004
     * + CONST2(2,K)*(ARG(IA)/Q-SE)													!février 2004
     * + CONST(4)* ( -2.*AAA* (ARG(IA)*DARG(IA,K)-ARG(IB)*DARG(IB,K))					!février 2004
     *              - 2.*BBB* (ARG(IA)*DARG(IB,K)+ARG(IB)*DARG(IA,K))					!février 2004
     *              - DAAA*AU2(I) - DBBB*AU1(I) + DARG(IA,K)/Q   )					!février 2004
     * + LAM(2)*TT*DARG(IA,K) -3.*CONST(6)* (DARG(IA,K)*AU2(I)						!février 2004
     *  -DARG(IB,K)*AU1(I))															!février 2004
      DCOEFF(I2,7,K)=-CONST2(5,K)*LAM(2)*ARG(IB)  -  CONST2(4,K)*AU6(I)				!février 2004
     * - CONST2(2,K)*(ARG(IB)/Q+SF)													!février 2004
     * - CONST(4)* (  2.*BBB* (ARG(IA)*DARG(IA,K)-ARG(IB)*DARG(IB,K))					!février 2004
     *              - 2.*AAA* (ARG(IA)*DARG(IB,K)+ARG(IB)*DARG(IA,K))					!février 2004
     *              + DBBB*AU2(I) - DAAA*AU1(I) + DARG(IB,K)/Q   )					!février 2004
     * - LAM(2)*TT*DARG(IB,K) +3.*CONST(6)* (DARG(IB,K)*AU2(I)						!février 2004
     *  +DARG(IA,K)*AU1(I))															!février 2004
   23 CONTINUE
      IF(IMPR.NE.0) THEN
      WRITE(66,*) 'K= ',K
      WRITE(66,*) '-------'
      WRITE(66,*) ' COEF DE d(U)/dXI  =',DCOEFF(I1,1,K),DCOEFF(I2,1,K)
      WRITE(66,*) ' COEF DE d(V)/dXI  =',DCOEFF(I1,2,K),DCOEFF(I2,2,K)
      WRITE(66,*) ' COEF DE d(Wø)/dXI =',DCOEFF(I1,3,K),DCOEFF(I2,3,K)
      WRITE(66,*) ' COEF DE d(Ny)/dXI =',DCOEFF(I1,4,K),DCOEFF(I2,4,K)
      WRITE(66,*) ' COEF DE d(Nxy)/dXI=',DCOEFF(I1,5,K),DCOEFF(I2,5,K)
      WRITE(66,*) ' COEF DE d(My)/dXI =',DCOEFF(I1,6,K),DCOEFF(I2,6,K)
      WRITE(66,*) ' COEF DE d(Ry)/dXI =',DCOEFF(I1,7,K),DCOEFF(I2,7,K)
      WRITE(66,*) ' COEF DE d(W)/dXI  =',DCOEFF(I1,8,K),DCOEFF(I2,8,K)
      WRITE(66,*) ' COEF DE d(Wøø)/dXI=',DCOEFF(I1,9,K),DCOEFF(I2,9,K)
      WRITE(66,*) ' COEF DEd(Wøøø)/dXI=',DCOEFF(I1,10,K),DCOEFF(I2,10,K)
      WRITE(66,*) ' COEF DE d(Uø)/dXI =',DCOEFF(I1,11,K),DCOEFF(I2,11,K)
      WRITE(66,*) ' COEF DE d(Vø)/dXI =',DCOEFF(I1,12,K),DCOEFF(I2,12,K)
      WRITE(66,*) ' COEF DE d(Vøø)/dXI=',DCOEFF(I1,13,K),DCOEFF(I2,13,K)
      WRITE(66,*) ' COEF DE d(Xo)/dXI =',DCOEFF(I1,14,K),DCOEFF(I2,14,K)
      WRITE(66,*) ' COEF DE d(Zo)/dXI =',DCOEFF(I1,15,K),DCOEFF(I2,15,K)
      WRITE(66,*) ' '
      ENDIF
      
   15 CONTINUE

 567  CONTINUE
      
	IF (ITYPE.NE.5) THEN												       !février 2004                      
      COEFF(I1,3)=-ARG(IA)                                            !  Wø
      COEFF(I2,3)= ARG(IB)                                          	
      COEFF(I1,4)=W*(1./Q+SA)-LAMB*G*AA - AU2(I)*CONST(4)             !  Ny
      COEFF(I2,4)=W*SB       -LAMB*G*BB + AU1(I)*CONST(4)
      COEFF(I1,5)=-C*LAMB*AA + G*(SA+1./Q) + LAM(2)*CONST(10)         !  Nx
      COEFF(I2,5)=-C*LAMB*BB + G*SB                                  	
      COEFF(I1,6)= (SC + LAMB*AAA)*Z                                  !  Nyx
      COEFF(I2,6)= (SD + LAMB*BBB)*Z                                 	
      COEFF(I1,7)=COEFF(I1,6)                                         !  Nxy
      COEFF(I2,7)=COEFF(I2,6)                                     	
      COEFF(I1,8)=X*AU2(I) - CONST(2)*ETA*LAM(2) - CONST(4)*(1./Q+SA) !  My
      COEFF(I2,8)=-AU1(I)*X-CONST(4)*SB                              	
      COEFF(I1,9)=-B*LAM(2) + CONST(2)*ETA*AU2(I) + CONST(10)*LAMB*AA !  Mx
      COEFF(I2,9)=-CONST(2)*ETA*AU1(I)+CONST(10)*LAMB*BB             	
      COEFF(I1,10)=-QF*LAMB*ARG(IA)+CONST(8) *(SC+LAMB*AAA)
      COEFF(I2,10)= QF*LAMB*ARG(IB)  +CONST(8) *(SD+LAMB*BBB)
      COEFF(I1,11)=-QE*LAMB*ARG(IA)+CONST(14)*(SC+LAMB*AAA)
      COEFF(I2,11)=+QE*LAMB*ARG(IB)  +CONST(14)*(SD+LAMB*BBB)
      COEFF(I1,12)= ARG(IA)*QB*LAM(2) - AU5(I)*X
     *                -CONST(14)* LAMB*(SC+LAMB*AAA)
     *                -CONST(4) * (-ARG(IA)/Q+AU2(I)*AAA+AU1(I)*BBB)
      COEFF(I2,12)=-ARG(IB)*QB*LAM(2) - AU6(I)*X
     *                -CONST(14)* LAMB*(SD+LAMB*BBB)
     *                -CONST(4) * (ARG(IB)/Q+AU2(I)*BBB-AU1(I)*AAA)
      COEFF(I1,13)= AU2(I)*QC*LAMB-B*LAM(3)+AA*LAM(2)*CONST(10)+
     *                 CONST(8)*(AU2(I)*AA+AU1(I)*BB-LAMB*SA)
      COEFF(I2,13)=-AU1(I)*QC*LAMB+BB*LAM(2)*CONST(10)+
     *                  CONST(8)*(AU2(I)*BB-AU1(I)*AA-LAMB*SB)          
      COEFF(I1,14)= CONST(4) * (ARG(IA)/Q-SE)                         !  Ry
     *          -(AAA*LAM(2)+LAMB*SC)*F + LAM(2)*ARG(IA)*QD - X*AU5(I)  
      COEFF(I2,14)=-CONST(4) * (ARG(IB)/Q+SF)
     *          -(BBB*LAM(2)+LAMB*SD)*F - LAM(2)*ARG(IB)*QD - X*AU6(I)
      COEFF(I1,15)=QD*AU2(I)*LAMB-B*LAM(3)+CONST(10)*AA*LAM(2)
     *          + F * (AU2(I)*AA+AU1(I)*BB+LAMB*SA)
      COEFF(I2,15)=-QD*AU1(I)*LAMB+CONST(10)*BB*LAM(2)
     *          + F * (AU2(I)*BB-AU1(I)*AA+LAMB*SB)
      COEFF(I1,16)=1.
      COEFF(I2,16)=0.
      COEFF(I1,37)=+AU2(I)                             !  Wøø
      COEFF(I2,37)=-AU1(I)                             !
      COEFF(I1,38)=-AU5(I)                             !  Wøøø
      COEFF(I2,38)=-AU6(I)                             !
      COEFF(I1,39)=SC                                  !  Uø
      COEFF(I2,39)=SD                                  !
      COEFF(I1,40)=SA                                  !  Vø
      COEFF(I2,40)=SB                                  !
      COEFF(I1,41)=SE                                  !  Vøø
      COEFF(I2,41)=SF                                          	
	ELSE																			!février 2004
      COEFF(I1,3)=-ARG(IA)										     !  Wø			!février 2004
      COEFF(I2,3)= ARG(IB)									             			!février 2004
      COEFF(I1,4)=CONST(3)*(SA+(1./Q))- AU2(I)*CONST(4)	             !  Ny			!février 2004
      COEFF(I2,4)=CONST(3)*SB + AU1(I)*CONST(4)										!février 2004
      COEFF(I1,5)=-CONST(9)*LAMB*AA +LAM(2)*CONST(10)		   		     !  Nx			!février 2004
      COEFF(I2,5)=-CONST(9)*LAMB*BB								         			!février 2004
      COEFF(I1,6)= (SC + LAMB*AAA)*SS								     !  Nyx			!février 2004
      COEFF(I2,6)= (SD + LAMB*BBB)*SS                               					!février 2004
      COEFF(I1,7)=COEFF(I1,6)									         !  Nxy			!février 2004
      COEFF(I2,7)=COEFF(I2,6)										                  	!février 2004
      COEFF(I1,8)=CONST(6)*AU2(I) - CONST(4)*(1./Q+SA)				 !  My	   		!février 2004
      COEFF(I2,8)=-AU1(I)*CONST(6)-CONST(4)*SB                           				!février 2004
      COEFF(I1,9)=-CONST(12)*LAM(2)+ CONST(10)*LAMB*AA				 !  Mx			!février 2004
      COEFF(I2,9)=CONST(10)*LAMB*BB             										!février 2004
      COEFF(I1,10)=-CONST(7)*LAMB*ARG(IA)							     !Myx			!février 2004
      COEFF(I2,10)= CONST(7)*LAMB*ARG(IB)												!février 2004
      COEFF(I1,11)=-CONST(13)*LAMB*ARG(IA)						     !Mxy			!février 2004
      COEFF(I2,11)=CONST(13)*LAMB*ARG(IB)												!février 2004
      COEFF(I1,12)= ARG(IA)*CONST(13)*LAM(2) - AU5(I)*CONST(6)		 !Qy			!février 2004
     *               -CONST(4) * (-ARG(IA)/Q+AU2(I)*AAA+AU1(I)*BBB)					!février 2004
      COEFF(I2,12)=-ARG(IB)*CONST(13)*LAM(2) - AU6(I)*CONST(6)						!février 2004
     *               -CONST(4) * (ARG(IB)/Q+AU2(I)*BBB-AU1(I)*AAA)					!février 2004
      COEFF(I1,13)= AU2(I)*CONST(7)*LAMB-CONST(12)*LAM(3)              !Qx			!février 2004
     *				+AA*LAM(2)*CONST(10)											!février 2004
      COEFF(I2,13)=-AU1(I)*CONST(7)*LAMB+BB*LAM(2)*CONST(10)							!février 2004
      COEFF(I1,14)= CONST(4) * (ARG(IA)/Q-SE)						     !  Ry			!février 2004
     *          + LAM(2)*ARG(IA)*TT - CONST(6)*AU5(I)									!février 2004
      COEFF(I2,14)=-CONST(4) * (ARG(IB)/Q+SF)											!février 2004
     *          - LAM(2)*ARG(IB)*TT - CONST(6)*AU6(I)									!février 2004
      COEFF(I1,15)=TT*AU2(I)*LAMB-CONST(12)*LAM(3)+CONST(10)*AA*LAM(2) !Rx			!février 2004
      COEFF(I2,15)=-TT*AU1(I)*LAMB+CONST(10)*BB*LAM(2)				   				!février 2004
      COEFF(I1,16)=1.													 !	W			!février 2004
      COEFF(I2,16)=0.																	!février 2004
      COEFF(I1,37)=+AU2(I)											 !  Wøø			!février 2004
      COEFF(I2,37)=-AU1(I)															!février 2004
      COEFF(I1,38)=-AU5(I)											 !  Wøøø		!février 2004
      COEFF(I2,38)=-AU6(I)															!février 2004
      COEFF(I1,39)=SC													 !  Uø			!février 2004
      COEFF(I2,39)=SD																	!février 2004
      COEFF(I1,40)=SA													 !  Vø			!février 2004
      COEFF(I2,40)=SB																	!février 2004
      COEFF(I1,41)=SE													 !  Vøø			!février 2004
      COEFF(I2,41)=SF							                           				!février 2004
	ENDIF																			!février 2004

      IF(MT.EQ.0) GOTO 13
      SG= AU2(I)*AA  + AU1(I)*BB
      SL= AU2(I)*BB  - AU1(I)*AA
      DO 1400 ITRA=1,MT
      JTRAV=6*(ITRA-1)
      JJTRAV=2*(ITRA-1)
      COEFF(I1,17+JJTRAV)=-AA*LAM(2)*CONST(15+JTRAV)
     *          + LAM(3)*CONST(16+JTRAV)
     *          + CONST(17+JTRAV)*(SA*LAMB+SG)
      COEFF(I2,17+JJTRAV)=-BB*LAM(2)*CONST(15+JTRAV)
     *                        + CONST(17+JTRAV)*(SB*LAMB+SL)
      COEFF(I1,18+JJTRAV)=-AU2(I)*LAM(2)*CONST(19+JTRAV)
     *         - AA*LAM(3)*CONST(16+JTRAV) + LAM(4)*CONST(18+JTRAV)
     *         - CONST(20+JTRAV) * (LAM(2)*SA+LAMB*SG)
      COEFF(I2,18+JJTRAV)= AU1(I)*LAM(2)*CONST(19+JTRAV)
     *   - BB*LAM(3)*CONST(16+JTRAV)
     *   - CONST(20+JTRAV)* (SB*LAM(2)+LAMB*SL)
 1400 CONTINUE
   13 CONTINUE

      JTRAV=16+MT*2
      DO 952 I=1,M
        DO 950 J=1,41
        IF((J.GT.JTRAV).AND.(J.LT.37)) GOTO 950
        COEFF(8*I-5,J)=-COEFF(8*I-6,J)
        COEFF(8*I-4,J)= COEFF(8*I-7,J)
        COEFF(8*I-3,J)= COEFF(8*I-7,J)*ISIGN(J)
        COEFF(8*I-2,J)= COEFF(8*I-6,J)*ISIGN(J)
        COEFF(8*I-1,J)=-COEFF(8*I-2,J)
        COEFF(8*I  ,J)= COEFF(8*I-3,J)
  950   CONTINUE
        DO 951 J=1,13+2*MT
        DO 951 KK=1,NBRXI
        K=NXI(KK)
        DCOEFF(8*I-5,J,K)=-DCOEFF(8*I-6,J,K)
        DCOEFF(8*I-4,J,K)= DCOEFF(8*I-7,J,K)
        DCOEFF(8*I-3,J,K)= DCOEFF(8*I-7,J,K)*ISIGN2(J)
        DCOEFF(8*I-2,J,K)= DCOEFF(8*I-6,J,K)*ISIGN2(J)
        DCOEFF(8*I-1,J,K)=-DCOEFF(8*I-2,J,K)
  951   DCOEFF(8*I  ,J,K)= DCOEFF(8*I-3,J,K)
  952 CONTINUE

      MM=8*M
      IF(IMPR.EQ.0)GOTO5
      WRITE(66,43)
      DO 56 J=1,41
      IF((J.GT.JTRAV).AND.(J.LT.37)) GOTO 56
      WRITE(66,*) J
      WRITE(66,14)(COEFF(I,J),I=1,MM)
   56 CONTINUE

   43 FORMAT(//' COEFFICIENTS DES FONCTIONS EXPONENTIELLES, ORDONNEES',
     1' COMME SUIT',/' A COS(BY) ,B COS(BY) ,A SIN(BY) ,B SIN(BY)'/
     *' E COSB(2*PI-Y) ,F COSB(2*PI-Y) ,E SINB(2*PI-Y) ,F SINB(2*PI-Y)'/
     *' ILS SE REFERENT SUCCESSIVEMENT A :'/
     *' U V Wø NY NX NYX NXY MY MX MYX MXY QY QX RY RX W '/
     *' X0DY Z0DY  (maximum 10 groupes XODY,ZODY)'/
     *' Wøø,Wøøø,Uø,Vø et Vøø '//)
   14 FORMAT(8(1X,E14.7))
    5 RETURN
      END
