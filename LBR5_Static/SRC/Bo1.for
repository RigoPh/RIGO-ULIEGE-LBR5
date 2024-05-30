      SUBROUTINE BO1(NEL,ETA,ITYPE,WIDTH,IMPR,IJK,NETO,NE,
     *               KLI,ZSN,ABA,BAB,SOLT,JLMAX,IBAT,ICHA,IMOM,IBUSC,
     *               NBRXI,NXI,IPT,YPT,NSOL,
     *               BL,DZ,IDZ,
     *               ANGLE,NOEUD,MCOMP,NNO,NC,NSIGN,ISS)	!!!aout04 ISS=n°cas de charge

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION BL(1),DZ(1),IDZ(1)  

      COMMON/DIM1/Nmax,NEmax,NVmax,      ! dimension max des vecteurs
     *            M1max,M1Tmax,M2max,M2Tmax,Mmax,NGmax,ISmax,IPTmax 

C  Common BLOKA   remplacer par BL
	J1 =1				    !BLOKA(NE,NE) soit 1.28 MIO

	J2 =1					!COEFF(32,41)		= 1312
	J3 =J2 +32*41			!DCOEFF(32,33,9)	= 9504
	J4 =J3 +32*33*9			!ALI(360)			=  360
	J5 =J4 +360				!DVARA(33,9,16)		=19008 = 4* 4752
	J6 =J5 +33*9*16			!DVARB(33,9,16)
	J7 =J6 +33*9*16			!DVARC(33,9,16)
	J8 =J7 +33*9*16			!DVARD(33,9,16)
	J9 =J8 +33*9*16			!DVARH(33,9,38,ISmax) =112860 =11286 * ISmax
	J10=J9 +33*9*38*ISmax	!ALIX(10,13) avec 10 nbre de traverses
	J11=J10+10*13			!ALIZ(10,13)  et 13 les 13 variables calculees
	J12=J11+10*13			!DALIX(10,13,9)
	J13=J12+10*13*9			!DALIZ(10,13,9)     = 2600
	J14=J13+10*13*9			!BID1(20,20)		=  400
	J15=J14+20*20			!BID2(20,8+ISmax)   =  360  = 160 + 20* ISmax
	J16=J15+20*(8+ISmax)	!DHYP(20,8+ISmax,9)	= 3240  = 1440 + 180*ISmax
      J17=J16+20*9*(8+ISmax)   ! Total = 34784 + 11486 * ISmax = 149.644

C  Common ALLONG   remplacer par  DZ
      I1=1          ! ALG(400) vecteur de travail COE,COMPLE ou pour AUX2(400) LINEA
	I2=I1+M1Tmax      ! ABCD(8)
	I3=I2+8           ! XI(10)
	I4=I3+ISmax       ! XF(10)
	I5=I4+ISmax       ! DISH(1710,10)
      I6=I5+1710*ISmax  ! DISB(720)
	I7=I6+720         ! DISC(720)
	I8=I7+720         ! DISA(720)
      I9=I8+720         ! DISD(720)
	I10=I9 +720       ! DTEMP(11286)
	I11=I10+11286     ! CHA(100,3,10)  
      I12=I11+100*3*ISmax !dCHA(100,NSOL,9)										!!!aout04
      I13=I12+100*9*ISmax    !BID = CHAMAX(5,NSOL) +12   pour lecture READ 99		!!!aout04
      I14=I13+5*NSOL+12	!BY(1710)												!!!aout04
	I15=I14+1710		!BZ(1710)												!!!aout04
	I16=I15+1710																!!!aout04
c	Total = 47176																!!!aout04

	K1=1           ! NPTS(5)
	K2=K1+ISmax    ! ILINEA(8)
	K3=K2+8        ! 

      CALL BO11(NEL,ETA,ITYPE,WIDTH,IMPR,IJK,NETO,NE,
     *          KLI,ZSN,ABA,BAB,SOLT,JLMAX,IBAT,ICHA,IMOM,IBUSC,
     *          NBRXI,NXI,IPT,YPT,NSOL,
     *          ANGLE,NOEUD,MCOMP,NNO,NC,NSIGN,ISS,						!!!aout04
     *          BL(J1),                           !BLOKA(NE,NE)
     *  	      BL(J2),BL(J3),BL(J4),BL(J5),		!COEFF,DCOEFF,ALI,DVARA,
     *          BL(J6),BL(J7),BL(J8),BL(J9),		!DVARB,DVARC,DVARD,DVARH,
     *          BL(J10),BL(J11),BL(J12),BL(J13),	!ALIX,ALIZ,DALIX,DALIZ,
     *          BL(J14),BL(J15),BL(J16),          !BID1,BID2,DHYP
     *          DZ(I1),DZ(I2),DZ(I3),DZ(I4),DZ(I5),DZ(I6),DZ(I7),
     *          DZ(I8),DZ(I9),DZ(I10),DZ(I11),DZ(I12),DZ(I13),   !!!aout04
     *		  DZ(I14),DZ(I15),								   !!!aout04
     *          IDZ(K1),IDZ(K2) )

      RETURN
	END
c*************************************************************************
c*************************************************************************

      SUBROUTINE BO11 (NEL,ETA,ITYPE,WIDTH,IMPR,IJK,NETO,NE,
     *          KLI,ZSN,ABA,BAB,SOLT,JLMAX,IBAT,ICHA,IMOM,IBUSC,
     *          NBRXI,NXI,IPT,YPT,NSOL,
     *          ANGLE,NOEUD,MCOMP,NNO,NC,NSIGN,ISS,				!!!aout04
     *          BLOKA,
     *          COEFF,DCOEFF,ALI,DVARA,DVARB,DVARC,DVARD,DVARH,
     *          ALIX,ALIZ,DALIX,DALIZ,BID1,BID2,DHYP,
     *          ALG,ABCD,XI,XF,DISH,DISB,DISC,
     *          DISA,DISD,DTEMP,CHA,dCHA,BID,						!!!aout04
     *		  BY,BZ,											!!!aout04
     *          NPTS,ILINEA)

C***********************************************************************
C     SUBROUTINE BO1
C     ***************
C     SUBROUTINE DE RESOLUTION DE LA PLAQUE CONSIDEREE COMME TELLE.
C     LA SUBROUTINE CALCULE LES ARGUMENTS DE LA FONCTION EXPONENTIELLE,
C     RESOUT L'EQUATION DIFFERENTIELLE DE CETTE FONCTION,CALCULE LES
C     COEFFICIENTS DES DIVERSES GRANDEURS UTILES,RESOUT LES QUATRE
C     SYSTEMES DE CHARGE LINEAIRE EN X=0,INTRODUIT LES CAS DE CHARGE
C     EXTERIEURE POSSIBLES,CALCULE LES REACTIONS HYPERSTATIQUESDES
C     TRAVERSES ,CALCULE LES COEFFICIENTS DE CONTINUITES AUX EXTREMITES
C     DU PANNEAU POUR RANGEMENT ET RESOLUTION DANS LA SUBROUTINE MDR.
C
C     Créer: Thèse de Ph. Rigo (1987)
C
C	Modifié : 1994     MODULE OPTIMISATION pour LBR-5 ;
C	        :18-03-96  Subr DLOCY et DLOCZ;
C	        :28-03-96  Subr MOM et DMOM, 1 call au lieu de 2;
C	        : 6-05-96  Dimension de BID(36) cfr READ(99);
C	        : 3-03-97  Dimension de BID(37) cfr READ(99);
C             :21-11-97  Suppression des termes pairs pour PRESS et HYDRO(IJK= 2,4,..);
C              23-03-99  Changements ds "Common" et le transfert des arguments;
C               1-03-02  Introduction des efforts de Buscage (et forces de bords)
c			: 4-02-04  Epontilles
c
C Dernière modif.: 04.02.2004

C***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)

      REAL*8    LAMB,LAM,IETOIL,JETOIL,KETOIL
      INTEGER*2 KLI

      CHARACTER*11 NOM   ! 'FichDessin\'
      CHARACTER*22 NOM1  ! 'A.MULTI.STRUC\STRUC-a\' ou ' '
 	COMMON/NOM/NOM,NOM1  

      COMMON/DIM1/Nmax,NEmax,NVmax,
     *            M1max,M1Tmax,M2max,M2Tmax,Mmax,NGmax,ISmax,IPTmax ! dimension max des vecteurs

      DIMENSION CONST(74),HXTR(10),DXTR(10),WXTR(10),TXTR(10),ABTR(10),
     *          EQU(9),EQU2(9),ARG(8),ARGQ(8),LAM(8),
     *          COEFF(32,41),COF(9),ROOT(8),ROOT1(8),
     *          ALI(360),
     *          SINU(4),COSI(4),EXPT(4),IP(4),K(4),BID(5*NSOL+12)
     
      DIMENSION  NXI(9),DEQU(9),DA(9),DCOEFF(32,33,9),PX(8),DPX(8,9),
     *     DVARA(33,9,16),DVARB(33,9,16),DVARC(33,9,16),DVARD(33,9,16),
     *     DVARH(33,9,38,NSOL)

c     POUR le COMMOM OPTI2 (IPT = 4 et NSOL=10 a changer manuellement)
      DIMENSION SENSH(13,4,9,10),   !SENSH(13,IPT,9,NSOL), 
     *  SENSA(13,4,9) ,SENSB(13,4,9), SENSC(13,4,9) ,SENSD(13,4,9) ,
     *  SENSAA(13,4,9),SENSBB(13,4,9),SENSCC(13,4,9),SENSDD(13,4,9),
     *    DEFA(13,4),   DEFB(13,4),    DEFC(13,4),    DEFD(13,4),
     *    DEFAA(13,4),  DEFBB(13,4),   DEFCC(13,4),   DEFDD(13,4)

	DIMENSION YPT(IPTmax),
     *      ALIX(10,13),   ALIZ(10,13), ! 10 = nbre max de traverse
     *     DALIX(10,13,9),DALIZ(10,13,9),  ! 13 = les 13 variables
     *     DHYP(20,8+ISmax,9), BID1(20,20), BID2(20,13)
C
      DIMENSION ABA(NE,NE),BAB(NE,NE+1),KLI(NE,2),BLOKA(NE,NE),
     *          ZSN(NE,NSOL),SOLT(NE,NE)
C
      DIMENSION ALG(1),ILINEA(8)
	DIMENSION CHA(100,3,NSOL),ABCD(8),XI(NSOL),XF(NSOL),NPTS(NSOL),
     *          DISH(1710,NSOL)
      DIMENSION DISB(720),DISC(720),DISA(720),DISD(720),DTEMP(11286)
c               DISLA(1710),DISLB(1710),DISLC(1710),DISLD(1710),
c               DISMA(1710),DISMB(1710),DISMC(1710),DISMD(1710)

C     DTEMP= Vect de travail DTEMP(1102)  pour calculer DISH et 
C                            DTEMP(11286) pour calculer DVARH
	DIMENSION dCHA(100,NSOL,9),BZ(1710),BY(1710)	!!!aout04
	DIMENSION NF(22)								!!!aout04

c     Les COMMONS
      COMMON/PY/  PI,SINU,COSI,EXPT,LAM
      COMMON      AUX(400),ZB(360)
      COMMON/ENT/ ABTR,ARGQ,CONST,HXTR,DXTR,WXTR,TXTR
      COMMON/HYP/ HYP(360)

      COMMON/OPTI/  IOPTI
      COMMON/OPTI2/ DEFA,DEFB,DEFC,DEFD,DEFAA,DEFBB,DEFCC,DEFDD,
     *              SENSH, SENSA, SENSB, SENSC, SENSD,
     *                     SENSAA,SENSBB,SENSCC,SENSDD

      COMMON/OPTI3/ CONST2(6,9),CONST3(2,10),DARG(8,9)

C***********************************************************************
C***********************************************************************

C     Cfr Subr ENT et BO2
      READ(99)ABTR,PHIL,MT,TETA,XI,XF,HXTR,WXTR,Q,KSE,KST,KSA,
     *        TXTR,DXTR,KSR,A1,B1,C1,D1,DELTO,IVARIA,CONST,CONST2,
     *        CONST3,BID,IBID
      IF(ICHA.EQ.1) READ(98) NPTS,CHA,dCHA !rem.: ici, dans BO1,ICHA=IPOIDS !!!aout04

C      Var. de concept. = Epaisseur du bordage (K=1)
C      ---------------------------------------------
c	 CONST(1)= D  -> CONST2(1,1)=d(D)/d(épaiss)			
c	 CONST(2)= K  -> CONST2(2,1)=d(K)/d(épaiss)			
c	 CONST(3)= Hy -> CONST2(3,1)=d(Hy)/d(épaiss)			
c	 CONST(4)= Ry -> CONST2(4,1)=d(Ry)/d(épaiss)			
c	 CONST(5)= Hx -> CONST2(5,1)=d(Hx)/d(épaiss)			
c	 CONST(6)= Rx -> CONST2(6,1)=d(Rx)/d(épaiss)			

C      Var. de concept. relative aux cadres (k=2,5) et aux raid.(k=6,9) 
C      ---------------------------------------------------------------
C      Constante	    Dérivée(k=2,5)   Constante	 Dérivée(k=6,9)
c    1 CONST(3)= Oy -> CONST2(1,k)     CONST( 9)= Ox -> CONST2(1,k)				
c    2 CONST(4)= Hy -> CONST2(2,k)     CONST(10)= Hx -> CONST2(1,k)				
c    3 CONST(5)= Sy -> CONST2(3,k)     CONST(11)= Sx -> CONST2(1,k)				
c    4 CONST(6)= Ry -> CONST2(4,k)     CONST(12)= Rx -> CONST2(1,k)				
c    5 CONST(7)= Ty -> CONST2(5,k)     CONST(13)= Tx -> CONST2(1,k)				
c    6 CONST(8)= Ly -> CONST2(6,k)     CONST(14)= Lx -> CONST2(1,k)				
C
C     CALCUL DES ARGUMENTS DES FONCTIONS EXPONENTIELLES
C     -------------------------------------------------
C     LES VALEURS ABSOLUES DE LEURS PARTIES REELLES ET COMPLEXES SONT
C     ENTREPOSEES SUCCESSIVEMENT DANS LE VECTEUR ARG.
!
      PI1=PI/180.   ! constant
      LAMB=LAM(1)
      ETA2=(1.-ETA)/2.
      H=CONST(1)+2.*(CONST(5)+CONST(11))
      G=CONST(1)*ETA
      Z=(H-G)/2.
      A=(H+G)/2.
      B=CONST(2)+CONST(12)
      C=CONST(1)+CONST(9)
      D=2.*CONST(2)+CONST(7)+CONST(13)
      F=CONST(8)+CONST(14)
      
	IF((ITYPE.EQ.1).OR.(ITYPE.EQ.2)) THEN
        X=CONST(2)+CONST(6)  ! cas normal
        W=CONST(1)+CONST(3)
      ELSE IF((ITYPE.EQ.3).OR.(ITYPE.EQ.4).OR.(ITYPE.EQ.5)) THEN			!février 2004
        X=CONST(6)  ! la contribution transversale du bordé est supprimée
        W=CONST(3)
	ELSE
	 WRITE(66,*) 'In BO1: ITYPE is wrong =',ITYPE
	 WRITE(29,*) 'In BO1: ITYPE is wrong =',ITYPE								!bug
	 PAUSE
	 STOP
	ENDIF

      BC=B*C-CONST(10)*CONST(10)
      XW=X*W-CONST(4)*CONST(4)
      CW=C*W-G*G
      BW=B*W+X*C
      OXY=C+W
	DXY=OXY-2.*G*ETA
	RXY=B+X
	WC=CONST(10)*W+CONST(4)*C
	GH=C*W-G*H
	SS=CONST(5)+CONST(11)                 ! Sx+Sy	!février 2004
	TT=CONST(7)+CONST(13)                 ! Tx+Ty	!février 2004

C     Z= D*(1-ETA)/2+(Sx+Sy)            ETA2 = (1-ETA)/2                 
C     A= D*(1+ETA)/2+(Sx+Sy)            BC =(K+Rx)*(D+Ox)-(Hx)**2           
C     B= (K+Rx)                         XW =(K+Ry)*(D+Oy)-(Hy)**2                    
C     C= (D+Ox)                         CW =(D+Ox)*(D+Oy)-(D*ETA)**2                      
C     D= 2K+(Tx+Ty)                     BW =(K+Rx)*(D+Oy)+(K+Ry)*(D+Ox)             
C     F= (Lx+Ly)                        DXY=2D*(1.-ETA**2)+Ox+Oy                      
C     G= D*ETA                          OXY=2D+(Ox+Oy)                          
C     H= D+2(Sx+Sy)                     RXY=2K+(Rx+Ry)      
C     X= (K+Ry)                         WC =Hx*(D+Oy)+Hy*(D+Ox)                      
C     W= (D+Oy)                         GH =(D+Ox)*(D+Oy)-D*ETA *(D+2(Sx+Sy))

	IF(ITYPE.EQ.5) THEN													!février 2004	 
C		CAS D'UNE EPONTILLE												!février 2004
		AETOIL=(CONST(9)*CONST(12)-(CONST(10)*CONST(10)))*SS			!février 2004
		CETOIL=CONST(12)*CONST(3)*CONST(9)+CONST(9)*SS*TT				!février 2004
     *				-CONST(3)*CONST(10)*CONST(10)						!février 2004
		DETOIL=0.D+00													!février 2004
		EETOIL=0.D+00													!février 2004
		FETOIL=(SS*(CONST(12)*CONST(3)+CONST(6)*CONST(9)				!février 2004
     *				+2*CONST(10)*CONST(4)))+TT*CONST(9)*CONST(3)		!février 2004
		JETOIL=CONST(6)*CONST(3)*CONST(9)+SS*TT*CONST(3)				!février 2004
     *				-CONST(9)*CONST(4)*CONST(4)							!février 2004
		KETOIL=SS*(CONST(3)*CONST(6)-CONST(4)*CONST(4))					!février 2004
		BETOIL=0.D+00													!février 2004
		GETOIL=0.D+00													!février 2004
		HETOIL=0.D+00													!février 2004
		IETOIL=0.D+00													!février 2004
	ELSE																!février 2004
      AETOIL=Z*BC
      CETOIL=(B*W+D*Z)*C - (G*F+W*CONST(10))*CONST(10) - B*G*H
      FETOIL=D*GH + F*WC + CONST(4)*CONST(10)*2.*A + Z*BW
C     GETOIL=0.  (cfr version p : Nxy=Nyx)
      JETOIL=W*(C*X+D*Z) - CONST(4)*(CONST(4)*C+G*F) - G*H*X
      KETOIL=Z*XW
      IF(Q.GE.(1.E5)) THEN
C         CAS D'UNE PLAQUE RAIDIE  (rayon très grand > 10000 m)
          BETOIL=0.D+00
          DETOIL=0.D+00
          EETOIL=0.D+00
          IETOIL=0.D+00
        ELSE
C         CAS D'UNE COQUE RAIDIE  (rayon <10000 m)
          BETOIL=2.*G*CONST(10)*Z/Q
          DETOIL=Z*CW/(Q*Q)
          EETOIL=-(F*CW+2.*Z*WC)/Q
          IETOIL=2.*CONST(4)*G*Z/Q
      ENDIF
	ENDIF																!février 2004

C     Calcul des coefficients (EQU) de l'équation diff. du 8ème ordre
C        (voir dans Subr. DARG)
C     ------------------------------------------------------------------
      CALL ASSEMB (LAM,EQU,AETOIL,BETOIL,CETOIL,DETOIL,EETOIL,FETOIL,
     *                     IETOIL,JETOIL,KETOIL)

      IF(IMPR.EQ.0) GOTO 5
        WRITE(66,4)  NEL
        WRITE(66,*) 'A*  B*  C* ',AETOIL,BETOIL,CETOIL,DETOIL
        WRITE(66,*) 'D*  E*  F*,',DETOIL,EETOIL,FETOIL
        WRITE(66,*) 'I*  J*  K*,',IETOIL,JETOIL,KETOIL
        WRITE(66,12) IJK,(EQU(I),I=1,9)
   5  CONTINUE

C     RESOLUTION DE L'EQUATION DIFFERENTIELLE DU 8ème ORDRE (call POLRT)
C     DISCUSSION DES RESULTATS  (call DISCU)
C     ------------------------------------------------------------------

	DO 515 I=1,9
  515 EQU2(I)=EQU(I)
      IF=1

      CALL POLRT (EQU2,COF,8,ROOT,ROOT1,IER)

c      WRITE(66,*) 'ROOT  =',ROOT
c      WRITE(66,*) 'ROOT1 =',ROOT1
c      WRITE(66,*) 'IER =',IER

      CALL DISCU (ROOT,ROOT1,IER,ARG,IF,M) 

      IF(IF.EQ.0) THEN
      WRITE(*,*)"ERROR dans la résolution de l'équ. diff.(subr. Discu)"
      WRITE(*,*)"ERROR solving Diff. equation (Subr. Discu)"
      WRITE(*,*)
      WRITE(*,*)"VERIFIER"
      WRITE(*,*)"Les dimensions des raidisseurs et cadres"
      WRITE(*,*)"peuvent être trop petites."
      WRITE(*,*)"CHECK"
      WRITE(*,*)"The dimensions of the stiffeners and frames"
      WRITE(*,*)"are maybe to small."
      WRITE(66,*)"ERROR dans la résolution de l'équ. diff.(subr. Discu)"
      WRITE(66,*)"ERROR solving Diff. equation (Subr. Discu)"
      WRITE(66,*)
      WRITE(66,*)"VERIFIER"
      WRITE(66,*)"Les dimensions des raidisseurs et cadres"
      WRITE(66,*)"peuvent être trop petites."
      WRITE(66,*)"CHECK"
      WRITE(66,*)"The dimensions of the stiffeners and frames"
      WRITE(66,*)"are maybe to small."
      WRITE(29,*)'ERROR dans la résolution de l''équ. diff.(subr.Discu)'					!bug																								
      WRITE(29,*)'VERIFIER Les dimensions des raidisseurs et cadres 
     * peuvent être trop petites.'														!bug
      WRITE(29,*)																			!bug

        PAUSE 'STOP!'
        STOP
      ENDIF

      MM=2*M
      MS=2*MT
      DO 789 I=1,MM
 789  ARGQ(I)=ARG(I)*Q
      IF(IMPR.EQ.1) WRITE(66,42) (ARG(I),I=1,MM)

C     Calcul des DERIVEES des ARG(i) par rapport aux var. de conception,
C     soit DARG(i,k) avec i=1,M et k=1,NBRXI                                           
C     ------------------------------------------------------------------
      IF(IOPTI.GE.1) THEN
        IF((IMPR.NE.0).AND.(NBRXI.NE.0)) WRITE(66,644)
        IT=1
        DO 642  IK=1,NBRXI
          KK=NXI(IK)
c         WRITE(66,*) 'entrée dans DARG'
          CALL DARGS(LAM,DEQU,Q,CONST,A,B,C,D,F,H,G,X,W,Z,BC,CW,BW,
     *               XW,DXY,WC,OXY,RXY,GH,ETA,ETA2,KK,DA,NXI,NBRXI,
     *			   SS,TT,ITYPE)											!février 2004
c         WRITE(66,*) 'sortie dans DARG'

          DO 643 I=1,M
c         WRITE(66,*) 'avant entrée dans COMPLE, I=',I
          CALL COMPLE(EQU,DEQU,ARG,DARG,NBRXI,I,KK,
     *                DISA(1),DISA(33),IT,ALG)
  643     CONTINUE

  	    IF(IMPR.NE.1) GOTO 642
          WRITE(66,'(A30,I2)') 'Variable de conception nø ',KK 
          WRITE(66,'(A25,9E12.5)')'d(EQU)/dXI  =',DEQU 
          WRITE(66,'(A25,4E12.5)')'d(alpha)/dXi=',(DARG(2*I-1,KK),I=1,M)
          WRITE(66,'(A25,4E12.5)')'d(béta )/dXi=',(DARG(2*I,KK),I=1,M)
	    IT=IT+1
  642   CONTINUE
      ENDIF
	
C     CALCUL DES COEFFICIENTS DES DIVERSES GRANDEURS UTILES (call COE)  
C     ---------------------------------------------------------------             
C     ENTREPOSE EN COEFF(I,J) LES COEFFICIENTS DE EXP(-A*Y)*COS(B*Y),   
C     EXP(-A*Y)*SIN(B*Y), EXP(-A(2*PI-Y))*COS(B(2*PI-Y)), EXP(-A(2*PI-Y))*
C     SIN(B(2*PI-Y)),..., POUR CHAQUE RACINE COMPLEXE PRECEDEMENT DEFINIE
C     ET POUR CHAQUE EFFET A CONSIDERER, CORRESPONDANT RESPECTIVEMENT A  
C     U,V,DW/DY,NY,NX,NYX,NXY,MY,MX,MYX,MXY,QY,QX,RY,RX,W ET 10 FOIS    
C     (XODX,ZODX)  POUR UN MAXIMUM DE 10 TRAVERSES

      DO 501 I=1,4
         SINU(I)=0.
         COSI(I)=0.
         EXPT(I)=0.
  501 CONTINUE
      DO 500 I=1,M
        EXPT(I)=EXPO(ARGQ(2*I-1)*2*PI)
  500 CONTINUE

      CALL COE(CONST,ARG,COEFF,LAM,ETA,ETA2,MT,M,Z,X,W,A,B,C,F,H,G,
     *         IMPR,Q,NBRXI,NXI,DCOEFF,DARG,CONST2,CONST3,ALG,IOPTI,
     *		 ITYPE)														!février 2004

C     CALCUL DES EFFETS DES QUATRE CHARGES LINEAIRES ET DE LA CHARGE
C     EXTERIEURE
C     --------------------------------------------------------------
C     ILS SONT ENTREPOSES DANS DISC(I),DISB(I),DISA(I),DISD(I), AVEC 16
C     MEMOIRES POUR CHAQUE EFFET, ET DANS DISH(I) AVEC 37 MEMOIRES POUR
C     CHAQUE EFFET.

      DO 507 I=1,4
        K(I)=1
 507  CONTINUE

      GOTO(503,504,505),M-1
 503  IP(1)=1
      IP(2)=2
      IP(3)=9
      IP(4)=10
      K(3)=2
      K(4)=2
      SINU(1)=DSIN(ARGQ(2)*2.*PI)
      SINU(2)=DSIN(ARGQ(4)*2.*PI)
      COSI(1)=DCOS(ARGQ(2)*2.*PI)
      COSI(2)=DCOS(ARGQ(4)*2.*PI)
      GOTO 506
 504  IP(1)=1
      IP(2)=9
      IP(3)=17
      IP(4)=18
      K(2)=2
      K(3)=3
      K(4)=3
      SINU(3)=DSIN(ARGQ(6)*2.*PI)
      COSI(1)=DCOS(ARGQ(2)*2.*PI)
      COSI(2)=DCOS(ARGQ(4)*2.*PI)
      COSI(3)=DCOS(ARGQ(6)*2.*PI)
      GOTO 506
 505  IP(1)=1
      IP(2)=9
      IP(3)=17
      IP(4)=25
      K(2)=2
      K(3)=3
      K(4)=4
      COSI(1)=DCOS(ARGQ(2)*2.*PI)
      COSI(2)=DCOS(ARGQ(4)*2.*PI)
      COSI(3)=DCOS(ARGQ(6)*2.*PI)
      COSI(4)=DCOS(ARGQ(8)*2.*PI)
 506  CONTINUE

      IF((IMPR.EQ.1).AND.(IOPTI.EQ.0)) WRITE(66,50)
      IF((IMPR.EQ.1).AND.(IOPTI.GE.1)) WRITE(66,51)
      Q2PI = 2.*PI*Q


      IF(IMPR.EQ.1) WRITE(66,*) 'Calcul relatif à DISC'
      IF(IMPR.EQ.1) WRITE(66,*) '______________________'

      CALL LINEA(M,COEFF,DCOEFF,2,6,3,14,DISC,HYP,K,MT,IP,1,IMPR,Q2PI,
     *           DPX,PX,DARG,NXI,NBRXI,ALG,ILINEA(1),ILINEA(5),IOPTI)
      IF(IOPTI.GE.1)
     *  CALL DPAR(M,PX,COEFF,DCOEFF,DPX,DVARC,1,NXI,NBRXI,MT)

      IF((IMPR.NE.0).AND.(IOPTI.GE.1)) THEN
       DO  2356 IL=1,NBRXI
         L=NXI(IL)
         WRITE(66,*) 'Var. de concept. nø',L
         WRITE(66,47) (DPX(I,L),I=1,8)
   47    FORMAT('DPX=',5X,8(E13.6,1X))
         WRITE(66,*) ' '
       DO 2356 II=1,13+2*MT
         WRITE(66,123)II,(DVARC(II,L,I),I=1,16)
 2356  CONTINUE
  123  FORMAT('Fct.',I2,3X,8(E13.6,1X)/,9X,8(E13.6,1X))
      ENDIF


      IF(IMPR.EQ.1) WRITE(66,*) 'Calcul relatif à DISB'
      IF(IMPR.EQ.1) WRITE(66,*) '______________________'

      CALL LINEA(M,COEFF,DCOEFF,2,3,14,6,DISB,HYP,K,MT,IP,1,IMPR,Q2PI,
     *           DPX,PX,DARG,NXI,NBRXI,ALG,ILINEA(1),ILINEA(5),IOPTI)
      IF(IOPTI.GE.1)
     * CALL DPAR(M,PX,COEFF,DCOEFF,DPX,DVARB,1,NXI,NBRXI,MT)


      IF(IMPR.EQ.1) Write(66,*) 'Calcul relatif à DISA'
      IF(IMPR.EQ.1) Write(66,*) '______________________'
      CALL LINEA(M,COEFF,DCOEFF,16,1,8,4,DISA,HYP,K,MT,IP,-1,IMPR,Q2PI,
     *           DPX,PX,DARG,NXI,NBRXI,ALG,ILINEA(1),ILINEA(5),IOPTI)
      IF(IOPTI.GE.1) 
     * CALL DPAR(M,PX,COEFF,DCOEFF,DPX,DVARA,-1,NXI,NBRXI,MT)


      IF(IMPR.EQ.1) Write(66,*) 'Calcul - relatif à DISD'
      IF(IMPR.EQ.1) Write(66,*) '______________________'
      CALL LINEA(M,COEFF,DCOEFF,16,1,4,8,DISD,HYP,K,MT,IP,-1,IMPR,Q2PI,
     *           DPX,PX,DARG,NXI,NBRXI,ALG,ILINEA(1),ILINEA(5),IOPTI)
      IF(IOPTI.GE.1)
     * CALL DPAR(M,PX,COEFF,DCOEFF,DPX,DVARD,-1,NXI,NBRXI,MT)

C ----------------------------------------------------------------------
C     CALCUL DE Vø,Uø,Wø, Vøø,Uøø,Wø, Vøøø,Uøøø,Wøøø                     
C     POUR LES CAS DE CHARGE DISA,DISB,DISC,DISD   ( Cfr Subroutine  DERIV)       
C      &                                                                 
C     COMPLEMENT AU CALCUL DES COEFFICIENTS DES DIVERSES GRANDEURS      
C     UTILES  C-A-D  NX,NXY,MX,MXY,MYX,QX,RX   (Cfr Subroutine MANQ )
C ----------------------------------------------------------------------
C
      CALL LINDE(3,M,01,DISA,ARG,30)
      CALL LINDE(3,M,30,DISA,ARG,33)
      CALL LINDE(3,M,33,DISA,ARG,36)
      CALL LINDE(3,M,01,DISB,ARG,30)
      CALL LINDE(3,M,30,DISB,ARG,33)
      CALL LINDE(3,M,33,DISB,ARG,36)
      CALL LINDE(3,M,01,DISC,ARG,30)
      CALL LINDE(3,M,30,DISC,ARG,33)
      CALL LINDE(3,M,33,DISC,ARG,36)
      CALL LINDE(3,M,01,DISD,ARG,30)
      CALL LINDE(3,M,30,DISD,ARG,33)
      CALL LINDE(3,M,33,DISD,ARG,36)
      CALL MANQ(LAM,ETA,CONST,DISA,16,Q,ITYPE)				!février 2004
      CALL MANQ(LAM,ETA,CONST,DISB,16,Q,ITYPE)				!février 2004
      CALL MANQ(LAM,ETA,CONST,DISC,16,Q,ITYPE)				!février 2004
      CALL MANQ(LAM,ETA,CONST,DISD,16,Q,ITYPE)				!février 2004

C     INTEGRATION DE LA PRESSION HYDRAULIQUE et CHARGES DE BORD.
C     ===========================================================
C     (Subroutines HYDROS, PRESS, BORD, MOM )
C
      DO I=1,1710  ! mise a Zero (initialisation de DISH)
         DO IS=1,NSOL
            DISH(I,IS)=0.0
         ENDDO
      ENDDO
c
      IF(IOPTI.GE.1) THEN   ! mise a Zero (initialisation de DVARH)
        DO 8427 IS=1,NSOL
        DO 8427 II=1,13+2*MT
        DO 8427 KK=1,NBRXI
          K1=NXI(KK)
          DO 8427 J=1,38
            DVARH(II,K1,J,IS)=0.
 8427   CONTINUE
      ENDIF

c     Subr PRESS et HYDROS
	BKSE=1.
      ZE  =1.D-60
      IF(KSE.EQ.1) BKSE=-1.

      IF(IBAT.EQ.1) THEN  !IBAT=0 si terme pair (-->pas utile pour charge sym)
       IF(KSE.NE.0) THEN
        IF(IVARIA.EQ.1) THEN  ! SUBR PRESS et DPRESS
          DO  IS=1,NSOL
            CALL PRESS(ARG,M,XI(IS),XF(IS),PHIL,DISC,DISH(1,IS),Q,IJK,
     *                 MS,BKSE)
	    ENDDO
          IF((IOPTI.GE.1).AND.(NBRXI.GE.1)) THEN
            DO  IS=1,NSOL
              CALL DPRESS(ARG,M,XI(IS),XF(IS),PHIL,Q,DISC,DVARC,
     *                    DVARH(1,1,1,IS),BKSE,IJK,NXI,NBRXI,MT)
	      ENDDO
          ENDIF
        ELSE    ! SUBR HYDRO et DHYDRO
	    DO  IS=1,NSOL
            CALL HYDRO(ARG,M,XI(IS),XF(IS),PHIL,DISC,DISH(1,IS),TETA,Q,
     *                 IJK,MS,BKSE)
	    ENDDO
          IF((IOPTI.GE.1).AND.(NBRXI.GE.1)) THEN
            DO  IS=1,NSOL
            CALL DHYDRO(ARG,M,XI(IS),XF(IS),PHIL,Q,DISC,DVARC,
     *                  DVARH(1,1,1,IS),BKSE,IJK,TETA,NXI,NBRXI,MT)
	      ENDDO
          ENDIF
        ENDIF  ! IF(IVARIA.EQ.1)
	 ENDIF  ! IF(KSE.NE.0) 

c     Subr. BORD (8 forces et moments de bord unitaires)
c     CALL BORD(ARG,M,A1,ZE,ZE,ZE,PHIL,DISB,DISLA,Q,IJK,WIDTH,DELTO,1,MS)
c     CALL BORD(ARG,M,ZE,B1,ZE,ZE,PHIL,DISB,DISLB,Q,IJK,WIDTH,DELTO,1,MS)
c     CALL BORD(ARG,M,ZE,ZE,C1,ZE,PHIL,DISB,DISLC,Q,IJK,WIDTH,DELTO,1,MS) 
c     CALL BORD(ARG,M,ZE,ZE,ZE,D1,PHIL,DISB,DISLD,Q,IJK,WIDTH,DELTO,1,MS) 
c     CALL BORD(ARG,M,A1,ZE,ZE,ZE,PHIL,DISC,DISMA,Q,IJK,WIDTH,DELTO,2,MS) 
c     CALL BORD(ARG,M,ZE,B1,ZE,ZE,PHIL,DISC,DISMB,Q,IJK,WIDTH,DELTO,2,MS) 
c     CALL BORD(ARG,M,ZE,ZE,C1,ZE,PHIL,DISC,DISMC,Q,IJK,WIDTH,DELTO,2,MS) 
c     CALL BORD(ARG,M,ZE,ZE,ZE,D1,PHIL,DISC,DISMD,Q,IJK,WIDTH,DELTO,2,MS)
	ENDIF  ! IF(IBAT.EQ.1)


      IF(IOPTI.GE.1)THEN     ! initialisation - mise à zéro                                      
        CALL ANNULD(DTEMP,11286)   ! DTEMP = DVARH(33,9,38)
	ELSE
        CALL ANNULD(DTEMP,1710)    ! DTEMP = DISH(1710)		!!!aout04
	ENDIF

c     ** Boucle sur LOC,LOCY et LOCZ + MOM (bateau et BUS)   
      DO 5326 IS=1,NSOL    ! BOUCLE SUR LES IS CAS DE CHARGE
	
      ! Subr LOC,LOCY et LOCZ   
      IF(ICHA.NE.0) THEN                                               
       CALL LOCY(ARG,M,1.0D+00,PHIL,DISA,DTEMP,DISH(1,IS),TETA,Q,
     *          IJK,LAMB,WIDTH,IMPR,MS,IS,CHA,NPTS,NSOL,BY)			!!!aout04

       CALL LOCZ(ARG,M,-1.0D+00,PHIL,DISC,DTEMP,DISH(1,IS),TETA,Q,
     *          IJK,LAMB,WIDTH,IMPR,MS,IS,CHA,NPTS,NSOL,BZ)			!!!aout04

       CALL LOC(ARG,M,PHIL,DISC,DTEMP,DISH(1,IS),TETA,Q,
     *         IJK,LAMB,WIDTH,IMPR,MS,IS,CHA,NPTS,BKSE,NSOL)

       IF((IOPTI.GE.1).AND.(NBRXI.GE.1)) THEN
        CALL DLOCY(ARG,M,+1.0D+00,PHIL,DISA,DVARA,DTEMP,DVARH(1,1,1,IS),
     *         TETA,Q,IJK,LAMB,WIDTH,IMPR,CHA,dCHA,NPTS,NXI,NBRXI,MT,IS,  !!!aout04
     *         NSOL,BY)                                                   !!!aout04

        CALL DLOCZ(ARG,M,-1.0D+00,PHIL,DISC,DVARC,DTEMP,DVARH(1,1,1,IS),
     *         TETA,Q,IJK,LAMB,WIDTH,IMPR,CHA,dCHA,NPTS,NXI,NBRXI,MT,IS,  !!!aout04
     *         NSOL,BZ)                                                   !!!aout04

        CALL DLOC(ARG,M,PHIL,DISC,DVARC,DTEMP,DVARH(1,1,1,IS),TETA,
     *         Q,IJK,LAMB,WIDTH,IMPR,CHA,NPTS,BKSE,NXI,NBRXI,MT,IS,NSOL)
c        Write(66,*)' Après DLOC : Fonct nø 8 (=W)'
c        Write(66,*) (DVARH(8,1,I,1),I=1,6)
c        WRITE(66,*) (DVARH(8,1,I,1),I=7,14)
c        WRITE(66,*) (DVARH(8,1,I,1),I=15,22)
c        WRITE(66,*) (DVARH(8,1,I,1),I=23,30)
       ENDIF
      ENDIF  
 
C     Lecture des données relatives aux moments de bords
      IF(ITYPE.EQ.5) GOTO 5326								!coefficient de participation
	IF(IMOM.EQ.0) THEN
	  CALL ANNULD(ABCD,8)
	ELSE
        READ(97) ABCD
	ENDIF

C ************************************************************************
C     Lecture des efforts Nb et moments Mb de bord(dans File BUSC.txt et BUSM.txt)
c     ! Nb et Mb  sont les mêmes pour tous les cas de charge !!!!
      IF(IBUSC.EQ.0) GOTO 32

c     a) FORCE Nb (agissant dans le plan du bordé)
c        ---------
      IF((NEL.EQ.1).and.(IS.EQ.1).and.(IJK.EQ.1)) THEN
         OPEN(96,FILE=NOM1//'BUSC.txt',STATUS='OLD',ERR=657)
	   READ(96,'(A1)') IBUS ! Skip ligne titre
	   GOTO 658
 657     Write(*,*)'STOP: NOM du Fichier (BUSC.txt) est INCORRECT'
         Write(*,*)'    : ERROR, (BUSC.txt) DATA FILE not valid'
         	   Write(29,*)'STOP: NOM du Fichier (BUSC.txt) est INCORRECT'					!bug
         STOP  
 658     CONTINUE
	ENDIF

      IF(IS.EQ.1) THEN
       IF((NEL.EQ.1).and.(IJK.GT.1)) THEN
         REWIND 96
	   READ(96,'(A1)') IBUS ! Skip ligne titre
	 ENDIF
	 READ(96,*)BUSC1,BUSC2,BUSC3,BUSC4,BUSC5,BUSC6,BUSC7,BUSC8
       IF(IJK.EQ.1) THEN
	   WRITE(66,'(A,I3,8(1x,E11.4),A)')
     *       'NEL=',NEL,BUSC1,BUSC2,BUSC3,BUSC4,BUSC5,BUSC6,BUSC7,BUSC8,
     *       ' forces(N/m)'
	 ENDIF
	ENDIF

c	! Charge en X**3
        ABCD(1)=ABCD(1)+ BUSC1   ! Les efforts de buscage Nb 
        ABCD(5)=ABCD(5)+ BUSC5   ! s'ajoutent aux effets du moment d'ensemble (Subr Bateau)
c	! Charge en X**2
        ABCD(2)=ABCD(2)+ BUSC2
        ABCD(6)=ABCD(6)+ BUSC6
c	! Charge en X
        ABCD(3)=ABCD(3)+ BUSC3
        ABCD(7)=ABCD(7)+ BUSC7
c	! Charge uniforme
	  ABCD(4)=ABCD(4)+ BUSC4
        ABCD(8)=ABCD(8)+ BUSC8

c      IF(IJK.EQ.1) THEN
c	 WRITE(66,*)                            
c	 WRITE(66,'(2(A,I3))')' PANEL=',NEL,' Cas de charge=',IS                      
c	 WRITE(66,'(A,4E12.5)')'ABCD(1 à 4)=',(ABCD(I),I=1,4)                              
c	 WRITE(66,'(A,4E12.5)')'ABCD(5 à 8)=',(ABCD(I),I=5,8) 
c	ENDIF

c     b) Moments Mb
c        ----------
      IF((NEL.EQ.1).and.(IS.EQ.1).and.(IJK.EQ.1)) THEN
         OPEN(95,FILE=NOM1//'BUSM.txt',STATUS='OLD',ERR=659)
	   READ(95,'(A1)') IBUS ! Skip ligne titre
	   GOTO 660
 659     Write(*,*)'STOP: NOM du Fichier (BUSM.txt) est INCORRECT'
         Write(*,*)'    : ERROR, (BUSM.txt) DATA FILE not valid'
      	   Write(29,*)'STOP: NOM du Fichier (BUSM.txt) est INCORRECT'					!bug
         STOP  
 660     CONTINUE
      ENDIF

      IF(IS.EQ.1) THEN
       IF((NEL.EQ.1).and.(IJK.GT.1)) THEN
         REWIND 95
	   READ(95,'(A1)') IBUS ! Skip ligne titre
	 ENDIF
	 READ(95,*)BUSM1,BUSM2,BUSM3,BUSM4,BUSM5,BUSM6,BUSM7,BUSM8
       IF(IJK.EQ.1) THEN
	   WRITE(66,'(A,I3,8(1x,E11.4),A)')
     *     'NEL=',NEL,BUSM1,BUSM2,BUSM3,BUSM4,BUSM5,BUSM6,BUSM7,BUSM8,
     *     ' Moments(N.m/m)'
	 ENDIF
	ENDIF

c     Transfer Moment (Mb en N.m/m) --> Charge (q en N/m /m)
c      avec  q= Mb*(IJK*PI/L)  (N/m par mètre courant selon Y)
c            q est du même type que la charge unitaire  DISC
c            DISC = 10000 sin(lamb X) selon -0Z
        XMULT = IJK*PI/WIDTH  ! avec Mb développé sur 5% de L

c	! Moment en X**3  : BUSM1 & BUSM5
c	! Moment en X**2  : BUSM2 & BUSM6
c	! Moment en X     : BUSM3 & BUSM7
c	! Moment uniforme : BUSM4 & BUSM8

C     FORCES "q" CONCENTREES ENTRE X=0 et X=D=DELTO)
c     (On divise par 10000 car DISB correspond à une charge unitaire de 10000)
c     (  avec DISC = 10000 sin(lamb X) ) selon -OZ
        E1=2.*DSIN(IJK*PI*DELTO/WIDTH) /(IJK*PI*DELTO*10000.)  
C     FORCES "q" CONCENTREES ENTRE X=L-D ET X=L)
        E2=2.*DSIN(IJK*PI*(WIDTH-DELTO)/WIDTH) /(IJK*PI*DELTO*10000.)
C     WRITE(6,*)' POUR M1, E1=',E1,' POUR M2, E2=',E2
        A2=XMULT*(BUSM1*E1+BUSM5*E2)
        B2=XMULT*(BUSM2*E1+BUSM6*E2)
	  C2=XMULT*(BUSM3*E1+BUSM7*E2)
	  D2=XMULT*(BUSM4*E1+BUSM8*E2)

      CALL MOM(ARG,M,A2,B2,C2,D2,PHIL,DISC,DTEMP,DISH(1,IS),Q,MS)

      IF((IOPTI.GE.1).AND.(NBRXI.GE.1)) THEN
        CALL DMOM(ARG,M,PHIL,DISC,DVARC,DTEMP,DVARH(1,1,1,IS),TETA,Q,
     *            IMPR,NXI,NBRXI,MT,A2,B2,C2,D2)
      ENDIF

  32  CONTINUE
C *************Fin Lecture forces de BUSCAGE *****************************************

C     MOMENT M1 + Nb (FORCES CONCENTREES ENTRE X=0 et X=D=DELTO)
c     (On divise par 10000 car DISB correspond à une charge unitaire de 10000)
c     (  avec DISB = 10000 cos(lamb X) )selon OX
        E1=2.*DSIN(IJK*PI*DELTO/WIDTH) /(IJK*PI*DELTO*10000.)  
C     MOMENT M2 +Nb (FORCES CONCENTREES ENTRE X=L-D ET X=L)
        E2=2.*DSIN(IJK*PI*(WIDTH-DELTO)/WIDTH) /(IJK*PI*DELTO*10000.)
C     WRITE(6,*)' POUR M1, E1=',E1,' POUR M2, E2=',E2
        A2=ABCD(1)*E1+ABCD(5)*E2
        B2=ABCD(2)*E1+ABCD(6)*E2
	  C2=ABCD(3)*E1+ABCD(7)*E2
	  D2=ABCD(4)*E1+ABCD(8)*E2

      CALL MOM(ARG,M,A2,B2,C2,D2,PHIL,DISB,DTEMP,DISH(1,IS),Q,MS)

      IF((IOPTI.GE.1).AND.(NBRXI.GE.1)) THEN
        CALL DMOM(ARG,M,PHIL,DISB,DVARB,DTEMP,DVARH(1,1,1,IS),TETA,Q,
     *            IMPR,NXI,NBRXI,MT,A2,B2,C2,D2)
      ENDIF

 5326 CONTINUE  ! ** Fin de Boucle sur les cas de charge


C ----------------------------------------------------------------------
C     CALCUL DE Vø,Uø,Wø, Vøø,Uøø,Wø, Vøøø,Uøøø,Wøøø  ( Cfr Subroutine  DERIV)                   
C     POUR LES CAS DE CHARGE                                       
C     DISH,  DISLA,DISLB,DISLC,DISLD   et   DISMLA,DISMA,DISMC,DISMD                                 
C       +                                                                
C     COMPLEMENT AU CALCUL DES COEFFICIENTS DES DIVERSES GRANDEURS      
C     UTILES  C-A-D  NX,NXY,MX,MXY,MYX,QX,RX   (Cfr Subroutine MANQ )
C ----------------------------------------------------------------------
      DO IS=1,NSOL
        CALL DERIV(3,M,01,DISH(1,IS),ARG,30,Q)
        CALL DERIV(3,M,30,DISH(1,IS),ARG,33,Q)
        CALL DERIV(3,M,33,DISH(1,IS),ARG,36,Q)
        CALL MANQ(LAM,ETA,CONST,DISH(1,IS),38,Q,ITYPE)				!février 2004
      ENDDO

      IF(IBAT.EQ.1) THEN
c      CALL DERIV(3,M,01,DISLA,ARG,30,Q)
c      CALL DERIV(3,M,30,DISLA,ARG,33,Q)
c      CALL DERIV(3,M,33,DISLA,ARG,36,Q)
c      CALL DERIV(3,M,01,DISLB,ARG,30,Q)
c      CALL DERIV(3,M,30,DISLB,ARG,33,Q)
c      CALL DERIV(3,M,33,DISLB,ARG,36,Q)
c      CALL DERIV(3,M,01,DISLC,ARG,30,Q)
c      CALL DERIV(3,M,30,DISLC,ARG,33,Q)
c      CALL DERIV(3,M,33,DISLC,ARG,36,Q)
c      CALL DERIV(3,M,01,DISLD,ARG,30,Q)
c      CALL DERIV(3,M,30,DISLD,ARG,33,Q)
c      CALL DERIV(3,M,33,DISLD,ARG,36,Q)
c      CALL DERIV(3,M,01,DISMA,ARG,30,Q)
c      CALL DERIV(3,M,30,DISMA,ARG,33,Q)
c      CALL DERIV(3,M,33,DISMA,ARG,36,Q)
c      CALL DERIV(3,M,01,DISMB,ARG,30,Q)
c      CALL DERIV(3,M,30,DISMB,ARG,33,Q)
c      CALL DERIV(3,M,33,DISMB,ARG,36,Q)
c      CALL DERIV(3,M,01,DISMC,ARG,30,Q)
c      CALL DERIV(3,M,30,DISMC,ARG,33,Q)
c      CALL DERIV(3,M,33,DISMC,ARG,36,Q)
c      CALL DERIV(3,M,01,DISMD,ARG,30,Q)
c      CALL DERIV(3,M,30,DISMD,ARG,33,Q)
c      CALL DERIV(3,M,33,DISMD,ARG,36,Q)
c      CALL MANQ(LAM,ETA,CONST,DISLA,38,Q)
c      CALL MANQ(LAM,ETA,CONST,DISLB,38,Q)
c      CALL MANQ(LAM,ETA,CONST,DISLC,38,Q)
c      CALL MANQ(LAM,ETA,CONST,DISLD,38,Q)
c      CALL MANQ(LAM,ETA,CONST,DISMA,38,Q)
c      CALL MANQ(LAM,ETA,CONST,DISMB,38,Q)
c      CALL MANQ(LAM,ETA,CONST,DISMC,38,Q)
c      CALL MANQ(LAM,ETA,CONST,DISMD,38,Q)
      ENDIF

c      DO 755 I=1065,1102
c      DISH(I,1)=DISH(I+114,1)
c 755  CONTINUE

      IF((IJK.GE.2).OR.(IMPR.EQ.0)) GOTO 315
      WRITE(66,54)
      DO 750 I=1,3
c     DO 750 I=1,(3+MS)
        J1=(I-1)*16+1
        WRITE(66,55)(DISC(J),DISB(J),DISA(J),DISD(J),J=J1,(J1+4*M-1))
 750  CONTINUE

c       DO 751 I=24,45
c          J1=(I-1)*16+1
c 751   WRITE(66,55)(DISC(J),DISB(J),DISA(J),DISD(J),J=J1,(J1+4*M-1))


      IF((KSE.EQ.0).AND.(ICHA.EQ.0).AND.(IMOM.EQ.0)) GOTO 315
      WRITE(66,58)
      DO IS=1,NSOL
        IF(IS.EQ.1) THEN
           WRITE(66,*) ' Cas de charge nø',IS
	     CALL PIMPR(DISH(1,IS),MS,M)
	  ENDIF
      ENDDO
      IF(IBAT.EQ.1) THEN
c      WRITE(66,2007)
c      WRITE(66,2003)
c      CALL PIMPR(DISLA,MS,M)
c      WRITE(66,2004)
c      CALL PIMPR(DISLB,MS,M)
c      WRITE(66,2005)
c      CALL PIMPR(DISLC,MS,M)
c      WRITE(66,2006)
c      CALL PIMPR(DISLD,MS,M)
c      WRITE(66,2008)
c      WRITE(66,2003)
c      CALL PIMPR(DISMA,MS,M)
c      WRITE(66,2004)
c      CALL PIMPR(DISMB,MS,M)
c      WRITE(66,2005)
c      CALL PIMPR(DISMC,MS,M)
c      WRITE(66,2006)
c      CALL PIMPR(DISMD,MS,M)
      ENDIF
  315 CONTINUE

  
      DO IJ=1,400
        AUX(IJ)=0.
      ENDDO
      DO IJ=1,360
        ZB(IJ)=0.
        HYP(IJ)=0.
        ALI(IJ)=0.
      ENDDO

C  =====================================================================

C  Expression partielle des dérivées des DISA, DISB, ... et DISH (Voir LINEA)
C  --------------------------------------------------------------------------
C	DVARA(33,9,16), DVARB, DVARC et DVARD (pour DISC, DISA, DISB et DISD) 
C          avec 13+2*MT variables (U V Wø Ny Nyx My Ry W Wøø Wøøø Uø Vø Vøø +
C                       Xo, Zo),
C		        9 variables de conception,
C		  et   16 termes.
C       DVARH(33,9,38,IS) avec 38 termes pour DISH avec IS le nø du cas de
C       charge (IS=1,NSOL)
        
C     Voir Subr DPAR et Subr DPRESS ou DHYDRO (ci-avant)

 	IF((IMPR.NE.0).AND.(IOPTI.NE.0).AND.(NBRXI.NE.0)) THEN

c	   DO IVAR=1,NBRXI
c		IV=NXI(IVAR)
c      	Write(66,*)' Dérivée de DVARA,B,C,D , Var. XI nø',IV
c      	Write(66,*)' ----------------------------------------'
c		DO 4597 II=1,13+2*MT
c      	Write(66,*)' Fonct nø',II
c      	Write(66,*)' DVARA ',(DVARA(II,IV,I),I=1,8)
c		WRITE(66,*)'       ',(DVARA(II,IV,I),I=9,16)
c      	Write(66,*)' DVARB ', (DVARB(II,IV,I),I=1,8)
c		WRITE(66,*)'       ',(DVARB(II,IV,I),I=9,16)
c      	Write(66,*)' DVARC ',(DVARC(II,IV,I),I=1,8)
c		WRITE(66,*)'       ',(DVARC(II,IV,I),I=9,16)
c      	Write(66,*)' DVARD ',(DVARD(II,IV,I),I=1,8)
c		WRITE(66,*)'       ',(DVARD(II,IV,I),I=9,16)
c        ENDDO

	   DO 4598 IS=1,NSOL
      	 Write(66,*)' Cas de charge nø',IS
      	 Write(66,*)' ====================='
	   DO 4598 IVAR=1,NBRXI
		 IV=NXI(IVAR)
      	 Write(66,*)' Dérivée de DISH , Var. XI nø',IV
      	 Write(66,*)' ---------------------------------'
	   DO 4598 II=1,13+2*MT
      	 Write(66,'(A10,I3,6E12.5)')' Fonct nø',II,
     *                          (DVARH(II,IV,I,IS),I=1,6)
		 WRITE(66,'(8E12.5)') (DVARH(II,IV,I,IS),I=7,14)
		 WRITE(66,'(8E12.5)') (DVARH(II,IV,I,IS),I=15,22)
       	 WRITE(66,'(8E12.5)') (DVARH(II,IV,I,IS),I=23,30)
 4598    CONTINUE

	ENDIF
	
C***********************************************************************
C     PARTIE II:
C     -----------
C     Début des calculs relatifs aux traverses
C***********************************************************************
      IF(MT.EQ.0) GOTO 622

C     CALCUL DE LA MATRICE DU SYSTEME DE RESOLUTION DES INCONNUES
C     HYPERSTATIQUES    (EFFETS DES TRAVERSES)
C     ------------------------------------------------------------------
      DO I=1,2
        CALL INTEQ(ARGQ,DISB,MT,M,I,1,ABTR,AUX)
        CALL INTEQ(ARGQ,DISC,MT,M,I,2,ABTR,AUX)
      ENDDO

C     CALCUL DES TERMES INDEPENDANTS DU SYSTEME DE RESOLUTION DES       
C     INCONNUES HYPERSTATIQUES (EFFETS DES TRAVERSES)
C     -----------------------------------------------------------
C     CHARGE EXTERIEURE (SUBROUTINES HYP1 ET HYP2)
C     8 CAS DE CHARGES AUX BORDS DU PANNEAU (SUBROUTINE VECTOR)

      IF((KSE.EQ.0).AND.(ICHA.EQ.0).AND.(IMOM.EQ.0)) GOTO 6801
      CALL HYP1(DISH(1,1),MT,ABTR,TETA,ZB,1)
      DO IS=2,NSOL
        IS1= 8+IS
        CALL HYP1(DISH(1,IS),MT,ABTR,TETA,ZB,IS1)
      ENDDO
 6801 CONTINUE

      IF(IBAT.EQ.1) THEN
c      CALL HYP1(DISLA,MT,ABTR,TETA,ZB,10)
c      CALL HYP1(DISLB,MT,ABTR,TETA,ZB,11)
c      CALL HYP1(DISLC,MT,ABTR,TETA,ZB,12)
c      CALL HYP1(DISLD,MT,ABTR,TETA,ZB,13)
c      CALL HYP1(DISMA,MT,ABTR,TETA,ZB,14)
c      CALL HYP1(DISMB,MT,ABTR,TETA,ZB,15)
c      CALL HYP1(DISMC,MT,ABTR,TETA,ZB,16)
c      CALL HYP1(DISMD,MT,ABTR,TETA,ZB,17)
      ENDIF

      DO 64 KK=1,M
      DO 64 I=1,MT
      SM =ARGQ(2*KK-1)*ABTR(I)*PI1
      SN =ARGQ(2*KK)  *ABTR(I)*PI1
      SP =ARGQ(2*KK-1)*(PHIL-ABTR(I))*PI1
      SQ =ARGQ(2*KK)  *(PHIL-ABTR(I))*PI1
      SMM=ARGQ(2*KK-1)*(360.-ABTR(I))*PI1
      SNN=ARGQ(2*KK)  *(360.-ABTR(I))*PI1
      SPP=ARGQ(2*KK-1)*(360.-(PHIL-ABTR(I)))*PI1
      SQQ=ARGQ(2*KK)  *(360.-(PHIL-ABTR(I)))*PI1
        SM =EXPO(SM)
        SP =EXPO(SP)
        SMM=EXPO(SMM)
        SPP=EXPO(SPP)
      TM =SM *DCOS(SN)
      TN =SM *DSIN(SN)
      TP =SP *DCOS(SQ)
      TQ =SP *DSIN(SQ)
      TMM=SMM*DCOS(SNN)
      TNN=SMM*DSIN(SNN)
      TPP=SPP*DCOS(SQQ)
      TQQ=SPP*DSIN(SQQ)
      KH=8*KK+37+76*I

      IF((KSE.EQ.0).AND.(ICHA.EQ.0).AND.(IMOM.EQ.0)) GOTO 6802
      CALL HYP2(DISH(1,1),MT,ZB,TM,TN,TP,TQ,TMM,TNN,TPP,TQQ,KH,I,1)
      DO IS=2,NSOL
        IS1= 8+IS
        CALL HYP2(DISH(1,IS),MT,ZB,TM,TN,TP,TQ,TMM,TNN,TPP,TQQ,KH,I,IS1)
      ENDDO
6802  CONTINUE

      IF(IBAT.EQ.1) THEN
c      CALL HYP2(DISLA,MT,ZB,TM,TN,TP,TQ,TMM,TNN,TPP,TQQ,KH,I,10)
c      CALL HYP2(DISLB,MT,ZB,TM,TN,TP,TQ,TMM,TNN,TPP,TQQ,KH,I,11)
c      CALL HYP2(DISLC,MT,ZB,TM,TN,TP,TQ,TMM,TNN,TPP,TQQ,KH,I,12)
c      CALL HYP2(DISLD,MT,ZB,TM,TN,TP,TQ,TMM,TNN,TPP,TQQ,KH,I,13)
c      CALL HYP2(DISMA,MT,ZB,TM,TN,TP,TQ,TMM,TNN,TPP,TQQ,KH,I,14)
c      CALL HYP2(DISMB,MT,ZB,TM,TN,TP,TQ,TMM,TNN,TPP,TQQ,KH,I,15)
c      CALL HYP2(DISMC,MT,ZB,TM,TN,TP,TQ,TMM,TNN,TPP,TQQ,KH,I,16)
c      CALL HYP2(DISMD,MT,ZB,TM,TN,TP,TQ,TMM,TNN,TPP,TQQ,KH,I,17)
      ENDIF

      CALL VECTO(DISC,TM,TN,I,KK,MT,2,+1.0D+00,TMM,TNN,ZB)
      CALL VECTO(DISC,TP,TQ,I,KK,MT,3,+1.0D+00,TPP,TQQ,ZB)
      CALL VECTO(DISD,TM,TN,I,KK,MT,4,+1.0D+00,TMM,TNN,ZB)
      CALL VECTO(DISD,TP,TQ,I,KK,MT,5,-1.0D+00,TPP,TQQ,ZB)
      CALL VECTO(DISA,TM,TN,I,KK,MT,6,+1.0D+00,TMM,TNN,ZB)
      CALL VECTO(DISA,TP,TQ,I,KK,MT,7,-1.0D+00,TPP,TQQ,ZB)
      CALL VECTO(DISB,TM,TN,I,KK,MT,8,+1.0D+00,TMM,TNN,ZB)
      CALL VECTO(DISB,TP,TQ,I,KK,MT,9,+1.0D+00,TPP,TQQ,ZB)
      CALL PLIMIT(DISC,DISB,+TM,+TN,TMM,TNN,I,KK,MT,01,03,ALI)
      CALL PLIMIT(DISC,DISB,+TP,+TQ,TPP,TQQ,I,KK,MT,02,03,ALI)
      CALL PLIMIT(DISC,DISB,+TM,+TN,TMM,TNN,I,KK,MT,03,02,ALI)
      CALL PLIMIT(DISC,DISB,+TP,+TQ,TPP,TQQ,I,KK,MT,04,02,ALI)
      CALL PLIMIT(DISC,DISB,+TM,+TN,TMM,TNN,I,KK,MT,05,24,ALI)
      CALL PLIMIT(DISC,DISB,+TP,+TQ,TPP,TQQ,I,KK,MT,06,24,ALI)
      CALL PLIMIT(DISC,DISB,+TM,+TN,TMM,TNN,I,KK,MT,07,25,ALI)
      CALL PLIMIT(DISC,DISB,+TP,+TQ,TPP,TQQ,I,KK,MT,08,25,ALI)
      CALL PLIMIT(DISC,DISB,-TM,-TN,-TMM,-TNN,I,KK,MT,09,27,ALI)
      CALL PLIMIT(DISC,DISB,+TP,+TQ,+TPP,+TQQ,I,KK,MT,10,27,ALI)
      CALL PLIMIT(DISC,DISB,-TM,-TN,-TMM,-TNN,I,KK,MT,11,26,ALI)
      CALL PLIMIT(DISC,DISB,+TP,+TQ,+TPP,+TQQ,I,KK,MT,12,26,ALI)
      CALL PLIMIT(DISC,DISB,-TM,-TN,-TMM,-TNN,I,KK,MT,13,01,ALI)
      CALL PLIMIT(DISC,DISB,+TP,+TQ,+TPP,+TQQ,I,KK,MT,14,01,ALI)
      CALL PLIMIT(DISC,DISB,-TM,-TN,-TMM,-TNN,I,KK,MT,15,29,ALI)
      CALL PLIMIT(DISC,DISB,+TP,+TQ,+TPP,+TQQ,I,KK,MT,16,29,ALI)
   64 CONTINUE

C     RESOLUTION DES EFFETS HYPERSTATIQUES DES TRAVERSES
C     ---------------------------------------------------
c     NBREQU=17
      NBREQU=8+NSOL  ! + 2*4 (8 efforts de bord unitaires Nb et Mb)

      CALL RESOLU(MS,NBREQU,HYP,AUX,ZB,KLI,BLOKA,IMPR)
c     ============
C     Après RESOLU, la matrice AUX est la matrice inverse (A conserver)
C                et la matrice HYP contient les vecteurs solutions.
      IF(IMPR.GE.1) THEN
        WRITE(66,65)
        DO 66 I=1,NBREQU
          IJ=1+(I-1)*20
          JM=IJ+MS-1
   66   WRITE(66,67) (HYP(J),J=IJ,JM)
      ENDIF

  622 CONTINUE

      IF((IMPR.GE.1).AND.(MT.GE.1)) THEN
        Write(66,*)' '
        Write(66,*)'Vecteur ALI (W,U,..) pour les MT cas de charges'
        DO 4599 II=1,16
 4599   WRITE(66,'(I2,3(8E14.5/))') II,(ALI(I+II*20),I=1,MS)
 	ENDIF

C     Fin du calcul des inconnues hyperstatiques des traverses (partie II)

C********************************************************************************
C     PARTIE III:
C     -----------
C   Calcul des sensibilités (dérivées des déplac. par rapport aux var. de Concept.
C********************************************************************************
C     Calcul des dérivées des DISA, DISB, ... et DISH aux points choisis
C     ------------------------------------------------------------------
C     SENSA(13,10,9) avec 13 variables W,U,V,Wø,My,Ny,Nyx,Ry,Wøø,Wøøø,Uø,Vø,Vøø
C	   		     et 10 points de calcul des sensibilités,
C                  idem avec SENSB,SENSC,SENSD et SENSH
C     YPT(10)  coordonnées selon Oy des pts de calcul des sensibilités 

        IF(IOPTI.GE.1) THEN
c        IF(IMPR.NE.0) WRITE(*,*) ' AVANT Subr. SENS'

        IF(MT.GE.1) THEN
        CALL DHYPS(PHIL,TETA,Q,M,ARGQ,DISH,DISA,DISB,DISC,DISD,
     *           DVARA,DVARB,DVARC,DVARD,DVARH,IMPR,MT,MS,ABTR,
     *           NBRXI,NXI,HYP,AUX,BID1,BID2,DHYP,NSOL)
      ENDIF


        CALL SENS (PHIL,TETA,Q,M,ARGQ,DISH,DISA,DISB,DISC,DISD,
     *   DVARA,DVARB,DVARC,DVARD,DVARH,IMPR,MT,NBRXI,NXI,IPT,YPT,
     *   ABTR,ALIX,ALIZ,DALIX,DALIZ,DHYP,NSOL)

        ENDIF

C********************************************************************************
C     PARTIE IV:
C     -----------
C     Calcul de la matrice "d'assemblage" pour le calcul des inc. hyper. de bord
C********************************************************************************

      IF(MT.EQ.0) GOTO 202                                   		
C     CALCUL DES DEPLACEMENTS ET REACTIONS  SUR LES BORDS (1er partie)  
C     (CHARGE EXTERIEURE) - INF1 (effet des traverses)                  
C     -----------------------------------------------------------------
      CALL PINF1 (SUM,ALI,ZB,HYP,0,0,MS)

      DO IS=2,NSOL
        IS1= IS*20+140     ! IS1 = donnée
        IS2= (IS-1)*16     ! IS2 = solution
        CALL PINF1 (SUM,ALI,ZB,HYP,IS1,IS2,MS)
      ENDDO
                                                                           
      IF(IBAT.EQ.1) THEN
c     CALL PINF1 (SUM,ALI,ZB,HYP,180,16,MS)
c     CALL PINF1 (SUM,ALI,ZB,HYP,200,32,MS)
c     CALL PINF1 (SUM,ALI,ZB,HYP,220,48,MS)
c     CALL PINF1 (SUM,ALI,ZB,HYP,240,64,MS)
c     CALL PINF1 (SUM,ALI,ZB,HYP,260,80,MS)
c     CALL PINF1 (SUM,ALI,ZB,HYP,280,96,MS)
c     CALL PINF1 (SUM,ALI,ZB,HYP,300,112,MS)
c     CALL PINF1 (SUM,ALI,ZB,HYP,320,128,MS)
      ENDIF
 202  CONTINUE

C     CALCUL DES DEPLACEMENTS ET REACTIONS SUR LES BORDS (2ème partie)
C     (CHARGE EXTERIEURE) - INF2 et INF3                          	
C     ------------------------------------------------------------------
      IF(DABS(TETA).NE.180.) GOTO 444
      C1=-1.
      S1=0.
      C2=-DCOS(PHIL*PI1)
      S2=-DSIN(PHIL*PI1)
      GOTO 446
444   IF(DABS(TETA).NE.90.) GOTO 445
      II=1.
      IF(TETA.EQ.-90.) II=-1
      C1=0.
      S1=II
      C2=-II*DSIN(PHIL*PI1)
      S2=II*DCOS(PHIL*PI1)
      GOTO 446
  445 C1=DCOS(TETA*PI1)
      S1=DSIN(TETA*PI1)
      C2=DCOS((TETA+PHIL)*PI1)
      S2=DSIN((TETA+PHIL)*PI1)
  446 PH=PHIL*PI1
      PH2=PH*PH
      PH3=PH2*PH
C
      IF((KSE.EQ.0).AND.(ICHA.EQ.0).AND.(IMOM.EQ.0)) GOTO 6803
      DO IS=1,NSOL
        IS1= (IS-1)*16
        CALL PINF2(ZB,DISH(1,IS),S1,S2,C1,C2,PH,PH2,PH3,IS1)
      ENDDO
 6803 CONTINUE

      IF(IBAT.EQ.1) THEN
c     CALL PINF2(ZB,DISLA,S1,S2,C1,C2,PH,PH2,PH3,16)
c     CALL PINF2(ZB,DISLB,S1,S2,C1,C2,PH,PH2,PH3,32)
c     CALL PINF2(ZB,DISLC,S1,S2,C1,C2,PH,PH2,PH3,48)
c     CALL PINF2(ZB,DISLD,S1,S2,C1,C2,PH,PH2,PH3,64)
c     CALL PINF2(ZB,DISMA,S1,S2,C1,C2,PH,PH2,PH3,80)
c     CALL PINF2(ZB,DISMB,S1,S2,C1,C2,PH,PH2,PH3,96)
c     CALL PINF2(ZB,DISMC,S1,S2,C1,C2,PH,PH2,PH3,112)
c     CALL PINF2(ZB,DISMD,S1,S2,C1,C2,PH,PH2,PH3,128)
      ENDIF

C     CALCUL DES DEPLACEMENTS ET REACTIONS  SUR LES BORDS
C      (8 CHARGES UNITAIRES DE BORDS)
C     --------------------------------------------------------------
      DO 683 KK=1,400
  683 AUX(KK)=0.

      DO 79 KK=1,M
      SP =ARGQ(2*KK-1)*PHIL*PI1
      SQ =ARGQ(2*KK)  *PHIL*PI1
      SMM=ARGQ(2*KK-1)*2*PI
      SNN=ARGQ(2*KK)  *2*PI
      SPP=ARGQ(2*KK-1)*(360.-PHIL)*PI1
      SQQ=ARGQ(2*KK)  *(360.-PHIL)*PI1
      SP =EXPO(SP)
      SMM=EXPO(SMM)
      SPP=EXPO(SPP)
      TM=1.
      TN=0.
      TP =SP *DCOS(SQ)
      TQ =SP *DSIN(SQ)
      TMM=SMM*DCOS(SNN)
      TNN=SMM*DSIN(SNN)
      TPP=SPP*DCOS(SQQ)
      TQQ=SPP*DSIN(SQQ)



      IF((KSE.EQ.0).AND.(ICHA.EQ.0).AND.(IMOM.EQ.0)) GOTO 6804
      DO IS=1,NSOL
        IS1= (IS-1)*16
        CALL PINF3(DISH(1,IS),TP,TQ,TMM,TNN,TPP,TQQ,KK,IS1)
      ENDDO
 6804 CONTINUE

      IF(IBAT.EQ.1)THEN
c     CALL PINF3(DISLA,TP,TQ,TMM,TNN,TPP,TQQ,KK,16)
c     CALL PINF3(DISLB,TP,TQ,TMM,TNN,TPP,TQQ,KK,32)
c     CALL PINF3(DISLC,TP,TQ,TMM,TNN,TPP,TQQ,KK,48)
c     CALL PINF3(DISLD,TP,TQ,TMM,TNN,TPP,TQQ,KK,64)
c     CALL PINF3(DISMA,TP,TQ,TMM,TNN,TPP,TQQ,KK,80)
c     CALL PINF3(DISMB,TP,TQ,TMM,TNN,TPP,TQQ,KK,96)
c     CALL PINF3(DISMC,TP,TQ,TMM,TNN,TPP,TQQ,KK,112)
c     CALL PINF3(DISMD,TP,TQ,TMM,TNN,TPP,TQQ,KK,128)
      ENDIF

      CALL PEDGE(TM,TN,TP,TQ,TMM,TNN,TPP,TQQ,
     *         KK,01,03,DISA,DISB,DISC,DISD,+1.0D+00,-1.0D+00)
      CALL PEDGE(TM,TN,TP,TQ,TMM,TNN,TPP,TQQ,
     *         KK,03,02,DISA,DISB,DISC,DISD,+1.0D+00,-1.0D+00)
      CALL PEDGE(TM,TN,TP,TQ,TMM,TNN,TPP,TQQ,
     *         KK,05,24,DISA,DISB,DISC,DISD,+1.0D+00,-1.0D+00)
      CALL PEDGE(TM,TN,TP,TQ,TMM,TNN,TPP,TQQ,
     *         KK,07,25,DISA,DISB,DISC,DISD,+1.0D+00,-1.0D+00)
      CALL PEDGE(TM,TN,TP,TQ,TMM,TNN,TPP,TQQ,
     *         KK,09,27,DISA,DISB,DISC,DISD,-1.0D+00,+1.0D+00)
      CALL PEDGE(TM,TN,TP,TQ,TMM,TNN,TPP,TQQ,
     *         KK,11,26,DISA,DISB,DISC,DISD,-1.0D+00,+1.0D+00)
      CALL PEDGE(TM,TN,TP,TQ,TMM,TNN,TPP,TQQ,
     *         KK,13,01,DISA,DISB,DISC,DISD,-1.0D+00,+1.0D+00)
      CALL PEDGE(TM,TN,TP,TQ,TMM,TNN,TPP,TQQ,
     *         KK,15,29,DISA,DISB,DISC,DISD,-1.0D+00,+1.0D+00)
   79 CONTINUE

      J1=1
      II=1
 3002 DO 3001 J=J1,J1+3
        AUX(    2*J)= AUX( 19+2*J)*II
        AUX(20 +2*J)= AUX( -1+2*J)*II
        AUX(40 +2*J)=-AUX( 59+2*J)*II
        AUX(60 +2*J)=-AUX( 39+2*J)*II
        AUX(80 +2*J)=-AUX( 99+2*J)*II
        AUX(100+2*J)=-AUX( 79+2*J)*II
        AUX(120+2*J)= AUX(139+2*J)*II
        AUX(140+2*J)= AUX(119+2*J)*II
 3001 CONTINUE
      IF(J1.EQ.5) GOTO 3003
        J1=5
        II=-1
        GOTO 3002
 3003 CONTINUE
C
C     Contribution des traverses à la matrice de rigidité
C     ---------------------------------------------------
      IF(MT.GT.0) THEN
        DO 75 J=1,8
        DO 75 I=1,16
          IJ=I+(J-1)*20
          SUM=0.
          DO 76 KK=1,MS
            KH=KK+I*20
            JM=KK+J*20
            SUM=SUM+ALI(KH)*HYP(JM)
   76     CONTINUE
          AUX(IJ)=AUX(IJ)+SUM/10000.
   75   CONTINUE
      ENDIF

      IF(IMPR.EQ.0) GOTO 777
      WRITE(66,4002) NEL
      DO 8 I=1,16
         WRITE(66,4001) (AUX((II-1)*20+I),II=1,8),
     *                  (ZB(I+(II-1)*16),II=1,NSOL)
    8 CONTINUE
  777 CONTINUE

C***********************************************************************
C     PARTIE V:
C     -----------
C     STOKAGE DES RESULTATS INTERMEDIAIRES
C***********************************************************************
      WRITE(NEL+100)DISA,DISB,DISC,DISD,ARGQ,M,HYP,LAMB,IJK

      IF((KSE.EQ.0).AND.(ICHA.EQ.0).AND.(IMOM.EQ.0)) THEN
c       WRITE(NEL+100)((DISH(IS,I),I=1,114),IS=1,NSOL)
      ELSE
        WRITE(NEL+100) DISH
      ENDIF

      IF(IOPTI.GE.1) THEN
        WRITE(NEL+400) SENSH,SENSA, SENSB, SENSC, SENSD,				!extension neto
     *                       SENSAA,SENSBB,SENSCC,SENSDD,
     *              DEFA,DEFB,DEFC,DEFD,DEFAA,DEFBB,DEFCC,DEFDD
c     REWIND(NEL+400)  
      BACKSPACE(NEL+400)		!extension neto
      ENDIF

C***********************************************************************
C     PARTIE VI:
C     -----------
C     RANGEMENT DES COEFFICIENTS DES EQUATIONS DE CONTINUITE et D'EQUILIBRE  
C     ET RESOLUTION LORS DU PASSAGE pour le dernier PANNEAU.
C***********************************************************************

      NVSMB=NSOL
c     IF(IND.NE.0) NVSMB=NSOL+8*NETO  si avec effets de bord (JLBORD)

      CALL MDR(NEL,NETO,NE,ABA,BAB,BLOKA,KLI,ZSN,SOLT,PHIL,NVSMB,
     *         IJK,JLMAX,IMPR,NSOL,
     *         ANGLE,NOEUD,MCOMP,NNO,NC,NSIGN)

      RETURN

C     ******************************************************************
C     FORMATS
C     ******************************************************************
    4 FORMAT(1H1,43X,16H PANNEAU NUMERO ,I2,/45X,18(1H+) )
   12 FORMAT(    14H TERME NUMERO ,I2/' DU DEVELOPPEMENT DE LA PRESS',
     1'ION HYDROSTATIQUE EN SERIE DE FOURIER'/1X,72(1H+),//' COEFFICIENT
     3S DE L''EQUATION CARACTERISTIQUE,'/' ORDONNES PAR PUISSANCES ',
     4'CROISSANTES',//(6(2X,E15.8)))
   42 FORMAT(/'ARGUMENTS DES FONCTIONS EXPONENTIELLES (alpha,béta)',
     *  /(4(2X,E14.7)))
   50 FORMAT(' Résolutions des 4 cas de charge de base (C B A D)',
     *' dont le vecteur indépendant est 0 0 0 -5000'/' La matrice des',
     *' coeff. et le vect solution sont :'/)
   51 FORMAT(' Résolutions des 4 cas de charge de base (C B A D)',
     *' dont le vecteur indépendant est 0 0 0 -5000'/' La matrice des',
     *' coeff., le vect solution (PX) et les dérivées (DPX)'/
     * 'de ce vecteur par rapport aux var. de conception sont :'/)
   54 FORMAT(1H1,' COEFFICIENTS DES FONCTIONS EXPONENTIELLES,'/
     *' POUR LES 4 CAS DE CHARGES LINEAIRES DE BASE',/1X,45(1H+),/
     *' CHAQUE COLONNE CONTIENT LES COEFFICIENTS DE EXP(A*Y)*COS(B*Y),'/
     *' EXP(A*Y)*SIN(B*Y),EXP(A(2*PI-Y))*COS(B(..))ET EXP(A(..))*SIN(B(.
     *..)),'/' ORDONNES COMME DANS ARG(I),SUCCESSIVEMENT',/
     * ' POUR V,U,W,XODX,ZODX (MAXIMUM 10 GROUPES XODX,ZODX),'/
     * ' MY,NY,RY,NYX,QY,Wø,  Vø,Uø,Wø,Vøø,Uøø,Wøø,Vøøø,Uøøø,Wøøø et ',
     * 'NX,NXY,MX,MXY,MYX,QX,RX.'//T4,'CHARGE (C)',T21,'CHARGE (B)',
     * T38,'CHARGE (A)',T55,'CHARGE (D)',//)
   55 FORMAT(16(4(2X,E15.8)/))
   58 FORMAT(1H1,' COEFFICIENTS DES FONCTIONS EXPONENTIELLES, POUR LA PR
     *ESSION HYDROSTATIQUE'/1X,74(1H+)/' CHAQUE GROUPE  CONTIENT SUCCESS
     *IVEMENT LE TERME INDEPENDANT, '/1X,'LES COEFFICIENTS DE Y, Y**2 ET
     * Y**3, LE COEFFICIENT DE COS(TETA+Y), SIN(TETA+Y)' /' DE EXP(A*Y)*
     *COS(B*Y),EXP(A*Y)*SIN(B*Y),EXP(A*(PHIL-Y))*COS(B*(PHIL-Y)),...'/1X
     *,'DE EXP(A*(2PI-Y)*COS(B*(2PI-Y)),EXP(..)*SIN(..),..'/1X,
     *'DE EXP(A(2PI-(PHIL-Y)))*COS(...),EXP(..)*SIN(..)'/1X,
     *'ORDONNES COMME DANS ARG(I)., SUCCESSIVEMENT POUR'/
     *' VO UO WO XODX ZODX (10 GROUPES XODX ZODX MAXIMUM),'/
     *' MY NY RY NYX QY Wø      Vø,Uø,Wø,Vøø,Uøø,Wøø,Vøøø,Uøøø,Wøøø'
     *' et  NX NXY MX MXY MYX QX RX.'//)
   65 FORMAT(/'EFFETS HYPERSTATIQUES EXERCES PAR LES TRAVERSES'/
     *48(1H+)/' ILS SONT ORDONNES COMME SUIT :'/
     *' - LES LIGNES: PHI ET F SUCCESSIVEMENT POUR CHAQUE TRAVERSES '/
     *' - LES COLONNES, CHARGE EXTERIEUR, CHARGE C SUP, CHARGE C INF,',
     *' idem avec les charges C, D et A'/
     *'   AVEC DE PLUS LES EFFETS POUR LES 4 CAS DE CHARGES DE BORDS ',
     *'(A,0,0,0 // 0,B,0,0 // 0,0,C,0 // 0,0,0,D)'/)
   67 FORMAT(/6(1X,E11.4))
  644 FORMAT(/'Dérivées des coef. de l''équ. diff. du 8éme ordre par',
     * ' rapport aux variables de conception'/85(1H-))
 2003 FORMAT(/' CAS DE CHARGE DE BORD TYPE A : A*(Q*PHIL)**3=1000.,',
     * 'B=C=D=0'/T2,67(1H-)//)
 2004 FORMAT(/' CAS DE CHARGE DE BORD TYPE B : A=0, B(Q*PHIL)**2=1000.,'
     *,' C=D=0 '/T2,67(1H-)//)
 2005 FORMAT(/' CAS DE CHARGE DE BORD TYPE C : A=0, B=0, C(Q*PHIL)=1000.
     *, D=0'/T2,67(1H-)//)
 2006 FORMAT(/' CAS DE CHARGE DE BORD TYPE D : A=0, B=0, C=0, D=1000.'/
     *T2,62(1H-)//)
 2007 FORMAT(/,T5,30(1H*)/T5,1H*,T10,'LES FORCES DE BORDS',T35,1H*/T5,
     *1H*,T35,1H*/T5,30(1H*))
 2008 FORMAT(/,T5,30(1H*)/T5,1H*,T10,'LES MOMENTS DE BORDS',T35,1H*/T5,
     *1H*,T35,1H*/T5,30(1H*))
 4001 FORMAT(/4(1X,E11.4)/4(1X,E11.4)/9(1X,E11.4))
 4002 FORMAT(1H1,'POUR LE CALCUL DES EFFETS HYPERSTATIQUES DE BORD DU ',
     *' PANNEAU ',I3, ' (ordre C D A B)'/
     *' VOICI PAR LIGNE LA MATRICE DES COEFF. DES INC. HYPERSTATIQUES,'/
     *' PUIS LES 9 TERMES INDEP. POUR LA P.H. ET LES 8 EFFETS DE BORDS :
     *'/' W(Y=0),W(Y=PHIL),U(Y=0),U(..),MY,..,NY,..,NYX,..,RY,..,'/
     *' V,..,DW/DY,...')
      END

