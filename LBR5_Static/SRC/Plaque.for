      SUBROUTINE PLAQUE(EPSA,EPSR,ENTR,DELTA,SENS3,dSIG1,NEL,
     *HYA,HXR,WIDTH,ABTR,PHIL,Q,
     *IPTS2,EFFCOMB,CHAMAX,dCHAMAX,PLOC,NETO,E,ETA,NBRXI,SIG1,			!!!aout04
     *NXIT,EFF,IPT,SigPlaque,DSigPlaque,SIGXPlaque,SIGYPlaque,PHIPlaque,
     *SIGVMTPlaque,SIGVMCPlaque,INDPlaque,SIGX,SIGY)

	IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION SENS3(21,IPTmax,9,NETO),dSIG1(9),
     *IPTS2(IPTmax,NETO),EFF(9690),EFFCOMB(9690),CHAMAX(5),dCHAMAX(5,9),	!!!aout04
     *DSigPlaque(IPTmax,9),dSIGMAG(9),dSGMEM1(9),dSGMEM2(9),dSIG1T(9),
     *dSIG2T(9),dSIG1C(9),dSIG2C(9),SigPlaque(IPTmax),NXIT(9,NETO)
	DIMENSION PBCOEF1(2), SIGMEM(2),SIGVM(2),LOOP1(2),SGMEM1(2),
     *SGMEM2(2),SIG1T(2),SIG2T(2),SIG1C(2),SIG2C(2),SIGVMT(2),SIGVMC(2),
     *SIGMAG(2),SIGBEN(2),SKBEN(2),dSIGMEM(2),PHI(2)
	DIMENSION dGLOAD(9),dPRSABS(9),dSIGBEN(9)							!!!aout04

      DIMENSION SIGXPlaque(IPTmax),SIGYPlaque(IPTmax),PHIPlaque(IPTmax),
     *SIGVMTPlaque(IPTmax),SIGVMCPlaque(IPTmax),INDPlaque(IPTmax),
     *SIGX(IPTmax),SIGY(IPTmax)

	COMMON/DIM1/Nmax,NEmax,NVmax,
     *            M1max,M1Tmax,M2max,M2Tmax,Mmax,MGmax,ISmax,IPTmax ! dimension max des vecteurs
	COMMON/PY/PI

C***********************************************************************
C     SUBROUTINE PLAQUE
C     ==================
C     Cette sous-routine est appelée dans BO2.
C     Elle calcule la valeur de la contrainte "plaque" aux différents points
C     IPT ainsi que ses sensibilités, pour le panneau NEL.
C     
C
C   INPUTS  : 

C   OUTPUTS : SigPlaque(IPTmax)    = contrainte "plaque"
C             DSigPlaque(IPTmax,9) = vecteur contenant les sensibilités de SigPlaque

C  
C
C************************************************************************
C************************************************************************
C************************************************************************

      CALL ANNULD(DSigPlaque,9*IPTmax)
	CALL ANNULD(SigPlaque,IPTmax)
	CALL ANNULD(dSIGMAG,9)
	CALL ANNULD(dSGMEM1,9)
	CALL ANNULD(dSGMEM2,9)
	CALL ANNULD(dSIG1T,9)
	CALL ANNULD(dSIG2T,9)
	CALL ANNULD(dSIG1C,9)
	CALL ANNULD(dSIG2C,9)

	PBCOEF1(1) = 0.19
	PBCOEF1(2) = 0.03
      PIQ = PI*Q/180.0

      TPL = DELTA
	

C     Détermination de XLEN,YLEN, les dimensions (a et b) du panneau
C     --------------------------------------------------------------

      XLEN = EPSA
	YLEN = ENTR

      
      IF((HYA/DELTA).LE.2) THEN
        XLEN=WIDTH
      ELSE
        IF(EPSA.GT.WIDTH) THEN
          XLEN=WIDTH
        ENDIF
      ENDIF

      HAUT=Q*PHIL*PI/180.
      IF((HXR/DELTA).LE.2) THEN
        YLEN=HAUT

        IF(MT.GT.0) THEN
          YA=0.
          Y1=ABTR(1)*PIQ
          YT=Y1-YA
          YLEN=YT
          DO 2 I=1,MT-1
             Y1=ABTR(I)*PIQ
             Y2=ABTR(I+1)*PIQ
             YT=Y2-Y1
             IF(YLEN.LT.YT) YLEN=YT
   2      CONTINUE
          YMT=ABTR(MT)*PIQ
          YB=HAUT
          YT=YB-YMT
          IF(YLEN.LT.YT) YLEN=YT
        ENDIF
      ELSE
        IF(ENTR.GT.HAUT) THEN
          YLEN=HAUT
        ENDIF
      ENDIF

      
	IF (XLEN.GT.YLEN) THEN
	  RLONG = XLEN
	  SHORT = YLEN
	ELSE
	  RLONG = YLEN
	  SHORT = XLEN
	ENDIF

      DO 1 IND1=1,IPT
	IND2 = IPTS2(IND1,NEL)

C     Détermination des contraintes nécessaires au calcul de SigPlaque
C     ----------------------------------------------------------------
      
	IF (XLEN.GT.YLEN) THEN
	  SGSHRT = EFFCOMB(4080+4*51+IND2)
	  SGLONG = EFFCOMB(4335+4*51+IND2)
	ELSE
	  SGSHRT = EFFCOMB(4335+4*51+IND2)
	  SGLONG = EFFCOMB(4080+4*51+IND2)
	ENDIF
	TAU = 0

C     Détermination de la pression nécessaire au calcul de SigPlaque
C     --------------------------------------------------------------

      IF (IND2.LE.11) THEN
	 GLOAD = (CHAMAX(2) - CHAMAX(1))/(11 - 1)*(IND2 - 1) + CHAMAX(1)
	 DO K=1,9															!!!aout04
	   dGLOAD(K) = (dCHAMAX(2,K)-dCHAMAX(1,K))/(11-1)*(IND2-1)			!!!aout04
     *               + dCHAMAX(1,K)										!!!aout04
	 ENDDO																!!!aout04
	ELSE IF (IND2.LE.16) THEN
       GLOAD = (CHAMAX(3) - CHAMAX(2))/(16 - 11)*(IND2 - 11) + CHAMAX(2)
	 DO K=1,9															!!!aout04
	   dGLOAD(K) = (dCHAMAX(3,K)-dCHAMAX(2,K))/(16-11)*(IND2-1)			!!!aout04
     *               + dCHAMAX(2,K)										!!!aout04
	 ENDDO
	ELSE IF (IND2.LE.21) THEN
	 GLOAD = (CHAMAX(4) - CHAMAX(3))/(21 - 16)*(IND2 - 16) + CHAMAX(3)
	 DO K=1,9															!!!aout04
	   dGLOAD(K) = (dCHAMAX(4,K)-dCHAMAX(3,K))/(21-16)*(IND2-1)			!!!aout04
     *               + dCHAMAX(3,K)										!!!aout04
	 ENDDO
	ELSE IF (IND2.LE.31) THEN
	 GLOAD = (CHAMAX(5) - CHAMAX(4))/(31 - 21)*(IND2 - 21) + CHAMAX(4)
	 DO K=1,9															!!!aout04
	   dGLOAD(K) = (dCHAMAX(5,K)-dCHAMAX(4,K))/(31-21)*(IND2-1)			!!!aout04
     *               + dCHAMAX(4,K)										!!!aout04
	 ENDDO
	ENDIF
      PRSABS = DABS(GLOAD)
	SIGNE=1.															!!!aout04
	IF (GLOAD.LT.0) SIGNE=-1.											!!!aout04
	IF (PRSABS.LT.DABS(PLOC)) THEN										!!!aout04
	  PRSABS = DABS(PLOC)												!!!aout04
c	ELSE																!!!aout04
	  SIGNE=0.															!!!aout04
	ENDIF																!!!aout04
	PRSABS = PRSABS*9810.0 ! (N/m²)

	DO K=1,9															!!!aout04
	  dPRSABS(K)=SIGNE*dGLOAD(K)*9810.0									!!!aout04
	ENDDO																!!!aout04

C     Calcul du SigPlaque
C     -------------------

C     (ILOOP= 1 flexion selon X et ILOOP=2 flexion selon Y)
C     On va prendre la contrainte la plus grande des 2
      DO ILOOP=1,2
	

	IF (YLEN.LT.XLEN) THEN
	  ILOOP2 = ILOOP
	ELSE
	  ILOOP2 = 3 - ILOOP
	ENDIF	

      SIGMEM(1) = SGSHRT
      SIGMEM(2) = SGLONG
      SGMEM1(ILOOP) = SIGMEM(ILOOP2)
      SGMEM2(ILOOP) = SIGMEM(3-ILOOP2)
      BOVERA = SHORT/RLONG
      ARPLT = 1.0/BOVERA
      BOT2 = (SHORT/TPL)**2
      SKBEN(ILOOP) = 0.31 + PBCOEF1(ILOOP2)*TANH(2.*ILOOP2*(ARPLT-1.))
      SIGBEN(ILOOP) = SKBEN(ILOOP)*PRSABS*BOT2
      PHI(ILOOP) = 1.0

      IF (ILOOP2 .EQ. 2) GO TO 24  ! skip; no magnification (PHI=1) as bending in the long direction.

      IF (SGSHRT .GE. 0.) GO TO 24  ! skip; no membrane stress in the short direction ==> no need for ampllification

      SKWIDE = 0.96*(0.9 + 1.8*BOVERA**2)**2
      SGSHCR = SKWIDE*E/BOT2

      OMEGA = -SGSHRT/SGSHCR
      XIPANL = 0.5*PI*SQRT(ABS(OMEGA))
      XIZERO = (1.0 + 0.35*BOVERA) * XIPANL
      IF (XIZERO .LT. 0.02) THEN
         XZERO2 = XIZERO*XIZERO
         XZERO4 = XZERO2*XZERO2
         PHI(ILOOP) = (1. + 0.4*XZERO2 + 17./105.*XZERO4) /
     *         (1. + XZERO2/3.0 + 2.0/15.0*XZERO4)
      ELSE IF (XIZERO .LE. 3.10) THEN
         PHI(ILOOP) = 3. * ( TAN(XIZERO) - XIZERO ) /
     *                    ( XIZERO**2 * TAN(XIZERO) )
      ELSE
         PHI(ILOOP) = 2.5
      END IF

  24  SIGMAG(ILOOP) = PHI(ILOOP)*SIGBEN(ILOOP)

C  TENSILE (T) BENDING STRESS:
C     STRESS IN PRIMARY (1) BENDING DIRECTION
      SIG1T(ILOOP) = SGMEM1(ILOOP) + SIGMAG(ILOOP)
C     STRESS IN SECONDARY (2) DIRECTION (WITH POISSON'S RATIO BENDING STRESS)
      SIG2T(ILOOP) = SGMEM2(ILOOP) + ETA*SIGMAG(ILOOP)
      SIGVMT(ILOOP) = SQRT(SIG1T(ILOOP)**2 + SIG2T(ILOOP)**2 
     *                   + 3.*TAU**2 - SIG1T(ILOOP)*SIG2T(ILOOP))
C
C  COMPRESSIVE (C) BENDING STRESS
C     STRESS IN PRIMARY (1) BENDING DIRECTION
      SIG1C(ILOOP) = SGMEM1(ILOOP) - SIGMAG(ILOOP)
C     STRESS IN SECONDARY (2) DIRECTION (WITH POISSON'S RATIO BENDING STRESS)
      SIG2C(ILOOP) = SGMEM2(ILOOP) - ETA*SIGMAG(ILOOP)
      SIGVMC(ILOOP) = SQRT(SIG1C(ILOOP)**2 + SIG2C(ILOOP)**2 
     *                  + 3.*TAU**2 - SIG1C(ILOOP)*SIG2C(ILOOP))
      
	IF (SIGVMT(ILOOP).GT.SIGVMC(ILOOP)) THEN
        SIGVM(ILOOP) = SIGVMT(ILOOP)
	  LOOP1(ILOOP) = 1      !LOOP1 = 1 -> Traction; = 2 -> Compression
      ELSE
        SIGVM(ILOOP) = SIGVMC(ILOOP)
	  LOOP1(ILOOP) = 2
      ENDIF

      ENDDO

	IF (SIGVM(1).GT.SIGVM(2)) THEN
	  L = 1  ! indice L correspond au cas où la contrainte est la plus sévère
	ELSE
	  L = 2
	ENDIF
	SigPlaque(IND1) = SIGVM(L)

	
C     Enregistrement de certaines valeurs à afficher dans Sol2
C     --------------------------------------------------------

      IF (IND2.NE.0) THEN
      IF (L.EQ.1) THEN
	  SIGXPlaque(IND1) = ETA*SIGMAG(L)
	  SIGYPlaque(IND1) = SIGMAG(L)
	ELSE
        SIGYPlaque(IND1) = ETA*SIGMAG(L)
	  SIGXPlaque(IND1) = SIGMAG(L)
	ENDIF
	PHIPlaque(IND1) = PHI(L)
	SIGVMTPlaque(IND1) = SIGVMT(L)
	SIGVMCPlaque(IND1) = SIGVMC(L)
	INDPlaque(IND1) = IND2
	ENDIF
	SIGX(IND1) = EFFCOMB(4335+4*51+IND2)
	SIGY(IND1) = EFFCOMB(4080+4*51+IND2)

C     Calcul du DSigPlaque
C     --------------------


	IF (YLEN.LT.XLEN) THEN
	  L2 = L
	ELSE
	  L2 = 3 - L
	ENDIF

c      NBRXI=NVAR(NEL)

      DO 100 KK=1,NBRXI

      K=NXIT(KK,NEL)

      
	IJ=4*51 + IND2  
      
	
	
	dSIGMAX = SENS3(1,IND1,K,NEL)  ! = d(SIGMAX)/dx
      IF (EFF(IJ+4335)*SIG1.NE.0) THEN
	  DEN = EFF(IJ+4335)*SIG1
	  dSIGMAX = dSIGMAX + dSIG1(K)*DEN/DABS(DEN)
	ELSE
	  dSIGMAX = dSIGMAX + dSIG1(K) 
	ENDIF
      
	dSIGMAY = SENS3(2,IND1,K,NEL)  ! = d(SIGMAY)/dx
	
	IF (XLEN.GT.YLEN) THEN
	  dSGSHRT = dSIGMAY
	  dSGLONG = dSIGMAX
	ELSE
	  dSGSHRT = dSIGMAX
	  dSGLONG = dSIGMAY
	ENDIF

c        Calcul de dSigmag
      
      IF (K.EQ.1) THEN        ! Epaisseur (TPL)
        dARPLT = 0
	  dBOT2 = -2.*SHORT**2/TPL**3
	  dBOVERA = 0
	ELSE IF (K.EQ.2) THEN   ! Hauteur âme cadre
        dARPLT = 0
	  dBOT2 = 0
	  dBOVERA = 0
	ELSE IF (K.EQ.3) THEN   ! Epaisseur âme cadre
        dARPLT = 0
	  dBOT2 = 0
	  dBOVERA = 0 
	ELSE IF (K.EQ.4) THEN   ! Largeur semelle cadre
        dARPLT = 0
	  dBOT2 = 0
	  dBOVERA = 0
	ELSE IF (K.EQ.5) THEN   ! Ecartement des cadres (EPSA=RLONG)
        IF (HYA/TPL.LE.2.OR.EPSA.GT.WIDTH) THEN
          dARPLT = 0
	    dBOT2 = 0
	    dBOVERA = 0
	  ELSE
          IF (XLEN.GT.YLEN) THEN
	      dARPLT = 1/SHORT
	      dBOT2 = 0
	      dBOVERA = -SHORT/RLONG**2
	    ELSE
	      dARPLT = -RLONG/SHORT**2
	      dBOT2 = 2.*SHORT/TPL**2
	      dBOVERA = 1./RLONG
	    ENDIF
	  ENDIF
	ELSE IF (K.EQ.6) THEN   ! Hauteur âme raidisseur
        dARPLT = 0
	  dBOT2 = 0
	  dBOVERA = 0
	ELSE IF (K.EQ.7) THEN   ! Epaisseur âme raidisseur
        dARPLT = 0
	  dBOT2 = 0
	  dBOVERA = 0
	ELSE IF (K.EQ.8) THEN   ! Largeur semelle raidisseur
        dARPLT = 0
	  dBOT2 = 0
	  dBOVERA = 0
	ELSE IF (K.EQ.9) THEN   ! Ecartement des raidisseurs (EPSR diff. de ENTR)
	  IF ((HXR/DELTA).LE.2) THEN
          dARPLT = 0
	    dBOT2 = 0
	    dBOVERA = 0
	  ELSE
	    IF (XLEN.GT.YLEN) THEN
		  dARPLT = -RLONG/EPSR**2    ! car d(C)/d(EPSR)= d(C)/d(ENTR)*(ENTR/EPSR)**2
	      dBOT2 = 2.*SHORT**3/(TPL*EPSR)**2
	      dBOVERA = 1./RLONG*(SHORT/EPSR)**2
	    ELSE
	      dARPLT = ENTR**2/(SHORT*EPSR**2)
	      dBOT2 = 0
	      dBOVERA = -SHORT/EPSR**2
	    ENDIF
	  ENDIF
	ENDIF
      
      dSKWIDE = 0.96*2*(0.9 + 1.8*BOVERA**2)*1.8*2*BOVERA*dBOVERA
      
      dSGSHCR = E*(dSKWIDE*BOT2 - SKWIDE*dBOT2)/BOT2**2
      
	
	dOMEGA = -(SGSHCR*dSGSHRT - SGSHRT*dSGSHCR)/SGSHCR**2
	IF (OMEGA.LT.0) THEN
	  dXIPANL = -0.5*PI*dOMEGA/(2*SQRT(ABS(OMEGA)))  
	ELSE
        dXIPANL = 0.5*PI*dOMEGA/(2*SQRT(ABS(OMEGA)))  
	ENDIF
      
      dXIZERO = 0.35*dBOVERA*XIPANL + (1.0 + 0.35*BOVERA)*dXIPANL

      IF (L2.EQ.1.AND.SGSHRT.LT.0) THEN
        IF (XIZERO .LT. 0.02) THEN
          dPHI = ((0.8*XIZERO*dXIZERO + 68./105.*XIZERO**3*dXIZERO)*
     *         (1. + XIZERO**2/3 + 2./15.*XIZERO**4) 
     *       - (1. + 0.4*XIZERO**2 + 17./105.*XIZERO**4)*
     *         (2./3.*XIZERO*dXIZERO + 8./15.*XIZERO**3*dXIZERO))
     *        /(1. + XIZERO**2./3. + 2./15.*XIZERO**4)**2
	  ELSE IF (XIZERO .LE. 3.10) THEN
          dPHI = 3.*((dXIZERO*(1 + (TAN(XIZERO))**2) - dXIZERO)
     *                                         *(XIZERO**2*TAN(XIZERO)) 
     *           - (TAN(XIZERO) - XIZERO)*(2*XIZERO*dXIZERO*TAN(XIZERO) 
     *                     + XIZERO**2*dXIZERO*(1. + (TAN(XIZERO))**2)))
     *        /(XIZERO**2*TAN(XIZERO))**2
        ELSE
           dPHI = 0
        ENDIF
	ELSE
	  dPHI = 0
	ENDIF

      dSKBEN = PBCOEF1(L2)*(1 - (TANH(2.*L2*(ARPLT-1.)))**2)*2*L2*dARPLT

      dSIGBEN(K) = dSKBEN*PRSABS*BOT2 + SKBEN(L)*PRSABS*dBOT2				!!!aout04
     *			+ dPRSABS(K)*SKBEN(L)*BOT2								!!!aout04

      dSIGMAG(K) = PHI(L)*dSIGBEN(K) + SIGBEN(L)*dPHI						!!!aout04

c        Fin calcul de dSigmag

      
      dSIGMEM(1) = dSGSHRT
      dSIGMEM(2) = dSGLONG

      dSGMEM1(K) = dSIGMEM(L2)
	dSGMEM2(K) = dSIGMEM(3 - L2)

      IF (LOOP1(L).EQ.1) THEN        ! la traction est le cas plus restrictif
        dSIG1T(K) = dSGMEM1(K) + dSIGMAG(K)
	  dSIG2T(K) = dSGMEM2(K) + ETA*dSIGMAG(K)

	 DSigPlaque(IND1,K) = (2*SIG1T(L)*dSIG1T(K) + 2*SIG2T(L)*dSIG2T(K) 
     *    - SIG1T(L)*dSIG2T(K) - SIG2T(L)*dSIG1T(K)) /(2.*SIGVMT(L))      
	ELSE                              ! la compression est le cas plus restrictif
        dSIG1C(K) = dSGMEM1(K) - dSIGMAG(K)
	  dSIG2C(K) = dSGMEM2(K) - ETA*dSIGMAG(K)

	 DSigPlaque(IND1,K) = (2*SIG1C(L)*dSIG1C(K) + 2*SIG2C(L)*dSIG2C(K) 
     *    - SIG1C(L)*dSIG2C(K) - SIG2C(L)*dSIG1C(K)) /(2.*SIGVMC(L))
	ENDIF


  100 CONTINUE ! fin de la boucle sur les variables de conception

  1   CONTINUE ! fin de la boucle sur le nombre de points de calcul des sensibilités

	RETURN
	END