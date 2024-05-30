      SUBROUTINE COMBINE(EFF,SIG1,SIG2,SIG3,SIG4,Tau,
     *                   MT,EFFCOMB,NETO,NVAR,NXIT,NEL,IPTS,IPTS2,TXR)

      IMPLICIT REAL*8(A-H,O-Z)
	REAL*8 LAMB,LAMB2
	CHARACTER *18 NOM2
     
c	DIMENSION SENS2(16*IPTmax*9*NETO)
c      DIMENSION SENS2(69120)					!extension neto
c      DIMENSION SENS3(21*IPTmax*9*NETO)
c	DIMENSION SENS3(90720)					!extension neto
      DIMENSION EFF(9690),EFFCOMB(9690),TEMP2(1530)
      DIMENSION dSIG1(9),dSIG2(9),dSIG3(9),dSIG4(9),dTau(9),dFlr(9),
     *          dWpl1(9),dWpl2(9),NOM2(15)
	DIMENSION NVAR(NETO),NXIT(9,NETO),IPTS(NETO),IPTS2(IPTmax,NETO)

      COMMON/DIM1/Nmax,NEmax,NVmax,
     *            M1max,M1Tmax,M2max,M2Tmax,Mmax,MGmax,ISmax,IPTmax ! dimension max des vecteurs

	COMMON/OPTI/IOPTI,NTOT

	DATA NOM2/'Sbord Z=0 X=L/2','Tbord Z=0 X=0','Sbord z=-d/2 X=L/2',
     *         'Sbord z=+d/2 X=L/2','Tcadre JAS X=0','Scadre JAS X=L/2',
     *         'Tcadre JAB X=0','Scadre JAB X=L/2','Scadre Sem X=L/2',
     *         'Traid JAS X=0' ,'Traid JAB X=0'   ,'Sraid Sem X=L/2',
     *         'Ttrav JAS X=0' ,'Ttrav JAB X=0'   ,'Strav Sem X=L/2'/

C***********************************************************************
C     SUBROUTINE COMBINE
C     ==================
C     Cette sous-routine est appelée dans BO2.
C     Elle combine les résultats donnés par la sous-routine RESUL avec
C     ceux donnés par la sous-routine STIFF.
C
C   INPUTS : EFF,SIG1,SIG2,SIG3,SIG4,TAU
C            où EFF est calculé dans RESUL
C               SIG1,SIG2,SIG3,SIG4 et TAU sont calculés dans STIFF
C        
C   OUTPUTS : EFFCOMB

C  Créer: le 15-4-2003  par  F. Bair
C
C  Modifications: 
C     - 15 avril 2003 : Modif en vue de la combinaison des contraintes
C                               
C  Dernière modif : 20-5-2003	     	
C
C************************************************************************
C************************************************************************
C************************************************************************

      DO I=1,9690
	  EFFCOMB(I) = EFF(I)
	END DO
C 
C  1) CALCUL DU EFFCOMB
C
      KH = 31 + 2*MT
	DO 114 K=1,5  ! boucle sur les 5 points selon X
C
	DO 112 I=1,KH ! boucle sur les 31 points selon Y
	IJ = I + (K-1)*51
C
C     LE BORDAGE
C     **********
C     CALCUL DANS LE BORDAGE A MI-EPAISSEUR (Z=0)
C     (modification de SIGMAX, TXY et SIGMA COMP)

      IF (EFF(IJ+4335).NE.0) THEN
	EFFCOMB(IJ+4335) = EFF(IJ+4335)*(1 + DABS(SIG1/EFF(IJ+4335))) ! SIGMAX
	ELSE
      EFFCOMB(IJ+4335) = SIG1 ! quel signe ?
	ENDIF
c      EFFCOMB(IJ+4590) = EFF(IJ+4590) !+ TAU  ! TXY           

	EFFCOMB(IJ+4845)=DSQRT(EFFCOMB(IJ+4080)**2+EFFCOMB(IJ+4335)**2 
     *             - EFFCOMB(IJ+4080)*EFFCOMB(IJ+4335)
     *             + 3.D0*(EFFCOMB(IJ+4590)**2))  ! SIGMA COMP             

C     RAIDISSEURS
C     ***********
C     CALCUL DE : SIGMAX JAS, SIGMAX JAB et SIGMAX SEMELLE
c      EFFCOMB(IJ+7395) = EFF(IJ+7395)  !+ SIG4 ! SIGMAX JAS    !!!!!!!attention, changer signe
      IF (EFF(IJ+7395).NE.0) THEN
	EFFCOMB(IJ+7395) = EFF(IJ+7395)*(1 + DABS(SIG4/EFF(IJ+7395))) ! SIGMAX JAS
	ELSE
      EFFCOMB(IJ+7395) = SIG4 ! quel signe ?
	ENDIF

      IF (K.NE.1) THEN !!! pas additionner SIG3 en X=0 pour que la restriction 33 corresponde à
	                 !!! Tau(RESUL) + Tau(STIFF) sans SIGMAjab(STIFF)
	IF (EFF(IJ+7650).NE.0) THEN
	EFFCOMB(IJ+7650) = EFF(IJ+7650)*(1 + 
     *                                  DABS(SIG3*10./12./EFF(IJ+7650))) ! SIGMAX JAB
	ELSE
      EFFCOMB(IJ+7650) = SIG3*10./12. ! quel signe ? ! rem.: *10/12 pour avoir PL²/12 (et non PL²/10)
	ENDIF
	ENDIF

      IF (EFF(IJ+7905).NE.0) THEN
	EFFCOMB(IJ+7905) = EFF(IJ+7905)*(1 + DABS(SIG2/EFF(IJ+7905))) ! SIGMAX SEM
	ELSE
      EFFCOMB(IJ+7905) = SIG2 ! quel signe ?
	ENDIF

C     CALCUL DE : TXZ JAS, TXZ JAB et Tyx SEM 
C     + CALCUL DE : SIGMA COMP JAS, SIGMA COMP JAB , SIGMA COMP SEMELLE     
                                               
c      EFFCOMB(IJ+8160) = EFF(IJ+8160)	 !+ TAU		! TXZ JAS  !!!!!!!attention, changer signe
c      IF (EFF(IJ+8160).NE.0) THEN
c	EFFCOMB(IJ+8160) = EFF(IJ+8160)*(1 + DABS(TAU/EFF(IJ+8160))) ! TXZ JAS
c	ELSE
c      EFFCOMB(IJ+8160) = TAU ! quel signe ?
c	ENDIF

	IF (EFF(IJ+8415).NE.0) THEN
	EFFCOMB(IJ+8415) = EFF(IJ+8415)*(1 + DABS(TAU/EFF(IJ+8415))) ! TXZ JAB
	ELSE
      EFFCOMB(IJ+8415) = TAU ! quel signe ?
	ENDIF
	
      EFFCOMB(IJ+8925) = DSQRT(EFFCOMB(IJ+7395)**2
     *                         + 3.D0*(EFFCOMB(IJ+8160)**2))  ! SIGMA COMP JAS
      EFFCOMB(IJ+9180) = DSQRT(EFFCOMB(IJ+7650)**2 
     *                         + 3.D0*(EFFCOMB(IJ+8415)**2))	! SIGMA COMP JAB
      IF(TXR.NE.0.) THEN
       EFFCOMB(IJ+8670)= EFF(IJ+8670) !!!+ ???		! Tyx SEM
       EFFCOMB(IJ+9435)=DSQRT(EFFCOMB(IJ+7905)**2
     *                         +3.D0*(EFFCOMB(IJ+8670)**2))	! SIGMA COMP SEMELLE
	ENDIF


  112 CONTINUE   ! boucle sur les 31+2*MT points (selon Y)          

  114 CONTINUE  ! boucle sur les 5 coupes (selon X)

      RETURN

C ******************************************************************************
C 	LES FORMATS
C 	-----------
  282 FORMAT(A18,1X,5(1X,E13.6))
  288 FORMAT(' Variable de conception nø',I3,' du panneau nø',I3/
     *         50(1H-))
  294 FORMAT('     Fct.             Pt.1          Pt.2',
     *     '          Pt.3          Pt.4          Pt.5')

	END