      SUBROUTINE HUGHES(IPRINT,CHAMAX,PLOC,E,ETA,SYLDP,SIGM,
     *            WIDTH,PHIL,Q,MT,ABTR,HYA,EPSA,HXR,EPSR,ENTR,DELTA,
     *            EFF,TPLMIN,TPLA,TPLR,TPLD,
     *            IDD,IAA,IRR,ISTOP,NEL,IOPTION,ITERA,IS,
     *            EFFCOMB,SIGG)                                  !avril2003

C***********************************************************************
C     SUBROUTINE HUGHES 
C     =================
c
c  IOPTION = 1 : avec calcul des sensibilités (si restriction d'optimisation)
c  IOPTION = 0 : sans calcul des sensibilités (uniquement calcul du dmin)
c
C     SUBROUTINE de calcul de l'épaisseur minimale d'un plaque non raidie 
C     sollicitée dans son plan (in plane loads) et par une pression latérale.
C
C     L'épaisseur minimale est calculée de 2 manières (la + grande est retenue):
C      - Subr.  PLTBEN 
C        Il s'agit d'un simple analyse statique élastique.
C        Cette subroutine n'intègre pas de vérification au voilement du
C        panneau étudié.
C        Seul l'effet de magnification des contraintes est considéré mais
C        indépendamment d'un risque de voilement.
C        Cette subroutine est une adaptation du programme de O. Hughes 
C	   (ref : SNAME, "Ship Structure Design", Chap.9: Plate Bending.)
C          (LAST REVISION: 21 MARCH 1993).
c        Safety on load (=Sy/Smax).
C      - Subr.  BUCKLE
C        Cette subroutine intègre une vérification au voilement du panneau
C        étudié soumis à Sx, Sy et Tau
C        Elle sert au calcul de l'épaisseur minimale requise pour éviter
C        le voilement.
C        Elle a été développée par Dr. Ph. Rigo, ANAST, ULG (1996)
C        Safety on load (=Sy/Smax) and Safety on strength =1.10
C
C  OUTPUT: TPLMIN = Epaisseur minimale
C          TPLA   = Dérivée de TPLMIN selon EPSA
C          TPLR   = Dérivée de TPLMIN selon EPSR avec EPSR= Fct(D=ENTR)
C          TPLD   = Dérivée de TPLMIN selon Delta
C
C    Créer : Avril 96  pour LBR-5 (Janvier 1996), ANAST par le Dr. Ph. Rigo	
C    -------
C    Modif : 22 mai 96 : Epaiss. minimale pour le  voilement; Subr. BUCKLE
C    ------  31 mai 96 : Dérivée de dmin par delta calculée par différence
C                        finie
C            6 mars 97 : Introduction de D(raid) c.à.d. ENTR non égal à EPSR
C                         d(C)/d(EPSR)= d(C)/d(D) * (D/EPSR)**2
C           16 Juil 97 : Correction Subr. Buckle , RS= ... *ALPHA2**2
C            6 Déc  99   Problème de convergence avec PLSOLV
c           13 Janv 02 : Avec et sans calcul des sensibilités (IOPTION),
c                        Impression des contraintes critiques obtenues.
c             Mai 2003 : Combinaison  SX(Resul)  +  Sx(STIFF)  (F. BAIR)   !avril2003
C            Juin 2003 : Combinaison ISX(Resul)I + ISx(STIFF)I (F. BAIR)   !juin2003
C           
C   Dernière modif: 26 nov 2003
C   --------------
C*************************************************************************

      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*10 TYP(3)
      CHARACTER*18 POSIT
      DIMENSION VAR(2),SX(5),SY(5),TAU(5),EFF(9690),ABTR(10)
	DIMENSION SXCOMB(5),SYCOMB(5),TAUCOMB(5),EFFCOMB(9690) !avril2003
      DIMENSION CHAMAX(5),XP(5)
      DIMENSION GV1(2),GV2(2)

      COMMON/OPTI/ IOPTI
      COMMON/PY/ PI
      COMMON/LANGUE/ LANGUE        ! 1 French (par defaut), 2 English 

      DATA TYP/'Yielding','Buckling','Undefined'/

      PIQ=PI*Q/180.0
	
 	JPRINT=IPRINT  ! Impression Standard : IPRINT= 1 si IOPTI=3
c      JPRINT=1  !Pour impressions complementaires localisées à cette Subroutine

c     WRITE (67,*) ! Passer 1 ligne
C***********************************************************************
C***********************************************************************
C               Œ Y et Sigma Y
C               I
C               *****************************
C               *                           *  b=XLEN
C               *                           *
C               *                           *
C               ***************************** --------> X et Sigma X
c               <----------- a = YLEN ------>

C  1.0 Données à déterminer
C  ************************
c      XLEN,YLEN        = a,b       (m)
c      SYLDP            = Contr. Elastique  (N/m2), (ex, Re=240 N/mm2)
c      SIGM             = Contr. admissible (N/m2)
c      XP               = P         (m d'eau)
c      SIGBX,SIGBY,TAU  = Sx,Sy,Tau (N/m2)

C  1.1 GAMMAS, le coef de sécurité sur les charges (SECU ds BUCKLE et PLTBEN)
C  =========================================================================
       GAMMAS = SYLDP/SIGM   ! en plus, il y a "the Safety on Strength" = 1.10 dans BUCKLE

C  1.2 XLEN,YLEN  les dimensions (a et b) du panneau
C  ==================================================
      XLEN=EPSA
      YLEN=ENTR
      IA=0
      IR=0

      IF((HYA/DELTA).LE.2) THEN
        IA=1
        XLEN=WIDTH
c       WRITE(*,*)  'Sbr. Hughes : Cadres trop petits !!!'
c       WRITE(*,*)  'On prend XLEN=WIDTH= ',WIDTH,' (m)'
        WRITE(67,*)'Warning: Sbr. Hughes : Cadres trop petits !!!'
        WRITE(67,*)'On prend Portée de la plaque=WIDTH=',WIDTH,' (m)',
     *              ' au lieu de l''entredistance entre cadres.'
        WRITE(29,*)'Warning: Sbr. Hughes : Cadres trop petits !!!'					!bug
        WRITE(29,*)'On prend Portée de la plaque=WIDTH=',WIDTH,' (m)',				!bug
     *              ' au lieu de l''entredistance entre cadres.'						!bug

      ELSE
        IF(EPSA.GT.WIDTH) THEN
          IA=1
          XLEN=WIDTH
        ENDIF
      ENDIF

      HAUT=Q*PHIL*PI/180.
      IF((HXR/DELTA).LE.2) THEN
        IR=1
        YLEN=HAUT
        WRITE(67,'(A,I3,A)') 'Panel n°',NEL,
     *               ' Raidisseurs trop petits !!! (Sbr. Hughes)'

        WRITE(29,'(A,I3,A)') 'Panel n°',NEL,											!bug
     *               ' Raidisseurs trop petits !!! (Sbr. Hughes)'						!bug

        IF(MT.GT.0) THEN
          IR=0
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
c         WRITE(*,*) 'Il y a des traverses alors'
c         WRITE(*,*) 'on prend l''entredistance max = ',YLEN,' (m)'
          WRITE(67,'(2A,F8.4,A)') 'Il y a des traverses alors ',
     *         'on prend l''entredistance max = ',YLEN,'(m)'
          WRITE(29,'(2A,F8.4,A)') 'Il y a des traverses alors ',						!bug
     *         'on prend l''entredistance max = ',YLEN,'(m)'							!bug

        ELSE
c         WRITE(*,*) 'Il n''y a pas de traverses alors'
c         WRITE(*,*) 'on prend comme largeur le panneau= ',YLEN,' (m)'
          WRITE(67,'(2A,F8.4,A)') 'Il n''y a pas de traverses alors ',
     *         'on prend comme largeur le panneau= ',YLEN,'(m)'
          WRITE(29,'(2A,F8.4,A)') 'Il n''y a pas de traverses alors ',				!bug
     *         'on prend comme largeur le panneau= ',YLEN,'(m)'						!bug

        ENDIF
      ELSE
        IF(ENTR.GT.HAUT) THEN
          IR=1
          YLEN=HAUT
        ENDIF
      ENDIF

C  1.3 CHARGE = XP, la pression (en y = 0; 1/3; 1/3; 2/3 et 1)
C  ============================================================
      DO  I=1,5
	  XP(I)=DABS(CHAMAX(I)) ! le sens n'est pas important
	  IF(XP(I).LE.DABS(PLOC)) XP(I)=DABS(PLOC)   ! PLOC la pression max. localisée (charge locale)
	ENDDO


c     XP(1 à 5) pressions en mètre d'eau (calculées dans subr. Ent)

C  1.4 SIGBX,SIGBY,TAU   Les contraintes (en y = 0; 1/3; 1/2; 2/3 et 1)
C  ====================================================================
      SX(1)= EFF(1+4335+204)
      SY(1)= EFF(1+4080+204)
      TAU(1)=EFF(1+4590)
      SX(2)= EFF(11+4335+204)
      SY(2)= EFF(11+4080+204)
      TAU(2)=EFF(11+4590)
      SX(3)= EFF(16+4335+204)
      SY(3)= EFF(16+4080+204)
      TAU(3)=EFF(16+4590)
      SX(4)= EFF(21+4335+204)
      SY(4)= EFF(21+4080+204)
      TAU(4)=EFF(21+4590)
      SX(5)= EFF(31+4335+204)
      SY(5)= EFF(31+4080+204)
      TAU(5)=EFF(31+4590)

      SXCOMB(1)= EFFCOMB(1+4335+204) !avril2003
      SYCOMB(1)= EFFCOMB(1+4080+204) !avril2003
      TAUCOMB(1)=EFFCOMB(1+4590)  !avril2003
      SXCOMB(2)= EFFCOMB(11+4335+204) !avril2003
      SYCOMB(2)= EFFCOMB(11+4080+204) !avril2003
      TAUCOMB(2)=EFFCOMB(11+4590) !avril2003
      SXCOMB(3)= EFFCOMB(16+4335+204) !avril2003
      SYCOMB(3)= EFFCOMB(16+4080+204) !avril2003
      TAUCOMB(3)=EFFCOMB(16+4590) !avril2003
      SXCOMB(4)= EFFCOMB(21+4335+204) !avril2003
      SYCOMB(4)= EFFCOMB(21+4080+204) !avril2003
      TAUCOMB(4)=EFFCOMB(21+4590) !avril2003
      SXCOMB(5)= EFFCOMB(31+4335+204) !avril2003
      SYCOMB(5)= EFFCOMB(31+4080+204) !avril2003

C***********************************************************************
C      WRITE(67,*)'XLEN,YLEN   =',XLEN,YLEN
C      WRITE(67,*)'Hield Stress=',SYLDP
C      WRITE(67,*)'Sig MAX     =',SIGM
C      WRITE(67,*)'XP =',XP
C      WRITE(67,*)'SX =',SX
C      WRITE(67,*)'SY =',SY
C      WRITE(67,*)'TAU=',TAU
c      PAUSE'ok?'
C***********************************************************************

C 2. IMPRESSION DES DONNEES:
C   -----------------------
c     JPRINT=1  ! pour debugging
      IF(JPRINT.GE.1) THEN
        WRITE (67, 999)
        WRITE (67,1003)
        WRITE (67,1007) XLEN,YLEN,E,SYLDP,SIGM
        WRITE (67,1011)
      ENDIF

C***********************************************************************
C 3.0 SOLVE FOR THE UNKNOWN: THICKNESS=? (First Yielding)
C     -----------------------------------------------------
C     (le calcul des dérivées est effectués par différence finies en XI
C***********************************************************************

C***********************************************************************
C 3.1 CAS 1 : Calcul avec EPSA et ENTR (données courantes)
C***********************************************************************
      TPLMIN=0.00
      ZERO=0.0E+00
      IC=0

C Boucle sur les 5 points de calculs (Y=0, 1/3, 1/2, 2/3 et 1)
c ------------------------------------------------------------

      DO 26 ICAS =1,5
C      WRITE(67,*) 'ICAS=',ICAS
C      WRITE(67,*) '**************'
C      JPRINT=1
c      XP(ICAS)=0.0099
c      SX(ICAS)=-75.E6
c      SY(ICAS)=-75.E6
c      TAU(ICAS)=50.E6

C     Comme épaisseur de départ (TPL) pour les itérations on prend la somme de
C     l'épaisseur minimale en flexion "SHORT * SQRT(0.5*XP(ICAS)*9810/SYLDP)
C     et de l'épaisseur minimale de voilement "(SHORT/2.) * SQRT(SYLDP/E)
C     et on impose un élancement de 2 (Béta = plate slenderness).

      SHORT = MIN(XLEN,YLEN)
      TPL   = SHORT * SQRT(0.5*XP(ICAS)*9810/SYLDP) +
     *                (SHORT/2.) * SQRT(SYLDP/E)

C     BENDING IN BOTH DIRECTIONS IS EXAMINED (TWO LOOPS : loop 24).
C     WE SELECT THE MORE CONSERVATIVE CASE (GREATER TPL)

C     Calcul de l'épaisseur minimale pour Sig x et sig Y (Tau=0)
c     ----------------------------------------------------------
C     (ILOOP= 1 flexion selon X et ILOOP=2 flexion selon Y (cfr Subr PLTBEN)
      DO 24 ILOOP = 1,2
c        WRITE(67,*) 'ILOOP=',ILOOP,' SIGMA'
C        WRITE(67,*) '================='
         VAR(1) = TPL
         VAR(2) = 0.8*TPL
C        (WE NEED TO GET TWO INITIAL ESTIMATES FOR TPL = VAR(1) and VAR(2) )
C        (FOR STATIC AND QUASISTATIC PRESSURE, USE THE INITIAL YIELD VALUE)

         CALL PLSOLV(ILOOP,VAR,XLEN,YLEN,GV1,GV2,XP(ICAS),
     *              SX(ICAS),SY(ICAS),ZERO,E,ETA,SYLDP,GAMMAS,ISTOP,IH,
     *              SXCOMB(ICAS),SYCOMB(ICAS),ZERO,SIGMAG)             !avril2003

         IF(ISTOP.EQ.1) THEN
		  WRITE (67,1010) XP(ICAS),SX(ICAS),SY(ICAS),ZERO
            RETURN
         ENDIF
         IF(JPRINT.GE.1) THEN
          IF(ILOOP.EQ.1) THEN ! pas intéressant d'imprimer pour ILOOP=2 car dmin tjrs plus faible
           SR11=2.0/(1.0+GV1(1)) - 1.0 
           SR12=2.0/(1.0+GV1(2)) - 1.0
           SR21=2.0/(1.0+GV2(1)) - 1.0
           SR22=2.0/(1.0+GV2(2)) - 1.0
c           WRITE (67,*) 'ICAS=',ICAS ,'impression de XP,SX,SY,tau,dmin'
           WRITE(67,1010)XP(ICAS),SX(ICAS),SY(ICAS),ZERO,VAR(2),TYP(IH),
     *                            SR11,SR12,SR21,SR22
           WRITE (67,*)'    Combined stress=', SXCOMB(ICAS),SYCOMB(ICAS)
	    ENDIF
	   ENDIF

         IF(TPLMIN.LE.VAR(2)) THEN
           IHH   = IH
           TPLMIN= VAR(2)
           POSIT = 'Sx and Sy (X=L/2)'
	     IC    = ICAS
	     SSX   = SX(ICAS)
	     SSY   = SY(ICAS)
	     TXY   = 0.0
           SSXCOMB   = SXCOMB(ICAS) !avril2003
	     SSYCOMB   = SYCOMB(ICAS) !avril2003
	     TXYCOMB   = 0.0          !avril2003
	     SIGG = SIGMAG            !avril2003
	     PLAT  = XP(ICAS)
	     IND1  = ILOOP ! soit 1 ou 2
         ENDIF
  24  CONTINUE

C     Calcul de l'épaisseur minimale avec Tau (Sig x =0 et Sig y =0)
c       WRITE(67,*) 'ILOOP=1  TAU'
C       WRITE(67,*) '================='
        VAR(1) = TPL
        VAR(2) = 0.8*TPL

      CALL PLSOLV(1,VAR,XLEN,YLEN,GV1,GV2,XP(ICAS),
     *               ZERO,ZERO,TAU(ICAS),E,ETA,SYLDP,GAMMAS,ISTOP,IH,
     *               ZERO,ZERO,TAUCOMB(ICAS),SIGMAG)                   !avril2003

        IF(ISTOP.EQ.1) THEN
           WRITE (67,1010) XP(ICAS),ZERO,ZERO,TAU(ICAS)
           RETURN
        ENDIF

        IF(JPRINT.GE.1) THEN
         SR11=2.0/(1.0+GV1(1)) - 1.0
         SR12=2.0/(1.0+GV1(2)) - 1.0
         SR21=2.0/(1.0+GV2(1)) - 1.0
         SR22=2.0/(1.0+GV2(2)) - 1.0
         WRITE (67,1010) XP(ICAS),ZERO,ZERO,TAU(ICAS),VAR(2),TYP(IH),
     *                  SR11,SR12,SR21,SR22
	  ENDIF

        IF(TPLMIN.LE.VAR(2)) THEN
           IHH   = IH
           TPLMIN= VAR(2)
           POSIT = 'Shear Stress (X=0)'
	     IC    = ICAS
	     SSX   = 0.0
	     SSY   = 0.0
	     TXY   = TAU(ICAS)
	     SSXCOMB   = 0.0           !avril2003
	     SSYCOMB   = 0.0           !avril2003
	     TXYCOMB   = TAUCOMB(ICAS) !avril2003
	     SIGG = SIGMAG             !avril2003
	     PLAT  = XP(ICAS)
	  ENDIF

  26  CONTINUE  ! boucle sur ICAS


C   RESULTATS FINALS (Dmin) pour cas de base (CAS 1):
C   -------------------------------------------------
      PIC = (1.0*IC-1.)/4.  ! IC=1 -> PIC=0.0  et IC=5 --> PIC= 1.0
	IF(IC.EQ.2) PIC=0.33
	IF(IC.EQ.4) PIC=0.66

C  Calcul des sécurités (SR1 et SR2) correspondant au DELTA (épaiss) en cours
c  ---------------------------------------------------------------------------
      LCHECK=0    ! 1 si dernière itération
	ICYC=1      ! soit itération initiale
	ISTOP=0
c   Subr PLTBEN
c      si Dmin correspond à ILOOP=1 --> IND1=1 
c      si Dmin correspond à ILOOP=2 --> IND1=2 
      
	GLOAD=PLAT*9810.
      IG=0  
      IF(DABS(PLAT).LE.(0.01)) IG=1             ! ***********
       ! S'il n'y a pas de pression latérale la subr.PLTBEN ne peux pas être utiliséé. 
	 ! Pourquoi ?
	 ! PLTBEN prend en compte les contraintes membranaires et les contraintes
	 ! flexionnelles engendrées par la pression latérale.
	 ! L'epaisseur de la plaque influence le niveau des contraintes flexionnelles
	 ! mais il est supposé que cette épaisseur n'influence pas (en réalité très peu)
	 ! les contraintes membranaires qui résultent du comportement d'ensemble.
	 ! En conséquence, en l'absence de pression latérale, l'épaisseur n'a pas d'effet sur les
	 ! contraintes et il est par conséquent pas possible de rechercher l'épaisseur minimale.
	 ! Dans ce cas on donne à SR1=0 (valeur par défaut de SR1= sécurité vis à vis de la plastification)
	 ! - Si les contraintes membranaires sont trop fortes, c'est via un remforcement global 
	 !   de la structure que l'on fera chuter les contraintes. Cela est effectué via d'autres restrictions.
	 ! - Si les contraintes membranaires sont trop faibles, l'épaisseur minimale sera définie via
	 !   la Subr. BUCKLE.
	 
      IF (IG.EQ.0) THEN  !(IG=0 : avec pression laterale)
        IF (YLEN .LT. XLEN) THEN
           CALL PLTBEN(IND1,DELTA,YLEN,XLEN,E,ETA,SYLDP,GAMMAS,
     *           SSYCOMB,SSXCOMB,TXYCOMB,GLOAD,IND1,GVAL1,ICYC,LCHECK,  !avril2003
     *           ISTOP,SIGMAG)                                          !avril2003
        ELSE
           CALL PLTBEN(3-IND1,DELTA,XLEN,YLEN,E,ETA,SYLDP,GAMMAS,
     *           SSXCOMB,SSYCOMB,TXYCOMB,GLOAD,IND1,GVAL1,ICYC,LCHECK,  !avril2003
     *           ISTOP,SIGMAG)                                          !avril2003
        END IF
        IF (ISTOP.EQ.1) THEN
          WRITE(67,'(A,A)')'SR1: Calculation of ratio (Applied Stress/',
     *               'Yield Stress) failed (must be checked manually).'
	  ELSE
	    SR1=2.0/(1.0+GVAL1) - 1.0
	  ENDIF
      ELSE  !(IG=1 : sans pression laterale)
c       WRITE(67,'(A,A)')'The ratio (Applied Stress/Yield Stress)',
c    *             ' is not evaluated (SR1=0).'
	  SR1=0.0
      END IF


c  Subr BUCKLE
      IGG=0
	ISTOP=0
      IF (YLEN .LT. XLEN) THEN
        CALL BUCKLE(DELTA,XLEN,YLEN,E,ETA,SYLDP,GAMMAS,
     *                 SSX,SSY,TXY,GVAL2,ICYC,LCHECK,ISTOP,IGG,
     *                 SR2,SXcr,SYcr,TAUcr)
      ELSE
        CALL BUCKLE(DELTA,YLEN,XLEN,E,ETA,SYLDP,GAMMAS,
     *                 SSY,SSX,TXY,GVAL2,ICYC,LCHECK,ISTOP,IGG,
     *                 SR2,SYcr,SXcr,TAUcr)				               !mars2004
      ENDIF

      IF ((ISTOP.EQ.1).OR.(SR2.GT.1.3)) THEN
       WRITE(67,'(A,A)')'*** SR2 too large or Calculation of buckling ',
     *            'critical stresses failed (Please check!!).'
	ENDIF

c Minimum plate thickness assessment : Yielding and buckling
c--------------------------------------------------------------
c      SR1 = Ratio applied stress/yield Stress (?<? 1.0)
c      SR2 = Global Interaction Ratio: Applied Stress/Critical Stress (?<? 1.0)
c
cPanel <-- Panel Dimensions -->  Minimum Thickness <- Collapse Scenario & applied stress --->  YIELDING   BUCKLING  Critical Stresses
c      L(long) B(trans) d(thick)  d(min)  d/d(min)   Mode  Section   Sx    Sy     Tau    Plat  (S/Sy<1)  (S/Scr<1) Sx(cr) Sy(cr) T(cr)
c         (m)     (m)    (mm)      (mm)                      Y/Yo   N/mm2  N/mm2  N/mm2  (m)   Von-Mises Interact  N/mm2  N/mm2  N/mm2
c                                                                                                SR1     Form(SR2)
c  1    0.371   0.873     8.00     7.82    1.02   Yielding   1.00    47.1   51.7    0.0   0.00  1.000      0.000   337.9  117.8  513.0
c1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890

	IF (TYP(IHH).EQ.'Buckling  ') THEN                           !avril2003
	  WRITE (67,1023) NEL,XLEN,YLEN,1000.*DELTA,1000.*TPLMIN,
     *    DELTA/TPLMIN,TYP(IHH),PIC,SSX/1.0E06,SSY/1.0E06,TXY/1.0E06,
     *    PLAT,SR1,SR2,SXcr/1.0E06,SYcr/1.0E06,TAUcr/1.0E06
      ELSE                                                         !avril2003
	  WRITE (67,1023) NEL,XLEN,YLEN,1000.*DELTA,1000.*TPLMIN,    !avril2003
     *    DELTA/TPLMIN,TYP(IHH),PIC,SSXCOMB/1.0E06,SSYCOMB/1.0E06, !avril2003
     *    TXYCOMB/1.0E06,PLAT,SR1,SR2,SXcr/1.0E06,SYcr/1.0E06,     !avril2003
     *    TAUcr/1.0E06                                             !avril2003
	ENDIF                                                        !avril2003

      IF((ITERA.GE.0).AND.(IOPTI.GE.1))  THEN		!13.12.05
	  IF (TYP(IHH).EQ.'Buckling  ') THEN                         !avril2003
	    WRITE(666,1023) NEL,XLEN,YLEN,1000.*DELTA,1000.*TPLMIN,
     *      DELTA/TPLMIN,TYP(IHH),PIC,SSX/1.0E06,SSY/1.0E06,TXY/1.0E06,
     *      PLAT,SR1,SR2,SXcr/1.0E06,SYcr/1.0E06,TAUcr/1.0E06
	  ELSE                                                       !avril2003
          WRITE (666,1023) NEL,XLEN,YLEN,1000.*DELTA,1000.*TPLMIN, !avril2003
     *    DELTA/TPLMIN,TYP(IHH),PIC,SSXCOMB/1.0E06,SSYCOMB/1.0E06, !avril2003
     *    TXYCOMB/1.0E06,PLAT,SR1,SR2,SXcr/1.0E06,SYcr/1.0E06,     !avril2003
     *    TAUcr/1.0E06                                             !avril2003
	  ENDIF                                                      !avril2003
	ENDIF

 1023 FORMAT(I3,3x,F6.3,2x,F6.3,3x,F6.2,3x,F6.2,2x,F6.2,3x,A10,F5.2,
     *          1x,3(F7.1),F7.2,F7.3,4x,F7.3,1x,3(F7.1))
c -------------------------------------------------

      IF(IOPTION.EQ.0) RETURN  ! si calcul des sensibilités pas requis pour ce panneau
c      IGG=0  
c	IG=0  
C***********************************************************************
C 3.2 CAS 2 : 0.99*EPSA et ENTR  (pour calcul par différence finie)
C***********************************************************************
      ICAS=IC
      TPLA=0.

      IF((IA.EQ.1).OR.(IAA.EQ.0)) THEN
        TPLA=0.
      ELSE

        XLEN2=0.99*XLEN
        SHORT = MIN(XLEN2,YLEN)
        TPL = SHORT * SQRT(0.5*XP(ICAS)*9810/SYLDP) +
     *        (SHORT/2.) * SQRT(SYLDP/E)

C Calcul de l'épaisseur minimale pour Sig x, Sig y  et Tau=0.
      DO 124 ILOOP = 1,2
         VAR(1) = TPL
         VAR(2) = 0.8*TPL

         CALL PLSOLV(ILOOP,VAR,XLEN2,YLEN,GV1,GV2,XP(ICAS),
     *             SX(ICAS),SY(ICAS),ZERO,E,ETA,SYLDP,GAMMAS,ISTOP,IH,
     *             SXCOMB(ICAS),SYCOMB(ICAS),ZERO,SIGMAG) !avril2003

         IF(ISTOP.EQ.1) THEN
            WRITE (666,1010) XP(ICAS),SX(ICAS),SY(ICAS),ZERO
            RETURN
         ENDIF

         TPLA = MAX(TPLA,VAR(2))
 124  CONTINUE
       IF(JPRINT.GE.1)
     *    WRITE (666,1010) XP(ICAS),SX(ICAS),SY(ICAS),ZERO,TPLA

C Calcul de l'épaisseur minimale avec Tau (Sig x =0 et Sig y =0)
       VAR(1) = TPL
       VAR(2) = 0.8*TPL
       CALL PLSOLV(1,VAR,XLEN2,YLEN,GV1,GV2,XP(ICAS),
     *             ZERO,ZERO,TAU(ICAS),E,ETA,SYLDP,GAMMAS,ISTOP,IH,
     *             ZERO,ZERO,TAUCOMB(ICAS),SIGMAG) !avril2003

       IF(ISTOP.EQ.1) THEN
          WRITE (666,1010) XP(ICAS),ZERO,ZERO,TAU(ICAS)
          RETURN
       ENDIF
       IF(JPRINT.GE.1)
     *    WRITE (666,1010) XP(ICAS),ZERO,ZERO,TAU(ICAS),VAR(2)

C Calcul de l'épaisseur minimale absolu pour Sig x, Sig y et Tau non nuls.
       TPLA = MAX(VAR(2),TPLA)
       TPLA=-100. *(TPLA-TPLMIN)/EPSA
      ENDIF

C   RESULTATS :
C   -----------------
      IF(JPRINT.GE.1) WRITE (666,1025) TPLA

C***********************************************************************
C 3.3 CAS 3 :  EPSA et 0.99*ENTR (pour calcul par différence finie)
C                 D=ENTR
C              C'est la dérivée par rapport à EPSR que l'on calcule
C              d(C)/d(D)   = -100. *(TPLR-TPLMIN)/D
C              d(C)/d(EPSR)= d(C)/d(D)  *  (D/EPSR)**2
C                          = -100. *(TPLR-TPLMIN) * (D/EPSR)/EPSR
C***********************************************************************
      TPLR=0.
      IF((IR.EQ.1).OR.(IRR.EQ.0)) THEN
        TPLR=0.
      ELSE

        YLEN2=0.99*YLEN
        SHORT = MIN(XLEN,YLEN2)
        TPL = SHORT * SQRT(0.5*XP(ICAS)*9810/SYLDP) +
     *        (SHORT/2.) * SQRT(SYLDP/E)

C Calcul de l'épaisseur minimale pour Sig x, Sig y  et Tau=0.
      DO 224 ILOOP = 1,2
         VAR(1) = TPL
         VAR(2) = 0.8*TPL

         CALL PLSOLV(ILOOP,VAR,XLEN,YLEN2,GV1,GV2,XP(ICAS),
     *            SX(ICAS),SY(ICAS),ZERO,E,ETA,SYLDP,GAMMAS,ISTOP,IH,
     *            SXCOMB(ICAS),SYCOMB(ICAS),ZERO,SIGMAG) !avril2003
         IF(ISTOP.EQ.1) THEN
            WRITE (666,1010) XP(ICAS),SX(ICAS),SY(ICAS),ZERO
            RETURN
         ENDIF

        TPLR = MAX(VAR(2),TPLR)
 224  CONTINUE

        IF(JPRINT.GE.1)
     *     WRITE (666,1010) XP(ICAS),SX(ICAS),SY(ICAS),ZERO,TPLR

C Calcul de l'épaisseur minimale avec Tau (Sig x =0 et Sig y =0)
        VAR(1) = TPL
        VAR(2) = 0.8*TPL

        CALL PLSOLV(1,VAR,XLEN,YLEN2,GV1,GV2,XP(ICAS),
     *             ZERO,ZERO,TAU(ICAS),E,ETA,SYLDP,GAMMAS,ISTOP,IH,
     *             ZERO,ZERO,TAUCOMB(ICAS),SIGMAG) !avril2003
        IF(ISTOP.EQ.1) THEN
            WRITE (666,1010) XP(ICAS),ZERO,ZERO,TAU(ICAS)
            RETURN
        ENDIF

        IF(JPRINT.GE.1)
     *     WRITE (666,1010) XP(ICAS),ZERO,ZERO,TAU(ICAS),VAR(2)

C Calcul de l'épaisseur minimale absolu pour Sig x, Sig y et Tau non nuls.
        TPLR = MAX(VAR(2),TPLR)

C       d(C)/d(EPSR)= d(C)/d(D)  *  (D/EPSR)**2
C                   = -100. *(TPLR-TPLMIN) * (D/EPSR)/EPSR
        TPLR=-100. *(TPLR-TPLMIN) * (ENTR/EPSR)/EPSR

      ENDIF

C   RESULTATS :
C   -----------------
      IF(JPRINT.GE.1) WRITE (666,1026) TPLR

C***********************************************************************
C 3.4 CAS 4 : 0.99*Delta (pour calcul par différence finie)
C                     avec SX/0.99,  SY/0.99 et  tau/0.99
C***********************************************************************
      TPLD=0.
      IF(IDD.EQ.0) THEN
        TPLD=0.
      ELSE
c       Si DELTA2=0.99*DELTA alors les contraintes (Sx,SY,Tau)
C                                  doivent être divisée par 0.99
        SX2=SX(ICAS)/0.99
        SY2=SY(ICAS)/0.99
        TAU2=TAU(ICAS)/0.99
        SX2COMB=SXCOMB(ICAS)/0.99 !avril2003
        SY2COMB=SYCOMB(ICAS)/0.99 !avril2003
        TAU2COMB=TAUCOMB(ICAS)/0.99 !avril2003
        SHORT = MIN(XLEN,YLEN)
        TPL = SHORT * SQRT(0.5*XP(ICAS)*9810/SYLDP) +
     *        (SHORT/2.) * SQRT(SYLDP/E)

C Calcul de l'épaisseur minimale pour Sig x, Sig y  et Tau=0.
      DO 324 ILOOP = 1,2
         VAR(1) = TPL
         VAR(2) = 0.8*TPL

         CALL PLSOLV(ILOOP,VAR,XLEN,YLEN,GV1,GV2,XP(ICAS),
     *            SX2,SY2,ZERO,E,ETA,SYLDP,GAMMAS,ISTOP,IH,
     *            SX2COMB,SY2COMB,ZERO,SIGMAG) !avril2003
         IF(ISTOP.EQ.1) THEN
            WRITE (666,1010) XP(ICAS),SX(ICAS),SY(ICAS),ZERO
            RETURN
         ENDIF

        TPLD = MAX(TPLD,VAR(2))
 324  CONTINUE

        IF(JPRINT.GE.1)
     *     WRITE (666,1010) XP(ICAS),SX(ICAS),SY(ICAS),ZERO,TPLD

C Calcul de l'épaisseur minimale avec Tau (Sig x =0 et Sig y =0)
        VAR(1) = TPL
        VAR(2) = 0.8*TPL

        CALL PLSOLV(1,VAR,XLEN,YLEN,GV1,GV2,XP(ICAS),
     *             ZERO,ZERO,TAU2,E,ETA,SYLDP,GAMMAS,ISTOP,IH,
     *             ZERO,ZERO,TAU2COMB,SIGMAG) !avril2003
        IF(ISTOP.EQ.1) THEN
            WRITE (666,1010) XP(ICAS),ZERO,ZERO,TAU(ICAS)
            RETURN
        ENDIF

        IF(JPRINT.GE.1)
     *     WRITE (666,1010) XP(ICAS),ZERO,ZERO,TAU(ICAS),VAR(2)

C Calcul de l'épaisseur minimale absolu pour Sig x, Sig y et Tau non nuls.
        TPLD = MAX(VAR(2),TPLD)
        TPLD=-100.*(TPLD-TPLMIN)/DELTA

      ENDIF

C   RESULTATS :
C   -----------------
      IF(JPRINT.GE.1) WRITE (666,1027) TPLD

C   IMPRESSIONS FINALES : avec TPLD= dérivées de (dmin-d) par rapport à
C   **********************
      TPLD=TPLD-1.
      IF(JPRINT.EQ.1)
     *  WRITE(666,198)NEL,TPLMIN,TPLA,TPLR,TPLD,TYP(IHH),POSIT,IC
      
C23456789012345678901234567890123456789012345678901234567890123456789012
C     LES FORMATS
CCC   -----------
  198 FORMAT('NEL=',I2,': d min =',E13.6,' d TPL/EPSA =',E11.4,
     *                  ' d TPL/EPSR =',E11.4,' d TPL/Delta=',E11.4,
     *                  ' (',A10,' et ',A10,I2,')')
  999 FORMAT(10X,'INPUT DATA AND RESULTS (en m et N)'/10X,23(1H*)/)
 1003 FORMAT(2X,'SIZE(m) (X,Y)',5X,'MODULUS(N/m2) YIELD STRESS(N/m2)',
     *         '  Max. STRESS(N/m2)')
 1007 FORMAT(F8.3,' x',F8.3,3X,G9.3,5X,G9.3,5X,G9.3/)
 1010 FORMAT(1X,5(E14.7,2X),A10,4(1x,F6.3))
 1011 FORMAT(2X'PRESSURE(m)',7X,'XSTRESS',7X,'YSTRESS',9X,'TAU(N/m2)'
     *  ,7X,'THICKNESS(m)',13x,'SR1 yield(1et2) SR2 Buckl(1et2)')
 1025 FORMAT('Dérivée selon EPSA   =',E14.7)
 1026 FORMAT('Dérivée selon EPSR   =',E14.7)
 1027 FORMAT('Dérivée selon Delta  =',E14.7)
      RETURN
      END


CCC ********************************************************************
CCC ********************************************************************
      SUBROUTINE PLSOLV(ILOOP,VAR,XLEN,YLEN,GVAL1,GVAL2,XP,
     *                  SIGBX,SIGBY,TAU,E,ETA,SYLDP,GAMMAS,ISTOP,IH,
     *                  SIGBXCOMB,SIGBYCOMB,TAUCOMB,SIGMAG)          !avril2003
CCC ********************************************************************
CCC NAME: PLSOLV                             LAST REVISION: 23 APR 1992
CCC													(Dec. 99 by Rigo Ph.)
CCC                                                   (Mai 2003, F. Bair )
CCC PROGRAM SEGMENT: NOT IN MAESTRO
CCC
CCC PURPOSE: USES NEWTON-RAPHSON TO GET THE MINIMUM PLATE THICKNESS.
CCC
CCC COMMON BLOCKS: NIL
CCC
CCC CALLED BY: MAIN
CCC
CCC CALLS TO: PLEVAL
CCC
CCC EXTERNAL FILES: NIL
CCC ********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION VAR(2),GVAL1(2),GVAL2(2)

      COMMON/PY/  PI

      GLOAD=XP*9810.0   !  XP en m eau et GLOAD en N/m2

	IH=3  ! Actibe mode = undefined
C
C  Vérification :  GLOAD>0 ?
C  --------------------------
C     IG =0 avec une pression latérale sur la plaque
C     IG =1 sans pression latérale sur la plaque (donc pas de flexion),
c           dans ce cas la subr PLTBEN n'est pas utilisable (voir explications ci-avant)
C     IGG=0 avec des charges ds le plan (Sx, Sx, Tau)
c     IGG=1 si les charges ds le plan sont nulles(Subr BUCKLE pas utilisable)

      IG=0  
      IF(DABS(XP).LE.(0.01)) THEN      !*********
c        Write(*, *) 'La pression latérale est nulle! (subr. PLSOLV)'
c        Write(67,*)'La pression latérale est nulle! (subr. PLSOLV)'
         IG=1
         IF(ILOOP.EQ.2) THEN
            VAR(2)=0.0001
            RETURN
         ENDIF
      ENDIF

C  ------------------
c    GVAL1(i) valeurs obtenues pour la subr PLTBEN
c    GVAL2(i) valeurs obtenues pour la subr BUCKLE
c     i = 1  pour VAR(1)
c     i = 2  pour VAR(2)
C  ------------------
      LCHECK = 0 ! LCHECK=1 lorsque la convergence est atteinte et on fait une dernière itération.
      TOL = 1.E-4
      ICYCM=30 ! Nbre maxi d'itérations

	ITRY=1 ! Compteur de nbre d'essais (avec différentes valeurs initiales de VAR1 et VAR2)
             ! ITRY = 1 correspond à VAR1 = TPL défini ci-avant et VAR2=0.8*TPL
 200  CONTINUE
c
c  ----- Initialisation avec VAR(1)
C      WRITE (67,*)
C      WRITE (67,*) 'Avt PLEVAL1 (itération prélim)'
C      WRITE (67,*) '******************************'
C      WRITE (67,*) 'ILOOP,VAR(1),XLEN,YLEN,GLOAD'
C      WRITE (67,*)  ILOOP,VAR(1),XLEN,YLEN,GLOAD
C      WRITE (67,*) 'SIGBX,SIGBY,TAU'
C      WRITE (67,*)  SIGBX,SIGBY,TAU
C      WRITE (67,*) 'LCHECK,ISTOP,IG,IGG=',LCHECK,ISTOP,IG,IGG
c      PAUSE'P1'

      CALL PLEVAL(ILOOP,VAR(1),XLEN,YLEN,GLOAD,GVAL1(1),GVAL2(1),0,
     *           SIGBX,SIGBY,TAU,E,SYLDP,ETA,GAMMAS,LCHECK,ISTOP,IG,IGG,
     *           SIGBXCOMB,SIGBYCOMB,TAUCOMB,SIGMAG)                  !avril2003

C        WRITE (67,*) 'après PLEVAL 1'
C        WRITE (67,*) 'ISTOP=',ISTOP
C        WRITE (67,*) 'VAR(1)=',VAR(1)
C        WRITE (67,*) 'GVAL1(1),GVAL2(1)=',GVAL1(1),GVAL2(1)
C        WRITE (67,*)
c       PAUSE'P2'

      IF(ISTOP.EQ.1) RETURN
c
c ------------- DEBUT DES ITERATIONS --------
      DO 32 ICYC = 1,ICYCM

C        WRITE (67,*)
C        WRITE (67,*) 'avt PLEVAL 2  Itération nø =',ICYC
C        WRITE (67,*) '-----------------------------------'
C        WRITE (67,*) 'LCHECK,ISTOP,IG=',LCHECK,ISTOP,IG

   18 CALL PLEVAL(ILOOP,VAR(2),XLEN,YLEN,GLOAD,GVAL1(2),GVAL2(2),ICYC,
     *           SIGBX,SIGBY,TAU,E,SYLDP,ETA,GAMMAS,LCHECK,ISTOP,IG,IGG,
     *           SIGBXCOMB,SIGBYCOMB,TAUCOMB,SIGMAG) !avril2003

        IF(ISTOP.EQ.1) RETURN

C        WRITE (67,*)
C        WRITE (67,*) 'après PLEVAL 2'
C        WRITE (67,*) 'GVAL1(2)=',GVAL1(2),' GVAL2(2)=',GVAL2(2)
C        WRITE (67,*) 'LCHECK,ISTOP,IG,IGG=',LCHECK,ISTOP,IG,IGG
c        PAUSE'P3'

        IF((IGG.EQ.1).AND.(IG.EQ.1)) THEN ! si aucune sollicition n'existe
          VAR(2)=0.0001
          RETURN
        ENDIF

      IF (LCHECK.EQ.1) THEN
C       IF CONVERGED, GO BACK AND DO ONE LAST CYCLE TO CHECK THE VALIDITY LIM
        IF(ITRY.GT.1)
     *         WRITE(67,*)'ITRY=',ITRY,' Nbre itération Hughes =',ICYC
        RETURN
      ELSE ! TESTS DE CONVERGENCE
        IF (IG.EQ.1) THEN ! si Plat = 0 (seul la Subr Buckle est utilisée
          IF (ABS(GVAL2(2)).LT.TOL) THEN
            LCHECK = 1
            GO TO 18 ! vers une dernière itération (LCHECK=1)
          ENDIF
        ELSE IF ((IGG.EQ.1).OR.(ILOOP.EQ.2)) THEN ! si Sx=0 (seul la Subr PLTBEN est utilisée
          IF (ABS(GVAL1(2)).LT.TOL) THEN
            LCHECK = 1
            GO TO 18
          ENDIF
        ELSE
          IF ((ABS(GVAL1(2)).LT.TOL).AND.(GVAL2(2).GE.0.)) THEN
            LCHECK = 1
            GO TO 18
C           GVAL1=0 : car sig = Sig(von-mises) c.a.d. que la limitation à la plastification (PLTBEN) est active
C                                       et pas la limitation au voilement (BUCKLE)
          END IF
          IF ((ABS(GVAL2(2)).LT.TOL).AND.(GVAL1(2).GE.0.)) THEN
            LCHECK = 1
C           GVAL2=0 : car sig = Sig(bukling) c.a.d. que la limitation au voilement (BUCKLE) est active
C                         et pas la limitation à la plastification (PLTBEN)
            GO TO 18
          END IF
        END IF
      END IF

c      PAUSE'P4'

c     Calcul de la variation de l'épaisseur DLTVAR en vue de la prochaine itération
      IF (IG.EQ.0) THEN
        DENOM = GVAL1(2) - GVAL1(1)
c       WRITE (67,*) 'DENOM1=',DENOM

        IF (DENOM.EQ.0.) THEN
           WRITE (67,1001)
           WRITE (*, 1001)
           WRITE (29, 1001)															!bug
c          PAUSE'OK?'
           RETURN
        END IF
        DLTVAR = -GVAL1(2) * (VAR(2) - VAR(1)) / DENOM
   20   IF(((VAR(2)+DLTVAR).LE.0.).OR.(ABS(DLTVAR/VAR(1)).GT.0.3))THEN
          DLTVAR = 0.5*DLTVAR
          GO TO 20
        END IF
      ELSE
        DLTVAR=-999.
      END IF

c    Calcul de la variation de l'épaisseur DLTVAR2 en vue de la prochaine itération
      IF ((ILOOP.EQ.1).AND.(IGG.EQ.0)) THEN
        DENOM = GVAL2(2) - GVAL2(1)
c       WRITE (67,*) 'DENOM2=',DENOM
c       PAUSE'P5'

        IF (DENOM.EQ.0.) THEN
           WRITE (67,1001)
           WRITE (*, 1001)
	     WRITE (29, 1001)															!bug
c          PAUSE'OK?'
           RETURN
        END IF
        IF((ICYC.GE.15).AND.(GVAL2(2).GE.(0.95)).AND.(IG.EQ.0)) THEN
          DLTVAR2=-999.
        ELSE
        DLTVAR2= -GVAL2(2) * (VAR(2) - VAR(1)) / DENOM
   21   IF(((VAR(2)+DLTVAR2).LE.0.).OR.(ABS(DLTVAR2/VAR(1)).GT.0.3))THEN
          DLTVAR2= 0.5*DLTVAR2
          GO TO 21
        END IF
        END IF
      ELSE
        DLTVAR2=-999.
      END IF

C     WRITE (67,*) 'GVAL1(1)=',GVAL1(1),'  GVAL2(1)=',GVAL2(1)
C     WRITE (67,*) 'GVAL1(2)=',GVAL1(2),'  GVAL2(2)=',GVAL2(2)
C     WRITE (67,*) 'VAR1=',VAR(1),'  VAR2=',VAR(2)
C     WRITE (67,*) 'DLTVAR=',DLTVAR,'  DLTVAR2=',DLTVAR2
c     PAUSE'P6'

c    Calcul de la nouvelle épaisseur VAR(2) en vue de la prochaine itération
      IH=1  !  via PLTBEN
      IF(DLTVAR.LE.DLTVAR2) THEN
        IH=2  ! via BUCKLE
        DLTVAR=DLTVAR2
      ENDIF
      VAR(1) = VAR(2)
      VAR(2) = VAR(2) + DLTVAR
	IF(VAR(2).LE.0) VAR(2)= 0.001
      GVAL1(1) = GVAL1(2)
      GVAL2(1) = GVAL2(2)

C     WRITE (67,*) 'DLTVAR=',DLTVAR,'  New Epaiss =',VAR(2)
C     WRITE (67,*) '-------------------------------------------'

   32 CONTINUE  ! END of Iteration LOOP

      ITRY=ITRY + 1  ! Compteur d'essais

	IF(ITRY.EQ.2) THEN
	  VAR(1)=0.001
	  VAR(2)=0.0015
	  GOTO 200
	ENDIF

	IF(ITRY.EQ.3) THEN
	  VAR(1)=0.001
	  VAR(2)=0.0011
	  ICYCM=100
	  GOTO 200
	ENDIF

	IF(ITRY.EQ.4) THEN
	  VAR(1)=VAR(1)*2.
	  VAR(2)=VAR(1)*1.05
	  GOTO 200
	ENDIF

	! Si ITRY > 4 : Stop PLSOLV FAILED !!!
      WRITE (*,  1000)
      WRITE (67,1000)
	WRITE (29, 1000)															!bug
      
	RETURN

 1000 FORMAT(/,' *******************ERROR***********************'/
     1 ,' SOLUTION ROUTINE PLSOLV FAILED TO CONVERGE IN 30 CYCLES'/
     2 ,' PRESS "RETURN" TO CONTINUE...')
 1001 FORMAT(/,' ERROR'/ '*********'/
     1 ,' IN ROUTINE PLSOLV THE SOLUTION FUNCTION GVAL1'
     2 ,' HAS BECOME STATIONERY (ZERO DERIVATIVE)'/
     3 ,' PRESS "RETURN" TO CONTINUE...')
      END




CCC ********************************************************************
CCC ********************************************************************
      SUBROUTINE PLEVAL(ILOOP,TPL,XLEN,YLEN,GLOAD,GVAL1,GVAL2,ICYC,
     *        SIGBX,SIGBY,TAU,E,SYLDP,ETA,GAMMAS,LCHECK,ISTOP,IG,IGG,
     *        SIGBXCOMB,SIGBYCOMB,TAUCOMB,SIGMAG)                   !avril2003
CCC ********************************************************************
CCC NAME: PLEVAL                             LAST REVISION: 1997 (Subr BUCKLE - Rigo)
C                                                           2003 mai (F. Bair)
CCC
CCC PROGRAM SEGMENT: NOT IN MAESTRO
CCC
CCC PURPOSE: MANAGES CALLS TO APPROPRIATE PLATE RESPONSES: yielding and buckling
CCC
CCC CALLED BY: PLSOLV
CCC
CCC CALLS TO: PLTBEN, BUCKLE (modified by RIGO Ph.) 
CCC
CCC EXTERNAL FILES: NIL
CCC ********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/PY/  PI

C     FOR STATIC PRESSURE LOAD, CALL PLTBEN TO CALCULATE THE PLATE BENDI
C     ILOOP  = 1: BENDING ACROSS THE SHORT SPAN
C            = 2: BENDING ACROSS THE LONG  SPAN
C     IG     = 0: with lateral load
C            = 1: without lateral load
C     IGG    = 0: with in plane loads
C            = 1: without in plane loads


c     WRITE (67,*) 'Avt PLTBEN'
c     WRITE (67,*) 'ILOOP,TPL,YLEN,XLEN,E,ETA'
c     WRITE (67,*)  ILOOP,TPL,YLEN,XLEN,E,ETA
c     WRITE (67,*) 'SYLDP,SIGBY,SIGBX,TAU,GLOAD,ILOOP'
c     WRITE (67,*)  SYLDP,SIGBY,SIGBX,TAU,GLOAD,ILOOP
c     WRITE (*,*)  'IG=',IG
c     WRITE (*,*)  'ICYC=',ICYC

      IF (IG.EQ.0) THEN  !(IG=0 : avec pression lat)
c        WRITE (67,*) 'Subr PLTBEN'
         IF (YLEN .LT. XLEN) THEN
            CALL PLTBEN(ILOOP,TPL,YLEN,XLEN,E,ETA,SYLDP,GAMMAS,
     *           SIGBYCOMB,SIGBXCOMB,TAUCOMB,GLOAD,ILOOP,GVAL1,ICYC,
     *           LCHECK,ISTOP,SIGMAG)                                !avril2003
         ELSE
            CALL PLTBEN(3-ILOOP,TPL,XLEN,YLEN,E,ETA,SYLDP,GAMMAS,
     *           SIGBXCOMB,SIGBYCOMB,TAUCOMB,GLOAD,ILOOP,GVAL1,ICYC,
     *           LCHECK,ISTOP,SIGMAG)                                !avril2003
         END IF
      ELSE
         GVAL1=0.  ! IG=1 , PLTBEN pas utilisé
      END IF

      IF (ILOOP.EQ.1) THEN   !Subr BUCKLE'
         IGG=0  ! càd avec contraintes membranaires (SX, ..)
         IF (YLEN .LT. XLEN) THEN
           CALL BUCKLE(TPL,XLEN,YLEN,E,ETA,SYLDP,GAMMAS,
     *                 SIGBX,SIGBY,TAU,GVAL2,ICYC,LCHECK,ISTOP,IGG,
     *                 a1,a2,a3,a4)
         ELSE
           CALL BUCKLE(TPL,YLEN,XLEN,E,ETA,SYLDP,GAMMAS,
     *                 SIGBY,SIGBX,TAU,GVAL2,ICYC,LCHECK,ISTOP,IGG,
     *                 a1,a2,a3,a4)
         ENDIF
      ELSE
         GVAL2=0.   ! IGG=1 , BUCKLE pas utilisé car Sx,Sy, ..; = 0
      END IF

c     WRITE (*,*) 'Sortie leval'

      RETURN
      END




CCC ********************************************************************
CCC ********************************************************************
      SUBROUTINE PLTBEN(ILOOP2,TPL,SHORT,RLONG,E,ETA,SYLDP,GAMMAS,
     *      SGSHRT,SGLONG,TAU,PRSABS,ILOOP,GVAL1,ICYC,LCHECK,ISTOP,  !avril2003
     *      SIGMAG)                                                  !avril2003
CCC ********************************************************************
CCC NAME: PLTBEN                             LAST REVISION: 23 APR 1992
CCC
CCC PROGRAM SEGMENT: PART6
CCC
CCC PURPOSE: EVALUATE THE ADEQUACY PARAMETER FOR LOCAL PLATE BENDING
CCC          IN THE SHORT OR LONG DIRECTION (ILOOP2 = 1 OR 2) TO CAUSE
CCC          SURFACE YIELD OR, IF RELEVANT, A SPECIFIED AMOUNT OF
CCC          PERMANENT SET WPLIM.  THESE ARE USED IN THE TWO
CCC          SERVICEABILITY CONSTRAINTS FOR PLATE BENDING -
CCC          PSPBT,PSPBL (PANEL SERVICEABILITY, PLATE BENDING, TRANSVERSAL
CCC          OR LONGITUDINAL).
CCC          - FOR BENDING IN THE SHORT DIRECTION CALCULATE THE CLAMPED EDGE
CCC            STRESS DUE TO PLATE BENDING.
CCC          - IN THE SECOND CASE ALLOW FOR MAGNIFICATION DUE TO COMPRESSION
CCC            THE SHORT DIRECTION (EQ. 12.5.11 OF REFERENCE 1).
CCC          - IF NO PERMANENT SET, CALCULATE THE VON MISES EQUIVALENT STRESS
CCC            THE COMBINATION OF MEMBRANE (SIG1, SIG2 AND TAU) AND BENDING
CCC            (+ AND -, CORRESP. TO UPPER AND LOWER PLATE SURFACES).

CCC Rem : Cette subroutine n'intègre pas de vérification au voilement
CCC       du panneau étudié. Il s'agit d'un simple analyse statique élastique
CCC       Seul l'effet de magnification des contraintes est considéré mais
CCC       indépendamment d'un risque de voilement.

CCC
CCC ARGUMENTS: ILOOP2 : 1 OR 2 FOR BENDING ACROSS SHORT OR LONG DIRECTION
CCC            WPLIM  : ALLOWABLE PERMANENT SET (deflection)
CCC            TPL    : PLATE THICKNESS
CCC            SHORT,RLONG   : PLATE DIMENSIONS
CCC            SGSHRT,SGLONG : MEMBRANE STRESS IN SHORT AND LONG DIRECTION
CCC            TAU    : SHEAR STRESS
CCC            PRSABS : PRESSURE
CCC            ILOOP  : TELLS WHICH DIRECTION OF BENDING IN TERMS OF PANEL
CCC                     1 = TRANSVERSE (Y AXIS); 2 = LONGITUDINAL (X AXIS)
CCC            GVAL1  : CONSTRAINT FUNCTION VALUE (Von Mises)
CCC            ICYC   : Number of iterations already used < CYC(max)=30 (ICYCM)
CCC            SYLDP  : SIG Elastique = Yield Stress (N/m2)
CCC            GAMMAS : Security factor
CCC            ISTOP  : 0 convergence , 1 non convergence
CCC
CCC COMMON BLOCKS: NIL
CCC
CCC CALLED BY: PLEVAL
CCC
CCC CALLS TO : PSETPR
CCC
CCC EXTERNAL FILES: NIL
CCC
CCC ********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION PBCOEF(2), SIGMEM(2)

      COMMON/PY/PI

      DATA PBCOEF / 0.19, 0.03 /
C**********************END OF ALL DECLARATIONS**************************

C  *********************************************************************
C                 LOGIC IS EXPLAINED VIA
C                 PRINCIPAL ALGORITHM STEPS
C                    § 1.0   TO   §  9.0
CCC ********************************************************************
C
c      WRITE(67,*) 
c      WRITE(67,*) 'départ   SGSHRT=',SGSHRT,' SGLONG=',SGLONG
c      WRITE(67,*) '         ILOOP=',ILOOP,  ' ILOOP2=',ILOOP2

CCC ***
CCC 1.0 CALCULATE BASIC PARAMETERS (voir Fig. 9.6 p.338)
CCC ***
      SIGMEM(1) = SGSHRT
      SIGMEM(2) = SGLONG
      SGMEM1 = SIGMEM(ILOOP2)
      SGMEM2 = SIGMEM(3-ILOOP2)
      BOVERA = SHORT/RLONG
      ARPLT = 1.0/BOVERA
      BOT2 = (SHORT/TPL)**2
      SKBEN = 0.31 + PBCOEF(ILOOP2)*TANH(2.*ILOOP2*(ARPLT-1.))
      SIGBEN = SKBEN*PRSABS*BOT2
      PHI = 1.0

c      WRITE(67,*) 'SYLDP                =',SYLDP
c      WRITE(67,*) 'SIGBEN (effet plaque)=',SIGBEN

C
C  FOR BENDING IN THE LONG DIRECTION, MAGNIFICATION IS NOT RELEVANT
c      WRITE(67,*) 'Avt 3.0  SKBEN =',SKBEN
c      WRITE(67,*) '         SGMEM1=',SGMEM1,' SGMEM2=',SGMEM2
c      WRITE(67,*) '         BOVERA=',BOVERA,' TPL=',TPL
c      WRITE(67,*) '         SIGBEN=',SIGBEN
c      WRITE(67,*) 'SYLDP=',SYLDP

      IF (ILOOP2 .EQ. 2) GO TO 24  ! skip; no magnification (PHI=1) as bending in the long direction.
CCC ***
CCC 3.0 BENDING ACROSS THE SHORT SPAN; CHECK FOR MAGNIFICATION
CCC ***
      IF (SGSHRT .GE. 0.) GO TO 24  ! skip; no membrane stress in the short direction ==> no need for ampllification
CCC
CCC 3.1  FOR USE IN EQ. 12.5.11, CALCULATE SGSHCR, THE CRITICAL VALUE
CCC      OF SGSHORT THAT WOULD CAUSE ELASTIC BUCKLING OF A PLATE WITH
CCC      LOADED EDGES SIMPLY SUPPORTED AND SIDES CLAMPED.  THE BUCKLING
CCC      COEFFICIENT IS GIVEN IN FIG. 12.5B, BUT WE HERE CHANGE IT TO A
CCC      WIDE PLATE FORM (SKWIDE) AND USE A "CURVE-FITTED" EXPRESSION.
CCC
CCC  NOTE:  IN CHAPTER 12 - BUCKLING OF WIDE PLATES - "A" AND "B" ARE
CCC         THE REVERSE OF THE CONVENTION USED HERE (FOR LATERALLY LOADED
CCC         PLATES, AS IN CHAPTER 9).
CCC
      SKWIDE = 0.96*(0.9 + 1.8*BOVERA**2)**2
      SGSHCR = SKWIDE*E/BOT2
CCC
CCC 3.2  CALCULATE THE MAGNIFICATION FACTOR, PHI, FROM EQ. 12.5.11.
CCC
      OMEGA = -SGSHRT/SGSHCR
      XIPANL = 0.5*PI*SQRT(ABS(OMEGA))
      XIZERO = (1.0 + 0.35*BOVERA) * XIPANL
      IF (XIZERO .LT. 0.02) THEN
         XZERO2 = XIZERO*XIZERO
         XZERO4 = XZERO2*XZERO2
         PHI = (1. + 0.4*XZERO2 + 17./105.*XZERO4) /
     1         (1. + XZERO2/3.0 + 2.0/15.0*XZERO4)
      ELSE IF (XIZERO .LE. 3.10) THEN
         PHI = 3. * ( TAN(XIZERO) - XIZERO ) /
     1                    ( XIZERO**2 * TAN(XIZERO) )
      ELSE
c        PHI = 99999.0
         PHI = 2.5
         IF (ICYC .EQ. 30 .OR. LCHECK .EQ. 1) THEN
            WRITE (*, 1000) SGSHRT, SGSHCR, PHI
            WRITE (67,1000) SGSHRT, SGSHCR, PHI
		  WRITE (29,1000) SGSHRT, SGSHCR, PHI										!bug
c           PAUSE'OK?'
c           ISTOP=1
         END IF
      END IF

CCC
CCC 3.4  CALCULATE THE MAGNIFIED PLATE BENDING STRESS.
CCC
  24  SIGMAG = PHI*SIGBEN

c      WRITE(67,*) 'SIGMAG (effet plaque*phi)=',SIGMAG

CCC ***
CCC 4.0  CALCULATE THE VON MISES STRESS FOR THE COMBINATION OF MEMBRANE
CCC ***  AND BENDING STRESSES (FOR BOTH TENSILE AND COMPRESSIVE VALUES O
CCC      THE LATTER) AND TAKE THE LARGER OF THE TWO : SIGVM.
CCC
C  VERIFICATION OF THE IN PLANE STRESS LEVEL:
      IF (ICYC .EQ. 0 ) THEN
         SIGVMT = GAMMAS *
     *        SQRT(SGMEM1**2 + SGMEM2**2 + 3.*TAU**2 - SGMEM1*SGMEM2)
         IF (SIGVMT.GT.SYLDP) THEN
            ISTOP=1
            WRITE (*, 1005) SIGVMT
            WRITE (67,1005) SIGVMT
		  WRITE (29,1005) SIGVMT												!bug
            RETURN
         END IF
      END IF

C  TENSILE (T) BENDING STRESS:
C     STRESS IN PRIMARY (1) BENDING DIRECTION
      SIG1T = SGMEM1 + SIGMAG
C     STRESS IN SECONDARY (2) DIRECTION (WITH POISSON'S RATIO BENDING STRE
      SIG2T = SGMEM2 + ETA*SIGMAG
      SIGVMT = SQRT(SIG1T**2 + SIG2T**2 + 3.*TAU**2 - SIG1T*SIG2T)
C
C  COMPRESSIVE (C) BENDING STRESS
C     STRESS IN PRIMARY (1) BENDING DIRECTION
      SIG1C = SGMEM1 - SIGMAG
C     STRESS IN SECONDARY (2) DIRECTION (WITH POISSON'S RATIO BENDING STRE
      SIG2C = SGMEM2 - ETA*SIGMAG
      SIGVMC = SQRT(SIG1C**2 + SIG2C**2 + 3.*TAU**2 - SIG1C*SIG2C)

c      WRITE(67,*) 
c      WRITE(67,*) 'Flex=',SIGBEN/SYLDP
c      WRITE(67,*) 'SIG1 et 2=',SGMEM1/SYLDP,SGMEM2/SYLDP
c      WRITE(67,*) 'PHI=',PHI,' SIGMAG=',SIGMAG/SYLDP
c      WRITE(67,*) 'SIGX et Y=',SIG1C/SYLDP,SIG2C/SYLDP
c      WRITE(67,*) 'TRACTION Sig C=',SIGVMT/SYLDP
c      WRITE(67,*) 'COMPRESSION Sig C=',SIGVMC/SYLDP

      IF (SIGVMT .GT. SIGVMC) THEN
c         SIGN = 1.0
          SIGVM = SIGVMT
c         IF (ILOOP .EQ. ILOOP2) THEN
c            SIGXM = SIG2T
c            SIGYM = SIG1T
c         ELSE
c            SIGXM = SIG1T
c            SIGYM = SIG2T
c         END IF
      ELSE
c         SIGN = -1.0
          SIGVM = SIGVMC
c         IF (ILOOP.EQ.ILOOP2) THEN
c            SIGXM = SIG2C
c            SIGYM = SIG1C
c         ELSE
c            SIGXM = SIG1C
c            SIGYM = SIG2C
c         END IF
      END IF

C     GAMMAS = SAFETY FACTOR
C     ----------------------
      GAMSTR = GAMMAS*SIGVM

c      WRITE(67,*) 'Sig von mises=',SIGVM/SYLDP
c      WRITE(67,*) 'Sig von mises * sec =',GAMSTR/SYLDP


CCC ***
CCC 5.0  CALCULATE THE FACTORED STRENGTH RATIO SR1, AND THE
CCC ***  CONSTRAINT FUNCTION VALUE GVAL1
CCC      Si SR1=1, alors GVAL1=0, on est a la solution.
CCC
      SR1 = GAMSTR/SYLDP
      GVAL1 = 2./(1.+SR1) - 1.

      IF (SR1 .GT. 1.02) THEN
         IF (ICYC .EQ. 30 .OR. LCHECK .EQ. 1) THEN
            ISTOP=1
            WRITE (*, 1005) SIGVM
            WRITE (67,1005) SIGVM
		  WRITE (29,1005) SIGVM													!bug
         END IF
      END IF

      RETURN
C
 1000 FORMAT(/,' ****************ERROR*************************'/,
     * ' EXCESSIVE IN-PLANE COMPRESSIVE STRESS IN THE SHORT DIRECTION'/
     * ' (',G10.3,', COMPARED TO A SIMPLY SUPPORTED BUCKLING STRESS',
     * ' OF',G10.3,').'/
     * ' HENCE THE MAGNIFICATION FACTOR, CALCULATED TO BE',G10.3,
     * ' FROM EQ. 12.5.11, IS NOT VALID.')
 1005 FORMAT(/' EXCESSIVE IN-PLANE COMPRESSIVE STRESS,'
     * ,' GIVING A VON MISES STRESS OF ',G10.3,' N/m2'/
     *  ' THIS IS TOO LARGE TO PERMIT THE CALCULATION OF A'
     * ,' PRESSURE THAT WOULD JUST INITIATE YIELD.'/
     *  ' Therefore we will not consider this constraint'/)
      END


CCC ********************************************************************
      SUBROUTINE BUCKLE(TPL,XLEN,YLEN,E,ETA,SYLDP,GAMMAS,
     *       SIGBX,SIGBY,TAU,GVAL2,ICYC,LCHECK,ISTOP,IGG,
     *       SR2,RXc,RYc,RSc)
CCC ********************************************************************
CCC NAME: BUCKLE                             CREATED ON   : 28-5-96
CCC                                          LAST REVISION: 16-7-97
CCC AUTHOR: Dr. Ph. Rigo
CCC
CCC PURPOSE: Cette subroutine intègre une vérification au voilement
CCC          du panneau étudié.
CCC          Elle sert au calcul de l'épaisseur minimale requise pour`
CCC          éviter le voilement.
CCC
CCC          Cette soubroutine est valable pour XLEN > YLEN

C               Œ Y et Sigma Y
C               I
C               *****************************
C               *                           *  b=YLEN
C               *                           *
C               *                           *
C               ***************************** --------> X et Sigma X
CCC             <---------- a=XLEN --------->
CCC
CCC
CCC ARGUMENTS: TPL    : PLATE THICKNESS
CCC            XLEN,YLEN   : PLATE DIMENSIONS (Long and Short)
CCC            SIGBX,SIGBY : MEMBRANE STRESS IN LONG AND SHORT DIRECTION
CCC            TAU    : SHEAR STRESS
CCC            GVAL2  : CONSTRAINT FUNCTION VALUE (Buckling)
CCC            ICYC   : Number of iterations already used < CYC(max)=30
CCC            SYLDP  : SIG Elastique = Yield Stress (N/m2)
CCC            SECU   : Security factor on the strength = 1.1 (as recommended by Eurocode for buckling)
CCC            SECU2  : Security factor on the stress  (= GAMMAS = Sadm/Sy)
CCC            LCHECK : 1 dernière itération (sinon 0)
CCC            ISTOP  : 0 convergence , 1 non convergence
CCC            IGG    : =1 si sollicitation (in plane loads) nulle (0 sinon)
CCC
CCC CALLED BY: PLEVAL
CCC
CCC ********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/PY/PI
C**********************END OF ALL DECLARATIONS**************************

C     SECU  : SAFETY ON STRENGTH
      SECU=1.10    ! 1.10 on the strength (see Eurocode)
c     SECU=1.0   

C     SECU2 : SAFETY ON LOADS  
c     (as recommended by Eurocode for non-permanent loads)
c     SECU2=1.0
	SECU2=GAMMAS ! GAMMAS = Sadm/Sy; (usually 1.50 for Eurocode)
c          Sadm and Sy are defined in the data file of LBR5

CCC ***
CCC 1.0 CALCULATE BASIC PARAMETERS
CCC ***
      IF(SIGBX.GT.0) THEN
        SX=0.
      ELSE
        SX=-SIGBX*SECU2
      ENDIF
      IF(SIGBY.GT.0) THEN
        SY=0.
      ELSE
        SY=-SIGBY*SECU2
      ENDIF

      ALPHA = XLEN/YLEN
      ALPHA2= 1./ALPHA
      BETA=YLEN/TPL*SQRT(SYLDP/E)

c      WRITE(67,*) 'SX,SY,TAU=',SX,SY,TAU
c      WRITE(67,*) 'ALPHA,BETA=',ALPHA,BETA

CCC ***
CCC 2.0 CALCULATE RX, RY AND RS, Eq. 12.4.7  p.416 de Hughes
CCC      (4 simply supported boundaries)
CCC ***
      TB =E*(TPL/YLEN)**2
      RXc=3.62 * TB
      RX =SX/RXc
      RYc=(0.905*(1.+ALPHA2**2)**2) * TB
      RY =SY/RYc
      RSc=0.905*(5.35+4.*ALPHA2**2) * TB
      RS =DABS(TAU*SECU2)/RSc

c      WRITE(67,*) 'RX,RY,RS=',RX,RY,RS

CCC ***
CCC 3.0  CALCULATE THE FACTORED STRENGTH RATIO SR2, AND THE
CCC ***  CONSTRAINT FUNCTION VALUE GVAL2
CCC      SR2=0.625*(1.+0.6/ALPHA)*RY / (1.-0.625*RX) +  RS*RS/(1.-RX)
CCC      Si SR2=1, alors GVAL2=0, on est a la solution.

      IF(RS.LE.0.001) THEN
        IF(RX.LE.0.01) THEN
          SR2=RY
        ELSE IF(RY.LE.0.01) THEN
          SR2=RX
        ELSE
          IF(RX.GT.0.99) RX=0.99
          SR2=0.625*(1.+0.6/ALPHA)*RY / (1.-0.625*RX)
        ENDIF
      ELSE
        SR2=RS
      ENDIF

c      WRITE(67,*) 'SR2=',SR2

      IF (SR2 .LE.0.0001) THEN
         IGG=1
c        WRITE(67,*) 'IGG=',IGG
         GVAL2=1
         RETURN
      END IF

C     SECU = SAFETY FACTOR ON STRENGTH
C     -----------------------------------
      SR2 = SECU*SR2

c      WRITE(67,*) 'SR2 * SECU =',SR2

      GVAL2 = 2./(1.+SR2) - 1.

      IF (SR2 .GT. 1.02) THEN
         IF (ICYC .EQ. 30 .OR. LCHECK .EQ. 1) THEN
            ISTOP=1
            WRITE (*, 1005) SR2
            WRITE (67,1005) SR2
		  WRITE (29,1005) SR2													!bug
         END IF
      END IF

      RETURN
C
 1005 FORMAT(/,' *********** ERROR IN BUKLE ********'/
     1 ,' AFTER 30 ITERATIONS !!!'
     1 ,' EXCESSIVE  STRESS, GIVING A RATIO'
     2 ,' OF ',G10.3/
     *  ' Therefore we can not consider this constraint'/)
      END
