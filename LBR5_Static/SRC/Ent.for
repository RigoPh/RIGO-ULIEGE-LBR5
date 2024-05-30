      SUBROUTINE ENTS(NEL,NETO,WIDTH,FAM,PHILN,QN,IBUSC,DESSIN,ICHA,
     *                POT,IPOIDS,IMPR,ITERA,IPRINT,NBRXI,ITYPE,ISECT,		!février 2004
     *                M1CONT,M2,
     *                NXI,IPT,YPT,IPT2,IPT3,
     *                NSOLM,NSOL,NNSOL,IFF,
     *     PLOC,CHA,dCHA,NPT,LM2,TETAQ,TETAN,DELT,DELT2,ANGLE,NOH,NSIGN,	!!!aout04
     *          E1,ETA1,SIGY1,SIGM1,SPEC1,XICOU,TFA,TFR,MODES,
     *          XIMIN,XIMAX,TRAV,V,M1TABL,
     *		  PART,CORRO,IMPR2,Spoids1,IANA)			!05.12.05		!coefficient de participation  !corrosion  !15.10.05 !r&d14

      IMPLICIT REAL *8 (A-H,O-Z)

      INTEGER  *2 DESSIN
      CHARACTER*3 MODES(NETO)
	DIMENSION   TRAV(1),V(1)
      COMMON/DIM1/Nmax,NEmax,NVmax,
     *            M1max,M1Tmax,M2max,M2Tmax,Mmax,MGmax,ISmax,IPTmax   !Dimension max des vecteurs

c  Vecteur (V) de travail: pour LCONT,INV  et pour CJMAX
c     CJMAX (NSOLM,NETO,M1max=20)    = 20x100x20   = 40.000 max (REAL*8)
c     LCONT (NSOLM,NETO,M1max=20, 2) = 20x100x20x2 = 80.000 max (INTEGER*4) ou 40.000 REAL*8
c     INV   (NSOLM,NETO,M1max=20)    = 20x100x20   = 40.000 max (INTEGER*4) ou 20.000 REAL*8
      IV1=1                           ! CJMAX
      IV2=IV1+NSOLM*NETO*M1max        ! LCONT
	IV3=IV2+(NSOLM*NETO*M1max*2)/2  ! INV
	IVmax=NSOLM*NETO*(M1max+2*M1max/2+M1max/2)  ! TOTAL max =100.000 (Equivalent REAL*8 )
	IF(NEL.EQ.1) CALL ANNULD(V,IVmax)

c  Vecteurs de travail:
c     XI(NSOL),XF(NSOL),XXI(NSOL),XXF(NSOL),CHAMAX(5,NSOL)
c     TRAV(1),TRAV(1+NSOL),TRAV(1+2*NSOL),TRAV(1+3*NSOL),TRAV(1+4*NSOL)

      CALL ENTT(NEL,NETO,WIDTH,FAM,PHILN,QN,IBUSC,DESSIN,ICHA,
     *          POT,IPOIDS,IMPR,ITERA,IPRINT,NBRXI,ITYPE,ISECT,		!février 2004
     *          M1CONT,M2,
     *          NXI,IPT,YPT,IPT2,IPT3,
     *          NSOLM,NSOL,NNSOL,IFF,
     *     PLOC,CHA,dCHA,NPT,LM2,TETAQ,TETAN,DELT,DELT2,ANGLE,NOH,NSIGN,	!!!aout04
     *          E1,ETA1,SIGY1,SIGM1,SPEC1,XICOU,TFA,TFR,MODES(NEL),
     *          XIMIN,XIMAX,
     *          TRAV(1),TRAV(1+NSOL),TRAV(1+2*NSOL),TRAV(1+3*NSOL),
     *          TRAV(1+4*NSOL),TRAV(1+9*NSOL),V(1),V(IV2),V(IV3),M1TABL,   !!!aout04
     *          PART,CORRO,IMPR2,Spoids1,IANA)					!05.12.05			!coefficient de participation  + !corrosion  !15.10.05  !r&d14
      RETURN
	END 

C *************************************************************************  
C *************************************************************************  
      SUBROUTINE ENTT(NEL,NETO,WIDTH,FAM,PHILN,QN,IBUSC,DESSIN,ICHA,
     *                POT,IPOIDS,IMPR,ITERA,IPRINT,NBRXI,ITYPE,ISECT,	!février 2004
     *                M1CONT,M2,
     *                NXI,IPT,YPT,IPT2,IPT3,
     *                NSOLM,NSOL,NNSOL,IFF,
     *     PLOC,CHA,dCHA,NPT,LM2,TETAQ,TETAN,DELT,DELT2,ANGLE,NOH,NSIGN,	!!!aout04
     *          E1,ETA1,SIGY1,SIGM1,SPEC1,XICOU,TFA,TFR,MODE,
     *          XIMIN,XIMAX,
     *          XI,XF,XXI,XXF,
     *          CHAMAX,dCHAMAX,CJMAX,LCONT,INV,M1TABL,   !!!aout04
     *          PART,CORRO,IMPR2,Spoids1,IANA)		!05.12.05						!coefficient de participation  + !corrosion  !15.10.05  !r&d14

      IMPLICIT REAL *8 (A-H,O-Z)

      INTEGER *2 DESSIN,IMPR2									!15.10.05
      CHARACTER*9 TYPES		!février 2004
      CHARACTER*7 SECTION		!février 2004
	CHARACTER*3 MODE,MOD,GFH

      COMMON/DIM1/Nmax,NEmax,NVmax,
     *            M1max,M1Tmax,M2max,M2Tmax,Mmax,MGmax,ISmax,IPTmax ! dimension max des vecteurs
      COMMON /PY/PI
      COMMON /LANGUE/LANGUE
      COMMON /OPTI/ IOPTI,NTOT,M1TOT,M2TOT

      DIMENSION FAM(6),ABTR2(10),NNSOL(ISmax)
      DIMENSION NOH(NETO,10),
     *          PHILN(NETO),QN(NETO),ANGLE(NETO),NSIGN(NETO),DELT(NETO),
     *          DELT2(NETO),TETAN(NETO),TETAQ(NETO)
      DIMENSION XI(NSOL),XF(NSOL),XXI(NSOL),XXF(NSOL),CHAMAX(5,NSOL)
	DIMENSION dCHAMAX(5,NSOL,9),dT1(9),dT2(9),dT3(9),dT4(9),dT5(9)   !!!aout04
      DIMENSION NXI(9)
      DIMENSION CHA(100,3,NSOL),NPT(NSOL),LM2(M2max) ! 100 est le nbre d'intervalles
 
      DIMENSION IPT(Nmax),IPT2(IPTmax,Nmax),IPT3(IPTmax,Nmax), 
     *                    YPT(IPTmax,Nmax)
      DIMENSION M1TABL(Nmax,ISmax),M11(20)   !M1TABL(Nmax,ISmax),M11(20) 
      DIMENSION LCONT(NSOLM,NETO,M1max,2),INV(NSOLM,NETO,M1max),
     *          CJMAX(NSOLM,NETO,M1max)
c      CJMAX (NSOLM,NETO,M1max=20)    = 20x100x20   = 40.000 max (REAL*8)
c      LCONT (NSOLM,NETO,M1max=20, 2) = 20x100x20x2 = 80.000 max (INTEGER*4) ou 40.000 REAL*8
c      INV   (NSOLM,NETO,M1max=20)    = 20x100x20   = 40.000 max (INTEGER*4) ou 20.000 REAL*8

      DIMENSION XIMIN(NVmax),XIMAX(NVmax)
	DIMENSION DVMIN(9),DVMAX(9)						!eugen
      DIMENSION XICOU(NVmax),TFA(NETO),TFR(NETO)

	DIMENSION PART(NETO)
	DIMENSION CORRO(NETO,3)					!corrosion
	DIMENSION dPOIDS(9),dCHA(100,NSOL,9) !!!aout04

C     For: ENT,BO1,BO2,HUGUES
      DIMENSION E1(NETO),ETA1(NETO),SIGY1(NETO),SIGM1(NETO),SPEC1(NETO)

      COMMON /ENT/ABTR(10),ARGQ(8),CONST(74),          ! dans GEOM,BO1,LOC,LOCY,BO2,MDR2
     *            HXTR(10),DXTR(10),WXTR(10),TXTR(10) 
      COMMON /OPTI3/CONST2(6,9),CONST3(2,10)           ! dans BO1,DPRESS,SENS,BO2
			
C***********************************************************************
C  SUBROUTINE ENTS
C  ***************
C  SUBROUTINE D'INTRODUCTION DES DONNEES RELATIVES A CHAQUE PANNEAU,
C  ECRITURE DES DONNEES,CALCUL DES COEFFICIENTS DE RAIDEUR DU PANNEAU,
C  INTRODUCTION DU POIDS PROPRE SUR LES PANNEAUX HORIZONTAUX.
C
C  Création :Thèse Ph. Rigo, L.B.R.-3 (1987)
C  Modif:   1994   L.B.R-5
C    : 8- 5-96  Charge max pour subr Hughes CHAMAX(5,IS); § 4.7
C    : 4- 6-96  Subroutine GEOM (Restrictions géométriques de Rahman);§ 3.4
C    : 3- 3-97  Definition par panneau de E,ETA,SIGY, etc.; § 2.O
C    :          Definition du MODE de raidissage long. (ENTR, EPSR et MODE)
C    :13- 3-98  Subroutine GEOM (Restrictions géométriques de Hughes); § 3.4
C    :30- 5-98  Calcul du coût, §4.8 : Subr COST,
C    :25- 5-99  LBR-5.1 : Modification des COMMON
C    :30-12-99  Write(46) Données graphiques pour VISIONN.for (§5.1)
c    :01-11-00  Version anglaise + DELT2
c    :16-11-00  YPT donnes en valeur relative 0<YPT<1 cad  (0<YPT<Q*PHIL)
c    :20-11-00  Subr SELECT (calcul de CHAMAX avec son signe)
c        11-01  Calcul du coût
c     20-01-02  Changement de la structures des données (LBR-5.6)
c    :04-02-04  Données pour l'élément épontille
c
C  Dernière modification:04.02.04
C***********************************************************************
      PI1=PI/180.

C 0.a   Lecture du type de panneau : COQUE OU PLAQUE
C     -----------------------------------------------
      READ(55,2,END=900) TYPES
    2 FORMAT(A9)											 !février 2004

      ! ITYPE= 1  COQUE		COQUE  standard
      ! ITYPE= 2	PLAQUE		PLAQUE standard
      ! ITYPE= 3	COQUE1		COQUE  sans contribution transversale du bordé
      ! ITYPE= 4	PLAQUE1		PLAQUE sans contribution transversale du bordé
	! ITYPE= 5  EPONTILLE   EPONTILLE élément poutre     !février 2004          

      BACKSPACE(55)
      IF(TYPES.EQ.'PLAQUE') THEN
        READ(55,'(T7,I1)',END=900) I
        IF(I.EQ.1) THEN
	    ITYPE=4  !PLAQUE1	PLAQUE sans contribution transversale du bordé
  	  ELSE
	    ITYPE=2  !PLAQUE	PLAQUE standard
	  ENDIF

      ELSEIF(TYPES.EQ.'COQUE') THEN		 ! TYPES=COQUE
       READ(55,'(T6,I1)',END=900) I
        IF(I.EQ.1) THEN
	    ITYPE=3  !COQUE1	COQUE sans contribution transversale du bordé
  	  ELSE
	    ITYPE=1  !COQUE		COQUE standard
	  ENDIF
 	ELSEIF (TYPES.EQ.'EPONTILLE') THEN     !TYPES=EPONTILLE		!février 2004
	  READ(55,*,END=900)										!février 2004
	  ITYPE=5													!février 2004
      ENDIF


      IF(TYPES.EQ.'PLAQUE') THEN
C       Cas de la "Plaque"
        READ(55,*,END=900) HIGHT,DELTA
        Q=10000000000.
        PHIL=HIGHT/Q/PI1
	ELSEIF(TYPES.EQ.'EPONTILLE') THEN								!février 2004
C	  Cas de l'"épontille"											!février 2004
	  READ(55,*,END=900) HIGHT,EPAIS								!février 2004
	  READ(55,3,END=900) SECTION	    !carré, cercle ou double T	!février 2004
 3	  FORMAT(A7)													!février 2004
	  Q=10000000000.												!février 2004
	  PHIL=HIGHT/Q/PI1												!février 2004
	  DELTA=0.00000													!février 2004
      ELSE
C       Cas de la "Coque"
        READ(55,*,END=900) PHIL,Q,DELTA
      ENDIF

C 0.b   Donnees relatives au raidissage (aig. et raid)
C     ----------------------------------------------
C     ENTR = entredistance réelle entre raid.=(D)raid
C     EPSR = largeur collaborante moyenne compte tenu du nbre de raid. défini
C            par MODE (type de répartition)
c     EPSA=(D)aig

	IF(ITYPE.NE.5) THEN												!février 2004
      READ(55,*,END=900) EPSA,ENTR,EPSA2,ENTR2, CORRO(NEL,1)			!corrosion			
      READ(55,*,END=900) HYA,DYA,WYA,TYA, CORRO(NEL,2)				!corrosion
      READ(55,*,END=900) HXR,DXR,WXR,TXR, CORRO(NEL,3)				!corrosion
	EPAIS=0.0000													!février 2004														
	ISECT=0															!février 2004
	ELSE															!février 2004
      READ(55,*,END=900) EPSA,Heff	! Heff: hauteur sans hiloire	!février 2004
		IF(SECTION.EQ.'CERCLE')THEN									!février 2004
		ISECT=1														!février 2004
		READ(55,*,END=900) HYA,CORRO(NEL,2)     					!février 2004  + !corrosion
		DYA=0.00000													!février 2004
		WYA=0.00000													!février 2004
		TYA=0.00000													!février 2004
		CORRO(NEL,1)=0.												!corrosion
		CORRO(NEL,3)=0.												!corrosion							
		ELSEIF(SECTION.EQ.'CARRE')THEN								!février 2004
		ISECT=2														!février 2004
		READ(55,*,END=900) HYA,CORRO(NEL,2)							!février 2004  + !corrosion
		DYA=0.00000													!février 2004
		WYA=0.00000													!février 2004
		TYA=0.00000													!février 2004
		CORRO(NEL,1)=0.												!corrosion
		CORRO(NEL,3)=0.												!corrosion							
		ELSEIF(SECTION.EQ.'DOUBLET')THEN							!février 2004
		ISECT=3														!février 2004
		READ(55,*,END=900) HYA,DYA,WYA,TYA,CORRO(NEL,2)
		EPAIS=0.0000												!février 2004				
		CORRO(NEL,1)=0.												!corrosion
		CORRO(NEL,3)=0.												!corrosion					
		ENDIF														!février 2004
	HXR=0.001														!février 2004
	DXR=0.001														!février 2004
	WXR=0.001														!février 2004			
	TXR=0.001														!février 2004
	EPSR=HIGHT/5													!17.11.05
	ENTR=HIGHT/5													!05.12.05
	EPSA2=0.00000													!février 2004
	ENTR2=0.00000													!février 2004
	ENDIF															!février 2004

C 	IF(ITYPE.EQ.5) THEN		!pour toujours avoir Sy différent de 0	!26.07.05
C	FAM(1)=1														!26.07.05
C	FAM(2)=1														!26.07.05
C	ENDIF															!26.07.05

      IF(EPSA.LE.(0.00001)) EPSA=1.0
      IF(ENTR.LE.(0.00001)) ENTR=Q*DABS(PHIL)*PI1

      IF((EPSA2.GE.(0.00001)).OR.(ENTR2.GE.(0.00001))) THEN
        IF(EPSA2.LE.(0.00001)) EPSA2=1.0
        IF(ENTR2.LE.(0.00001)) ENTR2=Q*DABS(PHIL)*PI1
        READ(55,*,END=900) HYA2,DYA2,WYA2,TYA2,KSA2
        READ(55,*,END=900) HXR2,DXR2,WXR2,TXR2,KSR2
      ENDIF

      IF(ITYPE.NE.5) THEN												!février 2004
      IF((IOPTI.EQ.0).OR.(ITERA.EQ.0)) THEN			!13.12.05
        READ(55,*,END=900) MODE
	ELSE															
        READ(55,*,END=900) MOD ! bidon						
	ENDIF
	ELSE															!février 2004							
	MOD='EE1'														!février 2004
	MODE= 'EE1'														!février 2004
	ENDIF															!février 2004

      IF(MODE.EQ.'EE1'.OR.MODE.EQ.'ES4') THEN
          Xmode=-1.0
        ELSEIF(MODE.EQ.'ES1'.OR.MODE.EQ.'ES3'.OR.
     *         MODE.EQ.'EC1'.OR.MODE.EQ.'EL3') THEN
          Xmode=-0.5
        ELSEIF(MODE.EQ.'SL1'.OR.MODE.EQ.'SL2'.OR.MODE.EQ.'EL2') THEN
          Xmode=+0.5
        ELSEIF(MODE.EQ.'LL1') THEN
          Xmode=+1.0
        ELSE
          Xmode=0.0
      ENDIF

      DENOM = (1.0/ENTR  + Xmode/(DABS(PHIL)*Q*PI1) )

 	IF(DENOM.LE.0.0) THEN
	  WRITE(66,*)
	  WRITE(66,*)'ATTENTION : ERROR dans le Panneau ',NEL
	  WRITE(66,*)'****************************************'
        WRITE(66,*)'Le paramètre MODE =',MODE,' est incompatible avec'
	  WRITE(66,*)'    l''entredistance entre les raidisseurs=',ENTR
        WRITE(66,*)'    et la largeur du panneau.'
	  WRITE(66,*)'L''écartement entre les raidisseurs est '
	  WRITE(66,*)'probablement supérieur à la largeur(L) du panneau.'
        WRITE(66,*)'    ENTR=',ENTR,' et MODE =',MODE
	  WRITE(66,*)'Cela peut induire un EPSR erroné (<0 ou infini)!!!'     
	  WRITE(66,*)'Il faut corriger cette situation!!'

	  WRITE(29,*)																!bug
	  WRITE(29,*)'ATTENTION : ERROR dans le Panneau ',NEL						!bug
	  WRITE(29,*)'****************************************'						!bug
        WRITE(29,*)'Le paramètre MODE =',MODE,' est incompatible avec'			!bug
	  WRITE(29,*)'    l''entredistance entre les raidisseurs=',ENTR				!bug
        WRITE(29,*)'    et la largeur du panneau.'								!bug
	  WRITE(29,*)'L''écartement entre les raidisseurs est '						!bug
	  WRITE(29,*)'probablement supérieur à la largeur(L) du panneau.'			!bug
        WRITE(29,*)'    ENTR=',ENTR,' et MODE =',MODE								!bug
	  WRITE(29,*)'Cela peut induire un EPSR erroné (<0 ou infini)!!!'			!bug
	  WRITE(29,*)'Il faut corriger cette situation!!'							!bug
 
        MODE='EE2'
	  Xmode=0.
	  EPSR=ENTR
        DENOM = 1.0/ENTR
	  WRITE(66,*)'Correction automatique: on a'
	  WRITE(66,*)'  MODE  =',MODE
	  WRITE(66,*)'  Xmode =',Xmode
	  WRITE(66,*)'  ENTR  =',ENTR
	  WRITE(66,*)'  EPSR  =',EPSR
	  WRITE(66,*)
	ENDIF
      EPSR = 1.0/DENOM
  
	IF (ENTR/(DABS(PHIL)*Q*PI1).GT.1.01)  THEN
	  WRITE(66,*)
	  WRITE(66,*)'ATTENTION : ERROR dans le Panneau ',NEL
	  WRITE(66,*)'****************************************'
        WRITE(66,*)'Le paramètre MODE =',MODE,' est incompatible avec'
	  WRITE(66,*)'    l''entredistance entre les raidisseurs=',ENTR
        WRITE(66,*)'    et la largeur du panneau.'
	  WRITE(66,*)'L''écartement entre les raidisseurs est '
	  WRITE(66,*)'probablement supérieur à la largeur(L) du panneau.'
        WRITE(66,*)'    ENTR=',ENTR,' et MODE =',MODE
	  WRITE(66,*)'Cela peut induire un EPSR erroné ou négatif!!!'
        WRITE(66,*)'EPSR=',EPSR,'  !!!'
	  WRITE(66,*)'Il faut corriger cette situation!!'
	  WRITE(66,*)
 
 	  WRITE(29,*)																!bug
	  WRITE(29,*)'ATTENTION : ERROR dans le Panneau ',NEL						!bug
	  WRITE(29,*)'****************************************'						!bug
        WRITE(29,*)'Le paramètre MODE =',MODE,' est incompatible avec'			!bug
	  WRITE(29,*)'    l''entredistance entre les raidisseurs=',ENTR				!bug
        WRITE(29,*)'    et la largeur du panneau.'								!bug
	  WRITE(29,*)'L''écartement entre les raidisseurs est '						!bug
	  WRITE(29,*)'probablement supérieur à la largeur(L) du panneau.'			!bug
        WRITE(29,*)'    ENTR=',ENTR,' et MODE =',MODE								!bug
	  WRITE(29,*)'Cela peut induire un EPSR erroné ou négatif!!!'				!bug
        WRITE(29,*)'EPSR=',EPSR,'  !!!'											!bug
	  WRITE(29,*)'Il faut corriger cette situation!!'							!bug
	  WRITE(29,*)																!bug
 
        MODE='EE2'
	  Xmode=0.
	  EPSR=ENTR
	  WRITE(66,*)'Correction automatique: on a'
	  WRITE(66,*)'  MODE  =',MODE
	  WRITE(66,*)'  Xmode =',Xmode
	  WRITE(66,*)'  ENTR  =',ENTR
	  WRITE(66,*)'  EPSR  =',EPSR
	  WRITE(66,*)
	ENDIF

      IF((EPSA2.GE.(0.00001)).OR.(ENTR2.GE.(0.00001))) THEN
          EPSR2=1.0/ ( 1.0/ENTR2 + Xmode/(DABS(PHIL)*Q*PI1) )
	ELSE										!05.12.05
		EPSR2 = ENTR2							!05.12.05
      ENDIF

	IF(ITYPE.EQ.5) GOTO 648			!pas de division par 0		!février 2004						
      QDELTA=Q/DELTA
      IF(QDELTA.GT.199.) GOTO 648
      WRITE(66,649)QDELTA

      IF(QDELTA.LT.101.)THEN
        WRITE(66,'(/A,I3/15(1H=))')'PANNEAU No ',NEL
        WRITE(* ,'(/A,I3/15(1H=))')'PANNEAU No ',NEL
	  WRITE(29,'(/A,I3/15(1H=))')'PANNEAU No ',NEL								!bug
        WRITE(66,*)' LE RAPPORT LIMITE 1/100 EST DEPASSE.',
     *             ' EXCECUTION STOPPEE. '							
        WRITE(29,*)' LE RAPPORT LIMITE 1/100 EST DEPASSE.',						!bug
     *             ' EXCECUTION STOPPEE. '										!bug
        WRITE(*,*)' Le rayon est ',Q,' et l''épaisseur est ',DELTA
        WRITE(*,*)
        WRITE(*,*)' LE RAPPORT LIMITE 1/100 EST DEPASSE.',
     *            ' EXCECUTION STOPPEE. '
        PAUSE     'STOP'
        IFF=1        ! STOP après SubR. ENT
      ELSE
       WRITE(66,'(/A,I3/15(1H=))')'PANNEAU No ',NEL
       WRITE(* ,'(/A,I3/15(1H=))')'PANNEAU No ',NEL
       WRITE(29,'(/A,I3/15(1H=))')'PANNEAU No ',NEL
       WRITE(66,*)' LE RAPPORT LIMITE EST COMPRIS ENTRE 1/100 ET 1/200.'			
       WRITE(*,*) ' LE RAPPORT LIMITE EST COMPRIS ENTRE 1/100 ET 1/200.'
       WRITE(29,*)' LE RAPPORT LIMITE EST COMPRIS ENTRE 1/100 ET 1/200.'			!bug
       WRITE(66,*)' ATTENTION :',
     *            ' LA VALIDITE DES RESULTATS N''EST PLUS ASSUREE.'
       WRITE(66,*)' *********  '
       WRITE(29,*)' ATTENTION :',													!bug
     *            ' LA VALIDITE DES RESULTATS N''EST PLUS ASSUREE.'				!bug
       WRITE(29,*)' *********  '													!bug
       WRITE(*,*) ' ATTENTION :',
     *            ' LA VALIDITE DES RESULTATS N''EST PLUS ASSUREE.'
      ENDIF
  648 CONTINUE

C     Impression du numero du panneau (titre)
c     ---------------------------------------  
      IF(LANGUE==1) THEN 
	  WRITE(66,4)NEL,TYPES
   4    FORMAT(/' PANNEAU No ',I3,' (C''est une "',A9,'")'/36(1H=))		!février 2004
      ELSE
        WRITE(66,'(/A,I3,A,A9,A/36(1H=))')								!février 2004
     *          'PANEL No ',NEL,' (It is a "',TYPES,'")'
	ENDIF

C------------------------------------------------------------------------------
C 1.	Transfer des nouvelles valeurs des variables de conception (cfr OPTI).
C     (Itération nø 2 et suivante)
C     ==================================================================
      IF((IOPTI.GE.1).AND.(ITERA.GT.0)) THEN			!13.12.05

        DO 123 I=1,NBRXI
          GOTO(124,125,120,126,127,128,121,129,130),NXI(I)
  124     DELTA=XICOU(I+NTOT)
          GOTO 123
  125     HYA=XICOU(I+NTOT)
          GOTO 123
  120     DYA=XICOU(I+NTOT)
          GOTO 123
  126     WYA=XICOU(I+NTOT)
          GOTO 123
  127     EPSA=XICOU(I+NTOT)
          GOTO 123
  128     HXR=XICOU(I+NTOT)
          GOTO 123
  121     DXR=XICOU(I+NTOT)
          GOTO 123
  129     WXR=XICOU(I+NTOT)
          GOTO 123
  130     EPSR=XICOU(I+NTOT)
          ENTR=1.0/( 1.0/EPSR - Xmode/(DABS(PHIL)*Q*PI1) )
  123   CONTINUE

        TYA=TFA(NEL)
        TXR=TFR(NEL)
c        IF(IPRINT.GE.1) THEN
c          WRITE(*,*) 'PANNEAU Nø ',NEL
c          WRITE(*,*) 'NBRXI= ',NBRXI,' et  NXI =',NXI
c          WRITE(*,*) 'Bord = ',DELTA
c          WRITE(*,*) 'Cadre= ',HYA,DYA,WYA,TYA,'  Spacing (D)=',EPSA
c          WRITE(*,*) 'Raid = ',HXR,DXR,WXR,TXR,'  Spacing (D)=',ENTR
c          WRITE(*,*) '                     Larg. coll.(EPSR) =',EPSR
c        ENDIF
      ENDIF
C-----------------------------------------------------------------------
C     LECTURE Des épaisseurs de corrosion			    !corrosion
C	=======================================
c	IF(ITYPE.NE.5) READ(55,*,END=900)	CORRO(NEL,1), CORRO(NEL,2), 
c     *   CORRO(NEL,3)
c	
c	IF(ITYPE.EQ.5) THEN 
c		CORRO(NEL,2)=0.
c		CORRO(NEL,3)=0.
c	ENDIF
C-----------------------------------------------------------------------


C     LECTURE DU COEFFICIENT DE PARTICIPATION			!coefficient de participation
C	=======================================
	IF(ITYPE.NE.5) READ(55,*,END=900) PART(NEL)		!coefficient de participation



C  2. SUITE DE LA LECTURES DES DONNEES: caract. des mat., charges, angle,...)
C     =======================================================================
      READ(55,*,END=900) E,ETA,SIGY,SIGM,SPEC

      IF(LANGUE==1) WRITE(66,420) E,ETA,SIGY,SIGM,SPEC
      IF(LANGUE==2) WRITE(66,421) E,ETA,SIGY,SIGM,SPEC
     	IF(ITYPE.EQ.5) THEN											!février 2004	
	ETA=0.000													!février 2004
	ENDIF														!février 2004
      SH=E/(2.*(1.+ETA))
      E1(nel)   =E
      ETA1(nel) =ETA
      SIGY1(nel)=SIGY
      SIGM1(nel)=SIGM
      SPEC1(nel)=SPEC
  420 FORMAT( ' MODULE DE YOUNG       = ',E14.7,' N/m2'/
     *        ' COEFFICIENT DE POISSON= ',F6.3/
     *        ' LIMITE ELASTIQUE      = ',E14.7,' N/m2'/
     *        ' CONTRAINTE ADMISSIBLE = ',E14.7,' N/m2'/
     *        ' POIDS SPECIFIQUE      = ',E14.7,' N/m3' )
  421 FORMAT( ' YOUNG  MODULUS        = ',E14.7,' N/m2'/
     *        ' POISSON COEFFICIENT   = ',F6.3/
     *        ' YIELD STRESS          = ',E14.7,' N/m2'/
     *        ' ALLOWABLE STRESS      = ',E14.7,' N/m2'/
     *        ' SPECIFIC WEIGHT       = ',E14.7,' N/m3' )

      READ(55,*,END=900) MT,KSR,KSA,KST,KSE,IPA,IVARIA
      READ(55,*,END=900) ANGLE(NEL)

C 2.1 Charges XI,XF
C     -------------
      CALL ANNULD(XI,NSOL)
      CALL ANNULD(XF,NSOL)
      CALL ANNULD(XXI,NSOL)
      CALL ANNULD(XXF,NSOL)

      IS=0
      DO 195 I=1,NSOLM
        ICC=0
        DO K=1,NSOL
          IF (NNSOL(K).EQ.I) ICC=1  ! ICC=1 cas de charge retenu
	  ENDDO
        IF (ICC.EQ.0) THEN           ! ICC=0 cas de charge pas retenu
          READ(55,*,END=900) TEMP
          GOTO 195
	  ELSE
	    IS=IS+1
	  ENDIF
        IF(IVARIA.EQ.1) GOTO 191
        READ(55,*,END=900) XI(IS)
        GOTO 192
  191   READ(55,*,END=900) XI(IS),XF(IS)
  192   ANG=ANGLE(NEL)
        IF(IVARIA.EQ.0) THEN                     ! Pression variant avec la prof.
          IF(TYPES.NE.'COQUE') THEN             ! PLAQUE et EPONTILLE		!février 2004
            XF(IS)=XI(IS)+DABS(HIGHT)*DSIN(ANG*PI1)
            IF(I.EQ.NSOLM) IVARIA=1
          ELSE                                   ! COQUE
            TETA=90.-ANG                         ! Calculé pour Subr Hughes
            XF(IS)=XI(IS)+Q*(DSIN((TETA-PHIL)*PI1)-DSIN(TETA*PI1)) ! 7-5-96
          ENDIF                                                         
        ENDIF
        XXI(IS)=XI(IS)
        XXF(IS)=XF(IS)
  195 CONTINUE

      READ(55,*,END=900) PLOC

C 2.2 PANNEAU MODIFIE CAR PHIL>0 N'EST PAS COMPATIBLE AVEC HYDROS (ISIGN=-1)
C     ----------------------------------------------------------------
      IF(PHIL.LE.0.) GOTO 80
      TETA=-ANGLE(NEL)-90.
      NSIGN(NEL)=-1
        DO 196 I=1,NSOL
        IF(IVARIA.EQ.0) GOTO 193
        XI(I)=XF(I)
        XF(I)=XXI(I)
        GOTO 194
  193   XI(I)=XI(I)+Q*(DSIN((TETA-PHIL)*PI1)-DSIN(TETA*PI1))
        XF(I)=XXI(I)              ! Calculé pour subr Hughes, 7-5-96
  194   CONTINUE
  196 CONTINUE
      IF(ANGLE(NEL).LT.-0.00001) GOTO 751
      IF(ANGLE(NEL).GT.0.00001) GOTO 752
      ANGLE(NEL)=180.
      GOTO 753
 751  ANGLE(NEL)=ANGLE(NEL)+180.
      GOTO 753
 752  ANGLE(NEL)=ANGLE(NEL)-180.
 753  CONTINUE
      IF(TYPES.NE.'COQUE') GOTO 81
      TETA=TETA-PHIL
      ANGLE(NEL)=ANGLE(NEL)+PHIL
      GOTO 81
   80 TETA=-ANGLE(NEL)+90.
      NSIGN(NEL)=1
   81 CONTINUE

C 2.3 TEST SUR TETA (-180. < TETA < 180. )
C     --------------------------------------
      IF(DABS(TETA).LT.180.) GOTO 721
      IF(TETA.GE.0.) GOTO 722
      TETA=TETA+360.
      GOTO 721
 722  TETA=TETA-360.
 721  CONTINUE

C 2.4 MISE EN MEMOIRE POUR LA SUBROUTINE EQUIL
C     --------------------------------------
      TETAQ(NEL)=TETA
      PHILN(NEL)=PHIL
      QN(NEL)=Q

C 2.5 MISE EN MEMOIRE POUR LE CALCUL DES COORDONNEES.
C     -----------------------------------------------
      IF(PHIL.LE.0) THEN
      TETAN(NEL)=-ANG+90.
      ELSE
      TETAN(NEL)=-ANG-90.
      ENDIF
C
C 2.6 IMPRESSIONS DIVERSES
C     ---------------------
      IF(LANGUE==1) THEN  ! Francais
        WRITE(66,801) DELTA,WIDTH
        IF(TYPES.NE.'COQUE') THEN
          WRITE(66,803) HIGHT
        ELSE
          WRITE(66,802) PHIL,Q
        ENDIF
        WRITE(66,8) ANG
 	  IF(ITYPE.NE.5) THEN									!février 2004
        WRITE(66,11) EPSA,HYA,DYA,WYA,TYA
        WRITE(66,13) ENTR,HXR,DXR,WXR,TXR,EPSR,MODE,Xmode
	  ELSE													!février 2004
		WRITE(66,804) SECTION,EPSA							!février 2004
		IF(SECTION.EQ.'CERCLE')THEN							!février 2004
	      WRITE(66,806) HYA,EPAIS							!février 2004
	 	ELSEIF(SECTION.EQ.'CARRE')THEN						!février 2004
	      WRITE(66,807) HYA,EPAIS							!février 2004
	 	ELSEIF(SECTION.EQ.'DOUBLET')THEN					!février 2004
	      WRITE(66,808) HYA,DYA,WYA,TYA						!février 2004
	    ENDIF												!février 2004
	  ENDIF											

        IF((EPSA2.GE.(0.00001)).OR.(ENTR2.GE.(0.00001))) THEN
          WRITE(66,14) EPSA2,KSA2,HYA2,DYA2,WYA2,TYA2,
     *                 ENTR2,KSR2,HXR2,DXR2,WXR2,TXR2
        ENDIF

        WRITE(66,1)
        DO IS=1,NSOL
          IF(IVARIA.EQ.1) THEN
            WRITE(66,7)IS,XXI(IS),XXF(IS)
          ELSE
            WRITE(66,5)IS,XXI(IS),XXF(IS)
          ENDIF
        ENDDO

        WRITE(66,16) PLOC
  16  FORMAT(/' PRESSION LOCALE MAXIMALE = ',F8.3,' m. (charge locale,',
     * 'ponctuelle, pour dimensionner les lisses et le bordé)')

      ELSE   ! English
        WRITE(66,901) DELTA,WIDTH
        IF(TYPES.NE.'COQUE') THEN
          WRITE(66,903) HIGHT
        ELSE
          WRITE(66,902) PHIL,Q
        ENDIF
        WRITE(66,'(2A,F7.3,A)')
     *  '    ANGLE at origin (Y=0) between the horizontal right hand',
     *  ' side line and the panel (its tangent at Y=0) = ',ANG,' Deg.'
	  IF(ITYPE.NE.5) THEN									!février 2004
        WRITE(66,904) EPSA,HYA,DYA,WYA,TYA
        WRITE(66,905) ENTR,HXR,DXR,WXR,TXR,EPSR,MODE,Xmode
	  ELSE													!février 2004
		WRITE(66,805) SECTION,EPSA							!février 2004
		IF(SECTION.EQ.'CERCLE')THEN							!février 2004
	      WRITE(66,809) HYA,EPAIS							!février 2004
	 	ELSEIF(SECTION.EQ.'CARRE')THEN						!février 2004
	      WRITE(66,810) HYA,EPAIS							!février 2004
	 	ELSEIF(SECTION.EQ.'DOUBLET')THEN					!février 2004
	      WRITE(66,811) HYA,DYA,WYA,TYA						!février 2004
	    ENDIF												!février 2004
	  ENDIF

        IF((EPSA2.GE.(0.00001)).OR.(ENTR2.GE.(0.00001))) THEN
          WRITE(66,906) EPSA2,KSA2,HYA2,DYA2,WYA2,TYA2,
     *                  ENTR2,KSR2,HXR2,DXR2,WXR2,TXR2
        ENDIF

        WRITE(66,'(/A/1X,40(1H+))')
     *           ' LATERAL HYDROSTATIQUE PRESSURE ON THE PANEL'
        DO IS=1,NSOL
          IF(IVARIA.EQ.1) THEN
            WRITE(66,907)NNSOL(IS),XXI(IS),XXF(IS)
          ELSE
            WRITE(66,908)NNSOL(IS),XXI(IS),XXF(IS)
          ENDIF
        ENDDO

        WRITE(66,'(/A,F8.3,A,A/)') ' MAXIMUM LOCAL PRESSURE =',PLOC,
     *     ' m. (local load, concentrated,',
     *     ' for sizing of stiffeners and plate thickness).' 

	ENDIF

C 2.7  LECTURE DES CHARGES VARIABLES SELON OX
C     -------------------------------------------
      READ(55,*,END=900)ICHA
        CALL ANNULI(NPT,NSOL)
        CALL ANNULD(CHA,100*3*ISmax)
	  CALL ANNULD(dCHA,100*ISmax*9)    !!!aout04
      IF(ICHA.EQ.0) GOTO 210
      IS=0
      DO 215 I=1,NSOLM
        ICC=0
        DO K=1,NSOL
          IF (NNSOL(K).EQ.I) ICC=1   ! ICC=1 cas de charge retenu
	  ENDDO
        IF (ICC.EQ.0) THEN           ! ICC=0 cas de charge pas retenu
          READ(55,*,END=900) GFH    ! titre
          READ(55,*,END=900) ITEMP   ! NPT
          DO   J=1,ITEMP
            READ(55,*,END=900) TEMP
          ENDDO
          GOTO 215
	  ELSE
	    IS=IS+1
	  ENDIF

        READ(55,*) GFH        ! Ligne titre, ex: "Cas de charge 3"
        READ(55,*,END=900) NPT(IS)
        WRITE(66,212)NNSOL(IS),NPT(IS)
	  IF(NPT(IS).GT.100) THEN 
	    WRITE(*,*)  ' *** ERROR IN PANEL ',NEL
	    WRITE(*,*)  ' CHARGES VARIABLES SELON OX (Max=100 pas)'
	    WRITE(29,*)  ' *** ERROR IN PANEL ',NEL									!bug
	    WRITE(29,*)  ' CHARGES VARIABLES SELON OX (Max=100 pas)'				!bug
 	    WRITE(666,*)' CHARGES VARIABLES SELON OX (Max=100 pas)'
 	    WRITE(666,*)' Load distributed along OX (Max=100 steps)'
  	    IFF=1
	    PAUSE 'Error'
	  ENDIF
        WRITE(66,213)
        DO  J=1,NPT(IS)
          READ(55,*,END=900) (CHA(J,IJ,IS),IJ=1,3)
        ENDDO
        IF(KSE.EQ.0) THEN
            KSE=2
            XI(IS)=0.
            XF(IS)=0.
            CALL ANNULD(CHA(1,2,IS),NSOL)
            CALL ANNULD(CHA(1,3,IS),NSOL)
            WRITE(66,218)NEL
            WRITE(29,218)NEL														!bug
            WRITE(66,7)NNSOL(IS),XI(IS),XF(IS)
            WRITE(*,*) 'Panneau n° ',NEL
            WRITE(*,*) ' KSE=0 est incompatible avec ICHA=1'
            WRITE(*,*) ' Nous proposons de prendre KSE=2 avec'
            WRITE(*,*) '       XI=XF=0  et  CHA2=CHA3=0 '
            WRITE(*,*)' Si OUI taper "0"'
            WRITE(*,*)' Si NON taper "1" '
            WRITE(*,*)'    (c.à.d STOP + corriger vos données)'
            READ (*,*) IFF

		  WRITE(29,*) 'Panneau n° ',NEL											!bug	
            WRITE(29,*) ' KSE=0 est incompatible avec ICHA=1'						!bug
            WRITE(29,*) ' Nous proposons de prendre KSE=2 avec'					!bug
            WRITE(29,*) '       XI=XF=0  et  CHA2=CHA3=0 '						!bug
	      WRITE(29,*)'    (c.à.d STOP + corriger vos données)'					!bug
 
        ENDIF
        DO J=1,NPT(IS)
          WRITE(66,214) J,CHA(J,1,IS),CHA(J,2,IS),CHA(J,3,IS)
        ENDDO
 215  CONTINUE

 210  CONTINUE

C 2.8 LECTURE DES NOH
C     -----------------
      READ(55,*,END=900) (NOH(NEL,J),J=1,10)
      WRITE(66,*)
	IF(LANGUE==1) THEN
       WRITE(66,*)'Liste des panneaux (NOH) qui suivent celui-ci: '
	ELSE
       WRITE(66,*)'List of the panels (NOH) that follows this one:'
	ENDIF
      WRITE(66,*)'------------------------------------------------'
      WRITE(66,'(10I3)') (NOH(NEL,J),J=1,10)

C 2.9 Lectures des TRAVERSES
C     ------------------------
      IF(MT.EQ.0) GOTO 41
      DO 2001 I=1,MT
        READ(55,*,END=900) HXTR(I),DXTR(I),WXTR(I),TXTR(I),ABTR(I)
	  IF (ABTR(I).GT.DABS(PHIL)*Q*PI1.OR.ABTR(I).LT.0) THEN           !juillet2003
	    WRITE(*,*)                                                    !juillet2003
	    WRITE(*,*)'ATTENTION : ERROR dans le Panneau ',NEL            !juillet2003
	    WRITE(*,*)'****************************************'          !juillet2003
	    WRITE(*,*)'Une traverse n''est pas positionnée sur le panneau'!juillet2003
	    WRITE(*,*)'Vérifiez vos données'                              !juillet2003
	    
		WRITE(29,*)																!bug
	    WRITE(29,*)'ATTENTION : ERROR dans le Panneau ',NEL						!bug
	    WRITE(29,*)'****************************************'					!bug
	    WRITE(29,*)'Une traverse n''est pas positionnée sur le panneau'			!bug
	    WRITE(29,*)'Vérifiez vos données'										!bug
	    
		STOP                                                          !juillet2003
        ENDIF                                                           !juillet2003
        ABTR2(I)=ABTR(I)
 2001 CONTINUE
      IF(LANGUE==1) WRITE(66,39)
      IF(LANGUE==2) WRITE(66,909)
      DO 40 I=1,MT
        WRITE(66,441) I,ABTR(I), TXTR(I), WXTR(I), DXTR(I), HXTR(I)
        ABTR(I)=ABTR(I)/(PI1*Q)
        IF(PHIL.GT.0.) ABTR(I)=PHIL-ABTR(I)
  40  CONTINUE
  41  CONTINUE

C 2.10 PHIL=DABS(PHIL)
C     ----------------
      PHIL=DABS(PHIL)

C 2.11 LES 4 CAS DE BASE POUR LES EFFETS DE BORDS A(Y**3)+B(Y**2)+C*Y+D
C      ----------------------------------------------------------------
      QPHIL=Q*PHIL*PI1
      D=1000.
      C=D/QPHIL
      B=C/QPHIL
      A=B/QPHIL

      DELTO=WIDTH*0.05
      IF(IBUSC.NE.0) THEN
        WRITE(66,6) DELTO
      ENDIF

C ______________________________________________________________________
C ______________________________________________________________________
C 3.    LECTURE DE VARIABLES DE CONCEPTION , XI, XIMAX et XIMIN
C     ==========================================================
	READ(55,*,END=900) GFH								   ! TITRE
      READ(55,*,END=900) NBRXI                               ! NBRXI
c      IF(NBRXI.GE.1) THEN
c        READ(55,*,END=900) (NXI(I),I=1,NBRXI)                ! NXI
c	  IF(ITERA.EQ.0)THEN				!13.12.05
c          READ (55,*,END=900) (XIMIN(I),I=NTOT+1,NTOT+NBRXI) ! XI min
c          READ (55,*,END=900) (XIMAX(I),I=NTOT+1,NTOT+NBRXI) ! XI max
cc	    IF(IPRINT.GE.1) THEN
cc     	  WRITE(*,*) 'PANNEAU N°',NEL 
cc           WRITE(*,'(A7,9E12.5)')'XIMAX=',(XIMAX(I),I=NTOT+1,NTOT+NBRXI) ! XI max
cc           WRITE(*,'(A7,9E12.5)')'XIMIN=',(XIMIN(I),I=NTOT+1,NTOT+NBRXI) ! XI max
cc	    ENDIF
c	  ELSE
cC         ITERA>1 : On conserve les XIMIN et XIMAX modifiés dans subr. OPTI
c          READ (55,*,END=900) TEMP                           ! XI min
c          READ (55,*,END=900) TEMP                           ! XI max
c	  ENDIF
c	ENDIF	

	IF(NBRXI.NE.0) READ(55,*,END=900) (NXI(I),I=1,NBRXI)	!eugen
	IF(ITERA.EQ.0)THEN										!13.12.05
	  READ (55,*,END=900) (DVMIN(I),I=1,9)					!eugen
	  READ (55,*,END=900) (DVMAX(I),I=1,9)					!eugen
	  DO I=1,NBRXI											!eugen					
	    XIMIN(NTOT+I)=DVMIN(NXI(I))							!eugen
	    XIMAX(NTOT+I)=DVMAX(NXI(I))							!eugen
	  ENDDO													!eugen
c	  IF(IPRINT.GE.1) THEN
c         WRITE(*,*) 'PANNEAU N°',NEL 
c         WRITE(*,'(A7,9E12.5)')'XIMAX=',(XIMAX(I),I=NTOT+1,NTOT+NBRXI) ! XI max
c         WRITE(*,'(A7,9E12.5)')'XIMIN=',(XIMIN(I),I=NTOT+1,NTOT+NBRXI) ! XI max
c	  ENDIF
	ELSE
C       ITERA>1 : On conserve les XIMIN et XIMAX modifiés dans subr. OPTI
        READ (55,*,END=900) TEMP                           ! XI min
        READ (55,*,END=900) TEMP                           ! XI max
	ENDIF

      IF(IOPTI.EQ.0) NBRXI=0

C 3.1 Vérification des variables de conception : XIMIN < XI < XIMAX
C     Et sauvetage des variables de conception dans le vecteur XICOU(N)
C     (lors de la première itération) 
C     ----------------------------------------------------------------------
      IF((ITERA.EQ.0).AND.(IOPTI.GE.1)) THEN		!13.12.05
        WRITE(666,283) NEL,NBRXI
        WRITE(666,'(A,9I2)') 'Soit les variables locales:',
     *                         (NXI(I),I=1,NBRXI)
        DO 131 I=NTOT+1,NTOT+NBRXI
	   IF(ITYPE.NE.5) THEN					!si plaques,coques		!février 2004
          GOTO(134,135,141,136,137,138,142,139,140),NXI(I-NTOT)
  134     CALL BORNE(DELTA,I,'= épaisseur bordage          ',
     *               NEL,XIMIN,XIMAX,IFF,IMPR2)		!15.10.05
          XICOU(I)=DELTA
          GOTO 131
  135     CALL BORNE(HYA,I,  '= hauteur âme transversaux   ',
     *               NEL,XIMIN,XIMAX,IFF,IMPR2)		!15.10.05
          XICOU(I)=HYA
          GOTO 131
  141     CALL BORNE(DYA,I,  '= épaisseur âme transversaux ',
     *               NEL,XIMIN,XIMAX,IFF,IMPR2)		!15.10.05
          XICOU(I)=DYA
          GOTO 131
  136     CALL BORNE(WYA,I,  '= largeur semelle transvers. ',
     *               NEL,XIMIN,XIMAX,IFF,IMPR2)		!!15.10.05
          XICOU(I)=WYA
          GOTO 131
  137     CALL BORNE(EPSA,I, '= larg. effect. transversaux ',
     *               NEL,XIMIN,XIMAX,IFF,IMPR2)		!15.10.05
          XICOU(I)=EPSA
          GOTO 131
  138     CALL BORNE(HXR,I,  '= hauteur âme raidisseurs    ',
     *               NEL,XIMIN,XIMAX,IFF,IMPR2)		!15.10.05
          XICOU(I)=HXR
          GOTO 131
  142     CALL BORNE(DXR,I,  '= épaisseur âme raidisseurs  ',
     *               NEL,XIMIN,XIMAX,IFF,IMPR2)		!15.10.05
          XICOU(I)=DXR
          GOTO 131
  139     CALL BORNE(WXR,I,  '= largeur semelle raidisseurs',
     *               NEL,XIMIN,XIMAX,IFF,IMPR2)		!15.10.05
          XICOU(I)=WXR
          GOTO 131
  140     CONTINUE  ! Entredistance entre raidisseurs ENTR et EPSR
          WRITE(666,'(A,I3,A)') 'Var. nø',I,': ENTR et EPSR'
          WRITE(666,'(A,E11.4,A,E11.4,A,E11.4)') 
     *              'Pour ENTR, les limites min et max sont:',
     *               XIMIN(I),' < ENTR=',ENTR,' < ',XIMAX(I)      
          WRITE(666,*) 'Mode =',MODE,' et Xmode =',Xmode
	    Temp= Xmode/(DABS(PHIL)*Q*PI1)
          DENOM1 = (1.0/XIMAX(I)  + Temp )
          DENOM2 = (1.0/XIMIN(I)  + Temp )
 	    IF((DENOM1.LT.0.0).OR.(DENOM2.LE.0.0)) THEN
	      WRITE(666,*)
	      WRITE(666,*)'ATTENTION : ERROR dans le Panneau ',NEL
	      WRITE(666,*)'****************************************'
            WRITE(666,*)'Le paramètre MODE est incompatible avec les'
	      WRITE(666,*)'bornes de variation de l''entredistance entre'
	      WRITE(666,*)'les raidisseurs et la largeur du panneau.'
	      WRITE(666,*)'Vous devez corriger cette situation !!'
	      WRITE(666,*)'Essayer en modifiant le paramère MODE'
	      WRITE(666,*)' - actuellement on a MODE =',MODE
	      WRITE(666,*)' - prenez plutôt     MODE = EE2'
	      WRITE(*,*)
	      WRITE(*,*)'ATTENTION : ERROR dans le Panneau ',NEL
	      WRITE(*,*)'****************************************'
            WRITE(*,*)'Le paramètre MODE est incompatible avec les'
	      WRITE(*,*)'bornes de variation de l''entredistance entre'
	      WRITE(*,*)'les raidisseurs et la largeur du panneau.'
	      WRITE(*,*)'Vous devez corriger cette situation !!'
	      WRITE(*,*)'Essayer en modifiant le paramère MODE'
	      WRITE(*,*)' - actuellement on a MODE =',MODE
	      WRITE(*,*)' - prenez plutôt     MODE = EE2'
	      
		  WRITE(29,*)															!bug
	      WRITE(29,*)'ATTENTION : ERROR dans le Panneau ',NEL					!bug
	      WRITE(29,*)'****************************************'					!bug
            WRITE(29,*)'Le paramètre MODE est incompatible avec les'				!bug
	      WRITE(29,*)'bornes de variation de l''entredistance entre'			!bug
	      WRITE(29,*)'les raidisseurs et la largeur du panneau.'				!bug
	      WRITE(29,*)'Vous devez corriger cette situation !!'					!bug
	      WRITE(29,*)'Essayer en modifiant le paramère MODE'					!bug
	      WRITE(29,*)' - actuellement on a MODE =',MODE							!bug
	      WRITE(29,*)' - prenez plutôt     MODE = EE2'							!bug
	      
		  PAUSE 'ERROR'
	      IFF=1
	    ENDIF
 	    IF (DENOM1.EQ.0.) THEN
	      WRITE(666,*)
	      WRITE(666,*)'ATTENTION : ERROR dans le Panneau ',NEL
	      WRITE(666,*)'****************************************'
            WRITE(666,*)'La borne MAXI de EPSR est incompatible avec'
	      WRITE(666,*)'le parametre MODE et la largeur du panneau.'
	      WRITE(666,*)'Vous devez corriger cette situation !!'
	      WRITE(666,*)'Essayer en modifiant le paramère MODE'
	      WRITE(666,*)' - on avait       XIMAX =',XIMAX(I)
	      WRITE(*,*)  'ATTENTION : ERROR dans le Panneau ',NEL
	      WRITE(*,*)  '****************************************'
	      WRITE(*,*)  ' - on avait       XIMAX =',XIMAX(I)
	      XIMAX(I)=XIMAX(I)*0.99
            DENOM1 = (1.0/XIMAX(I)  + Temp )
	      WRITE(666,*)' - on change pour XIMAX =',XIMAX(I)
	      WRITE(*,*)  ' - on change pour XIMAX =',XIMAX(I)
		  
		  WRITE(29,*)															!bug
	      WRITE(29,*)'ATTENTION : ERROR dans le Panneau ',NEL					!bug
	      WRITE(29,*)'****************************************'					!bug
            WRITE(29,*)'La borne MAXI de EPSR est incompatible avec'				!bug
	      WRITE(29,*)'le parametre MODE et la largeur du panneau.'				!bug
	    
		ENDIF

          WRITE(666,*)'Pour EPSR (vrai variable de conception), on a:'
          XIMAX(I)= 1.0/ DENOM1
          XIMIN(I)= 1.0/ DENOM2
          CALL BORNE(EPSR,I, '= larg. effect. raidisseurs  ',
     *               NEL,XIMIN,XIMAX,IFF,IMPR2)		!15.10.05
          XICOU(I)=EPSR
	   ELSE					! pour l'épontille							!février 2004
	    GOTO(143,144,145,146,147),NXI(I-NTOT)							!février 2004
  143     CALL BORNE(HYA,I,  '= 1/2 hauteur âme, diam,côté ',				!février 2004
     *               NEL,XIMIN,XIMAX,IFF,IMPR2)		!15.10.05
          XICOU(I)=HYA													!février 2004
          GOTO 131														!février 2004						
  144     CALL BORNE(DYA,I,  '= épaisseur âme épont.doubleT',				!février 2004
     *               NEL,XIMIN,XIMAX,IFF,IMPR2)		!15.10.05
          XICOU(I)=DYA													!février 2004
          GOTO 131														!février 2004
  145     CALL BORNE(WYA,I,  '= largeur semel.épont.doubleT',				!février 2004
     *               NEL,XIMIN,XIMAX,IFF,IMPR2)		!15.10.05
          XICOU(I)=WYA													!février 2004
          GOTO 131														!février 2004
  146		CALL BORNE(EPAIS,I,'=épaisseur paroi mince		 ',				!février 2004
     *               NEL,XIMIN,XIMAX,IFF,IMPR2)		!15.10.05
		XICOU(I)=EPAIS													!février 2004
		GOTO 131														!février 2004
  147     CALL BORNE(EPSA,I, '= larg. effect. épontille	 ',				!février 2004
     *               NEL,XIMIN,XIMAX,IFF,IMPR2)		!15.10.05
          XICOU(I)=EPSA													!février 2004
          GOTO 131														!février 2004
	   ENDIF															!février 2004
  131   CONTINUE
        IF((NTOT+NBRXI).GT.M1Tmax) THEN
          WRITE(*,*)  'Le nombre MAX de variables de conception est',
     *                ' dépassé :NTOT>NVmax ',NTOT,' > ',NVmax
          WRITE(666,*)'Le nombre MAX de variables de conception est',
     *                ' dépassé :NTOT>NVmax ',NTOT,' > ',NVmax
          WRITE(29,*)'Le nombre MAX de variables de conception est',				!bug
     *                ' dépassé :NTOT>NVmax ',NTOT,' > ',NVmax					!bug

          PAUSE 'ERROR'
          IFF=1 !stop à la sortie de la Subr ENT
        ENDIF
      ENDIF

C 3.2 Restrictions Structurelles: Lecture des IPT (points de calcul)
C     ----------------------------------------------------------------
      READ (55,*,END=900) GFH	   ! TITRE sur Restrictions Structurelles:
      READ (55,*,END=900) IM1    ! IM1 (nbre de restrictions struct.du panneau NEL)

C     Il faut tjrs avoir YPT(1)=0. et YPT(IPT)=PHIL (cfr subr. MDR2)
C     Donc IPT(min) = 2   et IPT(max) = 10

	IF(IM1.EQ.0) THEN  !IM1=0 : pas de restrictions structurelles
	  IPT(NEL)=2
	  GOTO 219    ! Pas de Restr Struc. pour ce panneau: Goto End of this section.

	ELSE IF(IM1.EQ.1) THEN  
        READ (55,*,END=900) IPT(NEL)    ! IPT(NEL)=nbre de pts pour le calcul des restrictions
	  IF(IOPTI.EQ.0) GOTO 110
          IF((IPT(NEL).GT.4).OR.(IPT(NEL).LT.2)) THEN  
		  WRITE(*,*)'**** Panneau ',NEL
		  WRITE(*,*)'IPT=',IPT(NEL),
     *                ' EST INCORRECTE, il faut 2=<IPT=<4)'
		  WRITE(666,*)'IPT=',IPT(NEL),
     *                  ' EST INCORRECTE, il faut 2=<IPT=<4)'
		  WRITE(29,*)'IPT=',IPT(NEL),											!bug
     *                  ' EST INCORRECTE, il faut 2=<IPT=<4)'						!bug
		  PAUSE 'ERROR'
		  IFF=1
          ENDIF  
 110    CONTINUE

        ! YPT = en valeur relative c.à.d.  (0<YPT<1) pour les donnees
	  !       mais en valeur relle pour le programme  0<YPT<Q*PHIL 
        READ (55,*,END=900) (YPT(I,NEL),I=1,IPT(NEL))  ! YPT (Ordonnées relatives des pts)

	  IF(IOPTI.GE.1) THEN
	    IF((YPT(1,NEL).GE.0.01).OR.(YPT(IPT(NEL),NEL).LE.0.99))THEN
            WRITE(666,431) ! message d'erreur
            WRITE(* , 431) 
		  WRITE(29, 431)														!bug
		ENDIF
	    YPT(1,NEL)  =0.0			
	    YPT(IPT(NEL),NEL)=1.0          
          ! Calcul des coordonnées réelles (= angle en degrés)
          DO I =1,IPT(NEL)
	      YPT(I,NEL)=YPT(I,NEL)*DABS(PHIL)  ! (Ordonnées ang. réelles des pts)
	    ENDDO
c	    IF(IPRINT.GE.1) THEN
c	      WRITE(*,*)' Aux pts de coord relatives YPT=',
c     *      (YPT(I,NEL),I=1,IPT(NEL)),' (% de la largeur du panneau)'
c         ENDIF
        ENDIF

      ELSE IF ((IM1.LE.-NEL).OR.(IM1.GT.1)) THEN
	  WRITE(*,*)    ' *** PANNEAU',NEL
	  WRITE(*,*)  ' Fausse valeur de IM1 =',IM1,'=< ',-NEL
	  WRITE(*,*)  ' ou IM1 > 1'
 	  WRITE(*,*)  ' Wrong value for IM1 =',IM1,'=< ',-NEL
	  WRITE(*,*)  ' or IM1 > 1'
 	  WRITE(666,*)' Wrong value for IM1 =',IM1,'=< ',-NEL
	  WRITE(666,*)' or IM1 > 1'
 	  
	  WRITE(29,*)    ' *** PANNEAU',NEL											!bug
	  WRITE(29,*)  ' Fausse valeur de IM1 =',IM1,'=< ',-NEL						!bug
	  WRITE(29,*)  ' ou IM1 > 1'												!bug
 	  
	  IFF=1
	  PAUSE 'Error'
	  RETURN

	ELSE    ! IM1<0 cad mêmes restrictions que celles du panneau IM1
	  IM1=-IM1
        IPT(NEL)=IPT(IM1)
        DO I=1,IPT(NEL)
	     YPT (I,NEL)=YPT (I,IM1)*PHILN(NEL)/PHILN(IM1)
	     IPT2(I,NEL)=IPT2(I,IM1)
	     IPT3(I,NEL)=IPT3(I,IM1)
	  ENDDO
	  DO IS=1,NSOL    ! IS = N° d'un cas de charge selectionné
	    IJ=NNSOL(IS)  ! IJ = N° de ce cas de charge comme defini dans les donnees
	    M1=M1TABL(IM1,IS)
          M1TABL(NEL,IS)=M1
          M1CONT=M1CONT+M1  ! nbre de restriction pour tout les cas de charges (pour le panneau NEL)
 	    M1TOT =M1TOT +M1  ! nbre de restriction pour tout les cas de charges (pour la structure)
          IF(ITERA.EQ.0) THEN			!13.12.05
            DO  I=1,M1
              LCONT(IJ,NEL,I,1)=LCONT(IJ,IM1,I,1)
		    LCONT(IJ,NEL,I,2)=LCONT(IJ,IM1,I,2)
		    CJMAX(IJ,NEL,I)  =CJMAX(IJ,IM1,I)
              INV  (IJ,NEL,I)  =INV  (IJ,IM1,I)
            ENDDO
	      ! vérification de la compatibilité entre variable XI et restriction 4 (dmin)
	      IF((LCONT(IJ,NEL,I,1).EQ.4).AND.(NXI(1).NE.1)) THEN
	        WRITE(*  ,220)
	        WRITE(*  ,220)
	        WRITE(*  ,220)
	        WRITE(*  ,220)
	        WRITE(666,220)
	        WRITE(66 ,220)
			write(29 ,220)														!bug
	      ENDIF	
	      ! vérification du choix des points de calcul des sensibilités vis à vis de traverses		
            IF(MT.GE.1) THEN
              IF((LCONT(IJ,NEL,I,1).GE.40).and.
     *          (LCONT(IJ,NEL,I,1).LE.45))     THEN
                DO IT=1,MT
                  AX=DABS( ABTR(IT)/YPT(LCONT(IJ,NEL,I,2),NEL) )   !
                  IF(AX.GE.(0.99)) THEN      !  IPT3= Nø de la TRAV. CONCERNEE
                    IF(AX.LE.(1.01)) IPT3(LCONT(IJ,NEL,I,2),NEL)=IT  !           
	            ENDIF
                ENDDO
	        ENDIF
	      ENDIF

	    ENDIF
	  ENDDO
	  GOTO 219
	ENDIF   ! IF(IM1.EQ.0) THEN 


	IF((IOPTI.GE.1).AND.(ITERA.EQ.0)) THEN		!13.12.05
         DO I=1,IPT(NEL)                                       ! Pt. de calcul des restrictions
           IPT2(I,NEL)=IDNINT(30.*YPT(I,NEL)/DABS(PHIL)) + 1   ! c.à.d. un pt de 1 à 31
         ENDDO
c	   IF(IPRINT.GE.1) WRITE(*,*)'aux pts IPT2=',(IPT2(I,NEL),I=1,IPT(NEL))
	ENDIF

C 3.3 Lecture des M1 restrictions structurelles et de leur borne
C     -----------------------------------------------------------
      IF(ITERA.EQ.0) CALL ANNULI (IPT3(1,NEL),IPTmax)       !  !13.12.05      IPT3=0

      IS=0  ! IS = compteur des cas de charge selectionnés (1 à Nsol)

      DO 216 IJ=1,NSOLM  ! IJ = compteur des cas de charge, selectionnés ou non (1 à NSOLM)
                         ! IS = compteur des cas de charge selectionnés (1 à Nsol)
      M11(IJ)=0  ! mise à zéro
      ICC=0      ! ICC=0 cas de charge non selectionné

      DO K=1,NSOL
        IF(NNSOL(K).EQ.IJ) THEN
		 ICC=1   ! ICC=1 cas de charge selectionné
	     IS=IS+1
           M1TABL(NEL,IS)=0  ! mise à zéro
	  ENDIF
	ENDDO

      READ (55,*,END=900) GFH		! TITRE cas de charge
      READ (55,*,END=900) M1        ! M1 (nbre de restrictions struct.du panneau NEL)

C     Lecture de la liste des restrictions (En fct de la valeur de M1)
c     LCONT(NSOLM,NETO,M1max,2),INV(NSOLM,NETO,M1max),CJMAX(NSOLM,NETO,M1max)

	IF(M1.GT.20) THEN 
	  WRITE(*,*)  ' *** PANNEAU',NEL
	  WRITE(*,*)  ' Nbre de restr. struct. incorrecte (Max=20) =',M1
 	  WRITE(666,*)' Nbre de restr. struct. incorrecte (Max=20) =',M1
 	  WRITE(666,*)' Too much Struct. Const. (Max=20 per panel) =',M1
	  WRITE(29,*)  ' *** PANNEAU',NEL													!bug
	  WRITE(29,*)  ' Nbre de restr. struct. incorrecte (Max=20) =',M1					!bug
  	  IFF=1
	  PAUSE 'Error'
	  RETURN

      ELSE IF (M1.EQ.0) THEN
	  GOTO 216          ! vers cas de charge suivant

      ELSEIF((M1.GE.1).AND.(M1.LE.20)) THEN
        IF(ICC.EQ.1) M1TABL(NEL,IS)=M1
	  M11(IJ)       =M1
c	  WRITE(66,*) ' NEL,IS,M1,M1CONT,M1TABL(NEL,IS)=',
c    *                NEL,IS,M1,M1CONT,M1TABL(NEL,IS)
        IF(ITERA.EQ.0) THEN			!13.12.05
          DO  I=1,M1
          READ (55,*,END=900) ICN,LCONT(IJ,NEL,I,1),CJMAX(IJ,NEL,I),
     *                            INV(IJ,NEL,I),  LCONT(IJ,NEL,I,2)  
c         ICN,  LCONT(IJ,NEL,I,1),CJMAX(IJ,NEL,I),INV(IJ,NEL,I),LCONT(IJ,NEL,I,2)  
C         Ordre,  Type               ,C(max)  ,   INV   ,      Nø du pt de calcul IPT
          ENDDO
	  ELSE
          DO  I=1,M1
            READ (55,*,END=900) TEMP
	    ENDDO
	  ENDIF

      ELSEIF((M1.LE.-1).AND.(M1.GT.-IJ))  THEN
          ISS=-M1    ! ISS cas de charge de référence
	    M1=M11(ISS)
	    M11       (IJ)=M1
          IF(ICC.EQ.1) M1TABL(NEL,IS)=M1
          IF(ITERA.EQ.0) THEN			!13.12.05
            DO  I=1,M1
              LCONT(IJ,NEL,I,1)=LCONT(ISS,NEL,I,1)
		    CJMAX(IJ,NEL,I)  =CJMAX(ISS,NEL,I)
              INV  (IJ,NEL,I)  =INV  (ISS,NEL,I)
		    LCONT(IJ,NEL,I,2)=LCONT(ISS,NEL,I,2)
            ENDDO
	     ENDIF

      ELSEIF (M1.LE.-IJ) THEN
	  WRITE(*,*)  ' *** PANNEAU',NEL
	  WRITE(*,*)  ' Fausse valeur de M1 =',M1,'=< ',-IJ
 	  WRITE(*,*)  ' Wrong value for M1  =',M1,'=< ',-IJ
 	  WRITE(666,*)' Wrong value for M1  =',M1,'=< ',-IJ
	  WRITE(29,*)  ' *** PANNEAU',NEL												!bug
	  WRITE(29,*)  ' Fausse valeur de M1 =',M1,'=< ',-IJ							!bug
  	  IFF=1
	  PAUSE 'Error'
	  STOP
      ENDIF


      IF (ICC.EQ.1) THEN 
        M1CONT=M1CONT+M1  ! Nbre de restriction pour tous les cas de charges (pour le panneau NEL)
	  M1TOT =M1TOT +M1  ! Nbre de restriction pour tous les cas de charges et toute la structure
      ENDIF


	IF((IOPTI.GE.1).AND.(ITERA.EQ.0).AND.(ICC.EQ.1)) THEN		!13.12.05
       DO 217  I=1,M1
	  ! vérification de la compatibilité entre variable XI et restriction 4 (dmin)
	  IF((LCONT(IJ,NEL,I,1).EQ.4).AND.(NXI(1).NE.1)) THEN
	    WRITE(*  ,220)
	    WRITE(*  ,220)
	    WRITE(*  ,220)
	    WRITE(*  ,220)
	    WRITE(666,220)
	    WRITE(66 ,220)
		WRITE(29 ,220)																!bug
	  ENDIF	
	  ! vérification du choix des points de calcul
  	  IF(LCONT(IJ,NEL,I,2).GT.IPT(NEL)) THEN
	    YP=LCONT(IJ,NEL,I,2)
	    WRITE(*  ,*)'IY =',YP,' > IPT=',IPT(NEL),' EST INCORRECTE'
	    WRITE(666,*)'IY =',YP,' > IPT=',IPT(NEL),' EST INCORRECTE'
		WRITE(29 ,*)'IY =',YP,' > IPT=',IPT(NEL),' EST INCORRECTE'					!bug
	    PAUSE 'STOP'
	    IFF=1
	  ENDIF

c	  ITEM=IPT2(LCONT(IJ,NEL,I,2),NEL)
c       IF(IPRINT.GE.1)THEN
c         IF(INV(I).EQ.-1)THEN
c           WRITE(*,427)I,CJMAX(I),LCONT(IJ,NEL,1,I),LCONT(IJ,NEL,2,I),ITEM
c         ELSE
c           WRITE(*,428)I,CJMAX(I),LCONT(IJ,NEL,1,I),LCONT(IJ,NEL,2,I),ITEM
c         ENDIF
c       ENDIF

	  ! vérification du choix des points de calcul des sensibilités vis à vis de traverses		
        IF(MT.GE.1) THEN
          IF((LCONT(IJ,NEL,I,1).GE.40).and.
     *       (LCONT(IJ,NEL,I,1).LE.45))     THEN
             DO IT=1,MT
               AX=DABS( ABTR(IT)/YPT(LCONT(IJ,NEL,I,2),NEL) )   !
               IF(AX.GE.(0.99)) THEN         !  IPT3= Nø de la TRAV. CONCERNEE
                 IF(AX.LE.(1.01)) IPT3(LCONT(IJ,NEL,I,2),NEL)=IT  !           
	         ENDIF
             ENDDO
	    ENDIF
	  ENDIF

 217	 CONTINUE
      ENDIF  ! 	IF((IOPTI.GE.1).AND.(ITERA.EQ.1)) THEN
 
 216  CONTINUE   ! Boucle sur les cas de charges

 219	CONTINUE  ! vient de GOTO (2)
 

c     Save List of constraints in Subr. ENT to be re-used in Subr. CONTR
c       OPEN(78,STATUS='SCRATCH',FORM='UNFORMATTED') in main
      IF((IOPTI.GE.1).AND.(ITERA.EQ.0).AND.(NEL.EQ.NETO)) THEN		!13.12.05
        DO  IS=1,NSOL
	    IJ=NNSOL(IS)
          DO  IP=1,NETO
c	      WRITE(67,*)' NEL,IS,M1TABL(IP,IS)',IP,IS,M1TABL(IP,IS) 
            DO  I=1,M1TABL(IP,IS)
               WRITE(78) LCONT(IJ,IP,I,1),LCONT(IJ,IP,I,2),
     *                   INV(IJ,IP,I),CJMAX(IJ,IP,I)
c              WRITE(67,*) LCONT(IJ,IP,I,1),LCONT(IJ,IP,I,2), 
c     *                  INV(IJ,IP,I),CJMAX(IJ,IP,I)
	      ENDDO
	    ENDDO
	  ENDDO
	ENDIF


      IF(IOPTI.GE.1) THEN
	   IA=M1TOT+M2TOT
	   IF(IA.GT.Mmax) THEN
           WRITE(*,*)'Le nombre MAX de restrictions est dépassé !!'
           WRITE(*,*)'Too much much constraints are considered  !!'
           WRITE(*,*) M1TOT,' Restr. struct. / Struct. Constraints'
           WRITE(*,*) M2TOT,' Restr. géom. /Geometrical Constraints'
           WRITE(*,*) 'CHANGE YOUR INPUT DATA (données)'
           WRITE(666,*)'Le nombre MAX de restrictions est dépassé !!'
           WRITE(666,*)'Too much much constraints are considered  !!'
           WRITE(666,*) M1TOT,' Restr. struct. / Struct. Constraints'
           WRITE(666,*) M2TOT,' Restr. géom. /Geometrical Constraints'
           WRITE(666,*) 'CHANGE YOUR INPUT DATA (données)'
		 WRITE(29 ,*)'Le nombre MAX de restrictions est dépassé !!'				!bug
           WRITE(29 ,*)'Too much much constraints are considered  !!'				!bug
           WRITE(29 ,*) M1TOT,' Restr. struct. / Struct. Constraints'				!bug
           WRITE(29 ,*) M2TOT,' Restr. géom. /Geometrical Constraints'			!bug
           WRITE(29 ,*) 'CHANGE YOUR INPUT DATA (données)'						!bug
           PAUSE 'ERROR'
           IFF=1
         ENDIF
      ELSE
c        M1=0
         M1CONT=0
      ENDIF


C 3.4 Lecture des M2 Restrictions Géométriques (IBM2 = nø du set de restrictions)
C     ----------------------------------------------------------------------
C   	M2 =  nbre de restr. géom. du panneau NEL

      READ (55,*,END=900) GFH				 ! TITRE
      READ (55,*,END=900) M2			
      IF(M2.EQ.99) THEN
          READ (55,*,END=900) IBM2					
C           avec IBM2  = nø du set de restrictions
          READ (55,*,END=900) ISEMA,ISEMR					
C           avec ISEMA > 0  l'épaisseur des semelles des transversaux est
C                           modifiée automatiquement
C                           ISEMA ou ISEMR =1 : Largeur Semelle < 16 *
C                                 Epaiss. semelle
C                           ISEMA ou ISEMR =2 : Largeur Semelle < 32 *
C                                 Epaiss. semelle
C           avec ISEMR > 0  l'épaisseur des semelles des raidisseurs  est
C                           modifiée automatiquement
          IF(IBM2.EQ.1) THEN
C           Set de restrictions de Hughes (pour profil T)
            M2=11
	      LM2(1)= 109
	      LM2(2)= 110
	      LM2(3)= 111
	      LM2(4)= 112
	      LM2(5)= 209
	      LM2(6)= 210
	      LM2(7)= 211
	      LM2(8)= 212
	      LM2(9) =301
	      LM2(10)=302
	      LM2(11)=303
          ELSE IF(IBM2.EQ.2) THEN
C           Set de restrictions de Hughes + set complémentaire (A) 
            M2=15
	      LM2(1)= 109
	      LM2(2)= 110
	      LM2(3)= 111
	      LM2(4)= 112
	      LM2(5)= 209
	      LM2(6)= 210
	      LM2(7)= 211
	      LM2(8)= 212
	      LM2(9) =301
	      LM2(10)=302
	      LM2(11)=303
	      LM2(12)=104
	      LM2(13)=105
	      LM2(14)=204
	      LM2(15)=205
          ELSE IF(IBM2.EQ.3) THEN
C           Set de restrictions de Hughes (pour cornières)
            M2=11
	      LM2(1)= 115
	      LM2(2)= 116
	      LM2(3)= 111
	      LM2(4)= 112
	      LM2(5)= 209
	      LM2(6)= 215
	      LM2(7)= 216
	      LM2(8)= 212
	      LM2(9) =301
	      LM2(10)=302
	      LM2(11)=303
	    ELSE IF(IBM2.EQ.4) THEN
C           Set de restrictions de RAHMAN
c           ISEMA=1
c           ISEMR=1
            M2=6
	      LM2(1)=101
	      LM2(2)=102
	      LM2(3)=103
	      LM2(4)=201
	      LM2(5)=202
	      LM2(6)=203
	    ELSE IF(IBM2.EQ.5) THEN
C           Set de restrictions de RAHMAN + set complémentaire (A)
c           ISEMA=1
c           ISEMR=1
            M2=10
	      LM2(1)=101
	      LM2(2)=102
	      LM2(3)=103
	      LM2(4)=201
	      LM2(5)=202
	      LM2(6)=203
	      LM2(7)=104
	      LM2(8)=105
	      LM2(9)=204
	      LM2(10)=205
	    ELSE
              IFF=1                          !  Message d'erreur
            WRITE(*,*)  ' Set des restr. géométriques inconnu! =',IBM2
            WRITE(666,*)' Set des restr. géométriques inconnu! =',IBM2
 		  WRITE(29 ,*)' Set des restr. géométriques inconnu! =',IBM2			!bug
            RETURN
            ENDIF                            ! IF(IBM2.EQ.1)
	    IF(IOPTI.GE.3)WRITE(*,*)'Nbre de restr. géométriques  = ',M2
	ELSE IF((M2.LT.0).OR.(M2.GT.20)) THEN 
            IFF=1                              !  Message d'erreur
	    WRITE(*,*)  ' Nbre de restr. géométriques incorrecte = ',M2
 	    WRITE(666,*)' Nbre de restr. géométriques incorrecte = ',M2
		WRITE(29 ,*)' Nbre de restr. géométriques incorrecte = ',M2				!bug
	    RETURN
	ELSE IF(M2.NE.0) THEN
	    IF(IOPTI.GE.3)WRITE(*,*)'Nbre de restr. géométriques  = ',M2
          READ (55,*,END=900) (LM2(I),I=1,M2)  !  Liste des M2 restrictions
          READ (55,*,END=900) ISEMA,ISEMR      !  Indice relatif aux semelles
	    IF(IOPTI.GE.3) WRITE(*,*)' Set  = ',(LM2(I),I=1,M2)
	ELSE IF(M2.EQ.0) THEN
	    ISEMA=0
          ISEMR=0                              !
        ENDIF                                  ! IF(M2.EQ.99)

      IF(IOPTI.GE.1) THEN
         IF(M2.GT.0) THEN                      ! Si M2>0
           IF(ITERA.EQ.0) THEN                 ! Si ITERA=0	!13.12.05
             IF(ISEMA.GE.1) THEN 				
	          WRITE(666,203) 'transversaux : ISEMA= ',ISEMA
	       ELSE
	          WRITE(666,207) 'transversaux'
	       ENDIF
             IF(ISEMR.GE.1) THEN 
	          WRITE(666,203) 'raidisseurs  : ISEMR= ',ISEMR
	       ELSE
	          WRITE(666,207) 'raidisseurs'
	       ENDIF
             IF((ISEMR.GE.1).OR.(ISEMA.GE.1)) THEN 
	          WRITE(666,204)
               ENDIF                           !
             ELSE                              ! Si ITERA>1
             IF((ISEMA.GE.1).OR.(ISEMR.GE.1)) THEN
	         IF(NEL.EQ.1) WRITE(666,204) 
	       ENDIF
	     ENDIF
	   ENDIF
	  
        IF(ISEMA.GE.1) THEN
	     DO 205 I=1,NBRXI
	       IF ((ITYPE.NE.5).AND.(NXI(I).EQ.4))THEN				!février 2004
		    NN=I+NTOT
                  CALL SEMEL(HYA,DYA,WYA,TYA,XIMIN(NN),XIMAX(NN), ! cadres
     *                    INDA,1,IPRINT,NEL,ISEMA,ITYPE,ISECT,IMPR2)    ! cadres	!15.10.05
                XICOU(NN)=WYA
			ELSEIF ((ISECT.EQ.3).AND.(NXI(I).EQ.3)) THEN		!février 2004
		    NN=I+NTOT											!février 2004
                  CALL SEMEL(HYA,DYA,WYA,TYA,XIMIN(NN),XIMAX(NN), ! épontille	!février 2004
     *                    INDA,1,IPRINT,NEL,ISEMA,ITYPE,ISECT,IMPR2)    ! cadres	!15.10.05
                XICOU(NN)=WYA										!février 2004

c	          WRITE(*,*) 'Sem aig,   NN=',NN
c	          WRITE(*,*) '------------------------'
c               WRITE(*,*) 'après DF=',WYA,'  TF=',TYA,'   INDA=',INDA
		 ENDIF
  205      CONTINUE
        ENDIF

        IF(ISEMR.GE.1) THEN
	     DO 206 I=1,NBRXI
	       IF(NXI(I).EQ.8) THEN
		    NN=I+NTOT
                CALL SEMEL(HXR,DXR,WXR,TXR,XIMIN(NN),XIMAX(NN), ! Raidisseurs
     *                    INDA,1,IPRINT,NEL,ISEMA,ITYPE,ISECT,IMPR2)    ! cadres	!15.10.05
                XICOU(NN)=WXR
c	          WRITE(*,*) 'Sem raid,   NN=',NN
c	          WRITE(*,*) '------------------------'
c	          WRITE(*,*) 'après DF=',WXR,'  TF=',TXR,'  INDR=',INDR
		 ENDIF
  206      CONTINUE
        ENDIF

        IF(ITERA.EQ.0) WRITE(666,208) M2,(LM2(I),I=1,M2)	!13.12.05

	  DO 202 IC=1,M2
	  IT=IC+M2TOT

        CALL GEOM(EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,HXR,DXR,WXR,TXR,
     *            IC,IT,NXI,NBRXI,ITERA,IPRINT,LM2,M2max,IMPR2)		!15.10.05

  202   CONTINUE
          TFA(NEL)=TYA
          TFR(NEL)=TXR
      ENDIF

      NTOT =NTOT +NBRXI
      M2TOT=M2TOT+M2

c     IF(IPRINT.GE.1) PAUSE 'pause' ! A activer si on désire un stop après chaque panneau
C

C ======================================================================
C  4.  CALCUL DES COEFFICIENTS DE RAIDEUR DU PANNEAU
C     ----------------------------------------------
C     ENTREPOSES DANS LE VECTEUR CONST,CONST2 et CONST3
C ======================================================================

      CALL ANNULD(CONST,74)

C 4.1 Raidissage principale
C     ----------------------
      CALL CARAC(E,ETA,EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,HXR,DXR,WXR,TXR,
     *HXTR,DXTR,WXTR,TXTR,CONST,SH,FAM,MT,KSR,KSA,KST,SPEC,POIDS,PHIL,Q,
     *WIDTH,DELT(NEL),DELT2(NEL),IMPR2,EPAIS,ITYPE,ISECT,AIRE,AIy,AIx,	!15.10.05
     *Sredy,Sredx,TORK	)											!12.05.04
	if(itype.eq.5) then
	WRITE(306)AIRE,AIy,AIx,Sredy,Sredx,TORK							!12.05.04		!extension neto
	endif				

c     IF(NEL.LE.2) CONST(7)=CONST(7)+2.63E6        ! Ty
c	WRITE(66,*) 'Ty=',CONST(7)
     
C 4.2 DERIVEE de CONST (opti)
C     ----------------------
      IF (IOPTI.GE.1) THEN
       CALL ANNULD(CONST2,54)
       CALL ANNULD(CONST3,20)

       CALL CARACD(E,ETA,EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,HXR,DXR,WXR,
     * TXR,CONST,SH,FAM,MT,KSR,KSA,KST,PHIL,Q,WIDTH,IMPR,CONST2,CONST3,
     * EPAIS,ITYPE,ISECT,dPOIDS,SPEC)								!février 2004,!!!aout04

      ENDIF

C 4.3 Raidissage secondaire
C     ----------------------
      IF((EPSA2.GE.0.00001).OR.(EPSR2.GE.0.00001)) THEN

       CALL CARAC2(E,ETA,EPSA2,EPSR2,DELTA,HYA2,DYA2,WYA2,TYA2,HXR2,
     * DXR2,WXR2,TXR2,CONST,SH,FAM,KSR2,KSA2,SPEC,POIDS,PHIL,Q,WIDTH,
     * DELT(NEL),DELT2(NEL),IMPR2,CONST2)			!15.10.05

      ENDIF

C 4.4 Impression de CONST2
C     ----------------------
      IF((IMPR.GE.1).AND.(IOPTI.GE.1)) THEN
		IF(ITYPE.NE.5) THEN									!février 2004
        WRITE(66,15)
        WRITE(66,*)' BORDAGE : X(1) = Epaisseur bordage'
        WRITE(66,12)    (CONST2(I,1),I=1,6)
        WRITE(66,*)' CADRES  : X(i),i=2,5'
        WRITE(66,*)'    X(2) = Hauteur âme'
        WRITE(66,12)    (CONST2(I,2),I=1,6)
        WRITE(66,*)'    X(3) = Epaisseur âme'
        WRITE(66,12)    (CONST2(I,3),I=1,6)
        WRITE(66,*)'    X(4) = Largeur semelle'
        WRITE(66,12)    (CONST2(I,4),I=1,6)
        WRITE(66,*)'    X(5) = Larg. Effective'
        WRITE(66,12)    (CONST2(I,5),I=1,6)
        WRITE(66,*)' RAIDISSEURS: X(i),i=6,9'
        WRITE(66,*)'    X(6) = Hauteur âme'
        WRITE(66,12)    (CONST2(I,6),I=1,6)
        WRITE(66,*)'    X(7) = Epaisseur âme'
        WRITE(66,12)    (CONST2(I,7),I=1,6)
        WRITE(66,*)'    X(8) = Largeur semelle'
        WRITE(66,12)    (CONST2(I,8),I=1,6)
        WRITE(66,*)'    X(9) = Larg. Effective'
        WRITE(66,12)    (CONST2(I,9),I=1,6)
        WRITE(66,*)' '
		ELSE										!épontille	!février 2004
		IF(ISECT.EQ.3) THEN										!février 2004
        WRITE(66,15)												!février 2004
        WRITE(66,*)' EPONTILLE :X(1) = Hauteur de l''âme '		!février 2004
        WRITE(66,12)    (CONST2(I,1),I=1,5)						!février 2004
        WRITE(66,*)'    X(2) = Epaisseur âme'						!février 2004
        WRITE(66,12)    (CONST2(I,2),I=1,5)						!février 2004
        WRITE(66,*)'    X(3) = Largeur semelle'					!février 2004
        WRITE(66,12)    (CONST2(I,3),I=1,5)						!février 2004
        WRITE(66,*)'    X(5) = Larg. Effective'					!février 2004
        WRITE(66,12)    (CONST2(I,5),I=1,5)						!février 2004
		ELSE													!février 2004
        WRITE(66,15)												!février 2004
        WRITE(66,*)' EPONTILLE :X(1) = Diamètre ou côté extérieur '	!février 2004	
        WRITE(66,12)    (CONST2(I,1),I=1,5)							!février 2004
        WRITE(66,*)'    X(2) = Epaisseur de la paroi mince'			!février 2004
        WRITE(66,12)    (CONST2(I,4),I=1,5)							!février 2004
        WRITE(66,*)'    X(5) = Larg. Effective'						!février 2004
        WRITE(66,12)    (CONST2(I,5),I=1,5)							!février 2004
		ENDIF														!février 2004
		ENDIF														!février 2004
        IF(MT.GE.1) THEN
           WRITE(66,*)' TRAVERSES:  X(1) = Epaisseur bordage'
           DO 20 J=1,MT
  20       WRITE(66,*)'Trav nø',J,'  : ',(CONST3(I,J),I=1,2)
        ENDIF
      ENDIF

C 4.5 Poids du panneau et de la structure
C     ---------------------------------
      POT=POT+POIDS                 ! POT= Poids total struct par mètre

C 4.6 Action du poids propre (POIDS=poids panneau par mètre)
C     ----------------------------------------------------
      IF(IPOIDS.EQ.0)GOTO 533
      WRITE(66,534) POIDS
      IF(IPA.EQ.1) THEN                                !!!aout04
	  POIDS=POIDS*(SPEC-9810.)/SPEC                  !!!aout04
	  DO K=1,9                                       !!!aout04
	    dPOIDS(K)=dPOIDS(K)*(SPEC-9810.)/SPEC        !!!aout04
	  ENDDO                                          !!!aout04
	ENDIF                                            !!!aout04
        DO 535 IS=1,NSOL
          IF(ICHA.EQ.1) THEN
            DO 532 I=1,NPT(IS)
            CHA(I,1,IS)=CHA(I,1,IS)+POIDS              !!!aout04
		  DO K=1,9                                   !!!aout04
		    dCHA(I,IS,K)=dCHA(I,IS,K)+dPOIDS(K)		 !!!aout04
		  ENDDO                                      !!!aout04
 532	      CONTINUE                                   !!!aout04
          ELSE
            NPT(IS)=1
            CHA(1,1,IS)=POIDS
            CHA(1,2,IS)=0.
            CHA(1,3,IS)=0.
		  DO K=1,9                                   !!!aout04
		    dCHA(1,IS,K)=dPOIDS(K)					 !!!aout04
	      ENDDO
          ENDIF
 535    CONTINUE
 533  CONTINUE

C     Sauvetage [ WRITE(98) ] de CHA1, CHA2 et CHA3
C     --------------------------------------------
      IF((IPOIDS.EQ.1).OR.(ICHA.EQ.1)) THEN
        DO 689 IS=1,NSOL
        DO 689 I=1,NPT(IS)
           CHA(I,1,IS)=CHA(I,1,IS)/QPHIL
		 DO K=1,9                                !!!aout04
		   dCHA(I,IS,K)=dCHA(I,IS,K)/QPHIL       !!!aout04
	     ENDDO
 689    CONTINUE                             !
        IF(ITERA.EQ.0) WRITE(98) NPT,CHA,dCHA     ! Sauvetage sur FILE 98 de CHA !!!aout04 !13.12.05
      ENDIF

C 4.7 Sauvetage, des pressions max pour la Subr  HUGHES ! Updated: mai 1996
C                                    et la subr. STIFF
C     -------------------------------------------------
C    La pression max est calculée en y= 0, 1/3, 1/2, 2/3 et 1
	IP=1
	IF(KSE.EQ.2) IP=-1
	IF(KSE.EQ.0) IP=0
      CALL ANNULD(CHAMAX,5*ISmax)
	CALL ANNULD(dCHAMAX,5*ISmax*9)    !!!aout04

      DO 690 IS=1,NSOL
c	  WRITE(66,*) 'NEL= ', NEL,' Cas de charge ', IS
c	  WRITE(66,*) 'CHA(1, =',CHA(1,1,IS),CHA(1,2,IS),CHA(1,3,IS)
c       WRITE(66,*) 'CHAMAX=',(CHAMAX(II,IS),II=1,5)
        C1=DCOS((ANG)           *PI1)/9810.       ! PI1=PI/180.
        C2=DCOS((ANG+PHIL/3.)   *PI1)/9810.
        C3=DCOS((ANG+PHIL/2.)   *PI1)/9810.
        C4=DCOS((ANG+2.*PHIL/3.)*PI1)/9810.
        C5=DCOS((ANG+PHIL)      *PI1)/9810.

	  XIF= XF(IS)-XI(IS) + CHA(1,3,IS)-CHA(1,2,IS)
        CHAMAX(1,IS)=(XI(IS)+CHA(1,2,IS))          *IP+C1*CHA(1,1,IS)
        CHAMAX(2,IS)=(XI(IS)+CHA(1,2,IS)+XIF/3.)   *IP+C2*CHA(1,1,IS)
        CHAMAX(3,IS)=(XI(IS)+CHA(1,2,IS)+XIF/2.)   *IP+C3*CHA(1,1,IS)
        CHAMAX(4,IS)=(XI(IS)+CHA(1,2,IS)+2.*XIF/3.)*IP+C4*CHA(1,1,IS)
        CHAMAX(5,IS)=(XF(IS)+CHA(1,3,IS))          *IP+C5*CHA(1,1,IS)

c         Calcul des dérivées des CHAMAX    !!!aout04	  
	  DO K=1,9
	    dCHAMAX(1,IS,K)=C1*dCHA(1,IS,K)
		dCHAMAX(2,IS,K)=C2*dCHA(1,IS,K)
		dCHAMAX(3,IS,K)=C3*dCHA(1,IS,K)
      	dCHAMAX(4,IS,K)=C4*dCHA(1,IS,K)
	    dCHAMAX(5,IS,K)=C5*dCHA(1,IS,K)
	  ENDDO
c		Fin calcul dérivées               !!!aout04

c	  WRITE(66,*) 'Pas 1'
c	  WRITE(66,*) 'CHA=',CHA(1,1,IS),CHA(1,2,IS),CHA(1,3,IS)
c	  WRITE(66,*) 'CHAMAX=',(CHAMAX(I,IS),I=1,5)
	  IF(NPT(IS).GE.2) THEN
	     DO I=2,NPT(IS)
	        XIF= XF(IS)-XI(IS) + CHA(I,3,IS)-CHA(I,2,IS)
              T1=(XI(IS)+CHA(I,2,IS))          *IP+C1*CHA(I,1,IS)
              T2=(XI(IS)+CHA(I,2,IS)+XIF/3.)   *IP+C2*CHA(I,1,IS)
              T3=(XI(IS)+CHA(I,2,IS)+XIF/2.)   *IP+C3*CHA(I,1,IS)
              T4=(XI(IS)+CHA(I,2,IS)+2.*XIF/3.)*IP+C4*CHA(I,1,IS)
              T5=(XF(IS)+CHA(I,3,IS))          *IP+C5*CHA(I,1,IS)

c			Calcul des dérivées des T1	    !!!aout04	  
	   		 DO K=1,9
				dT1(K)=C1*dCHA(I,IS,K)
				dT2(K)=C2*dCHA(I,IS,K)
				dT3(K)=C3*dCHA(I,IS,K)
				dT4(K)=C4*dCHA(I,IS,K)
				dT5(K)=C5*dCHA(I,IS,K)
			ENDDO
c			Fin calcul dérivées               !!!aout04


c             Recherche de la valeur Max avec son signe
	        CALL SELECT (CHAMAX(1,IS),T1,dCHAMAX(1,IS,1),dT1)	!!!aout04  !à revérifier
	        CALL SELECT (CHAMAX(2,IS),T2,dCHAMAX(2,IS,1),dT2)	!!!aout04
	        CALL SELECT (CHAMAX(3,IS),T3,dCHAMAX(3,IS,1),dT3)	!!!aout04
	        CALL SELECT (CHAMAX(4,IS),T4,dCHAMAX(4,IS,1),dT4)	!!!aout04
	        CALL SELECT (CHAMAX(5,IS),T5,dCHAMAX(5,IS,1),dT5)	!!!aout04
c	        CHAMAX(1,IS)=DMAX1( DABS(CHAMAX(1,IS)),DABS(T1) )
c	        CHAMAX(2,IS)=DMAX1( DABS(CHAMAX(2,IS)),DABS(T2) )
c	        CHAMAX(3,IS)=DMAX1( DABS(CHAMAX(3,IS)),DABS(T3) )
c	        CHAMAX(4,IS)=DMAX1( DABS(CHAMAX(4,IS)),DABS(T4) )
c	        CHAMAX(5,IS)=DMAX1( DABS(CHAMAX(5,IS)),DABS(T5) )
c	        WRITE(66,*) 'pas ',I
c	        WRITE(66,*) 'CHAMAX',(CHAMAX(II,IS),II=1,5)
           ENDDO
c          La valeur absolue est prise dans Subr.HUGHES mais le signe est gardé pour STIFF
c	     DO  I=1,5
c	       CHAMAX(I,IS)=DABS(CHAMAX(I,IS))
c          ENDDO
        ENDIF

        IF((IVARIA.EQ.0).AND.(TYPES.EQ.'COQUE')) THEN
           WRITE(*  ,691)
           WRITE(666,691)
		 WRITE(29 ,691)															!bug	
        ENDIF

 690  CONTINUE

	IF(IMPR2.GE.-1) THEN						!15.10.05
      WRITE(66,874)MT,KSR,KSA,KST,KSE,IPA,IVARIA	!15.10.05
	ENDIF								!15.10.05

C 4.8 Calcul du Coût  (voir Subr COST ci-dessous: mai 1998)
C     ----------------

      CALL COST(IOPTI,NEL,NETO,NXI,NBRXI,WIDTH,SPEC,
     *     EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,HXR,DXR,WXR,TXR,PHIL,Q,
     *	 EPAIS,ITYPE,ISECT,CORRO,IMPR2,Spoids1)		!05.12.05					!février 2004	!corrosion

c
C***********************************************************************
C 5.  SAUVETAGE DES DONNEES
C     ---------------------
c      WRITE(66,874)MT,KSR,KSA,KST,KSE,IPA,IVARIA		!15.10.05


C     Sauvetage pour Subr. BO1 et BO2
c      --------------------------------------
      WRITE(99)  ABTR,PHIL,MT,TETA,XI,XF,HXTR,WXTR,Q,KSE,KST,KSA,
     *           TXTR,DXTR,KSR,A,B,C,D,DELTO,IVARIA,CONST,CONST2,CONST3,
     *           DELTA,DYA,TYA,WYA,HYA,EPSA,HXR,DXR,WXR,TXR,EPSR,ENTR,
     *           CHAMAX,dCHAMAX,NSIGN(NEL),EPAIS,HIGHT,Heff		!février 2004 !!!aout04

C     Sauvetage pour Subr. OBJEC1, OBJEC2 (Test sur Sub. Objectif ALSTOM), mars 2003
c      ---------------------------------------------------------
      IF(NEL.EQ.1) THEN
	  REWIND(305)			!extension neto
	  WRITE(305,*)WIDTH		!extension neto
	ENDIF
      WRITE(305,*)NEL,DABS(PHIL*Q*PI1),DELTA			!extension neto
      WRITE(305,'(5E14.7)')HYA,DYA,WYA,TYA,EPSA,EPAIS	!extension neto
      WRITE(305,'(6E14.7)')HXR,DXR,WXR,TXR,EPSR,ENTR	!extension neto

C     Sauvetage pour Subr. OBJEC1, OBJEC2  et COORD (GRAVITE)
c      ---------------------------------------------------------
      IF((IOPTI.GE.1).OR.(IANA.EQ.2)) THEN
        IF(ITYPE.NE.5) THEN
	    IF(EPSA2.GE.(0.00001)) THEN										!r&d13
		  WRITE(302) KSA,KSR,KSE,EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,		!r&d14	!extension neto
     *			     HXR,DXR,WXR,TXR,PHIL,Q,EPAIS,EPSA2,
     *				 HYA2,DYA2,WYA2,TYA2
	    ELSE
		  WRITE(302) KSA,KSR,KSE,EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,		!extension neto	!r&d14
     *			     HXR,DXR,WXR,TXR,PHIL,Q,EPAIS,EPSA2					!février 2004	!r&d13
          ENDIF														    !r&d13
	  ELSE
	    WRITE(302) KSA,KSR,KSE,EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,		!extension neto	!r&d14
     *			   HXR,DXR,WXR,TXR,PHIL,Q,EPAIS						!février 2004
	  ENDIF
	ENDIF
	


c     SAUVETAGE POUR LE DESSIN (Pour Subr. VISION)
C     --------------------------------------------
      IF(DESSIN.NE.1) RETURN

    ! ITYP= 1 si COQUE ou COQUE1 et =2 si PLAQUE ou COQUE1
      ITYP1=ITYPE  ! OK si COQUE (ITYPE=1) ou si PLAQUE (ITYPE=2)
      IF(ITYPE.EQ.3) ITYP=1  ! si COQUE1
      IF((ITYPE.EQ.4).OR.(ITYPE.EQ.5)) ITYP=2  !si PLAQUE1 OU EPONTILLE	!février 2004
 
      WRITE(42) ITYP,PHILN(NEL),Q,HIGHT,ANGLE(NEL),IVARIA,XI,
     *          XF,KSE,TETA,POIDS,NPT

      DO  NS=1,NSOL
       DO  I=1,NPT(NS)
         WRITE(42) CHA(I,1,NS),CHA(I,2,NS),CHA(I,3,NS)
       ENDDO
      ENDDO
      WRITE(45) NEL,MT,KST
      IF(MT.NE.0) WRITE(45) HXTR,WXTR,ABTR2

C     Sauvetage des données courantes pour Subr. VISIONN (DESSIN) 
	WRITE(46) Delta,HYA,DYA,WYA,TYA,EPSA,HXR,DXR,WXR,TXR,EPSR
      
      RETURN
c -------------------------------------------------------------------------------
c -------------------------------------------------------------------------------
  900 WRITE(* ,*)'Erreur de lecture : "End of file"'
      WRITE(66,*)'Erreur de lecture : "End of file"'
 	WRITE(29,*)'Erreur de lecture : "End of file"'									!bug
      PAUSE 'STOP'
      STOP

C 6.  LES FORMATS.
C     ============
   1  FORMAT(/' CHARGE EN METRE DE LA COLONNE D''EAU SUR LE PANNEAU'
     */1X,49(1H+))
   5  FORMAT(' Cas de charge nø ',I2/
     * '  VARIATION LINEAIRE DE LA P.H.AVEC LA PROFONDEUR (hydrostat)'/
     * '  SUR LE BORD de DEPART (Y=0), XI=',F7.3,' m.'/
     * '  (NB: sur le bord d''arrivee,  XF=',F7.3,' m )'/)
   6  FORMAT(' EFFORTS DE BORDS DANS LE PLAN DU PANNEAU SELON L''AXE X'/
     *      ,' A REPARTIR SUR UNE DISTANCE DE ',F7.3,'  (m)'/)
   7  FORMAT(' CAS nø',I2,' VARIATION LINEAIRE DE LA P.H. (XI a XF) :'/
     * '    Au BORD de DEPART  (Y=0)   , XI=',F7.3,' m.'/
     * '    Au BORD d''ARRIVEE (Y=PHIL) , XF=',F7.3,' m.'/
     * '     NB: 1m = 1 m d''eau (colonne) = 9.81kN/m2')
   8  FORMAT('    ANGLE QUE FAIT LE PANNEAU EN Y=0 AVEC L''HORIZONTALE',
     *       ' = ',F9.3,' Degré')
  11  FORMAT(/' DIMENSIONS DES CADRES (transversaux)'/1X,36(1H+)/
     *'    DISTANCE ENTRE CADRES    = ',F8.5,' m.'//
     *'    HAUTEUR DE L''AME         = ',F8.5,' m.',
     *     ' (épaisseurs du bordé et semelle non comprises)'/
     *'    EPAISSEUR DE L''AME       = ',F8.5,' m.'/
     *'    LARGEUR DE LA SEMELLE    = ',F8.5,' m.'/
     *'    EPAISSEUR DE LA SEMELLE  = ',F8.5,' m.')
  12  FORMAT(6(1X,E11.4))
  13  FORMAT(/' DIMENSIONS DES RAIDISSEURS LONGITUDINAUX'/1X,40(1H+)/
     *'    DISTANCE ENTRE RAID. (=D) = ',F8.5,' m.'//
     *'    HAUTEUR DE L''AME          = ',F8.5,' m.',
     *     ' (épaisseurs du bordé et semelle non comprises)'/
     *'    EPAISSEUR DE L''AME        = ',F8.5,' m.'/
     *'    LARGEUR DE LA SEMELLE     = ',F8.5,' m.'/
     *'    EPAISSEUR DE LA SEMELLE   = ',F8.5,' m.'//
     *'    EPSR (larg. collaborante) = ',F8.5,' m.'/
     *'    MODE (répartition des cadres): ',A3,' N=L/D + ',F4.1)
  14  FORMAT(/'Nervures complémentaires (2ème lit; aig et raid)'/45(1H=)
     */' LES CADRES'/2x,12(1H-)/
     * '   Distance entre cadres compl.= ',F8.5,' m.  et  KSA2= ',I2/
     * '   HAUTEUR DE L''AME        ',F8.5,' m.'/
     * '   EPAISSEUR DE L''AME      ',F8.5,' m.'/
     * '   LARGEUR DE LA SEMELLE   ',F8.5,' m.'/
     * '   EPAISSEUR DE LA SEMELLE ',F8.5,' m.'/
     * ' LES RAIDISSEURS'/2X,12(1H-)/
     * '   Distance entre raidisseurs compl.= ',F8.5,
     *                                       ' m. et  KSR2= ',I2/
     * '   HAUTEUR DE L''AME        ',F8.5,' m.'/
     * '   EPAISSEUR DE L''AME      ',F8.5,' m.'/
     * '   LARGEUR DE LA SEMELLE   ',F8.5,' m.'/
     * '   EPAISSEUR DE LA SEMELLE ',F8.5,' m.'/)
  15  FORMAT('Derivées des coef. de raideur par rapport aux var. de ',
     * 'conception :'/65(1H-))
  39  FORMAT(/' CARACTERISTIQUES DES TRAVERSES'/1X,30(1H+)/
     * T20,'DISTANCE',T32,'EPAISSEUR DE',T45,'LARGEUR DE',T57,
     * 'EPAISSEUR',T67,'HAUTEUR'/T20,'PAR RAPPORT',T32,'LA SEMELLE',
     *T45,'LA SEMELLE',T57,'DE L AME',T67,'DE L AME'/T20,'AU SOMMET '/
     *T23,'(M)',T36,'(M)',T50,'(M)',T62,'(M)',T72,'(M)')
 203  FORMAT('Avec ajustement des dimensions des semelles des ',A,I2)
 204  FORMAT(/'Ajustement des dimensions des semelles'/38(1H-)/
     * 'Panneau',5x,'Anc. dimensions',15x,'Nouvelles dimensions',6x,
     * 'type','  IND')
 207  FORMAT('Les dimensions des semelles des ',A12,' sont fixes.')
 208  FORMAT(/'Nbre de restr. géom.= ',I2,'  Set = ',20I4)
 212  FORMAT(/'CAS nø',I2,': CHARGES VARIABLES SELON OX,',
     *                    ' DEFINIES EN ',I2,' INTERVALLES'/
     * T11,'REGULIEREMENT ESPACES (CHARGE EN KN PAR METRE COURANT)')
 213  FORMAT(' INTERVALLE nø     CHARGE VERTICALE (N/m)     PRESSION'
     * ' (m)  EN Y=0  et en Y=PHIL')
 214  FORMAT(T4,I2,T20,E14.7,' N/m',T45,E14.7,' m. ',E14.7,' m.')
 218  FORMAT(/'Panneau nø',I2,': KSE=0 est incompatible avec ICHA=1'/
     * '!!! Nous prenons KSE=2 avec XI=XF=0  et  CHA2=CHA3=0 !!!!!!!'/)
 220  FORMAT(/' ATTENTION'/1X,14(1H*)/' Vous avez sélectionné la ',
     * 'restrictions nø 4 (d<dmin) dite de Hughes'/' alors que d n''est'
     * ' pas une variable de conception.'/' Cela peut entraîner des'
     * ' difficultés dans CONLIN !!'/)
 237  FORMAT(/'Ajustement des dimensions des semelles'/38(1H-)/
     * 'Panneau',5x,'Anc. dimensions',20x,'Nouvelles dimensions',8x,
     * 'type','    IND')
 283  FORMAT(/'PANNEAU nø',I2/15(1H-)/
     *       'Nbre de variables de conception = ',I3)
 427  FORMAT('C(',I2,') > ',E11.5,' de type nø',I2,
     *  ' au pt YPT nø',I2,' et au pt de calcul nø',I2)
 428  FORMAT('C(',I2,') < ',E11.5,' de type nø',I2,
     *  ' au pt YPT nø',I2,' et au pt de calcul nø',I2)
 431  FORMAT(/' ATTENTION: La position des Pts de calculs',
     *        ' des sensibilites est errone.'/5x,   
     *        ' Il faut que le premier et le dernier pts',
     *        ' correspondent aux extremites.'/5x,
     *        ' !!!  La Correction est automatique !!!'/)
 441  FORMAT(' TRAVERSE N ',I2,6X,5(F8.5,4X))
 534  FORMAT(/'LE POIDS PROPRE PAR METRE (SELON OX) EST DE ',
     *         E14.7,' N/m'/)
 649  FORMAT(//' LE RAPPORT RAYON/EPAISSEUR EST DE ',E11.4,' m.'/)
 691  FORMAT(/' Approximation (COQUE avec IVARIA=0) !!!!!'/,
     *  ' Les pressions pour la Subr HUGHES sont calculées en ',
     *  'considérant une variation linéaire selon OY.'/
     *  ' L''erreur peut être importante si PHIL > 60 degré'/)
 801  FORMAT(/' DIMENSIONS DU PANNEAU'/1X,22(1H+)/
     *        '    EPAISSEUR DE BORDAGE = ',F9.6,' m.'/
     *        '    LONGUEUR DU PANNEAU  = ',F9.3,' m.')
 802  FORMAT( '    ANGLE D''OUVERTURE    = ',F11.5,' degré'/
     *        '    RAYON DU PANNEAU     = ',F10.3,' m.')
 803  FORMAT( '    LARGEUR DE LA PLAQUE = ',F9.5,' m.')
 804  FORMAT( '	LA SECTION EST DE TYPE	= ',A7/					!février 2004
     *		'	L''ENTREDISTANCE DES EPONTILLES =',F9.5,'m.')	!février 2004
 805  FORMAT(	'	SECTION''S TYPE =',A7/							!février 2004
     *		'	PILLAR''S SPACING =',F9.5,'m.')					!février 2004
 806  FORMAT(	'	DIAMETRE EXTERIEUR	=',F9.5,' m.'/				!février 2004
     *		'	EPAISSEUR DE LA PAROI MINCE =',F9.5,'m.')		!février 2004
 807  FORMAT(	'	LONGUEUR EXTERIEURE DU COTE	=',F9.5,' m.'/		!février 2004
     *		'	EPAISSEUR DE LA PAROI MINCE =',F9.5,'m.')		!février 2004
 808  FORMAT(	'	HAUTEUR DE LA DEMI AME	=',F9.5,' m.'/			!février 2004
     *		'	EPAISSEUR DE AME =',F9.5,' m.'/					!février 2004
     *		'	LARGEUR DE LA SEMELLE	=',F9.5,' m.'/			!février 2004
     *		'	EPAISSEUR DE LA SEMELLE	=',F9.5,' m.')			!février 2004
 809  FORMAT(	'	EXTERIOR DIAMETER 	=',F9.5,' m.'/				!février 2004
     *		'	CERCLE THICKNESS =',F9.5,'m.')					!février 2004
 810  FORMAT(	'	EXTERIOR LENGTH OF SIDE	=',F9.5,' m.'/			!février 2004
     *		'	SQUARE THICKNESS =',F9.5,'m.')					!février 2004
 811  FORMAT(	'	HALF WEB HIGHT	=',F9.5,' m.'/					!février 2004
     *		'	WEB THICKNESS =',F9.5,' m.'/					!février 2004
     *		'	FLANGE WIDTH =',F9.5,' m.'/						!février 2004
     *		'	FLANGE THICKNESS =',F9.5,' m.')					!février 2004
 874  FORMAT(' MT',I2,' ; KSR',I2,' ; KSA',I2,' ; KST',I2,' ; KSE',I2,
     *       ' ; IPA',I2,' ; IVARIA',I2//)
 901  FORMAT(/' DIMENSIONS OF THE PANEL'/1X,22(1H+)/
     *        '    PLATE THICKNESS     = ',F9.6,' m.'/
     *        '    LENGTH of the PANEL = ',F9.3,' m (=structure length')
 902  FORMAT( '    SHELL OPENING ANGLE = ',F11.5,' degré'/
     *        '    SHELL RADIUS        = ',F10.3,' m.')
 903  FORMAT( '    WIDTH of the PANEL  = ',F9.5,' m.')
 904  FORMAT(/' DIMENSIONS OF THE FRAMES'/1X,30(1H+)/
     *'    FRAME SPACING             = ',F8.5,' m.'//
     *'    WEB HEIGHT                = ',F8.5,' m. ',
     *       '(flange and plate thickness not included)'/
     *'    WEB THICKNESS             = ',F8.5,' m.'/
     *'    FLANGE WIDTH              = ',F8.5,' m.'/
     *'    FLANGE THICKNESS          = ',F8.5,' m.')
 905  FORMAT(/' DIMENSIONS OF LONGITUDINAL STIFFENERS'/1X,40(1H+)/
     *'    GEOMETRIC SPACING (=D)    = ',F8.5,' m. (as given)'//
     *'    WEB HEIGHT                = ',F8.5,' m. ',
     *       '(flange and plate thickness not included)'/
     *'    WEB THICKNESS             = ',F8.5,' m.'/
     *'    FLANGE WIDTH              = ',F8.5,' m.'/
     *'    FLANGE THICKNESS          = ',F8.5,' m.'/
     *'    EFFECTIVE SPACING (EPSR)  = ',F8.5,' m. (calculated)'/
     *'    MODE (frames distribution): ',A3,' N=L/D + ',F4.1)
 906  FORMAT(/'Complementary members (2nd layer; frame & stiff)'/45(1H=)
     */' FRAMES'/2x,12(1H-)/
     * '   SPACING                 ',F8.5,' m.  and  KSA2= ',I2/
     * '   WEB HEIGHT              ',F8.5,' m.'/
     * '   WEB THICKNESS           ',F8.5,' m.'/
     * '   FLANGE WIDTH            ',F8.5,' m.'/
     * '   FLANGE THICKNESS        ',F8.5,' m.'/
     * ' STIFFENERS'/2X,12(1H-)/
     * '   SPACING                 ',F8.5,' m.  and  KSR2= ',I2/
     * '   WEB HEIGHT              ',F8.5,' m.'/
     * '   WEB THICKNESS           ',F8.5,' m.'/
     * '   FLANGE WIDTH            ',F8.5,' m.'/
     * '   FLANGE THICKNESS        ',F8.5,' m.'/)
 907  FORMAT(' LOAD CASE Nø',I2,' Linear variation of pressure between'/
     * '    Departure Edge (Y=0)   , XI=',F7.3,' m.'/
     * '    Arrival Edge   (Y=PHIL), XF=',F7.3,' m.'/
     * '     NB: 1 m means a pressure of 9.81kN/m2 = 1 meter of water')
 908  FORMAT(' LOAD CASE Nø',I2,' acting (PHI=0):',F7.3,' m.'/
     * ' VARIATION LINEAIRE DE LA P.H.AVEC LA PROFONDEUR'/
     * '  (NB: sur le bord inf., XF=',F7.3,' m )'/)
 909  FORMAT(/' LONGITUDINAL GIRDER DIMENSIONS & POSITIONS'/1X,30(1H+)/
     * T20,'POSITIONS',T32,'FLANGE',T45,'FLANGE',T57,
     * ' WEB',T67,' WEB'/T20,'related to',T32,'THICKNESS',
     * T45,'WIDTH',T57,'THICKNESS',T67,'HEIGHT'/T20,'DEPART EDGE(Y=0)'/
     * T23,'(M)',T36,'(M)',T50,'(M)',T62,'(M)',T72,'(M)')

      END


C***********************************************************************
C***********************************************************************

      SUBROUTINE  BORNE(XI,I,TEXT,NEL,XM,XP,IFF,IMPR)			!15.10.05	

      IMPLICIT REAL *8 (A-H,O-Z)
      CHARACTER*29 TEXT
      DIMENSION XM(NVmax),XP(NVmax)  ! à la place du COMMON OPTI4
      COMMON/DIM1/Nmax,NEmax,NVmax   ! dimension max des vecteurs

C***********************************************************************
C     SUBROUTINE BORNE
C     **************
C     VERIFICATION DE LA SOLUTION DE DEPART
C       Il faut que les Xi de départ soient inclus entre les bornes
C      c.à.d.   XI(min) < XI  < XI(max)
C
C     modif:10-5-95                                  création :7-5-95
C***********************************************************************
	IF(IMPR.GE.-2) THEN								!15.10.05
        WRITE(666,285) I,TEXT,XM(I),XI,XP(I)
	ENDIF											!15.10.05

      IF((XI.LT.XM(I)).OR.(XP(I).LT.XI)) THEN
        WRITE(*,*) 'BORNES NON COMPATIBLES AVEC LES VALEURS INITIALES',
     *              ' DES VARIABLES DE CONCEPTION.'
        WRITE(*,*) 'XI(min) <  XI < XI(max) pas vérifié'
        WRITE(*,*) 'Panneau nø',NEL,'   variable nø',I
        WRITE(*,*)  XM(I),' ?<? ',XI,' ?<? ',XP(I),' pas vérifié'

	  WRITE(29,*) 'BORNES NON COMPATIBLES AVEC LES VALEURS INITIALES',			!bug
     *              ' DES VARIABLES DE CONCEPTION.'								!bug
        WRITE(29,*) 'XI(min) <  XI < XI(max) pas vérifié'							!bug
        WRITE(29,*) 'Panneau nø',NEL,'   variable nø',I							!bug
        WRITE(29,*)  XM(I),' ?<? ',XI,' ?<? ',XP(I),' pas vérifié'				!bug

        PAUSE 'ERROR'
        IFF=1 !stop à la sortie de la Subr ENT
      ENDIF
      RETURN

 285  FORMAT('Var. nø',I3,1X,A29,2X,E11.4,'<',E11.4,' < ', E11.4)
      END



C***********************************************************************
C***********************************************************************

      SUBROUTINE COST(IOPTI,NEL,NETO,NXI,NBRXI,WIDTH,SPEC,
     *     EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,HXR,DXR,WXR,TXR,PHIL,Q,
     *	 EPAIS,ITYPE,ISECT,CORRO,IMPR2,Spoids1)				!05.12.05		!février 2004	!corrosion

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION NXI(NBRXI)
	DIMENSION CORRO(NETO,3)													!corrosion

      COMMON/PY/PI

C     For: OPTI,OBJEC

      COMMON/COUT/ Poids,SPoids,
     *        Fmat,Fsou,FMdO,Fmat11,Fmat22,Fmat33,Fsou11,Fsou22,
     *        Fsou33,FMdO11,FMdO22,FMdO33,FMdO44,FMdO55,FMdO66,FMdO77,
     *        REND,EQP,Dref,DrefX,DrefY,C1,C2,C3,DC1,DW2,DW3,P10,DP10,
     *        P4,P5,DP4,DP5,P9X,P9Y,DP9X,DP9Y,BER,BET,P6,P7,C8,DC8,COUT,
     *        ICOUT,IALR,IALT

C ********************************************************************
C     Calcul de la fonction objectif COUT dans la subr. ENT.
C
C     Version du : 30-5-98    			          Créer 30-5-98
c
c     Modif: 11-01-02
C
C *********************************************************************
C
C LES VARIABLES DE CONCEPTION
C ---------------------------
C	1	DELTA =  épaisseur du bordage
C	2	HYA   =  hauteur de l'âme des aiguilles
C    3	DYA   =  épaisseur de l'âme des aiguilles
C	4	WYA   =  largeur des semelles des aiguilles
C	5	EPSA  =  entredistance entre aiguilles
C	6	HXR   =  hauteur de l'âme des raidisseurs
C	7	DXR   =  épaisseur de l'âme des raidisseurs
C	8	WXR   =  largeur des semelles des raidisseurs
C	9	EPSR  =  entredistance entre raidisseurs
C
C Variables Associées :
C ---------------------
C	 	TYA = épaisseur semelle aiguilles
C     	TXR = épaisseur semelle raidisseurs 
c
C  LES VARIABLES DE CONCEPTION POUR EPONTILLE
C  ------------------------------------------
C	1	HYA		=	hauteur demi âme ou diam ou côté ext.			!février 2004
c	2	DYA		=	épaisseur d'âme									!février 2004
c	3	WYA		=	largeur de semelle								!février 2004
c	4	EPAIS	=	épaisseur de paroi mince						!février 2004
c	5	EPSA	=	entredistance entre épontilles					!février 2004
C
C ********************************************************************
	
c	IF(IPRINT.GE.1) THEN
c      WRITE(*,*) 'PANNEAU Nø ',NEL 
c      WRITE(*,*) 'NBRXI= ',NBRXI,' et  NXI =',NXI
c      WRITE(*,*) 'Bord = ',DELTA
c      WRITE(*,*) 'Aig  = ',HYA,DYA,WYA,TYA,'   EPSA=',EPSA
c      WRITE(*,*) 'Raid = ',HXR,DXR,WXR,TXR,'   EPSR=',EPSR
c	PAUSE'OBJ2'
c	ENDIF
	
C  -------------------------

C     Fmat= Coût des matériaux (acier)
C     Fsou= Coût du soudage (energie + consommables)
C     FMdO= Coût Main d'oeuvre
C     FCT = Fmat + Fsou + FMdO  (d'un panneau NEL),
C     OBJ = Coût total          (de la structure complete)

      IF(NEL.EQ.1) Then
	 COUT=0. 
	 CALL ANNULD(Poids,18)
	Endif

	IF(ISECT.EQ.3) THEN				!âme double T entree=1/2 âme	!février 2004
	HYB=2*HYA														!février 2004
	ENDIF
	
	DCOR  = CORRO(NEL,1)				!corrosion
	DCOR2 = CORRO(NEL,2)				!corrosion
	DCOR3 = CORRO(NEL,3)				!corrosion	
															!février 2004
				
c      DCOR = epaisseur de corrosion pour bordé						!corrosion (mise en commentaire)
c	IF(NEL.EQ.1) THEN												!corrosion
c	  REWIND 57														!corrosion	
c	  READ(57,'(////,A1)') Abidon									!corrosion
c	  READ(57,*) IPAN,DCOR,DCOR2,DCOR3								!corrosion
c	ELSE															!corrosion
c	  READ(57,*) IPAN,DCOR,DCOR2,DCOR3								!corrosion
c	ENDIF															


	IF(ITYPE.NE.5) THEN												!février 2004
	DELTAC=DELTA+DCOR
	ELSE															!février 2004
	DELTAC =0														!février 2004
	ENDIF															!février 2004
c	DCOR2 ! epaisseur de corrosion pour cadres
	DYAC  =	DYA +DCOR2
	TYAC  = TYA +DCOR2
	EPAISC = EPAIS +DCOR2											!février 2004
c	DCOR3 ! epaisseur de corrosion pour lisses
	DXRC  =	DXR +DCOR3
	TXRC  = TXR +DCOR3
	IF(IMPR2.GE.-1) THEN			!15.10.05
	IF(ITYPE.NE.5) THEN												!février 2004
	WRITE(66,*) 
	WRITE(66,*) ' Avec sur-épaisseurs de corrosion'
	WRITE(66,*) ' --------------------------------'
	WRITE(66,*) ' *** Sur-épaisseur=',DCOR,' m'
	WRITE(66,*) ' - Epaiss bordé      =',DELTAC
	WRITE(66,*) ' - Epaiss âme cadres =',DYAC
	WRITE(66,*) ' - Epaiss sem cadres =',TYAC
	WRITE(66,*) ' - Epaiss âme raid   =',DXRC
	WRITE(66,*) ' - Epaiss sem raid   =',TXRC
	ELSE															!février 2004
	  IF(ISECT.EQ.3) THEN											!février 2004
	WRITE(66,*)														!février 2004
	WRITE(66,*) ' Avec sur-épaisseurs de corrosion'					!février 2004
	WRITE(66,*) ' --------------------------------'					!février 2004
	WRITE(66,*) ' - Epaiss âme épont. =',DYAC						!février 2004
	WRITE(66,*) ' - Epaiss sem épont. =',TYAC						!février 2004
	WRITE(66,*) ' - Epaiss âme raid   =',DXRC						!février 2004
	WRITE(66,*) ' - Epaiss sem raid   =',TXRC						!février 2004
	  ELSE															!février 2004
	WRITE(66,*)														!février 2004
	WRITE(66,*) ' Avec sur-épaisseurs de corrosion'					!février 2004
	WRITE(66,*) ' --------------------------------'					!février 2004
	WRITE(66,*) ' - Epaiss paroi mince =',EPAISC					!février 2004
	WRITE(66,*) ' - Epaiss âme raid   =',DXRC						!février 2004
	WRITE(66,*) ' - Epaiss sem raid   =',TXRC						!février 2004
	  ENDIF												
	ENDIF												
	ENDIF								!15.10.05									


	DENS =  SPEC/9.81
      TEMP =  PHIL * Q * WIDTH * PI/180.
	TEMP1=  TEMP * DENS  
	TEMP2=  TEMP * REND * EQP * 1000. * C1  !x 1000 car EQP en t/h

c     Supplément de Poids (en N) (sans les traverses)
	IF(ITYPE.NE.5) THEN							!plaque				!février 2004
      SPoid1 = TEMP*SPEC * ( DCOR +
     *                     (1.0+DW2)*(DCOR3*HXR+DCOR3*WXR)/EPSR +
     *                     (1.0+DW3)*(DCOR2*HYA+DCOR2*WYA)/EPSA    )
       Poid1 = TEMP*SPEC * ( DELTAC +
     *                     (1.0+DW2)*(DXRC*HXR+TXRC*WXR)/EPSR +
     *                     (1.0+DW3)*(DYAC*HYA+TYAC*WYA)/EPSA    )
	ELSE										!epontille			!février 2004	
	  IF(ISECT.EQ.3)THEN											!février 2004		
      Poid1 = TEMP*SPEC * ( DELTAC +									!février 2004	
c     *                     (1.0+DW2)*(DXRC*HXR+TXRC*WXR)/EPSR +		!05.12.05	
     *                     (1.0+DW3)*(DYAC*HYB+2*TYAC*WYA)/EPSA    )	!février 2004	
      SPoid1 = TEMP*SPEC * (!(1.0+DW2)*(DCOR3*HXR+DCOR3*WXR)/EPSR +	!05.12.05	
     *                     (1.0+DW3)*(DCOR2*HYB+2*DCOR2*WYA)/EPSA   ) !février 2004	
	  ELSEIF(ISECT.EQ.1)THEN										!février 2004		
	Poid1 = TEMP*SPEC * ( DELTAC +									!février 2004	
c     *                     (1.0+DW2)*(DXRC*HXR+TXRC*WXR)/EPSR +		!05.12.05	
     *                     (1.0+DW3)*(PI*(HYA*HYA-(HYA-2*EPAISC)**2))	!février 2004	
     *				   	/(4*EPSA))									!février 2004	
      SPoid1 = TEMP*SPEC * (!(1.0+DW2)*(DCOR3*HXR+DCOR3*WXR)/EPSR +	!05.12.05	
     *                     (1.0+DW3)*(PI*(HYA*HYA-(HYA-2*DCOR2)**2))	!février 2004	
     *				   	/(4*EPSA))									!février 2004	
	  ELSEIF(ISECT.EQ.2) THEN										!février 2004	
      Poid1 = TEMP*SPEC * ( DELTAC +									!février 2004	
c     *                     (1.0+DW2)*(DXRC*HXR+TXRC*WXR)/EPSR +		!05.12.05	
     *                     (1.0+DW3)*(HYA*HYA-(HYA-2*EPAISC)**2)/EPSA) !20.02.04	
      SPoid1 = TEMP*SPEC * (!(1.0+DW2)*(DCOR3*HXR+DCOR3*WXR)/EPSR +	!05.12.05	
     *                     (1.0+DW3)*((HYA*HYA-(HYA-2*DCOR2)**2))/EPSA)	!20.02.04	
	  ENDIF															!février 2004
	ENDIF															!février 2004	
	SPoids=SPoids+SPoid1 ! Supplément de poids dû à la corrosion
	 Poids =Poids+ Poid1 ! Poids + poids additionnel dû à DW2 et DW3

c     Coût des plaques
	IF(ITYPE.NE.5)THEN												!février 2004		
      Fmat1 = TEMP1 *C1* (1.+(DELTAC-Dref)*1000.*DC1) * DELTAC 
c     Coût des lisses
      Fmat2 = TEMP1 *C2* (1.+DW2) * (1.+(DXRC-Dref)*1000.*DC1)
     *                            * (DXRC*HXR+TXRC*WXR)/EPSR  
	ELSE															!12.05.04
	Fmat1=0.000														!12.05.04		
      Fmat2 =0.000													!12.05.04
	ENDIF															!12.05.04
c     Coût des cadres
	IF(ISECT.EQ.0) THEN												!février 2004		    
      Fmat3 = TEMP1 *C3* (1.+DW3) * (1.+(DYAC-Dref)*1000.*DC1)
     *                            * (DYAC*HYA+TYAC*WYA)/EPSA  
	ELSEIF(ISECT.EQ.1) THEN											!12.05.04
	Fmat3=TEMP1 * C3 *(1.+(EPAISC  -Dref)*1000.*DC1) * (1.0+DW3)    !12.05.04
     *                 * ((PI*(HYA*HYA-(HYA-2*EPAISC)**2))/(4*EPSA )) !12.05.04	
	ELSEIF(ISECT.EQ.2) THEN											!12.05.04
	Fmat3=TEMP1 *  C3 *(1.+(EPAISC  -Dref)*1000.*DC1) * (1.0+DW3)   !12.05.04
     *                 * ((HYA*HYA-(HYA-2*EPAISC)**2)/EPSA )			!12.05.04
	ELSEIF(ISECT.EQ.3) THEN
	Fmat3=TEMP1 * C3 *(1.+(DYAC  -Dref)*1000.*DC1) * (1.0+DW3)		!12.05.04
     *                      * ((2*DYAC*HYA+2*TYAC*WYA)/EPSA )			!12.05.04
	ENDIF															!février 2004	

c     Coût de la matière de soudage des lisses
	IF(ITYPE.NE.5) THEN												!février 2004	
      Fsou1 = TEMP * C8 * (1.+(DXRC-DrefX)*1000.*DC8)*(2.-IALR)/EPSR 
	ELSE															!février 2004	
	Fsou1=0.000														!février 2004	
	ENDIF															!février 2004	
c     Coût de la matière de soudage des cadres
	IF(ITYPE.NE.5) THEN												!février 2004	
      Fsou2 = TEMP * C8 * (1.+(DYAC-DrefY)*1000.*DC8)*(2.-IALT)/EPSA 
	ELSE															!février 2004	
	Fsou2 = 0.0000													!février 2004	
	ENDIF															!février 2004	
c     Coût de la matière de soudage des nappes
	Fsou3=0.  ! à introduire
c     Fsou3 = TEMP * C8 * ??

c     Coût de la main d'oeuvre : soudage des lisses sur bordé : P4
	IF(ITYPE.NE.5) THEN												!février 2004	
      FMdO1 = TEMP2 * P4 *(1.+(DXRC-DrefX)*1000.*DP4)  /EPSR
c     Coût de la main d'oeuvre : constitution des lisses + extra : P9X
      FMdO2 = TEMP2 * P9X*(1.+(DXRC-DrefX)*1000.*DP9X) /EPSR
c     Coût de la main d'oeuvre : soudage des cadres : P5
      FMdO3 = TEMP2 * P5 *(1.+(DYAC-DrefY)*1000.*DP5)  /EPSA
c     Coût de la main d'oeuvre : constitution cadres + extra : P9Y
      FMdO4 = TEMP2 * P9Y*(1.+(DYAC-DrefY)*1000.*DP9Y) /EPSA
c     Coût de la main d'oeuvre : slot : P6 
      FMdO5 = TEMP2 * P6         /(EPSA*EPSR) 
c     Coût de la main d'oeuvre : goussets et tap: P7
      FMdO6 = TEMP2 * BER*BET*P7 /(EPSA*EPSR) 
c     Coût de la main d'oeuvre : assemblmage des bordés nappes): P10
      FMdO7 = TEMP2 * P10*(1.+(DELTAC-Dref)*1000.*DP10) 
	ELSE															!février 2004	
	FMdO1 =0.0000													!février 2004	
	FMdO2 =0.0000													!février 2004	
	FMdO3 =0.0000													!février 2004	
	FMdO4 =0.0000													!février 2004	
	FMdO5 =0.0000													!février 2004	
	FMdO6 =0.0000													!février 2004	
	FMdO7 =0.0000													!février 2004	
	ENDIF															!février 2004	

c     Coûts cumulés: for all the panels
 
      Fmat11 = Fmat11+Fmat1 ! matière bordé
      Fmat22 = Fmat22+Fmat2 ! matièrelisses
      Fmat33 = Fmat33+Fmat3 ! matièrecadres
      Fsou11 = Fsou11+Fsou1 ! matière soudage lisses
      Fsou22 = Fsou22+Fsou2 ! matière soudage cadres
      Fsou33 = Fsou33+Fsou3 ! matière soudage nappes
      FMdO11 = FMdO11+FMdO1 ! MdO soudage lisses sur nappe
      FMdO22 = FMdO22+FMdO2 ! MdO constitution de lisses synthetiques + extra works
      FMdO33 = FMdO33+FMdO3 ! MdO soudage cadres sur nappe
      FMdO44 = FMdO44+FMdO4 ! MdO constitution de cadres synthetiques + extra works
      FMdO55 = FMdO55+FMdO5 ! MdO pour découpage des slot
      FMdO66 = FMdO66+FMdO6 ! MdO soudage des TAP et goussets
      FMdO77 = FMdO77+FMdO7 ! MdO assemblage et soudage des nappes

	FFmat = Fmat1+Fmat2+Fmat3
	FFsou = Fsou1+Fsou2+Fsou3
      FFMdO = FMdO1+FMdO2+FMdO3+FMdO4+FMdO5+FMdO6+FMdO7
	Fmat  = Fmat  + FFmat
	Fsou  = Fsou  + FFsou
	FMdO  = FMdO  + FFMdO
      FCT   = FFmat + FFsou + FFMdO
      COUT  = COUT  + FCT

	WRITE(66,*)
	WRITE(66,*) 'Extra Weight induced by corrosion',					!05.12.05
     *            ' (girder and secondary members not considered !!!)'	!05.12.05
	WRITE(66,*) '-------------------------------------------'
	PPo=Poid1-SPoid1
	WRITE(66,*) 'Net   weight =',PPo   , 'in N'
	WRITE(66,*) 'Gross weight =', Poid1, 'in N'
	WRITE(66,*) 'Extra weight =',SPoid1, 'in N'
      WRITE(66,*)

	WRITE(66,*) 'Cost of the panel (without girder)',
     *            ' with the SIMPLIFIED COST MODEL'
	WRITE(66,'(70(1H-))') 
	WRITE(66,*) 'Total Cost =     Fmat   +     Fsou    +    FMdO ',
     *              '   in Euro,$,...'
      WRITE(66,'(4(2x,E11.4) )') FCT,FFmat,FFsou,FFMdO
      WRITE(66,*)
      
      IF(ICOUT.LT.2) THEN			! Imp.diff. selon module coût simplifié ou pas !newcost		!obj inertie
	 IF(NEL.EQ.1) THEN
	  WRITE(67,'(2A)')'Panel#:   Cost =    Fmat   +    Fsou +   FMdO',
     *         ' (Euro,$ or..) - Gross Weight and Corrosion weight(N)'
	  WRITE(67,'(98(1H-))') 
	 ENDIF
       WRITE(67,'(2x,I3,6(1x,E11.4) )') NEL,FCT,FFmat,FFsou,FFMdO,
     *                                 Poid1,SPOID1
      ENDIF !newcost

	IF(NEL.EQ.NETO) THEN
	 PPo=Poids-Spoids
	 Spoids1 = Spoids				!05.12.05
       IZ=66
  20   CONTINUE
	 WRITE(IZ,*) 'Total Weight: Complete structure (girders and' 
       WRITE(IZ,*)	 'secondary members excluded)'						!05.12.05
	 WRITE(IZ,*) '-------------------------------------------------'
       WRITE(IZ,*) '   Weight (net   thickness)       = ',PPo   ,' N'
       WRITE(IZ,*) '   Weight (corrosion thickness)   = ',SPoids,' N'		!05.12.05
       WRITE(IZ,*) '   Weight (Gross thickness)       = ',Poids ,' N'
       WRITE(IZ,*)
	IF(IMPR2.GE.0)THEN								!15.10.05
	 WRITE(IZ,*) 'Total cost: Complete structure (girders excluded)'
	 WRITE(IZ,*) '-------------------------------------------------'
       WRITE(IZ,*) '1* Material from plating       = ',Fmat11,' Euro,$'
       WRITE(IZ,*) '   Material from long stiff.   = ',Fmat22,' Euro,$'
       WRITE(IZ,*) '   Material from trans. frame  = ',Fmat33,' Euro,$'
       WRITE(IZ,*) '   Material (TOTAL)            = ',Fmat  ,' Euro,$'
       WRITE(IZ,*)
       WRITE(IZ,*) '2* Consomables :stiff          = ',Fsou11,' Euro,$'
       WRITE(IZ,*) '   Consomables :frame          = ',Fsou22,' Euro,$'
       WRITE(IZ,*) '   Consomables :plate          =    not considered'
       WRITE(IZ,*) '   Consomables (TOTAL)         = ',Fsou  ,' Euro,$'
       WRITE(IZ,*)
       WRITE(IZ,*) '3* Labor Cost for'
       WRITE(IZ,*) '   - stiff. on plate      (P4) = ',FMdO11,' Euro,$'
       WRITE(IZ,*) '   - stiff buiding        (P9) = ',FMdO22,' Euro,$'
       WRITE(IZ,*) '   - frames. on plate     (P5) = ',FMdO33,' Euro,$'
       WRITE(IZ,*) '   - frames. building     (P9) = ',FMdO44,' Euro,$'
       WRITE(IZ,*) '   - SLOT cutting         (P6) = ',FMdO55,' Euro,$'
       WRITE(IZ,*) '   - Stiff.Bracket,TAP    (P7) = ',FMdO66,' Euro,$'
       WRITE(IZ,*) '   - plating building     (P10)= ',FMdO77,' Euro,$'
       WRITE(IZ,*) '   Labor Cost (TOTAL)          = ',FMdO  ,' Euro,$'
       WRITE(IZ,*)
 	ENDIF												!15.10.05
       WRITE(IZ,*) '*** TOTAL COST  (1+2+3)        = ',COUT  ,' Euro,$'
	 WRITE(IZ,*) '      !!! Using the simplified LBR5 model !!! '
       WRITE(IZ,*) ' This result will differ using a detailled database'
       WRITE(IZ,*)
	 IF(ICOUT.LT.2) THEN ! Opt. Poids ou coût selon module coût simplifié !newcost	!obj inertie
         IF(IZ.EQ.66) THEN
	     IZ=67
	     GOTO 20
	   ENDIF
	 ENDIF  !newcost
c	 REWIND 57												!corrosion (mise en comm.)
	ENDIF

      RETURN
      END


C***********************************************************************
C***********************************************************************

      SUBROUTINE SEMEL(HYA,DYA,WYA,TYA,XMIN,XMAX,IND,KK,IPRINT,
     *                 NEL,ISEM,ITYPE,ISECT,IMPR2)	!15.10.05						!février 2004

      IMPLICIT REAL *8 (A-H,O-Z)
      CHARACTER*6 TYPE(3)

C***********************************************************************
C     SUBROUTINE SEMEL
C     ****************
C     Cette subroutine de modification des dimensions des semelles en fonction 
C     de restrictions géométriques.
C
C     DW = hauteur   de l'âme (web),
C     TW = épaisseur de l'âme (web),
C     DF = hauteur   de la semelle (flange),
C     TF = épaisseur de la semelle (flange),
C                                                                       
C     Création     : 6-06-96 (Ph. Rigo pour L.B.R.-5)                                           
C     Modif: 13-3-98 : ISEM=1: Df < 16.Tf  et ISEM=2: Df < 32.Tf
C		   02-09-05 : ISEM = 3, 4 et 5
C
C     Last Modification:13-3-98
C					  02-09-2005
C
C***********************************************************************
C   Les variables de conception  ; NBRXI = NVAR(NEL)
C   -------------------------------------------------
C       1       DELTA =  épaisseur du bordage
C       2       HYA   =  hauteur de l'âme des aiguilles
C       3       DYA   =  épaisseur de l'âme des aiguilles
C       4       WYA   =  largeur des semelles des aiguilles
C       5       EPSA  =  entredistance entre aiguilles
C	  6	      HXR   =  hauteur de l'âme des raidisseurs
C	  7  	  DXR   =  épaisseur de l'âme des raidisseurs
C  	  8	      WXR   =  largeur des semelles des raidisseurs
C	  9	      EPSR  =  entredistance entre raidisseurs
c	Variables de conceptions pour les épontilles:				!février 2004
c		1		HYA	=	hauteur de la demi-âme					!février 2004
c		2		DYA	=	épaisseur de l'âme						!février 2004
c		3		WYA	=	largeur de la semelle					!février 2004
c		4		EPAIS=	épaisseur de la paroi mince				!février 2004
c		5		EPSA=	entredistance entre épontilles			!février 2004

	TYPE(1)='cadre'
	TYPE(2)='raid.'
	TYPE(3)='épont.'											!février 2004

C ****************************************************************
C 1.    Initialisations et calcul des ratios AL1, AL2 et AL3
C ****************************************************************
	
	DW=HYA
	TW=DYA
	DF=WYA
	TF=TYA
	IF (ISECT.EQ.3) THEN										!février 2004
	DW=2*HYA													!février 2004
	ENDIF														!février 2004

C     Pour condition Tw < 2.Tf
      AL1= 2.*TW/TF
	
	IF(ISEM.EQ.1) THEN
C       Pour condition  Df < 16.Tf
        SQ2= SQRT(2.)
C       Pour condition 8.Tf < Df
        AL2= DSQRT(DF/(8.*TF))
	ELSEIF(ISEM.EQ.2) THEN
C       Pour condition  Df < 32.Tf
        SQ2= 2.
C       Pour condition 8.Tf < Df
        AL2= DSQRT(DF/(8.*TF))
	ELSEIF(ISEM.EQ.3) THEN
C       Pour condition  Df < 30.Tf
        SQ2= SQRT(2.5)
C       Pour condition 12.Tf < Df
        AL2= DSQRT(DF/(12.*TF))
	ELSEIF(ISEM.EQ.4) THEN
C       Pour condition  Df < 15.Tf
        SQ2= 3.273
C       Pour condition 1.4.Tf < Df
        AL2= DSQRT(DF/(1.4*TF))
	ELSEIF(ISEM.EQ.5) THEN
	  AL1 = AL1*2.
C       Pour condition  Df < 15.Tf
        SQ2= 1.581
C       Pour condition 6.Tf < Df
        AL2= DSQRT(DF/(6*TF))
	ENDIF
	AL3= SQ2/AL2

	IF(IPRINT.GE.1) THEN
	     WRITE(666,*) 'AL1, AL2, AL3 =',AL1,'  ',AL2,'  ',AL3
	ENDIF
c	WRITE(666,*) 'valeurs initiales'
c	WRITE(666,*) '------------------'
c	WRITE(666,*) 'DF=',DF,'  TF=',TF

	
C ****************************************************************
C 2.  Algorithme  pour AL1>1    c.à.d  Tw < 2.Tf  est vérifié
C ****************************************************************
      IF (AL1.LT.1) GOTO1

c	WRITE(666,*) 'AL1 >1    § 2'

	AL11 = AL1/2.

	IF (AL11.LT.1.OR.ISEM.EQ.1.OR.ISEM.EQ.2) THEN !!!Condition Tf<Tw respectée

C 2.1  1 < AL2 < 1.41 (ou 2)
C      ---------------------
      IF ((AL2.GE.1).AND.(AL2.LE.SQ2)) GOTO2

C 2.2  AL2 < 1
C      ---------
      IF (AL2.LT.1) THEN
	   DF=DF/AL2
	   TF=TF*AL2
c	WRITE(666,*) 'AL1 >1  et  AL2 <1'
c	WRITE(666,*) 'DF=',DF,'  TF=',TF
	ENDIF

C 2.3  AL2 > 1.41 (ou 2)  (AL3 < 1)
C      ----------------------------
      IF (AL2.GT.SQ2) THEN
	   DF=DF*AL3
	   TF=TF/AL3
c	WRITE(666,*) 'AL1 >1  et  AL2 > 1.41 (ou 2)'
c	WRITE(666,*) 'DF=',DF,'  TF=',TF
	ENDIF

	ELSE							!!!Condition Tf<Tw pas respectée
	
C      1 < AL2 < 1.41 (ou 2)
C      ---------------------
      IF ((AL2.GE.1).AND.(AL2.LE.SQ2)) THEN
	   DF=DF/AL11
	   TF=TF*AL11
	ENDIF

C       AL2 < 1
C      ---------
      IF (AL2.LT.1) THEN
	   AL33 = 0.667*AL2 + 0.333*AL11
	   IF (AL33.GT.1) AL33=1.
	   DF=DF/AL33
	   TF=TF*AL33
	ENDIF
	
C      AL2 > 1.41 (ou 2)  (AL3 < 1)
C      ----------------------------
      IF (AL2.GT.SQ2) THEN
	   AL33=1./AL3
	   COEFF = MAX(AL11,AL33)
	   DF=DF/COEFF
	   TF=TF*COEFF
	ENDIF

	ENDIF !Fin condition sur Tf/Tw
		
      GOTO 2

   1  CONTINUE
C *****************************************************************
C 3.  Algorithme pour AL1<1     c.à.d  Tw < 2.Tf  n'est pas vérifié
C *****************************************************************
      AL22= AL2/AL1  ! on ne s'en sert pas apparemment

c       WRITE(666,*) 'AL1 <1    § 3'
c       WRITE(666,*) 'AL22= AL2/AL1 =',AL22

C 3.1  1 < AL22 < 1.41 (ou 2)
C      ---------------------
      IF ((AL2.GE.1).AND.(AL2.LE.SQ2)) THEN
           DF=DF/AL1
           TF=TF*AL1
c	WRITE(666,*) '1 < AL22 < 1.41 (ou 2)'
c	WRITE(666,*) 'DF=',DF,'  TF=',TF
	ENDIF

C 3.2  AL22 < 1
C      ---------
      IF (AL2.LT.1) THEN
	   COEFF = MIN(AL1,AL2)
	   DF=DF/COEFF !AL2
	   TF=TF*COEFF !AL2
c	WRITE(666,*) ' AL22 < 1'
c	WRITE(666,*) 'DF=',DF,'  TF=',TF
	ENDIF

C 3.3  AL2 > 1.41 (ou 2)  (AL3 < 1)   (! il s'agit bien de AL2 et pas de AL22)
C      -------------------------------------
      IF (AL2.GT.SQ2) THEN
           AL33=0.6667 * AL3 + 0.333 /AL1
         IF (AL33.GT.1.) AL33=1.
	   DF=DF*AL33
	   TF=TF/AL33
c	WRITE(666,*) ' AL22 > 1.41 (ou 2)'
c	WRITE(666,*) 'DF=',DF,'  TF=',TF
	ENDIF
	
	   
   2  CONTINUE
C ****************************************************************
C 4.  Vérification des bornes
C ****************************************************************
        K=0
      IF (DF.LT.XMIN) THEN
	  WRITE(666,*) ' Borne atteinte  DF=',DF,'  XMIN=',XMIN
	  WRITE(29 ,*) ' Borne atteinte  DF=',DF,'  XMIN=',XMIN							!bug
	  DF=XMIN
	  K=1
	ELSE IF (DF.GT.XMAX) THEN
	  WRITE(666,*) ' Borne atteinte  DF=',DF,'  XMAX=',XMAX
	  WRITE(29 ,*) ' Borne atteinte  DF=',DF,'  XMAX=',XMAX							!bug
	  DF=XMAX
	  K=1

	ENDIF

c      IF (DF.LT.(DW/2.)) THEN
c	  DF=DW/2.
c	  K=1
c	ELSE IF (DF.GT.DW) THEN
c	  DF=DW
c	  K=1
c	ENDIF
      
      IF (K.EQ.1) THEN
	   TF=WYA*TYA/DF
c          WRITE(666,*) ' Après modif des bornes'
c          WRITE(666,*) 'DF=',DF,'  TF=',TF
        ENDIF

   3  CONTINUE


C ******************************************************************
C 5.  calcul des coefficients pour modifier les sensibilités
C     concernant les restrictions  2.Df < Dw  < Df (code 201 et 202)
C ******************************************************************
C     Pour la condition Tf < 2.Tw avec les nouvelles valeurs Df et Tf
      AL1= 2.*TW/TF
      IF (AL1.LT.1) THEN
	  IND=1
	  IF(IPRINT.GE.1) THEN
	     WRITE(666,*) 'La condition (Tf < 2.Tw) n''est pas',
     *                 ' respectée'
		 WRITE(29 ,*) 'La condition (Tf < 2.Tw) n''est pas',					!bug
     *                 ' respectée'												!bug
	  ENDIF
	ELSE
        IND=0
	ENDIF



C ****************************************************************
C 6.  Résultats
C ****************************************************************
	IF(IMPR2.GE.-1)THEN				!15.10.05
	WRITE(666,100) NEL,WYA,TYA,DF,TF,TYPE(KK),IND
c	WRITE(  *,100) NEL,WYA,TYA,DF,TF,TYPE(KK),IND
	ENDIF						!15.10.05

	WYA=DF
	TYA=TF

      RETURN

 100  FORMAT(3x,I2,2x,E13.6,1X,E13.6,4X,E13.6,1X,E13.6,4X,A5,3x,I2)
      END

c ----------------------------------------------------

	SUBROUTINE SELECT(A,B,D,E)        !!!aout04
C     *****************************
      IMPLICIT REAL*8 (A-H,O-Z)

	DIMENSION D(9),E(9)	!!!aout04
C     A,B  INPUTS
C     C    OUTPUT avec C la valeur de maximun de A et B mais avec le signe
c       ex: A=-6 et B=5, d'où A= -6 car Abs(A) > Abs(B)

      C=DMIN1(A,B)
      A=DMAX1(A,B)
	IF (B.GT.A) THEN				!!!aout04
	  DO I=1,9						!!!aout04
	    D(I)=E(I)					!!!aout04
	  ENDDO							!!!aout04
	ENDIF							!!!aout04
	IF(DABS(A).LE.DABS(C)) THEN		!!!aout04
	  A=C							!!!aout04
	  IF (B.LT.A) THEN				!!!aout04
	    DO I=1,9					!!!aout04
	      D(I)=E(I)					!!!aout04
	    ENDDO						!!!aout04
	  ENDIF							!!!aout04
	ENDIF							!!!aout04

      RETURN
      END

