      SUBROUTINE COPY(ITERA,DON1,LM2,NETO,NVAR,MODES,
     *                NXIT,XICOU,TFA,TFR,XIMIN,XIMAX)

C     ********************************

      IMPLICIT REAL *8(A-H,O-Z)

      COMMON/NOM/NOM,NOM1  
	CHARACTER*11 NOM
      CHARACTER*22 NOM1  !'A.MULTI.STRUC\STRUC-a\'

      CHARACTER *1  NUM
      CHARACTER *24 DON1
      CHARACTER *3  MODES(NETO)
      CHARACTER *20 TEXT(5)
      INTEGER   *2  DESSIN
	REAL *8 KGMIN,KGMAX,KGY ! cfr COMMON/GRAV
	REAL *8 IMIN,MODMIN		!restri inertie

      COMMON/PY/   PI
      COMMON/OPTI/ IOPTI,NTOT,M1TOT,M2TOT

	DIMENSION DIS(5),FAM(6),NNO(2),MEGA(4),NNSOL(10)

c     COMMON/OPTI/ NVAR(30),  NXIT(9,30)
      DIMENSION    NVAR(NETO),NXIT(9,NETO) 

	DIMENSION    LM2(1)
      
c     COMMON/OPTI4/XIMIN(200), XIMAX(200)
	DIMENSION    XIMIN(NTOT),XIMAX(NTOT)

c     COMMON/OPTI5/XICOU(200), TFA(30),  TFR(30)
      DIMENSION    XICOU(NTOT),TFA(NETO),TFR(NETO)

      COMMON/USHULL/	DEPTH,DB,UHOGM,USAGM,
     *				SYSD,SySB1,SySB,SySSup,SySSdw,
     *                IULT,IRESTR,
     *				ND, ID(150),		!extension neto
     *				NB1,IB1(150),		!extension neto
     *				NB, IB(150),		!extension neto
     *				NS, IS(150),		!extension neto
     *				KD,KB1,KB,KSup,KSdw

	COMMON/GRAV/KGMIN,KGMAX,XK,YK,GRAV,IKGY

c
C***********************************************************************
C     SUBROUTINE COPY
C     *****************
C     Subroutine de sauvetage des données après chaque itération
C     en vue d'une réutilisation ultérieure.
C
C     Création: 21 mars 1997 ; Dr. Ph. Rigo
c
C     Modif.  : 05-03-99
c               06-12-99
C               29-12-99 (Sauvetage File 268 pour desin)
c               10-11-00 (Rest. Centre de gravité)
c               26-11-03
c			  05-02-04 (épontilles)
C***********************************************************************

      REWIND 55
c     OPEN(77,file='disque dur:L.B.R.-5:UPDATED-DATA:Up-'//DON1)
c     OPEN(77,file='Up-'//DON1)

      NUM=CHAR(96+ITERA)
      OPEN(77,file=NOM1//'Up-'//NUM//'-'//DON1)

C     Ligne 0							!Nom de la version
      READ (55,'(5A20)') TEXT
      WRITE(77,'(5A20)') TEXT

	READ(55,*) IANA						!r&d14
	WRITE(77,'(I2)') IANA				!r&d14

C     Ligne 1
      READ (55,'(5A20)') TEXT
      WRITE(77,'(5A20)') TEXT

C     Ligne 2
      READ(55,*)  IMPR,IMPR2,INDAIG,INDRAI,DESSIN,JLPH,JLBORD,NETO
      WRITE(77,'(5(I2,1x),I3,1x,I2,1x,I3,21x,A)') IMPR,IMPR2,INDAIG,	!extension neto
     *                  INDRAI,DESSIN,JLPH,JLBORD,NETO,'Commentaire'

	READ(55,*) IMULTI,RHO,W1,W2,W3									!multi obj
	WRITE(77,'(I2,1x,4(F8.4,1X),T45,A))') IMULTI,RHO,W1,W2,W3,		!multi obj
     *     'IMULTI,RHO,W1,W2,W3'										!multi obj

C     Ligne 3
      READ(55,*)  IOPTI,ITERAM
      WRITE(77,'(I2,1x,I3,T40,A))') IOPTI,ITERAM,
     *     'IOPTI (1 oui, 0 non) et ITEMAX = nbre d''itération'
	
C     Ligne 4 à 13
      READ(55,*) ICOUT             ! ICOUT
      READ(55,*) REND, EQP         ! REND, EQP
      READ(55,*) Dref,DrefX,DrefY  ! Epaisseurs de référence
      READ(55,*) C1,C2,C3,DC1      ! Cout des matériaux
      READ(55,*) DW2,DW3           ! Extra weight
      READ(55,*) P10,DP10          ! MdO bordé
      READ(55,*) P4,P5,DP4,DP5     ! MdO assembl. Membr-bordé
      READ(55,*) P9X,P9Y,DP9X,DP9Y ! MdO constr. Membr.
      READ(55,*) P6,P7,BER,BET     ! MdO Intersect et goussets
      READ(55,*) C8,DC8,IALR,IALT  ! Coût Soudure (Energ + consom.


      WRITE(77,'(I2,T45,A)')		      ICOUT,   'ICOUT'
      WRITE(77,'(F8.4,1X,F8.4,T45,A)')  REND,EQP,
     *'REND,EQP(0.01<k=EQP<0.1)'
      WRITE(77,'(3(F8.5,1x),T45,A)') Dref,DrefX,DrefY,
     *'Eo, Eox, Eoy  (épaisseur de référence en m)'
      WRITE(77,'(4(F8.3,1x),T45,A)')  C1,C2,C3,DC1,
     *'Prix Matér. : C1,C2,C3(tole,Euro/kg), DC1(tole,variation par mm)'
      WRITE(77,'(2(F8.3,1x),T45,A,A)')  DW2,DW3,'Extra weight: ',
     *'DW2(long,variation sur C2),DW3(trans,variation sur C3)'
      WRITE(77,'(2(F8.3,1X),T45,A)')  P10,DP10,
     *'MdO Bordé   : P10(h-h/m),  PC10(variation par mm) '
      WRITE(77,'(4(F8.3,1x),T45,A,A)')  P4,P5,DP4,DP5,'MdO Assembl.: ',
     *'P4x(Long,h-h/m),P5y(trans,h-h/m),DP4x(long),DP5y(trans)'
      WRITE(77,'(2X,4(F8.3,1x),T45,A,A)')  P9X,P9Y,DP9X,DP9Y,
     *'MdO Membr.  ',
     *': P9x(long,h-h/m),P9y(trans,h-h/m),DP9x(long),DP9y(trans)'
      WRITE(77,'(4(F8.3,1x),T45,A,A)')  P6,P7,BER,BET,'MdO Joints  ',
     *': P6(intersect),P7(gousset),Béta-X(long.),Béta-Y(transv), '
      WRITE(77,'(2(F8.3,1x),2(I2,6X),T45,A,A)')  C8,DC8,IALR,IALT,
     *'Consommables: C8(Euro/m),',
     *'DC8(variation par mm),Alpha-X(long.),Alpha-Y(transv)'

C     Lignes 14,15,16,17
      READ(55,*)  WIDTH
      READ(55,*) (DIS(I),I=1,5)
      READ(55,*) (FAM(I),I=1,6)
      READ(55,*) IPOIDS

        FAM(5)=0.
        FAM(6)=0.

      WRITE(77,'(F12.5,T70,A)') WIDTH,'Longueur de la structure (WIDTH)'
      WRITE(77,'(5(F12.5,1x),T70,A)') 
     *           (DIS(I),I=1,5),'Section de calcul (DIS)'
      WRITE(77,'(6(F6.3,1x),T70,A)') 
     *           (FAM(I),I=1,6),'Cisaillement des semelles (FAM)'
      WRITE(77,'(I2,T30,A)') IPOIDS,'IPOIDS ( 1 OUI, 0 NON)'

C     Ligne 18
      READ(55,*)   NSOLM, NSOL,(NNSOL(I),I=1,NSOL)    
      WRITE(77,12) NSOLM, NSOL,(NNSOL(I),I=1,NSOL)

      DO I=1,NSOLM                !nov2003
	  READ (55,'(5A20)') TEXT   !nov2003
	  WRITE (77,'(5A20)') TEXT  !nov2003
	ENDDO                       !nov2003

  12  FORMAT(12(I3),5x,
     *  'Load cases: Nbr available; Nbr selected, List of selected')


C 1.0 LECTURE DES DONNEES RELATIVES A CHAQUE PANNEAU
C     ================================================
      NTOT=0

C     File opened in  MAIN.for
C     OPEN(46,STATUS='SCRATCH',FORM='UNFORMATTED') ! Pour le dessin (subr ENT,VISIONN
      REWIND(46)

      DO 1 NEL=1,NETO

      CALL ENT2(NVAR(NEL),NXIT(1,NEL),LM2,NSOLM,MODES(NEL),DESSIN,
     *          XICOU,TFA,TFR,XIMIN,XIMAX,NEL,NETO)				
C     ******************************************

    1 CONTINUE

C ----------------------------------------------------------------------

C **********************************************************************
C 2.0 TRANSFORMATIONS DES DONNEES   (Subr. MODIF et COORD)
C     ====================================================

      READ (55,*) TEXT(1)
      WRITE(77,'(5A29)') 'BOUNDARY CONDITIONS; Y=0, Y=H'
      READ(55,*) NCONDI
      WRITE(77,'(I3,T30,A)')  NCONDI,'Nbre de conditions de bords'	!extension neto

      IF(NCONDI.GT.0) THEN
      DO 910 I=1,NCONDI
      READ(55,*) (NNO(J),J=1,2)
      WRITE(77,'(I3,3X,I2,T30,A,I3,A,I3)')(NNO(J),J=1,2),			!extension neto
     *                      'Cond nø',I,',  Panel ',NNO(1)
  910 CONTINUE
      ENDIF

      READ (55,'(5A20)') TEXT
      WRITE(77,'(5A65)') 'GRAVITY REPERE UTILISATEUR + RESTRICTION SUR L
     *E CENTRE DE GRAVITE'
      READ(55,*) XK,YK  ! XK et YK = Coord du centre K du repere utilisateur
      WRITE(77,'(F12.6,2X,F12.6,T30,A)') XK,YK,
     *                '(XK,YK) Coord du centre du repere utilisateur'
      READ(55,*) IGRAV,KGMIN,KGMAX  ! KGMIN=KG(min) et KGMAX=KG(max)
      WRITE(77,'(I2,2X,F9.6,2X,F9.6,T35,A)') IGRAV,KGMIN,KGMAX,
     *           'IGRAV(NO:0;Min:1;MAX:2;Min et max:3),KGMIN,KGMAX'

C **********************************************************************

C     RESTRICTION SUR INERTIE									!restri inertie
C     =======================

      READ (55,'(5A20)') TEXT
	WRITE(77,'(5A18)') 'INERTIA CONSTRAINT'
	READ(55,*) INERT,IMIN
	WRITE(77,'(I2,2X,E14.7,T30,A)') INERT,IMIN,'INERT,IMIN'	!restri inertie


C     RESTRICTION SUR MODULE SECTIONNEL							!restri module
C     =================================

      READ (55,'(5A20)') TEXT
	WRITE(77,'(5A26)') 'SECTION MODULUS CONSTRAINT'
	READ(55,*) IMOD,MODMIN,IPAN
	WRITE(77,'(I2,2X,E14.7,2X,I2,T40,A)') IMOD,MODMIN,IPAN,
     *'IMOD,MODMIN,IPAN'											!restri module


C     RESTRICTION SUR POIDS										!restri poids
C     =====================

      READ (55,'(5A20)') TEXT
	WRITE(77,'(5A17)') 'WEIGHT CONSTRAINT'
	READ(55,*) IWEIGHT,WMAX
	WRITE(77,'(I2,2X,E14.7,T30,A)') IWEIGHT,WMAX,'IWEIGHT,WMAX'		!restri poids


C     RESTRICTION SUR COUT											!restri cout
C     =====================

      READ (55,'(5A20)') TEXT
	WRITE(77,'(5A15)') 'COST CONSTRAINT'
	READ(55,*) IPRICE,PRICEMAX,ICOST
	WRITE(77,'(I2,2X,E14.7,2X,I2,T30,A)') IPRICE,PRICEMAX,ICOST,
     *									  'IPRICE,PRICEMAX,ICOST'	!restri cout
	

C 3.0 MOMENTS d'EXTREMITES   (Subr. BATEAU)
C     ======================================
      READ (55,'(5A20)') TEXT
      WRITE(77,'(5A6)') 'MOMENT'
      READ(55,*) IMOM
      WRITE(77,'(I2,T30,A)')IMOM,'(= 0:pas de moment, = 1:avec moment)'

      IF (IMOM.NE.0) THEN
         READ (55,*) YRED
         WRITE(77,'(F8.5,T30,A)')YRED,
     *          'Coeff. de réduction 1.24 pour Sig. et 1.04 pour W'
         DO 733 I=1,NSOLM
           READ (55,*) BM1,BM2,BM3,BM4                                  !nov2003
           WRITE(77,'(4(1X,E14.7),T63,A)') BM1,BM2,BM3,BM4,             !nov2003
     *                        'Mx> 0 en Hogging = pont tendu (N.m); My' !nov2003
  733    CONTINUE
      ENDIF

C **********************************************************************
C 4.0 LECTURE DES CONTRAINTES D'EGALITE (et impression)
C     =================================================
      READ (55,'(5A20)') TEXT
      WRITE(77,'(5A21)') 'EQUALITY RESTRICTIONS'
      READ(55,*) NEGALT
      WRITE(77,'(I4,T30,A,A)')NEGALT,'Nbre de restrictions d''égalité'

      DO 1333 I=1,NEGALT
       READ(55,*) MEGA,EGA
       WRITE(77,'(I3,2X,I3,6X,I3,2X,I3,6X,F8.4)')MEGA,EGA
 1333 CONTINUE

C **********************************************************************
C 5.0 DONNEES POUR LE CALCUL DE LA RESISTANCE ULTIME DE LA POUTRE NAVIRE
C     ==================================================================

      READ (55,'(5A20)') TEXT
      WRITE(77,'(5A17)') 'ULTIMATE STRENGHT'

      READ(55,*) IRESTR
      WRITE(77,'(I2,T15,A)')IRESTR,
     *  '(IRESTR: utilisation comme restriction ; 0 = NON;1 = OUI)'

      IF(IRESTR.EQ.1) THEN
          READ(55,*) UHOGM,USAGM
        WRITE(77,'(T2,E14.7,2X,E14.7,4X,A)')UHOGM,USAGM,
     *     '(if IREST=1 : UHOGG>0 et USAGG<0 (N.m)'
        ENDIF

      READ(55,*) IULT
      WRITE(77,'(I2,T15,A,A)')IULT,
     *           '(IULT =0 No analysis, = 1 for PAIK/Caldwell,',
     *                ' =2 for PROCOL(Smith), = 3  for...)'

      IF(IULT.EQ.1) THEN
        READ(55,*) DEPTH,DB
        WRITE(77,'(T4,2(F8.4,2X),T30,A)')DEPTH,DB,
     *                'Hull Depth & Double bottom hight (m.)'

        READ(55,*) ND, (ID (i), i=1,ND)
        READ(55,*) NB1,(IB1(i), i=1,NB1)
        READ(55,*) NB, (IB (i), i=1,NB)
        READ(55,*) NS, (IS (i), i=1,NS)

        WRITE(77,301)  ND, (ID(i), i=1,ND)
        WRITE(77,302)  NB1,(IB1(i),i=1,NB1)
        WRITE(77,303)  NB, (IB(i), i=1,NB)
        WRITE(77,304)  NS, (IS(i), i=1,NS)
 301    FORMAT(4X,I2,(20I3/),' Deck') 
 302    FORMAT(4X,I2,(20I3/),' Inner Bottom')
 303    FORMAT(4X,I2,(20I3/),' Outer Bottom')
 304    FORMAT(4X,I2,(20I3/),' Both Sides Plates')

        READ(55,*) KD,  SySD
        READ(55,*) KB1, SySB1
        READ(55,*) KB,  SySB
        READ(55,*) KSup,SySSup
        READ(55,*) KSdw,SySSdw

        WRITE(77,'(6x,I2,2x,E14.7,T30,A)')
     *             KD,  SySD, 'Ref panel & Sy for UPPER DECK'
        WRITE(77,'(6x,I2,2x,E14.7,T30,A)')
     *             KB1, SySB1,'Ref panel & Sy for INNER BOTTOM'
        WRITE(77,'(6x,I2,2x,E14.7,T30,A)')
     *             KB,  SySB, 'Ref panel & Sy for OUTER BOTTOM'
        WRITE(77,'(6x,I2,2x,E14.7,T30,A)')KSup,SySSup,
     *    'Ref panel & Sy for UPPER PART of the SIDE PLATE'
        WRITE(77,'(6x,I2,2x,E14.7,T30,A)')KSdw,SySSdw,
     *    'Ref panel & Sy for LOWER PART of the SIDE PLATE'
        ENDIF

      CLOSE(77)
      RETURN

C ======================================================================
C11.0 LES FORMATS
C     ============

      END

C **********************************************************************
C **********************************************************************
C **********************************************************************
C **********************************************************************
      SUBROUTINE ENT2(NBRXI,NXI,LM2,NSOLM,MODE,DESSIN,
     *                XICOU,TFA,TFR,XIMIN,XIMAX,NEL,NETO)	

      IMPLICIT REAL *8 (A-H,O-Z)

      CHARACTER*9  TYPES									!février 2004
      CHARACTER*3  MODE,BID
      CHARACTER*7  SECTION								!février 2004
      CHARACTER*20 TEXT(5)
      INTEGER  *2  DESSIN

      COMMON /PY/PI
      COMMON /OPTI/ IOPTI,NTOT,M1TOT,M2TOT

      DIMENSION NXI(9),XMIN(9),XMAX(9),CHA(3),YPT(10),NOH(10)

      DIMENSION LCONT(2),LM2(M2max)

      DIMENSION XIMIN(NTOT),XIMAX(NTOT)
      DIMENSION XICOU(NTOT),TFA(NETO),TFR(NETO)

	DIMENSION DVMIN(9),DVMAX(9)							!eugen	!fev2007

      COMMON/DIM1/Nmax,NEmax,NVmax,
     *            M1max,M1Tmax,M2max,M2Tmax,Mmax,NGmax,ISmax,IPTmax ! dimension max des vecteurs

C***********************************************************************
C     SUBROUTINE ENT2
C     ****************
C     SUBROUTINE de lecture DES DONNEES RELATIVES A CHAQUE PANNEAU.
C     Cette subr. est calquée sur la Subr ENT
C***********************************************************************

      PI1=PI/180.

C 1.a   Lecture du TYPES de panneau : COQUE OU PLAQUE
C     -----------------------------------------------
      READ(55,'(A9)') TYPES						!février 2004

      IF(TYPES.EQ.'PLAQUE') THEN
C       Cas de la "Plaque"
        READ(55,*,END=900) HIGHT,DELTA
        Q=10000000000.
        PHIL=HIGHT/Q/PI1
      ELSEIF(TYPES.EQ.'EPONTILLE') THEN			!février 2004 
C	  Cas de l'"épontille"						!février 2004
	  READ(55,*,END=900) HIGHT,EPAIS			!février 2004 
	  READ(55,3,END=900) SECTION			    !février 2004  
 3	  FORMAT(A7)								!février 2004
	  Q=10000000000.							!février 2004
	  PHIL=HIGHT/Q/PI1							!février 2004
	  DELTA=0.00000								!février 2004
	ELSE				
C       Cas de la "Coque"
        READ(55,*,END=900) PHIL,Q,DELTA
      ENDIF

C 1.b   Donnees relatives au raidissage (aig. et raid)
C     ----------------------------------------------
C     ENTR = entredistance réelle des raid.=(D)raid
C     EPSR = largeur collaborante compte tenu du nbre de raid. défini
C            par MODE (TYPES de répartition)
c     EPSA=(D)aig

	IF(TYPES.NE.'EPONTILLE') THEN							!février 2004
      READ(55,*,END=900) EPSA,ENTR,EPSA2,ENTR2			
      READ(55,*,END=900) HYA,DYA,WYA,TYA				
      READ(55,*,END=900) HXR,DXR,WXR,TXR				
	ISECT=0													!février 2004
	EPAIS=0.000												!février 2004
	ELSE													!février 2004
      READ(55,*,END=900) EPSA,Heff							!février 2004
		IF(SECTION.EQ.'CERCLE')THEN							!février 2004
		ISECT=1												!février 2004
		READ(55,*,END=900) HYA								!février 2004
		DYA=0.00000											!février 2004
		WYA=0.00000											!février 2004
		TYA=0.00000											!février 2004
		ELSEIF(SECTION.EQ.'CARRE')THEN						!février 2004
		ISECT=2												!février 2004
		READ(55,*,END=900) HYA								!février 2004
		DYA=0.00000											!février 2004
		WYA=0.00000											!février 2004
		TYA=0.00000											!février 2004
		ELSEIF(SECTION.EQ.'DOUBLET')THEN					!février 2004
		ISECT=3												!février 2004
		READ(55,*,END=900) HYA,DYA,WYA,TYA					!février 2004
		EPAIS=0.0000										!février 2004
		ENDIF												!février 2004
	HXR=0.001												!février 2004
	DXR=0.001												!février 2004
	WXR=0.001												!février 2004
	TXR=0.001												!février 2004
	EPSR=Hight/5											!05.12.05
	ENTR=Hight/5											!05.12.05
	EPSA2=0.00000											!février 2004
	ENTR2=0.00000											!février 2004
	ENDIF													!février 2004

      IF((EPSA2.GE.(0.00001)).OR.(ENTR2.GE.(0.00001))) THEN
        READ(55,*,END=900) HYA2,DYA2,WYA2,TYA2,KSA2
        READ(55,*,END=900) HXR2,DXR2,WXR2,TXR2,KSR2
      ENDIF

	IF(TYPES.NE.'EPONTILLE') THEN							!février 2004
      READ(55,*,END=900) BID   !lecture de MODE (bypass)
	ELSE													!février 2004
	BID='EE1'												!février 2004
	ENDIF													!février 2004

      IF(MODE.EQ.'EE1'.OR.MODE.EQ.'ES4') THEN
            Xmode=-1.0
          ELSEIF(MODE.EQ.'ES1'.OR.MODE.EQ.'ES3'.OR.
     *      MODE.EQ.'EC1'.OR.MODE.EQ.'EL3')  THEN
            Xmode=-0.5
          ELSEIF(MODE.EQ.'SL1'.OR.MODE.EQ.'SL2'.OR.MODE.EQ.'EL2') THEN
            Xmode=+0.5
          ELSEIF(MODE.EQ.'LL1') THEN
            Xmode=+1.0
          ELSE
            Xmode=0.0
      ENDIF

      DENOM = (1.0/ENTR  + Xmode/(DABS(PHIL)*Q*PI1) )

 	IF((DENOM.LE.0.0).or.(ENTR/(DABS(PHIL)*Q*PI1).GT.1.01)) THEN
c	  WRITE(66,*)
c	  WRITE(66,*)'ATTENTION : ERROR dans le Panneau ',NEL
c	  WRITE(66,*)'****************************************'
c       WRITE(66,*)'Le paramètre MODE =',MODE,' est incompatible avec'
c	  WRITE(66,*)'    l''entredistance entre les raidisseurs=',ENTR
c       WRITE(66,*)'    et la largeur du panneau.'
c	  WRITE(66,*)'L''écartement entre les raidisseurs est '
c	  WRITE(66,*)'probablement supérieur à la largeur(L) du panneau.'
c	  WRITE(66,*)'Cela peut induire un EPSR erroné (<0 ou infini)!!!'     
c	  WRITE(66,*)'On va corriger cette situation!!'
        MODE='EE2'
	  Xmode=0.
        DENOM = 1.0/ENTR  ! --> EPSR=ENTR
	ENDIF
      EPSR = 1.0/DENOM
  
C-----------------------------------------------------------------------
C 1.c   Utilisation des nouvelles valeurs des variables de conception.
C     ==================================================================

      DO 123 I=1,NBRXI
        GOTO(124,125,120,126,127,128,121,129,130),NXI(I)
  124   DELTA=XICOU(I+NTOT)
        GOTO 123
  125   HYA =XICOU(I+NTOT)
        GOTO 123
  120   DYA =XICOU(I+NTOT)
        GOTO 123
  126   WYA =XICOU(I+NTOT)
        GOTO 123
  127   EPSA=XICOU(I+NTOT)
        GOTO 123
  128   HXR =XICOU(I+NTOT)
        GOTO 123
  121   DXR =XICOU(I+NTOT)
        GOTO 123
  129   WXR =XICOU(I+NTOT)
        GOTO 123
  130   EPSR=XICOU(I+NTOT)

        Temp= ((1.0/EPSR) - (Xmode/(DABS(PHIL)*Q*PI1)) )
 	  IF(temp.LE.0.0) THEN
	      WRITE(666,*)
	      WRITE(666,*)'ATTENTION : ERROR dans le Panneau ',NEL
	      WRITE(666,*)'  (Subr. COPY)'
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
	      WRITE(*,*)'  (Subr. COPY)'
	      WRITE(*,*)'****************************************'
            WRITE(*,*)'Le paramètre MODE est incompatible avec les'
	      WRITE(*,*)'bornes de variation de l''entredistance entre'
	      WRITE(*,*)'les raidisseurs et la largeur du panneau.'
	      WRITE(*,*)'Vous devez corriger cette situation !!'
	      WRITE(*,*)'Essayer en modifiant le paramère MODE'
	      WRITE(*,*)' - actuellement on a MODE =',MODE
	      WRITE(*,*)' - prenez plutôt     MODE = EE2'
	      WRITE(29,*)																	!bug		
	      WRITE(29,*)'ATTENTION : ERROR dans le Panneau ',NEL							!bug
	      WRITE(29,*)'  (Subr. COPY)'													!bug
	      WRITE(29,*)'****************************************'							!bug
            WRITE(29,*)'Le paramètre MODE est incompatible avec les'						!bug
	      WRITE(29,*)'bornes de variation de l''entredistance entre'					!bug
	      WRITE(29,*)'les raidisseurs et la largeur du panneau.'						!bug
	      WRITE(29,*)'Vous devez corriger cette situation !!'							!bug
	      WRITE(29,*)'Essayer en modifiant le paramère MODE'							!bug
	      WRITE(29,*)' - actuellement on a MODE =',MODE									!bug
	      WRITE(29,*)' - prenez plutôt     MODE = EE2'									!bug
	      PAUSE 'STOP!'
	      STOP
	  ENDIF
        ENTR=1.0/ Temp
        IF(NEL.EQ.1)  WRITE(666,'(/A/1x,50(1H-)/A)') 
     *        ' Espacement effectif entre raidisseurs long.(lisses)',
     *        ' (Spacing to consider for the long. stiffeners)'

        WRITE(666,'(A,I3,A,F8.4,A)') 
     *            ' Panneau=',NEL,': Entredistance(raid.)=',ENTR,' (m)'
c       WRITE(666,*) 'Xmode=',Xmode

  123 CONTINUE
        TYA=TFA(NEL)
        TXR=TFR(NEL)
C-----------------------------------------------------------------------

C 1.d   Impression du TYPES de panneau : COQUE OU PLAQUE
C     -----------------------------------------------
      WRITE(77,'(A9,T15,A,I3)') TYPES,'PANNEAU nø ',NEL			!février 2004	!extension neto

      IF(TYPES.EQ.'PLAQUE') THEN
C       Cas de la "Plaque"
        WRITE(77,'(F12.5,2x,F8.5,T50,A)')HIGHT,DELTA,'Haut.,Epaiss.(m)'
      ELSEIF(TYPES.EQ.'EPONTILLE') THEN									!février 2004
C	   Cas de l'"épontille"												!février 2004
      WRITE(77,'(F12.5,2x,F8.5,T50,A)')HIGHT,EPAIS,'Haut.(m),Epaiss.(m)'  !février 2004
	IF(SECTION.EQ.'CERCLE') WRITE(77,'(A6)') 'CERCLE'                   !février 2004	!15.02.06
	IF(SECTION.EQ.'CARRE') WRITE(77,'(A5)') 'CARRE'						!15.02.06
	IF(SECTION.EQ.'DOUBLET') WRITE(77,'(A7)') 'DOUBLET'					!15.02.06
      ELSE
C       Cas de la "Coque"
        WRITE(77,'(F10.5,1x,E14.7,1x,F8.5,T50,A)') 
     *                    PHIL,Q,DELTA,'Angl.(deg), Rayon et Epaiss (m)'
      ENDIF

C 1.e   Impression relatives au raidissage (aig. et raid)
C     ----------------------------------------------
      
	IF (TYPES.NE.'EPONTILLE') THEN                              !février 2004
	  WRITE(77,'(4(F9.6,1x),T50,A)')EPSA,ENTR,EPSA2,ENTR2,
     *            'Entr. aig. et raid, idem raid. compl. (m)'
c	 IF((TYPES.EQ.'EPONTILLE').AND.(ISECT.NE.3)) THEN			!février 2004
c      WRITE(77,'((F9.6,1x),T50,A)') HYA,							!février 2004
c     *            'Epontille, diam ou coté.(m)'					!février 2004
c	 ELSE														!février 2004
        WRITE(77,'(4(F9.6,1x),T50,A)') HYA,DYA,WYA,TYA,
     *            'Cadres Transversaux (m)'
c	 ENDIF														!février 2004
        WRITE(77,'(4(F9.6,1x),T50,A)') HXR,DXR,WXR,TXR,
     *            'Raidisseurs  Long.  (m)'

        IF((EPSA2.GE.(0.00001)).OR.(ENTR2.GE.(0.00001))) THEN
          WRITE(77,'(4(F9.6,1x),2X,I2,T50,A)') HYA2,DYA2,WYA2,TYA2,KSA2,
     *          '  Cadres Transversaux Compl. (m)'
          WRITE(77,'(4(F9.6,1x),2X,I2,T50,A)') HXR2,DXR2,WXR2,TXR2,KSR2,
     *          '  Raidisseurs Long. Compl. (m)'
        ENDIF
        WRITE(77,'(A3,T50,A)') MODE,'Code : répartition raid. long.'
	
	ELSE  
	
	  WRITE(77,'(2(F9.6),27X,A)') EPSA,Heff,                     !février 2004
     *	       'Ecartement, hauteur sans hiloire'                !février 2004
	  IF (SECTION.NE.'Doublet') THEN                             !février 2004
	    WRITE(77,'(F9.6)') HYA                                   !février 2004
	  ELSE                                                       !février 2004
	    WRITE(77,'(F9.6)') HYA,DYA,WYA,TYA                       !février 2004
	  ENDIF                                                      !février 2004
	ENDIF                                                        !février 2004

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
c
C     LECTURE DU COEFFICIENT DE PARTICIPATION			!coefficient de participation
C	=======================================
	IF(TYPES.NE.'EPONTILLE') THEN 
	  READ(55,*,END=900) PART			                        !coefficient de participation  !février 2004
	  WRITE(77,'(F12.7,T45,A)') PART,'Coeff. de participation'  !coefficient de participation  !février 2004
	ENDIF
	

C  2. Suite de la lecture des données (caract. mat., charge et angle)
C     ================================
      READ(55,*,END=900) E,ETA,SIGY,SIGM,SPEC
      READ(55,*,END=900) MT,KSR,KSA,KST,KSE,IPA,IVARIA
      READ(55,*,END=900) ANGLE

      WRITE(77,'(E14.7,1x,F6.3,1X,3(E14.7,1x),T70,A)') 
     *       E,ETA,SIGY,SIGM,SPEC,'E, Poisson, Sy, Sadm, Poids Spec'
      WRITE(77,'(I2,3X,4(I2,1x),4x,I2,3X,I2,T45,A)') 
     *    MT,KSR,KSA,KST,KSE,IPA,IVARIA,'MT,KSR,KSA,KST,KSE,IPA,IVARIA'
      WRITE(77,'(F12.7,T45,A)') ANGLE,'Angle (degré))'

      DO 195 I=1,NSOLM
        IF(IVARIA.EQ.1) GOTO 191
        READ(55,*,END=900) XI
        WRITE(77,'(F12.4,T45,A,I2,A)') XI,'XI  cas nø',I,' (Ivaria=0)'
        GOTO 192
  191   READ(55,*,END=900) XI,XF
        WRITE(77,'(F12.4,1X,F12.4,T45,A,I2,A)') XI,XF,
     *            'XI, XF  cas nø',I,' (Ivaria=1)'
  192   CONTINUE
  195 CONTINUE

      READ(55,*,END=900) PLOC
      WRITE(77,'(F12.4,T45,A)') 
     *          PLOC,'PLoc = Pression max locale (lisse et maille)'

C 2.6  LECTURE DES CHARGES VARIABLES SELON OX
C     -------------------------------------------
      READ(55,*,END=900)ICHA
      WRITE(77,'(I2,T45,A,A)') ICHA,'Charge variable selon X',
     *      ' (= 0 pas de surcharges, =1 avec surcharge)'
      IF(ICHA.GT.0) THEN
      DO 215 IS=1,NSOLM
c       READ(55,4) TEXT
c       WRITE(77,4) TEXT
c   4   FORMAT(25A4)
	  READ (55,'(5A20)') TEXT
        WRITE(77,'(5A20)') TEXT
        READ(55,*,END=900) NPT
        WRITE(77,'(2X,I2,T45,A,A)') NPT,
     *            'Nbre d''intervalles'
        DO 211 I=1,NPT
          READ(55,*,END=900) CHA
          WRITE(77,'(2x,F12.4,4X,F10.4,1X,F10.4,T45,A)') CHA,
     *            'Charges : CHA1 (N/m) et CHA2,CHA3 (m)'
 211    CONTINUE
 215  CONTINUE
      ENDIF

C 2.7 LECTURE DES NOH
C     -----------------
      READ(55,*,END=900) NOH
      WRITE(77,'(10(I3,1x),T45,A)')					!extension neto 
     *     NOH,'Liste des 10 panneaux qui suivent'

C 2.8 Lectures des TRAVERSES
C     ------------------------
      IF(MT.EQ.0) GOTO 41
      DO 2001 I=1,MT
        READ(55,*,END=900) HXTR,DXTR,WXTR,TXTR,ABTR
        WRITE(77,'(4(F8.6,1X),3X,F10.6,T55,A,I2)') 
     *            HXTR,DXTR,WXTR,TXTR,ABTR,'Traverse nø',I
 2001 CONTINUE
  41  CONTINUE

C ______________________________________________________________________
C ______________________________________________________________________
C 3.    LECTURE DE VARIABLES DE CONCEPTION , XI, XIMAX et XIMIN
C     ==========================================================
	READ (55,'(5A20)') TEXT
      WRITE(77,'(5A16)') 'DESIGN VARIABLES' 
      READ(55,*,END=900) NBRXI
      WRITE(77,'(I2,T10,A)') NBRXI,'Nbre de var. de conception'

c      IF(NBRXI.GE.1) THEN
c	 READ(55,*,END=900) (NXI(I),I=1,NBRXI)
	IF(NBRXI.NE.0) THEN								!eugen
	  READ(55,*,END=900) (NXI(I),I=1,NBRXI)			!eugen
c       Lecture de NXI(I) (nø des var. de conception)
        WRITE(77,501)(NXI(I),I=1,NBRXI)
 501    FORMAT(9(I2,1X),T30,'Nø des var. de conception')
	ENDIF											!eugen
c       Lecture des XIMAX et XIMIN
	  READ (55,*,END=900) (DVMIN(I),I=1,9)		!eugen	!fev2007
	  READ (55,*,END=900) (DVMAX(I),I=1,9)		!eugen	!fev2007
	  WRITE(77,502)(DVMIN(I),I=1,9)				!eugen	!fev2007
	  WRITE(77,502)(DVMAX(I),I=1,9)				!eugen	!fev2007
 502    FORMAT(9(F8.6,1X))						!eugen	!fev2007

C       Bornes de EPSR (entredistance moyenne) = VRAI variable de CONCEPTION = XIMAX
C       Bornes de ENTR (entredistance réelle)  = Valeur dans le fichier données = XMAX
c        DO 2345 I=1,NBRXI
c          XMAX(I)=XIMAX(NTOT+I)
c          XMIN(I)=XIMIN(NTOT+I)
c          IF(NXI(I).EQ.9) THEN
c            Temp=Xmode/(DABS(PHIL)*Q*PI1)
c	      DENOM1= (1.0/XMAX(I)) - Temp 
c	      DENOM2= (1.0/XMIN(I)) - Temp 
c 	      IF((DENOM1.LT.0.0).OR.(DENOM2.LT.0.0)) THEN
c	        WRITE(666,*)
c	        WRITE(666,*)'ATTENTION : ERROR dans le Panneau ',NEL
c	        WRITE(666,*)'Subr COPY'
c	        WRITE(666,*)'****************************************'
c              WRITE(666,*)'Le paramètre MODE est incompatible ou'
c	        WRITE(666,*)'les bornes Min MAx sont trop incorrectes.'
c	        WRITE(666,*)'Vous devez corriger cette situation !!'
c	        WRITE(666,*)'Essayer en modifiant le paramère MODE'
c	        WRITE(666,*)' - actuellement on a MODE =',MODE
c	        WRITE(666,*)' - prenez plutôt     MODE = EE2'
c	        WRITE(*,*)
c	        WRITE(*,*)'ATTENTION : ERROR dans le Panneau ',NEL
c	        WRITE(*,*)'Subr COPY'
c	        WRITE(*,*)'****************************************'
c              WRITE(*,*)'Le paramètre MODE est incompatible ou'
c	        WRITE(*,*)'les bornes Min MAx sont trop incorrectes.'
c	        WRITE(*,*)'Vous devez corriger cette situation !!'
c	        WRITE(*,*)'Essayer en modifiant le paramère MODE'
c	        WRITE(*,*)' - actuellement on a MODE =',MODE
c	        WRITE(*,*)' - prenez plutôt     MODE = EE2'
c	        WRITE(29,*)															!bug	
c	        WRITE(29,*)'ATTENTION : ERROR dans le Panneau ',NEL					!bug
c	        WRITE(29,*)'Subr COPY'												!bug
c	        WRITE(29,*)'****************************************'				!bug
c              WRITE(29,*)'Le paramètre MODE est incompatible ou'					!bug
c	        WRITE(29,*)'les bornes Min MAx sont trop incorrectes.'				!bug
c	        WRITE(29,*)'Vous devez corriger cette situation !!'					!bug
c	        WRITE(29,*)'Essayer en modifiant le paramère MODE'					!bug
c	        WRITE(29,*)' - actuellement on a MODE =',MODE						!bug
c	        WRITE(29,*)' - prenez plutôt     MODE = EE2'						!bug
c
c	        PAUSE 'STOP!'
c	        STOP
c	      ENDIF
c
c            XMAX(I)=1.0/ DENOM1
c            XMIN(I)=1.0/ DENOM2
c          ENDIF
c 2345   CONTINUE

c        WRITE(77,5021)(XMIN(I),I=1,NBRXI)
c        WRITE(77,5022)(XMAX(I),I=1,NBRXI)

cc        WRITE(666,*) 'XMIN = ENTR = data'
cc        WRITE(666,5021)(XMIN(I),I=1,NBRXI)
cc        WRITE(666,5022)(XMAX(I),I=1,NBRXI)
cc        WRITE(666,*) 'XIMIN = EPSR = XI'
cc        WRITE(666,5021)(XIMIN(NTOT+I),I=1,NBRXI)
cc        WRITE(666,5022)(XIMAX(NTOT+I),I=1,NBRXI)

c 5021   FORMAT(9(F9.5,1X),' XImin')
c 5022   FORMAT(9(F9.5,1X),' XImax')
cc      ENDIF

      NTOT=NTOT+NBRXI

C 3.2 Restrictions Structurelles: 
C     ----------------------------
	READ (55,'(5A20)') TEXT
      WRITE(77,'(5A22)') 'STRUCTURAL CONSTRAINTS'
      READ (55,*,END=900) IM1  
      WRITE(77,'(I2,T10,2A)') IM1,'(0 without structural constraints',
     *                           ', 1 with structural constraints)'
      IF(IM1.LE.0) GOTO 516

      READ (55,*,END=900) IPT
      READ (55,*,END=900) (YPT(I),I=1,IPT)
      WRITE(77,'(3X,I2,T40,A)')IPT,
     *               'Nbre de pts pour le calcul des sensibilités'
      WRITE(77,601)(YPT(I),I=1,IPT)
  601 FORMAT(10(F9.4,1x),
     *            '  Pts choisis pour le calcul des sensibilités')

      DO 517 IS=1,NSOLM
	READ (55,'(5A20)') TEXT
      WRITE(77,'(5A20)') TEXT
      READ (55,*,END=900) M1
      WRITE(77,'(I2,T10,A)') M1,'Nbre de restrictions structurelles'

      IF((M1.GT.0).AND.(M1.LE.20)) THEN
        DO 515 I=1,M1
C         Lecture de
C             - ICN,LCONT(1,I)  Nø de réf de la Contr.ICN
c             - CJMAX(I),INV(I) C(max) : C<Cmax
c             - LCONT(2,I)              Nø du pt d'appl. YPT de la Contr  
          READ (55,*,END=900)ICN,LCONT(1),CJMAX,INV,LCONT(2) 
          WRITE(77,'(2x,I3,T8,I3,3x,E14.7,2x,I2,2x,I2,2x,A)') 
     *                  I,LCONT(1),CJMAX,INV,LCONT(2),
     *          '(#	Constraint Type		CJmax	INV   IPT)'
  515   CONTINUE
      ENDIF
  517 CONTINUE
  516 CONTINUE

C 3.4 Lecture des M2 Restrictions Géométriques (IBM2 = nø du set de rest
C     ------------------------------------------------------------------
C     M2 =  nbre de restr. géom. du panneau NEL

	READ (55,'(5A20)') TEXT
      WRITE(77,'(5A23)') 'GEOMETRICAL CONSTRAINTS'
      READ (55,*,END=900) M2
      WRITE(77,'(I2,T10,A)') M2,'M2=Nbre de restrictions geometriques'

      IF(M2.EQ.99) THEN       ! cas ou un set de restrictions est choisi
        READ (55,*,END=900) IBM2
        READ (55,*,END=900) ISEMA,ISEMR
        WRITE(77,'(I3,T20,A)') IBM2,'IBM2= nø du set de Restr. géom.)'
        WRITE(77,'(I3,2x,I3,T20,A)') ISEMA,ISEMR,'ISEMA,ISEMR'
      ELSE IF(M2.NE.0) THEN ! Cas ou les restriction sont définies individuellement
        READ (55,*,END=900) (LM2(I),I=1,M2)
        READ (55,*,END=900) ISEMA,ISEMR
        WRITE(77,503)(LM2(I),I=1,M2)
 503    FORMAT(20(I3,1X),'Nø des M2 restrictions géométriques')
        WRITE(77,'(I3,2x,I3,T20,A)') ISEMA,ISEMR,'ISEMA,ISEMR'
      ENDIF

c     M2TOT=M2TOT+M2


C 4.0 Sauvetage pour dessins des données courantes (pour VISIONN.for)
c     ---------------------------------------------------------------
      IF(DESSIN.NE.0) THEN
	  Write(46) Delta,HYA,DYA,WYA,TYA,EPSA,HXR,DXR,WXR,TXR,EPSR
      ENDIF

c ----------------------------------------------------------------------------
      RETURN

  900 WRITE(9 ,*)'Erreur de lecture : "End of file"'
      PAUSE 'STOP'      
	STOP

C 6.  LES FORMATS.
C     ============
      END
