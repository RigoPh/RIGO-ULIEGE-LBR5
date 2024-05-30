      SUBROUTINE USHULLS(NETO,NEL,SECT,SIGY,E,Q,DELTA,PHIL,									
     *   DYA,TYA,WYA,HYA,EPSA,KSA,HXR,DXR,WXR,TXR,EPSR,ENTR,KSR,
     *   DSUD,DSUB,DSUB1,DSUSU,DSUSL,DH,DG,DAD,DAB,DAB1,DAS,DULT,
     *   NVAR,NXIT)

C *******************************************************************
C     SUBROUTINE USHULLS
C     ==================
c
C  .  A COMPUTER PROGRAM TO EVALUATE THE ULTIMATE STRENGTH OF SHIPS .
C  .                                                                .
C  .  based on the model of Prof. JEOM KEE PAIK (November 1995)     .
C  .                                                                .
C  .  Adapted to LBR-5 by : Dr. Ph. RIGO (February 1997)            .
C  .  ---------------------                                         .
C  .    DEPARTMENT OF NAVAL ARCHITECTURE, ANAST                     .
C  .    UNIVERSITY OF LIEGE                                         .
C  .    6 quai Banning, 4000 Liege, BELGIUM                         .
C  .    (TEL) +32 4 366 9225  (FAX) +32 4 366 9133                  .
C  .    (e-mail) Ph.Rigo@ULg.ac.be                                  .
C  .                                                                .
C     Modified: 
C        - Limit States for H <0 or H > D                       (26-2-97)
C          H=zone of linear variation in the vertical stress distribution
C        - Introduce specific stiffener yield stress             (3-3-97) 
C        - SYS(up) _ SYS(down) in the calculation of Mp and Mu   (5-3-97) 
C        - Introduction of validity limit on column slenderness (10-3-97) 
C        - Sensibility analysis (to consider the Ultimate strength
C          of the ship girder as a global restriction)          (11-3-97) 
C        - Correction of the equation of the Static moment (Subr ULTIPAN)  (12-3-97) 
c          ZO= ... + HW*TW*(TP+HW/2.0) + ...   instead of  ... + HW*TW*(TP+HW)/2.0 + ...
C
C     Last modification: 25-5-99
C                
C *******************************************************************
c  ORGANIGRAMME
C  ------------
c  - USHUL
C      - ULTPAN  (calcul de Sult et dSult/dxi)
C          - ULT (calcul de Sult)
c
C      - DSECT   (calcul de Sect et dSect/dxi)
c
C      - ULTHULL (Calcul de Mult Sagging et Hogging.)
C
C      - DULTHULL(dMult/dxi, Sagg. et Hogg.)
c
C *******************************************************************
c23456789012345678901234567890123456789012345678901234567890123456789012

      IMPLICIT REAL*8(A-H,O-Z) 
	
c      COMMON/DI/BID1(57515),
c     *       BID2(5),Q,DELTA,PHIL,									!
c     *       DYA,TYA,WYA,HYA,EPSA,KSA,HXR,DXR,WXR,TXR,EPSR,ENTR,KSR,	!62 valeurs
c     *       BID3(41),												! 
      DIMENSION DSUD(9,NETO),DSUB(9,NETO),DSUB1(9,NETO),DSUSU(9,NETO),
     *          DSUSL(9,NETO),DH(9,NETO),DG(9,NETO),
     *          DAD(9,NETO),DAB(9,NETO),DAB1(9,NETO),DAS(9,NETO),
     *          DULT(9*NETO)

c     COMMON /OPTI/IOPTI,NTOT,MTOT(2),NVAR(30),MCONT(60),NXIT(9,30)
      COMMON /OPTI/IOPTI,NTOT
      DIMENSION NVAR(NETO),NXIT(9,NETO)

      COMMON/USHULL/DEPTH,DB,UHOGG,USAGG,				  ! HULL,BO2
     *              SYSD,SySB1,SySB,SySSup,SySSdw,
     *              IULT,IRESTR,
     *              ND,ID(150),NB1,IB1(150),NB,IB(150),NS,IS(150),	!extension neto
     *              KD,KB1,KB,KSup,KSdw
      
      COMMON/STR/   SYD,SYB,SYB1,SYSU,SYSL,SUD,SUB,SUB1,SUSU,SUSL,  ! Main
     *              AD,AB1,AB,AS

      COMMON/PY/ PI

C     Ship sizes
C     ----------
C  .  DEPTH = DEPTH OF THE VESSEL          (m.)
C  .  DB    = DEPTH OF DOUBLE-BOTTOM       (m.)

C     Modeling on the ship components (deck, bottoms, side) based on the NETO panels
C     -------------------------------------------------------------------------------

C   ND = Number of Panels included in the UPPER DECK   component
C   NB1= Number of Panels included in the INNER BOTTOM component (si double fond)
C   NB = Number of Panels included in the OUTER BOTTOM component
C   NS = Number of Panels included in the TWO   SIDES  component

C   ID (i=1,ND)  = List of Panels included in the UPPER DECK   component
C   IB1(i=1,NB1) = List of Panels included in the INNER BOTTOM component 
C   IB (i=1,NB)  = List of Panels included in the OUTER BOTTOM component
C   IS (i=1,NS)  = List of Panels included in the TWO   SIDES  component

C   NUMBER of a Panel to use to evaluate the Ultimate Strength of a component
C   KD   = nø of a Panel for calculation of the UPPER DECK   Strength 
C   KB1  = nø of a Panel for calculation of the INNER BOTTOM Strength
C   KB   = nø of a Panel for calculation of the OUTER BOTTOM Strength
C   KSup = nø of a Panel for calculation of the UPPER PART of the SIDE PLATE  Strength
C   KSdw = nø of a Panel for calculation of the LOWER PART of the SIDE PLATE  Strength

C Rem : - Il faut modéliser la structure complète (et pas une demi)
C       - Si seule la moitié de la structures a été modéliésée pour le calcul avec LBR,
c         alors il faut répéter 2 fois certains éléments de façon à générer la structure entière.
C       - Il faut donner les panneaux correspondants aux 2 côtés (side plates)
C
      IF(NEL.EQ.NETO) THEN
      WRITE(66,96)
96    FORMAT(//50(1H=)/
     *     '  U L T I M A T E   H U L L   S T R E N G T H '/50(1H=)/)

      WRITE(66,*)'Evaluation of the Ultimate Strength of ship girder'	
      WRITE(66,*) '    using the PAIK Algorithm	(Caldwell Method)'
      WRITE(66,'(50(1H-)/)')	

      Write(66,'(A,4(I2,1x))') 
     *  'Panels included in the ship components (ND,NB1,NB,NS)=',
     *             ND,NB1,NB,NS
      Write(66,'(A,5(20(I2,1x)/))') 'UPPER DECK   : ',(ID(i), i=1,ND)
      Write(66,'(A,5(20(I2,1x)/))') 'INNER BOTTOM : ',(IB1(i),i=1,NB1)
      Write(66,'(A,5(20(I2,1x)/))') 'OUTER BOTTOM : ',(IB(i), i=1,NB)
      Write(66,'(A,5(20(I2,1x)/))') 'TWO   SIDES  : ',(IS(i), i=1,NS)
      Write(66,'(/A,A)')'Panels used for the evaluation of the ',
     *            'Ult. Str. of Long. Compressed ship components:'
      Write(66,*) 'UPPER DECK               : ',KD
      Write(66,*) 'INNER BOTTOM             : ',KB1
      Write(66,*) 'OUTER BOTTOM             : ',KB
      Write(66,*) 'Upper part of SIDE PLATE : ',KSup
      Write(66,*) 'Lower part of SIDE PLATE : ',KSdw
      Write(66,*)
	ENDIF


C*************************************************************************
C*************************************************************************

C     DESCRITION OF DATA FOR USHULL
C     ==============================

C               Œ Y et Sigma Y
C               I
C               *****************************
C               *                           *  b=ENTR
C               *                           *
C               *                           *
C               ***************************** --------> X et Sigma X
c               <----------- a = EPSA ------>

c     EPSA,ENTR	= a,b        (m)
c     SIGY		= SIG Elastique (N/m2), (ex, Re=240 N/mm2)
  

C     CHARACTERISTICS IF THE STIFFENED ELEMENTS (EPSA,ENTR,DELTA,HXR,DXR,WXR,TXR,SIGY,SIGY)
C     ------------------------------------------
C  .  SL = EPSA : LENGTH OF STIFFENER BETWEEN TRANSVERSE FRAMES (m.)
C  .  BP = ENTR : LONGITUDINAL STIFFENER SPACING   (m.)
C  .  TP = DELTA: THICKNESS OF PARENT PLATE        (m.)
C  .  HW = HXR  : HEIGHT OF STIFFENER WEB          (m.)
C  .  TW = DXR  : THICKNESS OF STIFFENER WEB       (m.)
C  .  BF = WXR  : BREADTH OF STIFFENER FLANGE      (m.)
C  .  TF = TXR  : THICKNESS OF STIFFENER FLANGE    (m.)
C  .  SYP= SIGY : YIELD STRENGTH OF PARENT PLATE   (N/m2)   
C  .  SYW= SIGY : YIELD STRENGTH OF STIFFENER      (N/m2)   

c LBR5 : EPSA,ENTR,DELTA,HXR,DXR,WXR,TXR,SIGY,SIGY
c PAIK : SL,  BP,  TP,   HW, TW, BF, TF, SYP, SYW

C     PROPERTIES OF HULL SECTION COMPONENTS
C     --------------------------------------
C   . AB  = TOTAL SECTIONAL AREA OF OUTER BOTTOM (m2)
C   . AB1 = TOTAL SECTIONAL AREA OF INNER BOTTOM (m2)
C   . AD  = TOTAL SECTIONAL AREA OF DECK         (m2)
C   . AS  = HALF-SECTIONAL AREA OF ALL SIDES     (m2)
C           (INCLUDING LONG. BLKD, INNER SIDES, AND VERTICAL/INCLINED MEMBERS) 

C ********************************************************************
C ********************************************************************

C     PRINCIPAL DIMENSIONS OF THE SHIP

C  .  DEPTH = DEPTH OF THE VESSEL          (m.)
C  .  DB    = DEPTH OF DOUBLE-BOTTOM       (m.)

      IF(NEL.EQ.NETO) WRITE(66,16) DEPTH,DB
      
16    FORMAT('DEPTH OF THE VESSEL    (m.)        =',F12.6/
     *       'DEPTH OF DOUBLE-BOTTOM (m.)        =',F12.6)


C 1.0 ULTIMATE STRENGTH OF THE STIFFENED PLATES UNDER COMPRESSION
C     ============================================================
C     INPUT
C     ------
C  .  SL = EPSA : LENGTH OF STIFFENER BETWEEN TRANSVERSE FRAMES (m.)
C  .  BP = ENTR : LONGITUDINAL STIFFENER SPACING   (m.)
C  .  TP = DELTA: THICKNESS OF PARENT PLATE        (m.)
C  .  HW = HXR  : HEIGHT OF STIFFENER WEB          (m.)
C  .  TW = DXR  : THICKNESS OF STIFFENER WEB       (m.)
C  .  BF = WXR  : BREADTH OF STIFFENER FLANGE      (m.)
C  .  TF = TXR  : THICKNESS OF STIFFENER FLANGE    (m.)
C  .  SYP= SIGY : YIELD STRENGTH OF PARENT PLATE   (N/m2)   
C  .  SYW= SIGY : YIELD STRENGTH OF STIFFENER      (N/m2)   

c LBR5 : EPSA,ENTR,DELTA,HXR,DXR,WXR,TXR,SIGY,SIGY
c PAIK : SL,  BP,  TP,   HW, TW, BF, TF, SYP, SYW

C     OUTPUT
C     ------
C  .  SYD  = EQUIVALENT YIELD STRENGTH OF DECK         (N/m2)  
C  .  SYB  = EQUIVALENT YIELD STRENGTH OF OUTER BOTTOM (N/m2)  
C  .  SYB1 = EQUIVALENT YIELD STRENGTH OF INNER BOTTOM (N/m2)  
C  .  SYSU = EQUIVALENT YIELD STRENGTH OF SIDE SHELL-UP(N/m2)  
C  .  SYSL = EQUIVALENT YIELD STRENGTH OF SIDE SHELL-LW(N/m2)  
C  .  SUD  = ULTIMATE STRENGTH OF DECK                 (N/m2)  
C  .  SUB  = ULTIMATE STRENGTH OF OUTER BOTTOM         (N/m2)  
C  .  SUB1 = ULTIMATE STRENGTH OF INNER BOTTOM         (N/m2)  
C  .  SUSU = ULTIMATE STRENGTH OF UPPER SIDE SHELL     (N/m2)  
C  .  SUSL = ULTIMATE STRENGTH OF LOWER SIDE SHELL     (N/m2)  




c     SI KOPT _0 : calcul des sensibilités
c     SI KOPT =0 : pas de calcul des sensibilités
      KOPT=IOPTI*IRESTR

C...  1.1) STIFFENED PLATE IN DECK (element KD)
C     =============================
      IF(NEL.EQ.KD) THEN

      CALL ULTPAN(NETO,NEL,E,SIGY,SYSD,SYEQ,
     *            DELTA,EPSA,HXR,DXR,WXR,TXR,EPSR,ENTR,
     *            DSUD(1,NEL),SIGU,KOPT,NVAR,NXIT)

      SUD=SIGU
      SYD=SYEQ
      SULT=SIGU/SYEQ

      WRITE(66,46)
      WRITE(66,47) EPSA,ENTR,DELTA,HXR,DXR,WXR,TXR,SIGY,SYSD,
     *             SYEQ,SUD,SULT
      ENDIF

C...  1.2) STIFFENED PLATE IN OUTER BOTTOM (element KB)
c     =====================================
      IF(NEL.EQ.KB) THEN

      CALL ULTPAN(NETO,NEL,E,SIGY,SySB,SYEQ,
     *            DELTA,EPSA,HXR,DXR,WXR,TXR,EPSR,ENTR,
     *            DSUB(1,NEL),SIGU,KOPT,NVAR,NXIT)

      SUB=SIGU
      SYB=SYEQ
      SULT=SIGU/SYEQ

      WRITE(66,56)
      WRITE(66,47) EPSA,ENTR,DELTA,HXR,DXR,WXR,TXR,SIGY,SySB,
     *             SYEQ,SUB,SULT
      ENDIF

C...  1.3) STIFFENED PLATE IN INNER BOTTOM (element KB1)
c     =====================================
      IF(NEL.EQ.KB1) THEN

      CALL ULTPAN(NETO,NEL,E,SIGY,SySB1,SYEQ,
     *            DELTA,EPSA,HXR,DXR,WXR,TXR,EPSR,ENTR,
     *            DSUB1(1,NEL),SIGU,KOPT,NVAR,NXIT)

      SUB1=SIGU
      SYB1=SYEQ
      SULT=SIGU/SYEQ

      WRITE(66,66)
      WRITE(66,47) EPSA,ENTR,DELTA,HXR,DXR,WXR,TXR,SIGY,SySB1,
     *             SYEQ,SUB1,SULT
      ENDIF

C...  1.4) STIFFENED PLATE IN UPPER SIDE SHELL (element KSup)
c     =========================================
      IF(NEL.EQ.KSup) THEN

      CALL ULTPAN(NETO,NEL,E,SIGY,SySSup,SYEQ,
     *            DELTA,EPSA,HXR,DXR,WXR,TXR,EPSR,ENTR,
     *            DSUSU(1,NEL),SIGU,KOPT,NVAR,NXIT)

      SUSU=SIGU
      SYSU=SYEQ
      SULT=SIGU/SYEQ

      WRITE(66,76)
      WRITE(66,47) EPSA,ENTR,DELTA,HXR,DXR,WXR,TXR,SIGY,SySSup,
     *             SYEQ,SUSU,SULT
      ENDIF

C...  1.5) STIFFENED PLATE IN LOWER SIDE SHELL (element KSdw)
c     =========================================
      IF(NEL.EQ.KSdw) THEN

      CALL ULTPAN(NETO,NEL,E,SIGY,SySSdw,SYEQ,
     *            DELTA,EPSA,HXR,DXR,WXR,TXR,EPSR,ENTR,
     *            DSUSL(1,NEL),SIGU,KOPT,NVAR,NXIT)

      SUSL=SIGU
      SYSL=SYEQ
      SULT=SIGU/SYEQ

      WRITE(66,86)
      WRITE(66,47) EPSA,ENTR,DELTA,HXR,DXR,WXR,TXR,SIGY,SySSdw,
     *             SYEQ,SUSL,SULT
	ENDIF


 46    FORMAT(///,5X,'D E C K   P A N E L S :')
 56    FORMAT(///,5X,'O U T E R   B O T T O M   P A N E L S :')
 66    FORMAT(///,5X,'I N N E R   B O T T O M   P A N E L S :')
 76    FORMAT(///,5X,'U P P E R   S I D E   S H E L L S :')
 86    FORMAT(///,5X,'L O W E R   S I D E   S H E L L S :')
 47    FORMAT(
     *  /5X,'LENGTH OF STIFFENER            (m.)       =',F12.6,
     *  /5X,'LONGITUDINAL STIFFENER SPACING (m.)       =',F12.6,
     *  /5X,'THICKNESS OF PARENT PLATE      (m.)       =',F12.6,
     *  /5X,'HEIGHT OF STIFFENER WEB        (m.)       =',F12.6,
     *  /5X,'THICKNESS OF STIFFENER WEB     (m.)       =',F12.6,
     *  /5X,'BREADTH OF STIFFENER FLANGE    (m.)       =',F12.6,
     *  /5X,'THICKNESS OF STIFFENER FLANGE  (m.)       =',F12.6,
     *  /5X,'YIELD STRENGTH OF PARENT PLATE (N/m2)     =',E14.7,
     *  /5X,'YIELD STRENGTH OF STIFFENER    (N/m2)     =',E14.7,
     *  /5X,'EQUIVALENT YIELD STRENGTH      (N/m2)     =',E14.7,
     *  /5X,'ULTIMATE COMPRESSIVE STRENGTH  (N/m2)     =',E14.7
     *  /5X,'Relative Ultimate Strength (Sult/Sy)      =',F12.6)


c23456789012345678901234567890123456789012345678901234567890123456789012

C 2.0 SECTIONAL AREA  OF HULL COMPONENTS
C     ===================================
C   . AB  = TOTAL SECTIONAL AREA OF OUTER BOTTOM (m2)
C   . AB1 = TOTAL SECTIONAL AREA OF INNER BOTTOM (m2)
C   . AD  = TOTAL SECTIONAL AREA OF DECK         (m2)
C   . AS  = HALF-SECTIONAL AREA OF ALL SIDES     (m2)
C           (INCLUDING LONG. BLKD, INNER SIDES, AND VERTICAL/INCLINED MEMBERS) 

C     Mise à zéro des sensibilités des sections DAD,DAB,DAB1 et DAS : d(Ad)/dxi
      IF(KOPT.GE.1) THEN
	  IF(NEL.EQ.1) CALL ANNULD(DAD,270*4)
	ENDIF

	DO 10 I=1,ND
	IF(ID(I).EQ.NEL) THEN
	  AD=AD+SECT
        IF(KOPT.GE.1) THEN
          CALL DSECT(NETO,NEL,DELTA,HXR,DXR,WXR,TXR,EPSR,PHIL,Q,
     *               DAD(1,NEL),NVAR,NXIT)
          ENDIF
        ENDIF
  10  CONTINUE

c23456789012345678901234567890123456789012345678901234567890123456789012

        DO 11 I=1,NB1
        IF(IB1(I).EQ.NEL) THEN
          AB1=AB1+SECT
        IF(KOPT.GE.1) THEN
          CALL DSECT(NETO,NEL,DELTA,HXR,DXR,WXR,TXR,EPSR,PHIL,Q,
     *               DAB1(1,NEL),NVAR,NXIT)
          ENDIF
	ENDIF	
  11  CONTINUE

	DO 12 I=1,NB
	IF(IB(I).EQ.NEL) THEN
	  AB=AB+SECT
        IF(KOPT.GE.1) THEN
          CALL DSECT(NETO,NEL,DELTA,HXR,DXR,WXR,TXR,EPSR,PHIL,Q,
     *               DAB(1,NEL),NVAR,NXIT)
          ENDIF
        ENDIF
  12  CONTINUE

        DO 13 I=1,NS
        IF(IS(I).EQ.NEL) THEN
	  AS=AS+SECT/2.
        IF(KOPT.GE.1) THEN
          CALL DSECT(NETO,NEL,DELTA,HXR,DXR,WXR,TXR,EPSR,PHIL,Q,
     *               DAS(1,NEL),NVAR,NXIT)
         ENDIF
        ENDIF
  13  CONTINUE

C ---------------------------------------------------------------------
c      WRITE(666,*)
c      WRITE(666,*)'Valeurs avant la Subr DULTHULL'
c      WRITE(666,*)   '(PANEL nø   =',NEL,')'
c      WRITE(666,*)'------------------------------'
c        NBRXI=NVAR(NEL)
c        WRITE(666,*)'NBRXI =',NBRXI
c        WRITE(666,*)'NXIT  =',(NXIT(I,NEL),I=1,NBRXI)
c        WRITE(666,*)'DAD   =',(DAD(I,NEL),I=1,NBRXI)
c        WRITE(666,*)'DAB   =',(DAB(I,NEL),I=1,NBRXI)
c        WRITE(666,*)'DAB1  =',(DAB1(I,NEL),I=1,NBRXI)
c        WRITE(666,*)'DAS   =',(DAS(I,NEL),I=1,NBRXI)
c        WRITE(666,*)'DSUD  =',(DSUD(I,NEL),I=1,NBRXI)
c        WRITE(666,*)'DSUB  =',(DSUB(I,NEL),I=1,NBRXI)
c        WRITE(666,*)'DSUB1 =',(DSUB1(I,NEL),I=1,NBRXI)
c        WRITE(666,*)'DSUSL =',(DSUSL(I,NEL),I=1,NBRXI)
c        WRITE(666,*)'DSUSU =',(DSUSU(I,NEL),I=1,NBRXI)
c        WRITE(666,*) 
c
C ---------------------------------------------------------------------

C ********************************************************************
C 3.0 CALCULATION OF ULTIMATE HULL STRENGTH IN NORMAL CONDITION 
C     ==========================================================
C ********************************************************************
c       END OF Subr. USHULL (si NEL<NETO)'

      IF(NEL.EQ.NETO) THEN
           WRITE(*,*) 'avt ULTHULL'
C          PAUSE'USH1'

        IF(KOPT.EQ.0) THEN
C         Sans optimisation (KOPT=0)
C         ---------------------------

          CALL ULTHULL
C         ============

        ELSE
C         Avec optimisation (KOPT>0)
C         ---------------------------

C         Car AS et DAS ne sont relatif qu'à la moitié des parois verticales	
	    DO 14 J=1,NETO
	    DO 14 I=1,9
	      DAS(I,J)=DAS(I,J)/2.0
   14     CONTINUE

c         WRITE(*,*) 'Avant entrée dans DULTHULL'
C	    PAUSE'OK?'

          CALL DULTHULL(NETO,NVAR,NXIT,
     *                  DSUD,DSUB,DSUB1,DSUSU,DSUSL,DH,DH,DG,
     *                  DAD,DAB,DAB1,DAS,DULT)
	
c         WRITE(*,*) 'Après la sortie de DULTHULL'
C         PAUSE'OK?'
        ENDIF
      ENDIF

      RETURN
      END


C*******************************************************************************
C*******************************************************************************
C*******************************************************************************
C*******************************************************************************

      SUBROUTINE ULTPAN(NETO,NEL,E,SyP,SyW,SYEQ,
     *         DELTA,EPSA,HXR,DXR,WXR,TXR,EPSR,ENTR,
     *         DSPAIK,SPAIK,KOPT,NVAR,NXIT)

      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 IS

      DIMENSION DSPAIK(9)

      COMMON /PY/ PI
c     COMMON /OPTI/IOPTI,NTOT,MTOT(2),NVAR(30),MCONT(2*30),NXIT(9,30)
      COMMON /OPTI/IOPTI,NTOT
      DIMENSION NVAR(NETO),NXIT(9,NETO)


C*********************************************************************
C     SUBROUTINE ULTPAN 
C     ==================
C     Cette subrourine calcule la résistance ultime d'une plaque raidie
C     londitudinalement et comprimée, également, longitudinalement.
C     (La pression latéral n'est pas prise en compte)
C
C     Il s'agit de la formule empirique de J. PAIK.  qui est basée 
C     sur le principe de la poutre colonne.
C                                                                       
C   INPUT:E,SyP,SyW,DELTA,EPSA 
C           et  HXR,DXR,WXR,TXR,EPSR,ENTR
C                                                                       
C  OUTPUT:DCPAIK,SPAIK,OMT,
C	OMT = section raidiseur + bordage (complet)
C     SPAIK = résistance ultime selon la formule de J. PAIK
C     DCPAIK = dérivée de la résistance ultime SPAIK par rapport aux XI
C
C     Créer : le 22-7-96  par  Ph. Rigo
C
C     Modifications : 
C	- restrictions nø 15 + Sensibilités (voir Subr. Contr & Paik) 22-7-96  
C	- limite sur BETA et LAMDA	      				               1-8-96  
C	- Prise en Compte des différences entre D(raid) et de EPSR	   6-3-97 
C                               
C     Dernière modif : 10-3-97 				     	
C                                                                       
C*************************************************************************
C*************************************************************************
C               ^ Y et Sigma Y
C               I
C               *****************************
C               *                           *  b=EPSR (= variable de conception)
C               *                           *  b=ENTR = D (entredistance réelle)
C               *                           *
C               ***************************** --------> X et Sigma X
c               <----------- a = EPSA ------>
c
c      EPSA  	= a        (m)
c      ENTR,EPSR	= b        (m)
C      avec ENTR : entredistance réelle entre raidisseurs = D(raid)
C           EPSR : Largeur effective associée à 1 raid (= L/nbre de raid)
c      SyP		= Yield Stress of plating (N/m2), (ex, Re=240 N/mm2)
c      SyW		= Yield Stress of stiffener (N/m2), (ex, Re=240 N/mm2)
c
C*************************************************************************
  
C 1.0 Calcul de SPAIK de référence
C     ****************************

C 1.1 Caractéristiques géométriques et calcul de á et Lamda  
C     -----------------------------------------------------
      OM  =WXR*TXR + DXR*HXR                    ! section raid
      OMT =OM + ENTR*DELTA                      ! section raid + D(raid)
      SMCR=DELTA/2.+HXR+TXR/2.
      SMDR=(DELTA+HXR)/2.
      HX  =WXR*TXR*SMCR + DXR*HXR*SMDR          ! Moment statique (Z=0)
	
c      WRITE(66,*)'ds subr ULTPAN, NEL=',NEL
c      WRITE(66,*)'-----------------------'
c      WRITE(66,*)'E,SyP,SyW,DELTA = '
c      WRITE(66,*) E,SyP,SyW,DELTA
c      WRITE(66,*)'HXR,DXR,WXR,TXR,EPSR,ENTR ='
c      WRITE(66,*) HXR,DXR,WXR,TXR,EPSR,ENTR
c      WRITE(66,*)'OM ,OMT,HX ,SMCR,SMDR= ',OM,OMT,HX,SMCR,SMDR

c       TEMP3b=DXR*HXR
c       TEMP3= DXR*HXR*SMDR
c       WRITE(66,*) '(DELTA+HXR)/2.      =',SMDR
c       WRITE(66,*) 'DXR*HXR             =',TEMP3b
c       WRITE(66,*) 'DXR*HXR*SMDR        =',TEMP3
c       TEMP2b=WXR*TXR
c       TEMP2=WXR*TXR*SMCR
c       WRITE(66,*) 'DELTA/2.+HXR+TXR/2. =',SMCR
c       WRITE(66,*) 'WXR*TXR             =',TEMP2b
c       WRITE(66,*) 'WXR*TXR*SMCR        =',TEMP2
c       TEMP=TEMP2+TEMP3
c       WRITE(66,*) 'moment statique     =',TEMP
c       WRITE(66,*) 'moment statique (HX)=',HX
c       TEMP=TEMP/OMT
c       WRITE(66,*) 'YG                  =',TEMP
c       TEMP=2.0*HX/(2.0*OMT)
c       WRITE(66,*) 'YG                  =',TEMP

c     EQUIVALENT YIELD STRENGTH FOR THE WHOLE PANEL
      GAM=OM/(ENTR*DELTA)
      SYEQ=(SYP+GAM*SYW)/(1.0+GAM)

	GX=2.0*HX		
      YG=GX/(2.0*OMT)										! YG=position du centre de gravité.
        IS=(ENTR*DELTA**3 + DXR*HXR**3 + WXR*TXR**3)/12.	! IS=Inertie par rapport
     *    + DELTA*ENTR*YG*YG  +  HXR*DXR*(SMDR-YG)**2		! au centre de gravité.
     *                        +  WXR*TXR*(SMCR-YG)**2
      ROT=DSQRT(SYEQ/E)
      BETA=ENTR * ROT / DELTA
      SLEN=EPSA * ROT* DSQRT(OMT/IS) / PI

c       WRITE(66,*) 'Subr ULTPAN (suite)'
c       WRITE(66,*) 'SYEQ             =',SYEQ
c       WRITE(66,*) 'SECT             =',OMT
c       WRITE(66,*) 'Zo               =',YG
c       WRITE(66,*) 'Inertie          =',IS
c       WRITE(66,*) 'Rayon de gyration=',RAD

C 1.2 TEST SUR BETA et Lamda=SLEN (SLIM= élancement maximum pour un BETA donné)
C     -----------------------------------------------------------------------
      ILIM=0
	
	IF(BETA.LE.2) THEN
	   SLIM=3.0+(BETA-1.)/2.
	ELSE
	   SLIM=3.5+(BETA-2.)
	ENDIF
	
	IF(SLEN.GT.SLIM) THEN
	  WRITE(66,*)'ATTENTION: SLEN max =',SLIM,' < SLEN =',SLEN
	  WRITE(66,*)'=========='
	  WRITE(29,*)'ATTENTION: SLEN max =',SLIM,' < SLEN =',SLEN					!bug
	  WRITE(29,*)'=========='													!bug
	  ILIM=1
	  SLEN=SLIM
	ENDIF

C 1.3 Calcul de SPAIK de référence
C     -----------------------------
	SLEN2=SLEN*SLEN
	BETA2=BETA*BETA
	FCT=DSQRT(0.995 + 0.936*SLEN2 + 0.17*BETA2 + 0.188*BETA2*SLEN2
     *                - 0.067*SLEN2*SLEN2)
      SPAIK=SYEQ/FCT

C 1.4 Impressions
C     ------------
      IF(KOPT.EQ.0) THEN
 	  WRITE(66,*)
 	  WRITE(66,*)'Ds subr ULTPAN, NEL=',NEL
 	  WRITE(66,*)'--------------------------'
        WRITE(66,*)'BETA    = ',BETA
        WRITE(66,*)'LAMDA   = ',SLEN
        WRITE(66,*)'SPAIK   = ',SPAIK
	  
	  RETURN	  
	ENDIF

c     Calcul des sensibilités si KOPT_0
c
C 2.0 Dérivée de SIG(PAIK) : DSPAIK
C     *****************************

      IF(ILIM.EQ.0) GOTO 3
	
C 2.1 Cas où les valeurs limites de Béta et Lamda sont dépassées
C     ---------------------------------------------------------
C     Dans ce cas le calcul se fait par différences finies.

      NBRXI=NVAR(NEL)

      DO 5 K=1,NBRXI

      KK=NXIT(K,NEL)

        IF(KK.EQ.1) THEN                         !  XI=DELTA
c          WRITE(666,*)' XI=1  DELTA'
c	   WRITE(666,*)' $$$$$$$$$$$'
	   RATIO=1.05
         XXX=RATIO *DELTA
         CALL ULT(E,SYEQ,XXX,EPSA,HXR,DXR,WXR,TXR,ENTR,SPAIK1)
           DSPAIK(KK)=(SPAIK1-SPAIK)/(XXX-DELTA)
c	   WRITE(666,*)'Subr ULT: SPAIK1= ',SPAIK1
c	   WRITE(666,*)'DELTA1=',XXX,'  et DSPAIK1=',DSPAIK(KK)
        ELSE IF (KK.EQ.5) THEN                  ! XI=EPSA
c          WRITE(666,*)' XI=5  EPSA'
c	   WRITE(666,*)' $$$$$$$$$$$'
	   RATIO=0.5
         XXX=RATIO *EPSA
         CALL ULT(E,SYEQ,DELTA,XXX,HXR,DXR,WXR,TXR,ENTR,SPAIK1)
           DSPAIK(KK)=(SPAIK1-SPAIK)/(XXX-EPSA)
c	   WRITE(666,*)'Subr ULT: SPAIK5= ',SPAIK1
c	   WRITE(666,*)'EPSA5=',XXX,'  et DSPAIK5=',DSPAIK(KK)
        ELSE IF (KK.EQ.6) THEN                 ! XI=EPSA
           IND=0
   16    IND=IND+1
c	   WRITE(666,*)' XI=6  HXR'
c	   WRITE(666,*)' $$$$$$$$$$$'
	   RATIO=5.0*IND
         XXX=RATIO *HXR
         CALL ULT(E,SYEQ,DELTA,EPSA,XXX,DXR,WXR,TXR,ENTR,SPAIK1)
           DSPAIK(KK)=(SPAIK1-SPAIK)/(XXX-HXR)
c          WRITE(666,*)'Subr ULT: SPAIK6= ',SPAIK1
c	   WRITE(666,*)'HXR6=',XXX,'  et DSPAIK6=',DSPAIK(KK)
	   IF((DABS(DSPAIK(KK)).LE.1000.).AND.(IND.LE.4)) GOTO 16
        ELSE IF (KK.EQ.7) THEN                  ! XI=EPSA
c          WRITE(666,*)' XI=7   Tweb'
c	   WRITE(666,*)' $$$$$$$$$$$'
	   RATIO=10.0
         XXX=RATIO *DXR
         CALL ULT(E,SYEQ,DELTA,EPSA,HXR,XXX,WXR,TXR,ENTR,SPAIK1)
           DSPAIK(KK)=(SPAIK1-SPAIK)/(XXX-DXR)
c	   WRITE(666,*)'Subr ULT: SPAIK7= ',SPAIK1
c	   WRITE(666,*)'DXR7=',XXX,'  et DSPAIK7=',DSPAIK(KK)
        ELSE IF (KK.EQ.8) THEN                  ! XI=EPSA
c          WRITE(666,*)' XI=8   D flange'
c	   WRITE(666,*)' $$$$$$$$$$$'
	   RATIO=10.0
         XXX=RATIO *WXR
         CALL ULT(E,SYEQ,DELTA,EPSA,HXR,DXR,XXX,TXR,ENTR,SPAIK1)
           DSPAIK(KK)=(SPAIK1-SPAIK)/(XXX-WXR)
c          WRITE(666,*)'Subr ULT: SPAIK8= ',SPAIK1
c	   WRITE(666,*)'WXR8=',XXX,'  et DSPAIK8=',DSPAIK(KK)
        ELSE IF (KK.EQ.9) THEN                  ! XI=EPSA
c          WRITE(666,*)' XI=9  EPSR'
c	   WRITE(666,*)' $$$$$$$$$$$'
	   RATIO=0.95
         XXX=RATIO *ENTR
         CALL ULT(E,SYEQ,DELTA,EPSA,HXR,DXR,WXR,TXR,XXX,SPAIK1)
           DSPAIK(KK)=(SPAIK1-SPAIK)/(XXX-ENTR)
C        Prise en compte de  d(D)/d(EPSR) = (D/EPSR)**2
           DSPAIK(KK)=DSPAIK(KK)*(ENTR/EPSR)**2
c          WRITE(666,*)'Subr ULT: SPAIK9= ',SPAIK1
c          WRITE(666,*)'EPSR9=',XXX,'  et DSPAIK9=',DSPAIK(KK)
        ELSE
           DSPAIK(KK)=0.
        ENDIF
  5   CONTINUE
	
      GOTO 4

C 2.2 Cas normal: les valeurs de á et Lamda sont dans les limites
C     ---------------------------------------------------------
C     Dans ce cas le calcul se fait par calcul direct (analytique)
   3  CONTINUE

     	FAC1=(0.936+0.188*BETA2-2.*0.067*SLEN2)*SLEN
      FAC2=(0.170+0.188*SLEN2)*BETA
	TEMP= DELTA*ENTR*YG - HXR*DXR*(SMDR-YG) - WXR*TXR*(SMCR-YG)
      NBRXI=NVAR(NEL)

      DO 1 K=1,NBRXI

        KK=NXIT(K,NEL)

c        WRITE(*,*) 'NBRXI             =',NBRXI
c        WRITE(*,*) 'NXIT(K,NEL)       =',KK
c	  PAUSE'P1'

          IF(KK.EQ.1) THEN                      !  XI=DELTA
            DEPSA=0.                            !  d(EPSA)/dXI  * SLEN/EPSA
            DBETA=-BETA/DELTA                   !  DBETA=d(BETA)/dXI
            DGX=OM                              !          DGX  =d(GX) /dXI
            DOMT=ENTR                           !  DOMT =d(OMT)/dXI
            DIS=ENTR*(0.25*DELTA**2+YG**2) + HXR*DXR*(SMDR-YG) ! DIS =d(IS) /dXI
     *                                   + WXR*TXR*(SMCR-YG)
          ELSE IF (KK.EQ.5) THEN                ! XI=EPSA
            DEPSA=SLEN/EPSA                     ! d(EPSA)/dXI  * SLEN/EPSA
	    DBETA=0.
	    DGX=0.	
	    DOMT=0.									
	    DIS=0.
          ELSE IF (KK.EQ.6) THEN                !  XI=HXR
            DEPSA=0.
	    DBETA=0.
	    DGX=OM*2.+DXR*DELTA	
	    DOMT=DXR									
	    DIS=0.25*DXR*HXR**2 + DXR*(SMDR-YG)*(SMDR-YG+HXR)
     *                        + 2.*WXR*TXR*(SMCR-YG)
          ELSE IF (KK.EQ.7) THEN                !  XI=DXR=Tweb
	    DEPSA=0.
	    DBETA=0.
	    DGX=2.*HXR*SMDR	
	    DOMT=HXR									
	    DIS=(HXR**3)/12. +  HXR*(SMDR-YG)**2
          ELSE IF (KK.EQ.8) THEN                ! XI=WXR =Lsem.
	    DEPSA=0.
	    DBETA=0.
	    DGX=2.*TXR*SMCR
	    DOMT=TXR									
	    DIS=(TXR**3)/12. +  TXR*(SMCR-YG)**2
          ELSE IF (KK.EQ.9) THEN                ! XI=EPSR
            DEPSA=0.
	    DBETA=BETA/ENTR
	    DGX=0.	
	    DOMT=DELTA									
	    DIS=(DELTA**3)/12. +  DELTA* YG**2
	  ELSE
	    DEPSA=0.
	    DBETA=0.
	    DGX=0.	
	    DOMT=0.									
	    DIS=0.
	  ENDIF
        
	  DYG=(OMT*DGX-GX*DOMT)/(2.*OMT*OMT)
          DIS=DIS +2.*DYG* TEMP                                 ! DIS =d(IS) /dXI
          DSLEN=SLEN * (IS*DOMT-OMT*DIS) / (2.*OMT*IS)  + DEPSA
          DSPAIK(KK)=-SYEQ*(FAC1*DSLEN+FAC2*DBETA)/(FCT**3)     ! d(SPAIK)/dXI
	  
C       Prise en compte de  d(D)/d(EPSR) = (D/EPSR)**2 
        IF (KK.EQ.9) THEN
	     DSPAIK(KK)=DSPAIK(KK)*(ENTR/EPSR)**2
	  ENDIF
 
c 	  WRITE(666,*)'Ds subr ULTPAN, NEL=',NEL
c 	  WRITE(666,*)'--------------------------'
c	  WRITE(666,*)'------  KK = ',KK
c	  WRITE(666,*)'FAC1, FAC2 = ',FAC1,FAC2
c	  WRITE(666,*)'DOMT, OMT,IS = ',DOMT,OMT,IS
c	  WRITE(666,*)'DEPSA, = ',DEPSA,' DYG, DIS = ',DYG,DIS
c	  WRITE(666,*)'SLEN,DBETA, DSLEN = ',SLEN,DBETA,DSLEN
c	  WRITE(666,*)'DSPAIK(',KK,')= ', DSPAIK(KK)

   1  CONTINUE
 
C 2.3 Impressions
C     ------------
   4  CONTINUE

c 	WRITE(666,*)
c 	WRITE(666,*)'Ds subr ULTPAN, NEL=',NEL
c 	WRITE(666,*)'--------------------------'
c     WRITE(666,*)'BETA    = ',BETA
c     WRITE(666,*)'LAMDA   = ',SLEN
c     WRITE(666,*)'SPAIK   = ',SPAIK
c     WRITE(666,*)'DSPAIK  = ',DSPAIK

      RETURN 
      END


C **********************************************************************
C **********************************************************************
C **********************************************************************
	
      SUBROUTINE ULT(E,SIGY,DELTA,EPSA,HXR,DXR,WXR,TXR,ENTR,SPAIK)

      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 IS

      COMMON /PY/ PI

C***********************************************************************
C     SUBROUTINE ULT
C     ===================
C     Cette subrourine calcule la résistance ultime d'une plaque raidie
C     londitudinalement et comprimée, également, longitudinalement.
C     (La pression latérael n'est pas prise en compte)
C
C     Il s'agit de la formule empirique de J. PAIK.  qui est basée 
C     sur le principe de la poutre colonne.
C                                                                      
C     ENTR = D(raid) remplace EPSR qui était une approximation (bordé collaborant)
C
C*******************************************************************************
C   RAIDISSEURS                                                   
C  -------------                                !
      OMT =WXR*TXR + DXR*HXR + ENTR*DELTA       ! section raid + D(raid)
      SMCR=DELTA/2.+HXR+TXR/2.
      SMDR=(DELTA+HXR)/2.
      HX  =WXR*TXR*SMCR + DXR*HXR*SMDR          ! Moment statique (Z=0)

c	WRITE(666,*)'ds subr ULT' 
c	WRITE(666,*)'------------'
c	WRITE(666,*)'HXR,DXR,WXR,TXR,ENTR = ',HXR,DXR,WXR,TXR,ENTR
c	WRITE(666,*)'EPSA,DELTA= ',EPSA,DELTA

      YG=HX/OMT                                          ! YG=position du centre de gravité.
      IS=(ENTR*DELTA**3 + DXR*HXR**3 + WXR*TXR**3)/12.   ! IS=Inertie par rapport
     *    + DELTA*ENTR*YG*YG  +  HXR*DXR*(SMDR-YG)**2    !    au centre de gravité.
     *                        +  WXR*TXR*(SMCR-YG)**2
      ROT =DSQRT(SIGY/E)
      BETA=ENTR * ROT / DELTA
      SLEN=EPSA * ROT* DSQRT(OMT/IS) / PI

	WRITE(666,*)' BETA = ',BETA,' et SLEN(non modif) = ',SLEN
c
C   TEST SUR BETA et Lamda=SLEN (SLIM= élancement maximum pour un BETA donné)
C   ----------------------------------------------------------------------
      ILIM=0
	IF(BETA.LE.2) THEN
	   SLIM=3.0+(BETA-1.)/2.
	ELSE
	   SLIM=3.5+(BETA-2.)
	ENDIF
c	WRITE(666,*)'SLEN maximum = ',SLIM,' >?> SLEN = ',SLEN
	IF(SLEN.GT.SLIM) THEN
	  ILIM=1
	  SLEN=SLIM
c	  WRITE(666,*)' BETA = ',BETA,' et SLEN(modif) = ',SLEN
	ENDIF

C   Calcul de SPAIK
C   ----------------
	BETA2=BETA*BETA

      IF(HXR.LE.0.00001) THEN
        FCT=DSQRT(0.995+0.17*BETA2)
      ELSE
	  SLEN2=SLEN*SLEN
	  FCT=DSQRT(0.995 + 0.936*SLEN2 + 0.17*BETA2 + 0.188*BETA2*SLEN2
     *                - 0.067*SLEN2*SLEN2)
      ENDIF
	
      SPAIK=SIGY/FCT

c	WRITE(666,*)'FCT = ',FCT
c	WRITE(666,*)'****** SPAIK (modif)= ',SPAIK
	
      RETURN 
      END


C **********************************************************************
C **********************************************************************
C **********************************************************************


      SUBROUTINE DSECT(NETO,NEL,DELTA,HXR,DXR,WXR,TXR,EPSR,PHIL,Q,DA,
     *                 NVAR,NXIT)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION DA(9)

      COMMON /PY/ PI
      COMMON /OPTI/IOPTI,NTOT
      DIMENSION NVAR(NETO),NXIT(9,NETO)


C***********************************************************************
C     SUBROUTINE DSECT
C     ==================
C     Cette subrourine calcule les dérivées des sections par rapport
C     aux variables de conception (XI).
C     La section de chaque composant (deck, fond, side) est la somme des
C     sections de plusieurs panneaux.
C     La section tarnsversale d'un panneau est composé de
C       - section du bordage (PHIL*Q*DELTA)
C       - section des raid.  (HXR*DXR+WXR*TXR)* PHIL*Q/EPSR
C       - section des traverses
C
C     Les variables de conception qui interviennent dans ces sections sont:
C      DELTA et   HXR,DXR,WXR,TXR,EPSR
C                                                                       
C   INPUTS : DELTA,HXR,DXR,WXR,TXR,EPSR  + Q,PHIL
C                                                                       
C   OUTPUTS:
C	DA = DA +d(A)/dxi
C     avec A  la section d'un panneau inclus dans le composant (deck,fond, ..)
C          XI les variables de conception
C
C     Créer : le 11-3-97  par  Ph. Rigo
C
C     Modifications : 
C                               
C     Dernière modif : 11-3-97 				     	
C                                                                       
C*************************************************************************

      QPHIL=Q*PHIL*PI/180.

C 1.0 Calcul des sensibilités
C     ***********************
      NBRXI=NVAR(NEL)

      DO 1 K=1,NBRXI

        KK=NXIT(K,NEL)	


          IF(KK.EQ.1) THEN                      ! XI=DELTA
	    DA(KK)=DA(KK) + QPHIL

          ELSE IF (KK.EQ.6) THEN                 !  XI=HXR
            DA(KK)=DA(KK) + QPHIL*DXR/EPSR

          ELSE IF (KK.EQ.7) THEN                 !  XI=DXR=Tweb
            DA(KK)=DA(KK) + QPHIL*HXR/EPSR

          ELSE IF (KK.EQ.8) THEN                 !  XI=WXR =Lsem.
            DA(KK)=DA(KK) + QPHIL*TXR/EPSR

          ELSE IF (KK.EQ.9) THEN                 !  XI=EPSR
	    DA(KK)=DA(KK) - QPHIL*(HXR*DXR+WXR*TXR)/(EPSR*EPSR)
	  ENDIF
        
   1  CONTINUE
 
c 	WRITE(666,*)
c 	WRITE(666,*)'Ds subr DSECT, NEL=',NEL
c 	WRITE(666,*)'--------------------------'
c	TEMP=QPHIL*(HXR*DXR+WXR*TXR)/EPSR
c      WRITE(666,*)'A  = section      =',TEMP
c      WRITE(666,*)'DA = DA +d(A)/dxi = ',DA

      RETURN 
      END


C ********************************************************************
C ********************************************************************
C ********************************************************************

C ********************************************************************
      SUBROUTINE ULTHULL
C     -------------------

C     PREDICTION OF ULTIMATE HULL STRENGTH
C     USING THE ANALYTICAL FORMULA DERIVED BY PAIK & MANSOUR

C ********************************************************************

      IMPLICIT REAL*8(A-H,O-Z)
      
      COMMON/USHULL/DEPTH,DB,			      ! HULL,BO2
     *              FICTIF(7),
     *              IULT,IRESTR

      COMMON/STR/   SYD,SYB,SYB1,SYSU,SYSL,SUD,SUB,SUB1,SUSU,SUSL,
     *              AD,AB1,AB,AS
	
      WRITE(66,26) SYD,SYB,SYB1,SYSU,SYSL,SUD,SUB,SUB1,SUSU,SUSL
  26  FORMAT(/
     * 'EQUIVALENT YIELD STRESS OF DECK         (N/m2) ',E14.7/
     * 'EQUIVALENT YIELD STRESS OF OUTER BOTTOM (N/m2) ',E14.7/ 
     * 'EQUIVALENT YIELD STRESS OF INNER BOTTOM (N/m2) ',E14.7/
     * 'EQUIVALENT YIELD STRESS OF SIDE SHELL-UP(N/m2) ',E14.7/
     * 'EQUIVALENT YIELD STRESS OF SIDE SHELL-LW(N/m2) ',E14.7//
     * 'ULTIMATE STRENGTH OF DECK               (N/m2) ',E14.7/ 
     * 'ULTIMATE STRENGTH OF OUTER BOTTOM       (N/m2) ',E14.7/ 
     * 'ULTIMATE STRENGTH OF INNER BOTTOM       (N/m2) ',E14.7/  
     * 'ULTIMATE STRENGTH OF UPPER SIDE SHELL   (N/m2) ',E14.7/ 
     * 'ULTIMATE STRENGTH OF LOWER SIDE SHELL   (N/m2) ',E14.7/) 

      IF(SYB1.NE.0) THEN
	  TEMP=SUB1/SYB1
	ELSE
	  TEMP=0.
	ENDIF
	
      WRITE(66,27) SUD/SYD,SUB/SYB,TEMP,SUSU/SYSU,SUSL/SYSL
  27  FORMAT('RELATIVE ULTIMATE STRENGTH (Su/Sy)'/
     * '     - DECK             ',F6.4/ 
     * '     - OUTER BOTTOM     ',F6.4/ 
     * '     - INNER BOTTOM     ',F6.4/  
     * '     - UPPER SIDE SHELL ',F6.4/ 
     * '     - LOWER SIDE SHELL ',F6.4/) 

      WRITE(66,36) AB,AB1,AD,AS
  36  FORMAT('P R O P E R T I E S   O F  ',
     *            'H U L L   S E C T I O N :'/
     *  'TOTAL SECTIONAL AREA OF OUTER BOTTOM (m2) =',E14.7/
     *  'TOTAL SECTIONAL AREA OF INNER BOTTOM (m2) =',E14.7/
     *  'TOTAL SECTIONAL AREA OF DECK         (m2) =',E14.7/
     *  'HALF-SECTIONAL  AREA OF ALL SIDES    (m2) =',E14.7//
     *  'RESULTS OF ANALYSIS.'/23(1H*))
C--------------------------------------------------------------------

      D=DEPTH
	
C 1.0 FULLY PLASTIC BENDING MOMENT (Eqs 2.a & 3.a for "g" and "Mp")
C     ------------------------------
      G=D/2.0/AS/(SYSU+SYSL)
      G2=AD*SYD+2.0*AS*SYSU-AB*SYB
C     Si G>DB : G =(AD*SYD+2.0*AS*SYSU-AB*SYB-AB1*SYB1 ) *D/2.0/AS/(SYSU+SYSL)
        G1=(G2-AB1*SYB1) * G
C     Si G<DB : G =(AD*SYD+2.0*AS*SYSU-AB*SYB+AB1*SYB1 ) *D/2.0/AS/(SYSU+SYSL)
        G2=(G2+AB1*SYB1) * G

      IF(G1.LE.DB.AND.G2.GE.DB) THEN
C       G=DB (il n'y a pas d'équilibre pour Sb1= ñ SYB1 )
	  G=DB
        WRITE(66,*) ' Cas 1: G=DB:  G=',G
        FPBM=AD*(D-G)*SYD+AB*G*SYB
     *      +AS/D*(SYSU*(D-G)**2+SYSL*G*G)
      ELSEIF(G1.GE.DB) THEN
C       G=G1 ò DB
        G=G1
        WRITE(66,*) ' Cas 2: G>DB  :  G=',G
        IF(G.GE.D) THEN
         WRITE(66,*) 
	   G=D
         WRITE(66,*) '  G>D alors G=D=Creux (Sect. deck trop important)'
	  ENDIF
        FPBM=AD*(D-G)*SYD+AB*G*SYB+AB1*(G-DB)*SYB1
     *      +AS/D*(SYSU*(D-G)**2+SYSL*G*G)
      ELSE
C       G=G2 < DB
        G=G2
        WRITE(66,*) ' Cas 3: G<DB:  G=',G
        IF(G.LE.0.001) THEN
	   G=0.
         WRITE(66,*) '  G<0 alors G=0 (Sect. fond trop important)'
	  ENDIF
        FPBM=AD*(D-G)*SYD+AB*G*SYB+AB1*(DB-G)*SYB1
     *      +AS/D*(SYSU*(D-G)**2+SYSL*G*G)
      ENDIF

      WRITE(66,706) FPBM,G
706   FORMAT(/5X,'FULLY PLASTIC BENDING MOMENT  (N.m)  =',E15.7/
     *        5x,'  (G = ',F12.3,' m)')

      FPSHEAR= AS*(SYSU+SYSL) / 1.732

      WRITE(66,707) FPSHEAR
707   FORMAT(/5X,'FULLY PLASTIC SHEAR FORCE     (N)    =',E15.7)

C 2.0 NEUTRAL AXIS OF HULL SECTION
C     ----------------------------
      H=(D*(AD+AS)+AB1*DB)/(AD+AB+AB1+2.0*AS)
      
      WRITE(66,710) H
710   FORMAT(/5X,'NEUTRAL AXIS OF HULL(from bott.)(m) =',F12.3)

C 3.0 ELASTIC SECTION MODULUS AT DECK
C     -------------------------------
C     ZZ=AD*(D-H)**2+AB*H*H+AB1*(H-DB)**2+AS*D*(2.0*D-3.0*H)/3.0 (as defined by PAik)

      ZZ=AD*(D-H)**2 + AB*H*H + AB1*(H-DB)**2
     *               +2*AS*(D*D/12.+(D/2.-H)**2)
      ZD=ZZ/(D-H)

C 3.1 ELASTIC SECTION MODULUS AT BOTTOM
C     ---------------------------------
      ZB=ZZ/H

C 3.2 FIRST YIELD STRENGTH
C     ---------------------
      DYIELD=ZD*SYD
      BYIELD=ZB*SYB

c      DYIELD=DYIELD*1.0E-3
c      BYIELD=BYIELD*1.0E-3
      
      WRITE(66,806) DYIELD,BYIELD
806   FORMAT(/5X,'FIRST YIELD BENDING MOMENT (N.m):',
     *        /7X,'- AT DECK     =',E15.7,
     *        /7X,'- AT BOTTOM   =',E15.7)

c      ZD=ZD*1.0E-9
c      ZB=ZB*1.0E-9
      
      WRITE(66,711) ZD,ZB
711   FORMAT(/5X,'SECTION MODULUS, Z=I/v (m3):',
     *       /7X,'- AT DECK      =',E15.7,
     *       /7X,'- AT BOTTOM    =',E15.7)

C 4.0 ULTIMATE HULL STRENGTH
C     =======================
c     If there is no double deck (AB1=0)
      IF(AB1.LT.0.000001) AB1=0.000001
      IF(DB.LT.0.0001)  DB=0.0001
      IF((AB1/AB).LT.0.001) THEN
         DB=0.000001*D
      ENDIF

C 4.1 FOR SAGGING CONDITION ("H" from Eq.12.b ; "g" from Eq.14; "Usag" from Eq.17.b)
C     ======================
      SUS=SUSU
      SYS=SYSL
	SS=SUS+SYS
      
      BB=(AD*SUD+2.0*AS*SUS-AB*SYB-AB1*SYS)/AS/SS
      CC=AB1*DB/AS
      H=( BB*D+DSQRT(BB**2*D**2+4.0*CC*D) )/2.

      WRITE(66,*) 
      WRITE(66,*)'     H & Hb = HIGH of the LINEAR ZONE',
     *           ' (of the vertical stress distribution)' 
      WRITE(66,*)'     - Sagging:'
c     WRITE(66,*)'         H   = ',H
 
c23456789012345678901234567890123456789012345678901234567890123456789012

      Hb=0.
      IF(H.LE.DB) THEN     
C     Cas limite nø2 : H < DB ou H < 0 : Deck too strong or Bottom too weak 
C     ---------------
C       We have to recompute H for the correct stress distribution in the inner bottom
C          Sb' = stress in the inner bottom = SUB1 (ultimate strength of the panel)
	   WRITE(29,*)'subr USHULL'													!bug
	   WRITE(29,*)'Cas limite nø2 : H < DB ou H < 0
     * Deck too strong or Bottom too weak'										!bug

        H=D*(AD*SUD+2.0*AS*SUS+AB1*SUB1-AB*SYB)/AS/SS
        WRITE(66,*)'     as  H   = ',H,' <? <  DB  = ',DB
        IF(H.LE.0) THEN     
          H=D*1.00E-10
          G=0.
        ELSE
	  G=H*SYS/SS
        ENDIF
        WRITE(66,*)'         H   = ',H
        WRITE(66,*)'         G   = ',G
        USAG=-AD*(D-G)*SUD - AS/D*(D-H)*(D+H-2.0*G)*SUS
     *       -AB*G*SYB - AB1*(DB-G)*SUB1
     *       -AS*H/3.0/D*((2.0*H-3.0*G)*SUS-(H-3.0*G)*SYS)
     
      ELSEIF (H.GT.D) THEN

C     Cas limite nø4 : H>D : Tensile bottom too strong or Compressed Deck too weak 
C     ---------------
C        We assume H = 0 and
C                 a tensile yield zone is introduced = D-Hb
         WRITE(66,*)'  Cas limite nø4 : (H=',H,' > D)'
         WRITE(66,*)'     as H>D, a TENSILE ZONE (D-Hb) is required'

	   WRITE(29,*)'subr USHULL'													!bug
	   WRITE(29,*)'Cas limite nø4 : H>D 
     * Tensile bottom too strong or Compressed Deck too weak'						!bug

         H=D
C        First, we suppose  D-Hb > DB (this means large tensile zone)
         Hb=(-AD*SUD+AB1*SYB1+AB*SYB+2.0*AS*SYS)*D/(AS*SS)
	   IF(Hb.LE.0.0001) Hb=0.0001

         IF((D-Hb).GE.DB) THEN
C          H>D and  D-Hb > DB (large tensile yield zone) : cas nø 4.a
C          ----------------------------------------------
           WRITE(66,*)'   A large TENSILE ZONE (D-Hb) is valid'
           WRITE(66,*)'         Hb   = ',Hb
           WRITE(66,*)'         D-Hb = ',D-Hb,' > DB  = ',DB
           G=Hb*SUS/SS
           USAG=-AD*G*SUD - AS/D*(D-Hb)*(D+Hb-2.0*G)*SYS
     *       -AB*(D-G)*SYB - AB1*(D-G-DB)*SYB1
     *       -AS*Hb/3.0/D*((2.0*Hb-3.0*G)*SYS-(Hb-3.0*G)*SUS)
	   ELSE
C          H>D and  D-Hb < DB (small tensile yield zone) : cas nø 4.b
C          ----------------------------------------------
           WRITE(66,*)'   A small TENSILE ZONE (D-Hb) is valid'
           BB=-D*(-AD*SUD-AB1*SUS+2.0*AS*SYS+AB*SYB)/AS/SS
           CC=-AB1*D*(D-DB)/AS
           Hb=( -BB + DSQRT(BB*BB-4.0*CC) )/2.
           WRITE(66,*)'         Hb   = ',Hb
           WRITE(66,*)'         D-Hb = ',D-Hb,' < DB  = ',DB
           G=Hb*SUS/SS
           USAG=-AD*G*SUD - AS/D*(D-Hb)*(D+Hb-2.0*G)*SYS
     *         - AB*(D-G)*SYB - AB1*(D-G-DB)*(-SUS+(SYS+SUS)*(D-DB)/Hb)
     *         - AS*Hb/3.0/D*((2.0*Hb-3.0*G)*SYS-(Hb-3.0*G)*SUS)
	   ENDIF

      ELSE 
C       Normal case : DB <H < D
C       ---------------
        G=H*SYS/SS
        USAG=-AD*(D-G)*SUD - AS/D*(D-H)*(D+H-2.0*G)*SUS
     *       -AB*G*SYB + AB1*(G-DB)*(-SYS+(DB/H)*SS)
     *       -AS*H/3.0/D*((2.0*H-3.0*G)*SUS-(H-3.0*G)*SYS)
      ENDIF

      WRITE(66,*)'         H   = ',H
      WRITE(66,*)'         G   = ',G
      WRITE(66,*)'         Hb  = ',Hb


c23456789012345678901234567890123456789012345678901234567890123456789012

C 4.2 FOR HOGGING CONDITION ("H" from Eq.15.b ; "g" from Eq.16; "Uhog" from Eq.18.b)
C     ======================
      SUS=SUSL
      SYS=SYSU
	SS=SUS+SYS
      
      H=AB*SUB+AB1*SUB1+2.0*AS*SUS-AD*SYD
      H=H*D/(AS*SS)

      WRITE(66,*)'     - Hogging:'

      Hb=0.
      IF(H.LE.(D*1.00E-10)) THEN
C     Cas limite nø1 : H<0 : Deck too strong or too weak bottoms 
C     ---------------
        WRITE(66,*)'      H=',H,' < 0 , then H=0.0'
																				!bug
	  WRITE(29,*)'Cas limite nø1 : H<0 : Deck too strong
     *  or too weak bottoms'														!bug 
        H=0.
        G=0.
        UHOG=AB*D*SUB + AB1*(D-DB)*SUB1 + AS*D*SUS
	  
      ELSEIF(H.GT.(D-DB)) THEN
C     Cas limite nø3 : H>D-DB : Bottom very strong or deck too weak
C     ---------------
C       We have to recompute H for the correct stress distribution in the inner bottom
C          Sb' = stress in the inner bottom << SUB1 (ultimate strength of the panel)

	  WRITE(29,*)'
     * Cas limite nø3 : H>D-DB : Bottom very strong or deck too weak'				!bug

        WRITE(66,*)'  Cas limite nø3 : H=',H
        WRITE(66,*)'    as H>D-DB : H is recomputed'
        BB=D*(-AB*SUB-2.0*AS*SUS+AD*SYD+AB1*SYS)/AS/SS
        CC=-AB1*(D-DB)*D/AS
        H =( -BB + DSQRT(BB**2-4.0*CC) )/2.
c       WRITE(66,*)'         H   = ',H

        IF(H.GT.D) THEN
C       Cas limite nø3.a : H>D: tensile yield zone must be introduce = Hb 
C       ------------------
	     H=D
           BB=-D*(AD*SYD+2.0*AS*SYS-AB*SUB-AB1*SUS)/AS/SS
           CC=-AB1*DB*D/AS
           Hb=( -BB + DSQRT(BB**2-4.0*CC) )/2.
	     IF(Hb.LE.DB) Hb=DB+0.0001
           WRITE(66,*)'  Cas limite nø3 : H=',H
           WRITE(66,*)'    as H>D : yield zone Hb is introduced'
c          WRITE(66,*)'         Hb   = ',Hb
           G=Hb*SUS/SS
           UHOG=AD*SYD*(D-G) + AB*G*SUB 
     *        - AB1*(G-DB)*(-SUS+DB*(SYS+SUS)/Hb)
     *        + AS/D*(D-Hb)*(D+Hb-2.0*G)*SYS
     *        + AS*Hb/3.0/D*((2.0*Hb-3.0*G)*SYS-(Hb-3.0*G)*SUS)
	  ELSE
C       Cas limite nø3.b : D>H>D-DB: H can be used
C       -----------------
           G=H*SYS/SS
           UHOG=AD*SYD*G + AB*(D-G)*SUB 
     *         - AB1*(SYS-(D-DB)*(SYS+SUS)/H)*(D-DB-G)
     *         + AS/D*(D-H)*(D+H-2.0*G)*SUS
     *         + AS*H/3.0/D*((2.0*H-3.0*G)*SUS-(H-3.0*G)*SYS)
	  ENDIF
      ELSE
C     Normal Case: 0 < H < D-DB 
C     ---------------
	   WRITE(29,*)'subr USHULL'													!bug
	   WRITE(29,*)'Normal Case: 0 < H < D-DB'									!bug

        G=H*SYS/SS
        UHOG=AD*SYD*G + AB*(D-G)*SUB + AB1*SUB1*(D-G-DB)
     *      +AS/D*(D-H)*(D+H-2.0*G)*SUS
     *      +AS*H/3.0/D*((2.0*H-3.0*G)*SUS-(H-3.0*G)*SYS)
      ENDIF
      
      WRITE(66,*)'         H   = ',H
      WRITE(66,*)'         G   = ',G
      WRITE(66,*)'         Hb  = ',Hb

c23456789012345678901234567890123456789012345678901234567890123456789012

C 4.3 OUTPUTS
C     --------

      USAGr=USAG/FPBM
      UHOGr=UHOG/FPBM
      
      WRITE(66,816) USAG,USAGr,UHOG,UHOGr
816   FORMAT(/5X,'ULTIMATE BENDING MOMENT (N.m):'/5X,30(1H-)/
     *        7X,'- FOR SAGGING:'/
     *        7X,'     Mult.     =',E15.7/
     *        7X,'     Mult./Mp  =',F10.5/
     *        7X,'- FOR HOGGING:'/
     *        7X,'     Mult.     =',E15.7/
     *        7X,'     Mult./Mp  =',F10.5)


      RETURN
      END


C ********************************************************************
C ********************************************************************
C ********************************************************************
c23456789012345678901234567890123456789012345678901234567890123456789012

C ===================================================================
      SUBROUTINE DULTHULL(NETO,NVAR,NXIT,
     *                    DSUD,DSUB,DSUB1,DSUSU,DSUSL,DH,DHb,DG,
     *                    DAD,DAB,DAB1,DAS,DULT)

C ********************************************************************
C     Calcul des sensibilités liés à la restriction d'ensemble
C     imposée sur la RESISTANCE ULTIME DE LA POUTRE NAVIRE
C         MULT / M(max) < 1  en Hogging et en Sagging
C
C    Créer : 13-mars 1997 Dr. Ph. Rigo
C
C    Modified:
C ********************************************************************

      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER *12 VNOM2
      
      DIMENSION DSUD(9,NETO),DSUB(9,NETO),DSUB1(9,NETO),DSUSU(9,NETO),
     *          DSUSL(9,NETO),DH(9,NETO),DHb(9,NETO),DG(9,NETO),
     *          DAD(9,NETO),DAB(9,NETO),DAB1(9,NETO),DAS(9,NETO),
     *          DULT(9*NETO)

      COMMON /OPTI/IOPTI,NTOT
      DIMENSION NVAR(NETO),NXIT(9,NETO)

      COMMON/USHULL/DEPTH,DB,UHOGM,USAGM,				  ! HULL,BO2
     *              FICTIF(5),
     *              IULT,IRESTR

      COMMON/STR/   SYD,SYB,SYB1,SYSU,SYSL,SUD,SUB,SUB1,SUSU,SUSL,
     *              AD,AB1,AB,AS
		
C ********************************************************************
c23456789012345678901234567890123456789012345678901234567890123456789012

      WRITE(66,26) SYD,SYB,SYB1,SYSU,SYSL,SUD,SUB,SUB1,SUSU,SUSL
  26  FORMAT(/
     * 'EQUIVALENT YIELD STRESS OF DECK         (N/m2) ',E14.7/
     * 'EQUIVALENT YIELD STRESS OF OUTER BOTTOM (N/m2) ',E14.7/ 
     * 'EQUIVALENT YIELD STRESS OF INNER BOTTOM (N/m2) ',E14.7/
     * 'EQUIVALENT YIELD STRESS OF SIDE SHELL-UP(N/m2) ',E14.7/
     * 'EQUIVALENT YIELD STRESS OF SIDE SHELL-LW(N/m2) ',E14.7//
     * 'ULTIMATE STRENGTH OF DECK               (N/m2) ',E14.7/ 
     * 'ULTIMATE STRENGTH OF OUTER BOTTOM       (N/m2) ',E14.7/ 
     * 'ULTIMATE STRENGTH OF INNER BOTTOM       (N/m2) ',E14.7/  
     * 'ULTIMATE STRENGTH OF UPPER SIDE SHELL   (N/m2) ',E14.7/ 
     * 'ULTIMATE STRENGTH OF LOWER SIDE SHELL   (N/m2) ',E14.7/) 

      IF(SYB1.NE.0) THEN
	  TEMP=SUB1/SYB1
	ELSE
	  TEMP=0.
	ENDIF
	
      WRITE(66,27) SUD/SYD,SUB/SYB,TEMP,SUSU/SYSU,SUSL/SYSL
  27  FORMAT('RELATIVE ULTIMATE STRENGTH (Su/Sy)'/
     * '     - DECK             ',F6.4/ 
     * '     - OUTER BOTTOM     ',F6.4/ 
     * '     - INNER BOTTOM     ',F6.4/  
     * '     - UPPER SIDE SHELL ',F6.4/ 
     * '     - LOWER SIDE SHELL ',F6.4/) 

      WRITE(66,36) AB,AB1,AD,AS
  36  FORMAT('P R O P E R T I E S   O F  ',
     *            'H U L L   S E C T I O N :'/
     *  'TOTAL SECTIONAL AREA OF OUTER BOTTOM (m2) =',E14.7/
     *  'TOTAL SECTIONAL AREA OF INNER BOTTOM (m2) =',E14.7/
     *  'TOTAL SECTIONAL AREA OF DECK         (m2) =',E14.7/
     *  'HALF-SECTIONAL  AREA OF ALL SIDES    (m2) =',E14.7//
     *  'RESULTS OF ANALYSIS.'/23(1H*))
C--------------------------------------------------------------------
c     WRITE(*,*) 'ds ULTIHULL'
c	PAUSE'ULT1'

      D=DEPTH
	
C 1.0 FULLY PLASTIC BENDING MOMENT (Eqs 2.a & 3.a for "g" and "Mp")
C     ------------------------------
      G=D/2.0/AS/(SYSU+SYSL)
      G2=AD*SYD+2.0*AS*SYSU-AB*SYB
C     Si G>DB : G =(AD*SYD+2.0*AS*SYSU-AB*SYB-AB1*SYB1 ) *D/2.0/AS/(SYSU+SYSL)
        G1=(G2-AB1*SYB1) * G
C     Si G<DB : G =(AD*SYD+2.0*AS*SYSU-AB*SYB+AB1*SYB1 ) *D/2.0/AS/(SYSU+SYSL)
        G2=(G2+AB1*SYB1) * G

      IF(G1.LE.DB.AND.G2.GE.DB) THEN
C       G=DB (il n'y a pas d'équilibre pour Sb1= ñ SYB1 )
	  G=DB
        WRITE(66,*) ' Cas 1: G=DB:  G=',G
        FPBM=AD*(D-G)*SYD+AB*G*SYB
     *      +AS/D*(SYSU*(D-G)**2+SYSL*G*G)
      ELSEIF(G1.GE.DB) THEN
C       G=G1 ò DB
        G=G1
        WRITE(66,*) ' Cas 2: G>DB  :  G=',G
        IF(G.GE.D) THEN
         WRITE(66,*) 
	   G=D
         WRITE(66,*) '  G>D alors G=D=Creux (Sect. deck trop important)'
	   WRITE(29,*) '  G>D alors G=D=Creux (Sect. deck trop important)'					!bug
	  ENDIF
        FPBM=AD*(D-G)*SYD+AB*G*SYB+AB1*(G-DB)*SYB1
     *      +AS/D*(SYSU*(D-G)**2+SYSL*G*G)
      ELSE
C       G=G2 < DB
        G=G2
        WRITE(66,*) ' Cas 3: G<DB:  G=',G
        IF(G.LE.0.001) THEN
	   G=0.
         WRITE(66,*) '  G<0 alors G=0 (Sect. fond trop important)'
         WRITE(29,*) '  G<0 alors G=0 (Sect. fond trop important)'						!bug
	  ENDIF
        FPBM=AD*(D-G)*SYD+AB*G*SYB+AB1*(DB-G)*SYB1
     *      +AS/D*(SYSU*(D-G)**2+SYSL*G*G)
      ENDIF

      WRITE(66,706) FPBM,G
706   FORMAT(/5X,'FULLY PLASTIC BENDING MOMENT  (N.m)  =',E15.7/
     *        5x,'  (G = ',F6.3,' )')

      FPSHEAR= AS*(SYSU+SYSL) / 1.732
c      FPSHEAR=FPSHEAR*1.0E-3

      WRITE(66,707) FPSHEAR
707   FORMAT(/5X,'FULLY PLASTIC SHEAR FORCE     (N)    =',E15.7)

C 2.0 NEUTRAL AXIS OF HULL SECTION
C     ----------------------------
      H=(D*(AD+AS)+AB1*DB)/(AD+AB+AB1+2.0*AS)
      
      WRITE(66,710) H
710   FORMAT(/5X,'NEUTRAL AXIS OF HULL(from bott.)(m) =',F12.3)

C 3.0 ELASTIC SECTION MODULUS AT DECK
C     -------------------------------
C     ZZ=AD*(D-H)**2+AB*H*H+AB1*(H-DB)**2+AS*D*(2.0*D-3.0*H)/3.0 (as defined by PAik)

      ZZ=AD*(D-H)**2 + AB*H*H + AB1*(H-DB)**2
     *               +2*AS*(D*D/12.+(D/2.-H)**2)
      ZD=ZZ/(D-H)

C 3.1 ELASTIC SECTION MODULUS AT BOTTOM
C     ---------------------------------
      ZB=ZZ/H

C 3.2 FIRST YIELD STRENGTH
C     ---------------------
      DYIELD=ZD*SYD
      BYIELD=ZB*SYB

c      DYIELD=DYIELD*1.0E-3
c      BYIELD=BYIELD*1.0E-3
      
      WRITE(66,806) DYIELD,BYIELD
806   FORMAT(/5X,'FIRST YIELD BENDING MOMENT (N.m):',
     *        /7X,'- AT DECK     =',E15.7,
     *        /7X,'- AT BOTTOM   =',E15.7)

c      ZD=ZD*1.0E-9
c      ZB=ZB*1.0E-9
      
      WRITE(66,711) ZD,ZB
711   FORMAT(/5X,'SECTION MODULUS, Z=I/v (m3):',
     *       /7X,'- AT DECK      =',E15.7,
     *       /7X,'- AT BOTTOM    =',E15.7)

C ---------------------------------------------------------------------
C ---------------------------------------------------------------------
C     Impressions : Output 666 = "xxxx.opt"
c     -----------------------------------------

c      WRITE(666,*)
c      WRITE(666,*)'Valeurs au début de DULTHULL'
c      WRITE(666,*)'----------------------------'
c      WRITE(666,*)'SUD,SUB,SUB1,SUSU,SUSL'
c      WRITE(666,*) SUD,SUB,SUB1,SUSU,SUSL 
c      WRITE(666,*)'AD,AB1,AB,AS'
c      WRITE(666,*) AD,AB1,AB,AS 
c      WRITE(666,*) 
c      DO 23 NEL=1,NETO                                        
c         NBRXI=NVAR(NEL)
c         WRITE(666,*)
c         WRITE(666,*)'**** PANEL nø =',NEL
c         WRITE(666,*)'DAD   =',(DAD(I,NEL),I=1,NBRXI)
c         WRITE(666,*)'DAB   =',(DAB(I,NEL),I=1,NBRXI)
c         WRITE(666,*)'DAB1  =',(DAB1(I,NEL),I=1,NBRXI)
c         WRITE(666,*)'DAS   =',(DAS(I,NEL),I=1,NBRXI)
c         WRITE(666,*)'DSUD  =',(DSUD(I,NEL),I=1,NBRXI)
c         WRITE(666,*)'DSUB  =',(DSUB(I,NEL),I=1,NBRXI)
c         WRITE(666,*)'DSUB1 =',(DSUB1(I,NEL),I=1,NBRXI)
c         WRITE(666,*)'DSUSL =',(DSUSL(I,NEL),I=1,NBRXI)
c   23    CONTINUE
c
c      WRITE(*,*) 'IMPRESSIONS AU DEBUT DE DULTHULL'
c      PAUSE'OK?'

C ---------------------------------------------------------------------
	
C 4.0 ULTIMATE HULL STRENGTH
C     =======================
c     If there is no double deck (AB1=0)
      IF(AB1.LT.0.000001) AB1=0.000001
      IF(DB.LT.0.0001)  DB=0.0001
      IF((AB1/AB).LT.0.001) THEN
         DB=0.000001*D
      ENDIF
	
c23456789012345678901234567890123456789012345678901234567890123456789012

C 5.0 FOR SAGGING CONDITION ("H" from Eq.12.b ; "g" from Eq.14; "Usag" from Eq.17.b)
C     ======================
      SUS=SUSU
      SYS=SYSL
	SS=SUS+SYS
      
      WRITE(66,*) 
      WRITE(66,*)'     H & Hb = HIGH of the LINEAR ZONE',
     *           ' (of the vertical stress distribution)' 
      WRITE(66,*)'     - Sagging:'

      BB = - D*(AD*SUD+2.0*AS*SUS-AB*SYB-AB1*SYS) /(AS*SS)
      CC = - AB1*D*DB /AS
	RAC= DSQRT(BB*BB-4.0*CC)
      H  = (-BB + RAC)/2.

c      WRITE(666,*)
c      WRITE(666,*)'Sagging, calcul de dUsag/dxi'
c      WRITE(666,*)'----------------------------'

      DO 1 NEL=1,NETO
      NBRXI=NVAR(NEL)
      DO 2 K=1,NBRXI
        KK=NXIT(K,NEL)
        DCC=CC * ( DAB1(KK,NEL)/AB1-DAS(KK,NEL)/AS )
          DBB=-BB* ( DAS(KK,NEL)/AS + DSUSU(KK,NEL)/SS )
     *      - D* ( AD*DSUD(KK,NEL)+2.0*AS*DSUSU(KK,NEL)
     *            +DAD(KK,NEL)*SUD+2.0*DAS(KK,NEL)*SUS
     *            -DAB(KK,NEL)*SYB-DAB1(KK,NEL)*SYS  )/(AS*SS)
	  DH(KK,NEL)=-(DBB-(BB*DBB-2.*DCC)/RAC)/2.
        
   2  CONTINUE
   1  CONTINUE
 	
c23456789012345678901234567890123456789012345678901234567890123456789012

      Hb=0.
      IF(H.LE.DB) THEN     
C     Cas limite nø2 : H < DB ou H < 0 : Deck too strong or Bottom too weak 
C     ---------------
C       We have to recompute H for the correct stress distribution in the inner bottom
C          Sb' = stress in the inner bottom = SUB1 (ultimate strength of the panel)

	  write(29,*)'Cas limite nø2'													!bug
	  write(29,*)'H < DB ou H < 0 :Deck too strong or Bottom too weak'				!bug

        H=D*(AD*SUD+2.0*AS*SUS+AB1*SUB1-AB*SYB)/AS/SS
        WRITE(66,*)'     as  H   = ',H,' <? <  DB  = ',DB
	  I1=1
        IF(H.LE.0) THEN   
	    I1=0
          H=D*1.00E-10
          G=0.
        ELSE
	    G=H*SYS/SS
        ENDIF
        WRITE(66,*)'         H   = ',H
        WRITE(66,*)'         G   = ',G

        USAG=-AD*(D-G)*SUD - AS/D*(D-H)*(D+H-2.0*G)*SUS
     *       -AB*G*SYB - AB1*(DB-G)*SUB1
     *       -AS*H/3.0/D*((2.0*H-3.0*G)*SUS-(H-3.0*G)*SYS)     

C       Calcul de dH/dxi, dG/dxi et dUsag/dxi
        IM=0
        DO 46 NEL=1,NETO
          NBRXI=NVAR(NEL)
          DO 47 K=1,NBRXI
           KK=NXIT(K,NEL)
	     	     
           IF(I1.LE.0) THEN   
	       DG(KK,NEL)=0.
	       DH(KK,NEL)=0.
	     ELSE
	      DH(KK,NEL)=-H * ( DAS(KK,NEL)/AS + DSUSU(KK,NEL)/SS )
     *      +D*(AD*DSUD(KK,NEL)+2.0*AS*DSUSU(KK,NEL)+AB1*DSUB1(KK,NEL)
     *          +DAD(KK,NEL)*SUD+2.0*DAS(KK,NEL)*SUS+DAB1(KK,NEL)*SUB1
     *          -DAB(KK,NEL)*SYB  )/(AS*SS)
	      DG(KK,NEL)=(DH(KK,NEL)-G*DSUSU(KK,NEL)/SYS)*G/H
           ENDIF

           DULT(IM+K)=- AD * (D-G) * DSUD(KK,NEL)
     *      - (-AD*DG(KK,NEL)+DAD(KK,NEL)*(D-G)) * SUD 
     *      - (DAB(KK,NEL)*G+AB*DG(KK,NEL)) * SYB 
     *      - (DAB1(KK,NEL)*(DB-G)-AB1*DG(KK,NEL))*SUB1
     *      -  AB1*(DB-G) * DSUB1(KK,NEL)
     *      - (-AS*DH(KK,NEL)+DAS(KK,NEL)*(D-H)) * (D+H-2.0*G)*SUS/D
     *      -  AS*(D-H)/D * 
     *        ((DH(KK,NEL)-2.0*DG(KK,NEL))*SUS+(D+H-2.*G)*DSUSU(KK,NEL))
     *      - (AS*DH(KK,NEL)+DAS(KK,NEL)*H)
     *               *((2.0*H-3.0*G)*SUS-(H-3.0*G)*SYS)/(3.0*D)
     *      - ( (2.0*DH(KK,NEL)-3.0*DG(KK,NEL))*SUS
     *          +(2.*H-3.0*G)*DSUSU(KK,NEL)
     *          -(DH(KK,NEL)-3.0*DG(KK,NEL))*SYS  ) * AS*H/(3.0*D) 
           IMM=IM+K
c          WRITE(666,*)'XI nø',IMM,'  d(Usag)/dxi=',DULT(IMM)	     
  47      CONTINUE
c         WRITE(666,*)'DH   =',(DH(I,NEL),I=1,NBRXI)
c         WRITE(666,*)'DG    =',(DG(I,NEL),I=1,NBRXI)
          IM=IM+NBRXI
  46    CONTINUE

c23456789012345678901234567890123456789012345678901234567890123456789012

      ELSEIF (H.GT.D) THEN
C     Cas limite nø4 : H>D : Tensile bottom too strong or Compressed Deck too weak 
C     ---------------
C        We assume H = 0 and
C                 a tensile yield zone is introduced = D-Hb

	   WRITE(29,*)'subr USHULL'													!bug
	   WRITE(29,*)'Cas limite nø4 : H>D 
     * Tensile bottom too strong or Compressed Deck too weak'						!bug

         WRITE(66,*)'  Cas limite nø4 : (H=',H,' > D)'
         WRITE(66,*)'     as H>D, a TENSILE ZONE (D-Hb) is required'
         H=D
C        First, we suppose  D-Hb > DB (this means large tensile zone)
         Hb=(-AD*SUD+AB1*SYB1+AB*SYB+2.0*AS*SYS)*D/(AS*SS)
	   IF(Hb.LE.0.0001) Hb=0.0001

         IF((D-Hb).GE.DB) THEN
C          H>D and  D-Hb > DB (large tensile yield zone) : cas nø 4.a
C          ----------------------------------------------
           WRITE(66,*)'   A large TENSILE ZONE (D-Hb) is valid'
           WRITE(66,*)'         Hb   = ',Hb
           WRITE(66,*)'         D-Hb = ',D-Hb,' > DB  = ',DB
           G=Hb*SUS/SS
           USAG=-AD*G*SUD - AS/D*(D-Hb)*(D+Hb-2.0*G)*SYS
     *       -AB*(D-G)*SYB - AB1*(D-G-DB)*SYB1
     *       -AS*Hb/3.0/D*((2.0*Hb-3.0*G)*SYS-(Hb-3.0*G)*SUS)

C          Calcul de dH/dxi, dG/dxi et dUsag/dxi
           IM=0
           DO 42 NEL=1,NETO
           NBRXI=NVAR(NEL)
           DO 43 K=1,NBRXI
           KK=NXIT(K,NEL)

	     DHb(KK,NEL)=-Hb * ( DAS(KK,NEL)/AS + DSUSU(KK,NEL)/SS )
     *      +D*(-AD*DSUD(KK,NEL)
     *          -DAD(KK,NEL)*SUD+DAB1(KK,NEL)*SYB1
     *          +DAB(KK,NEL)*SYB+2.0*DAS(KK,NEL)*SYS  )/(AS*SS)
	     DG(KK,NEL)=(DHb(KK,NEL)/Hb+DSUSU(KK,NEL)*(1.0-G/Hb)/SUS)*G

           DULT(IM+K)=- AD * G * DSUD(KK,NEL)
     *      - (AD*DG(KK,NEL)+DAD(KK,NEL)*G) * SUD 
     *      - (DAB(KK,NEL)*(D-G)-AB*DG(KK,NEL)) * SYB 
     *      - (DAB1(KK,NEL)*(D-G-DB)-AB1*DG(KK,NEL))*SYB1
     *      - (-AS*DHb(KK,NEL)+DAS(KK,NEL)*(D-Hb)) * (D+Hb-2.0*G)*SYS/D
     *      -  AS*(D-Hb)/D * (DHb(KK,NEL)-2.0*DG(KK,NEL))*SYS
     *      - (AS*DHb(KK,NEL)+DAS(KK,NEL)*Hb)
     *               *((2.0*Hb-3.0*G)*SYS-(Hb-3.0*G)*SUS)/(3.0*D)
     *      - ( (2.0*DHb(KK,NEL)-3.0*DG(KK,NEL))*SYS
     *          -(Hb-3.0*G)*DSUSU(KK,NEL)
     *          -(DHb(KK,NEL)-3.0*DG(KK,NEL))*SUS  ) * AS*Hb/(3.0*D) 
           IMM=IM+K
c          WRITE(666,*)'XI nø',IMM,'  d(Usag)/dxi=',DULT(IMM)	     
  43       CONTINUE
c          WRITE(666,*)'DHb   =',(DHb(I,NEL),I=1,NBRXI)
c          WRITE(666,*)'DG    =',(DG(I,NEL),I=1,NBRXI)
           IM=IM+NBRXI
  42       CONTINUE

	   ELSE
C          H>D and  D-Hb < DB (small tensile yield zone) : cas nø 4.b
C          ----------------------------------------------
           WRITE(66,*)'   A small TENSILE ZONE (D-Hb) is valid'
           BB=-D*(-AD*SUD-AB1*SUS+2.0*AS*SYS+AB*SYB)/AS/SS
           CC=-AB1*D*(D-DB)/AS
	     RAC=DSQRT(BB*BB-4.0*CC)
           Hb=( -BB + RAC )/2.
           WRITE(66,*)'         Hb   = ',Hb
           WRITE(66,*)'         D-Hb = ',D-Hb,' < DB  = ',DB
           G=Hb*SUS/SS
	     
           USAG=-AD*G*SUD - AS/D*(D-Hb)*(D+Hb-2.0*G)*SYS
     *         - AB*(D-G)*SYB - AB1*(D-G-DB)*(-SUS+(SYS+SUS)*(D-DB)/Hb)
     *         - AS*Hb/3.0/D*((2.0*Hb-3.0*G)*SYS-(Hb-3.0*G)*SUS)

C          Calcul de dH/dxi, dG/dxi et dUsag/dxi
           IM=0
           DO 44 NEL=1,NETO
           NBRXI=NVAR(NEL)
           DO 45 K=1,NBRXI
           KK=NXIT(K,NEL)
	     	     
           DCC=CC * ( DAB1(KK,NEL)/AB1-DAS(KK,NEL)/AS )
	     DBB=-BB* ( DAS(KK,NEL)/AS + DSUSU(KK,NEL)/SS )
     *         - D* ( AD*DSUD(KK,NEL)-AB1*DSUSU(KK,NEL)
     *               -DAD(KK,NEL)*SUD-DAB1(KK,NEL)*SUS
     *               +2.0*DAS(KK,NEL)*SYS+DAB(KK,NEL)*SYB  )/(AS*SS)
	     DHb(KK,NEL)=-(DBB-(BB*DBB-2.*DCC)/RAC)/2.
	     DG(KK,NEL)=(DHb(KK,NEL)/Hb+DSUSU(KK,NEL)*(1.0-G/Hb)/SUS)*G

           DULT(IM+K)=- AD * G * DSUD(KK,NEL)
     *      - (AD*DG(KK,NEL)+DAD(KK,NEL)*G) * SUD 
     *      - (DAB(KK,NEL)*(D-G)-AB*DG(KK,NEL)) * SYB 
     *      - (DAB1(KK,NEL)*(D-G-DB)-AB1*DG(KK,NEL)) 
     *                * (-SUS+(D-DB)*SS/Hb)
     *      - AB1*(D-G-DB) * 
     *       (-(Hb-D+DB)*DSUSU(KK,NEL)/Hb-(D-Db)*SS*DHb(KK,NEL)/(Hb*Hb))
     *      - (-AS*DHb(KK,NEL)+DAS(KK,NEL)*(D-Hb)) * (D+Hb-2.0*G)*SYS/D
     *      -  AS*(D-Hb)/D * (DHb(KK,NEL)-2.0*DG(KK,NEL))*SYS
     *      - (AS*DHb(KK,NEL)+DAS(KK,NEL)*Hb)
     *               *((2.0*Hb-3.0*G)*SYS-(Hb-3.0*G)*SUS)/(3.0*D)
     *      - ( (2.0*DHb(KK,NEL)-3.0*DG(KK,NEL))*SYS
     *          -(Hb-3.0*G)*DSUSU(KK,NEL)
     *          -(DHb(KK,NEL)-3.0*DG(KK,NEL))*SUS  ) * AS*Hb/(3.0*D) 
           IMM=IM+K
c          WRITE(666,*)'XI nø',IMM,'  d(Usag)/dxi=',DULT(IMM)	     
  45       CONTINUE
c          WRITE(666,*)'DHb   =',(DHb(I,NEL),I=1,NBRXI)
c          WRITE(666,*)'DG    =',(DG(I,NEL),I=1,NBRXI)
           IM=IM+NBRXI
  44       CONTINUE

	   ENDIF

c23456789012345678901234567890123456789012345678901234567890123456789012

      ELSE 
C       Normal case : DB < H < D
C       ------------------------
        G=H*SYS/SS
        USAG=-AD*(D-G)*SUD - AS/D*(D-H)*(D+H-2.0*G)*SUS
     *       -AB*G*SYB + AB1*(G-DB)*(-SYS+(DB/H)*SS)
     *       -AS*H/3.0/D*((2.0*H-3.0*G)*SUS-(H-3.0*G)*SYS)

C       Calcul de dG/dxi et dUsag/dxi
        IM=0
        DO 40 NEL=1,NETO
          NBRXI=NVAR(NEL)
          DO 41 K=1,NBRXI
           KK=NXIT(K,NEL)

           DCC= CC * ( DAB1(KK,NEL)/AB1- DAS(KK,NEL)/AS )
	     DBB=-BB * ( DAS(KK,NEL)/AS  + DSUSU(KK,NEL)/SS )
     *         - D * (+AD*DSUD(KK,NEL)+2.0*AS*DSUSU(KK,NEL)
     *                +DAD(KK,NEL)*SUD+2.0*DAS(KK,NEL)*SUS
     *                -DAB(KK,NEL)*SYB-DAB1(KK,NEL)*SYS   )/(AS*SS)

	     DH(KK,NEL)=-(DBB-(BB*DBB-2.*DCC)/RAC)/2.
	     DG(KK,NEL)=(DH(KK,NEL)-G*DSUSU(KK,NEL)/SYS)*G/H

           DULT(IM+K)=- AD * (D-G) * DSUD(KK,NEL)
     *      - (-AD*DG(KK,NEL)+DAD(KK,NEL)*(D-G)) * SUD 
     *      - (DAB(KK,NEL)*G+AB*DG(KK,NEL)) * SYB 
     *      + (DAB1(KK,NEL)*(G-DB)+AB1*DG(KK,NEL))*(-SYS+DB*SS/H)
     *      +  AB1*(G-DB) * 
     *           (DSUSL(KK,NEL)*DB - SS*DB*DH(KK,NEL)/H) /H
     *      - (-AS*DH(KK,NEL)+DAS(KK,NEL)*(D-H)) * (D+H-2.0*G)*SUS/D
     *      -  AS*(D-H)/D * 
     *        ((DH(KK,NEL)-2.0*DG(KK,NEL))*SUS+(D+H-2.*G)*DSUSU(KK,NEL))
     *      - (AS*DH(KK,NEL)+DAS(KK,NEL)*H)
     *               *((2.0*H-3.0*G)*SUS-(H-3.0*G)*SYS)/(3.0*D)
     *      - ( (2.0*DH(KK,NEL)-3.0*DG(KK,NEL))*SUS
     *          +(2.*H-3.0*G)*DSUSU(KK,NEL)
     *          -(DH(KK,NEL)-3.0*DG(KK,NEL))*SYS  ) * AS*H/(3.0*D) 

           IMM=IM+K
c          WRITE(666,*)'XI nø',IMM,'  d(Usag)/dxi=',DULT(IMM)	     
  41      CONTINUE
c         WRITE(666,*)'DH   =',(DH(I,NEL),I=1,NBRXI)
c         WRITE(666,*)'DG    =',(DG(I,NEL),I=1,NBRXI)
          IM=IM+NBRXI
  40    CONTINUE

      ENDIF

      WRITE(66,*)'         H   = ',H
      WRITE(66,*)'         G   = ',G
      WRITE(66,*)'         Hb  = ',Hb

C     SAUVETAGE SUR DISQUE,  File 204 
C     -------------------------------
C     Sagging: Usag < 0 et Usag (max) < 0
C     Restriction : C(x)= USAG(max)/Usag(ult) < 1
C        dC(x)/dxi= - C(x)/Usag(ult) * dUsag(ult)/dxi
      VNOM2='ULT. SAGGING'
	C=USAGM/USAG
      CM=1.
	DO 48 I=1,NTOT
        DULT(I)= - (C/USAG) * DULT(I)
   48 CONTINUE
	WRITE(303) C,CM,VNOM2			!extension neto
	WRITE(303) (DULT(I),I=1,NTOT)	!extension neto

c      WRITE(666,*) 
c      WRITE(666,*) 'DC dans USHULL après write 204:'	
c      WRITE(666,*) '--------------------------------'
c      WRITE(666,*) 'C,CM,VNOM2=',C,CM,'  ',VNOM2
c      WRITE(666,'(9(E10.3,1X))') (DULT(I),I=1,NTOT)  		

c23456789012345678901234567890123456789012345678901234567890123456789012

c     ============================================================================
C 6.0 FOR HOGGING CONDITION ("H" from Eq.15.b ; "g" from Eq.16; "Uhog" from Eq.18.b)
C     ----------------------------------------------------------------------------
c     ============================================================================
      SUS=SUSL
      SYS=SYSU
	SS=SUS+SYS

c      WRITE(666,*)
c      WRITE(666,*)'Hoging, calcul de dUhog/dxi'
c      WRITE(666,*)'---------------------------'
      
      WRITE(66,*)'     - Hogging:'

C     Calcul de H (pour le cas normal)	
      H=(AB*SUB+AB1*SUB1+2.0*AS*SUS-AD*SYD) * D/(AS*SS)	
      Hb=0.
	
      IF(H.LE.(D*1.00E-10)) THEN
C 6.1 Cas limite nø1 : H<0 : Deck too strong or too weak bottoms 
C     ---------------
 	  write(29,*)'Cas limite nø1 : H<0'											!bug
	  write(29,*)'Deck too strong or too weak bottoms'							!bug
        
	  WRITE(66,*)'      H=',H,' < 0 , then H=0.0'
        H=0.
        G=0.

C       Calcul de Uhog	
        UHOG=AB*D*SUB + AB1*(D-DB)*SUB1 + AS*D*SUS
 
C       Calcul de dUhog/dxi	
        IM=0
        DO 15 NEL=1,NETO
        NBRXI=NVAR(NEL)
        DO 16 K=1,NBRXI
          KK=NXIT(K,NEL)
c          DG(KK,NEL)=0.
c          DH(KK,NEL)=0.
          DULT(IM+K)=  D * ( DAB(KK,NEL) *SUB  + AB *DSUB(KK,NEL)  ) 
     *           + (D-DB)* ( DAB1(KK,NEL)*SUB1 + AB1*DSUB1(KK,NEL) ) 
     *              +  D * ( DAS(KK,NEL) *SUS  + AS *DSUSL(KK,NEL) )   
          IMM=IM+K
c         WRITE(666,*)'XI nø',IMM,'  d(Uhogg)/dxi=',DULT(IMM)	     	  
  16    CONTINUE
        IM=IM+NBRXI
  15    CONTINUE
	  
      ELSEIF(H.GT.(D-DB)) THEN
C 6.2 Cas limite nø3 : H>D-DB : Bottom very strong or deck too weak
C     ---------------
C       We have to recompute H for the correct stress distribution in the inner bottom
C          Sb' = stress in the inner bottom << SUB1 (ultimate strength of the panel)
	   WRITE(29,*)'subr USHULL'													!bug
	   WRITE(29,*)'as limite nø3 : H>D-DB 
     * Bottom very strong or deck too weak'										!bug

        WRITE(66,*)'  Cas limite nø3 : H=',H
        WRITE(66,*)'    as H>D-DB : H is recomputed'

C       Nouveau calcul du H
        BB=D*(-AB*SUB-2.0*AS*SUS+AD*SYD+AB1*SYS)/AS/SS
        CC=-AB1*(D-DB)*D/AS
	  RAC= DSQRT(BB*BB-4.0*CC)
        H =( -BB + RAC )/2.

        IF(H.GT.D) THEN
C 6.2.1    Cas limite nø3.a : H>D: A tensile yield zone must be introduce = Hb 
C          ------------------
           WRITE(66,*)'  Cas limite nø3.a : H=',H
           WRITE(66,*)'    as H>D : yield zone Hb is introduced'
           H=D

C          Calcul du Hb et du G
           BB=-D*(AD*SYD+2.0*AS*SYS-AB*SUB-AB1*SUS)/AS/SS
           CC=-AB1*DB*D/AS
	     RAC= DSQRT(BB*BB-4.0*CC)
           Hb =( -BB + RAC )/2.
           WRITE(66,*)'             Hb  = ',Hb
	     IF(Hb.LE.DB) Hb=DB+0.0001
           G=Hb*SUS/SS

C          Calcul du Uhog
           UHOG=AD*SYD*(D-G) + AB*G*SUB 
     *        - AB1*(G-DB)*(-SUS+DB*SS/Hb)
     *        + AS/D*(D-Hb)*(D+Hb-2.0*G)*SYS
     *        + AS*Hb/3.0/D*((2.0*Hb-3.0*G)*SYS-(Hb-3.0*G)*SUS)
     
C          Calcul du dH/dxi, dG/dxi et dUhog/dxi 
           IM=0
           DO 19 NEL=1,NETO
           NBRXI=NVAR(NEL)
           DO 20 K=1,NBRXI
           KK=NXIT(K,NEL)

           DCC= CC * ( DAB1(KK,NEL)/AB1- DAS(KK,NEL)/AS )
	     DBB=-BB * ( DAS(KK,NEL)/AS  + DSUSL(KK,NEL)/SS )
     *        - D* (-AB*DSUB(KK,NEL)-AB1*DSUSL(KK,NEL)
     *              +DAD(KK,NEL)*SYD+2.0*DAS(KK,NEL)*SYS
     *              -DAB(KK,NEL)*SUB-DAB1(KK,NEL)*SUS   )/(AS*SS)

	     DHb(KK,NEL)=-(DBB-(BB*DBB-2.*DCC)/RAC)/2.
           DG(KK,NEL)=DHb(KK,NEL)*G/Hb + DSUSL(KK,NEL)*G*(1.-G/Hb)/SUS

           DULT(IM+K)=(-AD*DG(KK,NEL)+DAD(KK,NEL)*(D-G)) * SYD 
     *      + (DAB(KK,NEL)*G+AB*DG(KK,NEL))*SUB + AB*G*DSUB(KK,NEL) 
     *      - (DAB1(KK,NEL)*(G-DB)+AB1*DG(KK,NEL))*(-SUS+DB*SS/Hb)
     *      -  AB1*(G-DB) * 
     *           (DSUSL(KK,NEL)*(DB/Hb-1.) - SS*DB*DHb(KK,NEL)/(Hb*Hb))
     *      + (-AS*DHb(KK,NEL)+DAS(KK,NEL)*(D-Hb)) * (D+Hb-2.0*G)*SYS/D
     *      +  AS*(D-Hb) * (DHb(KK,NEL)-2.0*DG(KK,NEL)) *SYS/D
     *      + (AS*DHb(KK,NEL)+DAS(KK,NEL)*Hb)
     *               *((2.0*Hb-3.0*G)*SYS-(Hb-3.0*G)*SUS)/(3.0*D)
     *      + ( (2.0*DHb(KK,NEL)-3.0*DG(KK,NEL))*SYS
     *          -(Hb-3.0*G)*DSUSL(KK,NEL)
     *          -(DHb(KK,NEL)-3.0*DG(KK,NEL))*SUS  ) * AS*Hb/(3.0*D) 
           IMM=IM+K
c          WRITE(666,*)'XI nø',IMM,'  d(Uhogg)/dxi=',DULT(IMM)	     
  20       CONTINUE
c          WRITE(666,*)'DHb   =',(DHb(I,NEL),I=1,NBRXI)
c          WRITE(666,*)'DG    =',(DG(I,NEL),I=1,NBRXI)
           IM=IM+NBRXI
  19       CONTINUE

	  ELSE
C 6.2.2    Cas limite nø3.b : D>H>D-DB: H can still be used
C          -----------------
           G=H*SYS/SS
           WRITE(66,*)' Cas 3.b : D>H>D-DB'
           UHOG=AD*SYD*G + AB*(D-G)*SUB 
     *         - AB1*(SYS-(D-DB)*(SYS+SUS)/H)*(D-DB-G)
     *         + AS/D*(D-H)*(D+H-2.0*G)*SUS
     *         + AS*H/3.0/D*((2.0*H-3.0*G)*SUS-(H-3.0*G)*SYS)

C          Calcul du dH/dxi , dG/dxi et  dUhog/dxi 
           IM=0
           DO 17 NEL=1,NETO
           NBRXI=NVAR(NEL)
           DO 18 K=1,NBRXI
           KK=NXIT(K,NEL)
           DCC=CC * ( DAB1(KK,NEL)/AB1-DAS(KK,NEL)/AS )
	     DBB=-BB* ( DAS(KK,NEL)/AS + DSUSL(KK,NEL)/SS )
     *        + D* (-AB*DSUB(KK,NEL) -2.0*AS*DSUSL(KK,NEL)
     *              -DAB(KK,NEL)*SUB -2.0*DAS(KK,NEL)*SUS
     *              +DAB1(KK,NEL)*SYS+DAD(KK,NEL)*SYD   )/(AS*SS)

	     DH(KK,NEL)=-(DBB-(BB*DBB-2.*DCC)/RAC)/2.
           DG(KK,NEL)=(DH(KK,NEL)-G*DSUSL(KK,NEL)/SYS) * G/H

           DULT(IM+K)=
     *        (AD*DG(KK,NEL)+DAD(KK,NEL)*G) * SYD 
     *      + (DAB(KK,NEL)*(D-G)-AB*DG(KK,NEL)) * SUB 
     *      +  AB*(D-G)*DSUB(KK,NEL) 
     *      - (DAB1(KK,NEL)*(D-G-DB)-AB1*DG(KK,NEL))*(SYS-(D-DB)*SS/H)
     *      -  AB1*(D-G-DB)*(D-DB)*(SS*DH(KK,NEL)/H-DSUSL(KK,NEL))/H 
     *      + (-AS*DH(KK,NEL)+DAS(KK,NEL)*(D-H)) * (D+H-2.0*G)*SUS/D
     *      +  AS*(D-H) * ( (DH(KK,NEL)-2.0*DG(KK,NEL))*SUS+
     *                      (D+H-2.0*G)*DSUSL(KK,NEL)   )/D
     *      + (AS*DH(KK,NEL)+DAS(KK,NEL)*H)
     *               *((2.0*H-3.0*G)*SUS-(H-3.0*G)*SYS)/(3.0*D)
     *      + ( (2.0*DH(KK,NEL)-3.0*DG(KK,NEL))*SUS+
     *          (2.0*H-3.0*G)*DSUSL(KK,NEL)-
     *          (DH(KK,NEL)-3.0*DG(KK,NEL))*SYS  ) * AS*H/(3.0*D) 
           IMM=IM+K
c          WRITE(666,*)'XI nø',IMM,'  d(Uhogg)/dxi=',DULT(IMM)	     	  
  18       CONTINUE

C          WRITE(666,*)'DG    =',(DG(I,NEL),I=1,NBRXI)
C          WRITE(666,*)'DH    =',(DH(I,NEL),I=1,NBRXI)
           IM=IM+NBRXI
  17       CONTINUE
	  ENDIF
	  
      ELSE
C 6.3 Normal Case: 0 < H < D-DB 
C     ---------------
        G=H*SYS/SS

C       Calcul de Uhog	
        UHOG=AD*SYD*G + AB*(D-G)*SUB + AB1*SUB1*(D-G-DB)
     *      +AS/D*(D-H)*(D+H-2.0*G)*SUS
     *      +AS*H/3.0/D*((2.0*H-3.0*G)*SUS-(H-3.0*G)*SYS)

C       Calcul de dH/dxi, dG/dxi  et dUhog/dxi	
        IM=0
        DO 13 NEL=1,NETO
        NBRXI=NVAR(NEL)
        DO 14 K=1,NBRXI
          KK=NXIT(K,NEL)
	    DH(KK,NEL)=-H * ( DAS(KK,NEL)/AS + DSUSL(KK,NEL)/SS )
     *    + D*( AB*DSUB(KK,NEL)+AB1*DSUB1(KK,NEL)+2.0*AS*DSUSL(KK,NEL)
     *          +DAB(KK,NEL)*SUB+DAB1(KK,NEL)*SUB1+2.0*DAS(KK,NEL)*SUS
     *          -DAD(KK,NEL)*SYD  )/(AS*SS)
          DG(KK,NEL)=(DH(KK,NEL)-G*DSUSL(KK,NEL)/SYS) * G/H
          DULT(IM+K)=
     *        (AD*DG(KK,NEL)+DAD(KK,NEL)*G) * SYD 
     *      + (DAB(KK,NEL)*(D-G)-AB*DG(KK,NEL)) * SUB 
     *      +  AB*(D-G)*DSUB(KK,NEL) 
     *      + (DAB1(KK,NEL)*(D-G-DB)-AB1*DG(KK,NEL)) * SUB1
     *      +  AB1*(D-G-DB)*DSUB1(KK,NEL) 
     *      + (-AS*DH(KK,NEL)+DAS(KK,NEL)*(D-H)) * (D+H-2.0*G)*SUS/D
     *      +  AS*(D-H) * ( (DH(KK,NEL)-2.0*DG(KK,NEL))*SUS+
     *                      (D+H-2.0*G)*DSUSL(KK,NEL)   )/D
     *      + (AS*DH(KK,NEL)+DAS(KK,NEL)*H)
     *               *((2.0*H-3.0*G)*SUS-(H-3.0*G)*SYS)/(3.0*D)
     *      + ( (2.0*DH(KK,NEL)-3.0*DG(KK,NEL))*SUS+
     *          (2.0*H-3.0*G)*DSUSL(KK,NEL)-
     *          (DH(KK,NEL)-3.0*DG(KK,NEL))*SYS  ) * AS*H/(3.0*D) 
        IMM=IM+K
c       WRITE(666,*)'XI nø',IMM,'  d(Uhogg)/dxi=',DULT(IMM)	     	  
  14    CONTINUE

C       WRITE(666,*)'DH    =',(DH(I,NEL),I=1,NBRXI)
C       WRITE(666,*)'DG    =',(DG(I,NEL),I=1,NBRXI)
        IM=IM+NBRXI
  13    CONTINUE

      ENDIF
      
      WRITE(66,*)'         H   = ',H
      WRITE(66,*)'         G   = ',G
      WRITE(66,*)'         Hb  = ',Hb

C     SAUVETAGE SUR DISQUE,  File 204 
C     -------------------------------
C     Hogging: Uhog > 0 et Uhog (max) > 0
C     Restriction : C(x)= UHOG(max)/Uhog(ult) < 1
C        dC(x)/dxi= - C(x)/Uhog(ult) * dUhog(ult)/dxi
      VNOM2='ULT. HOGGING'
	C=UHOGM/UHOG
      CM=1.
	DO 49 I=1,NTOT
        DULT(I)= - (C/UHOG) * DULT(I)
   49 CONTINUE
	WRITE(303) C,CM,VNOM2			!extension neto
	WRITE(303) (DULT(I),I=1,NTOT)	!extension neto

c     WRITE(666,*) 
c	WRITE(666,*) 'C,CM,VNOM2=',C,CM,'  ',VNOM2
c     WRITE(666,*) 'DC dans USHULL après write 204:'	
c     WRITE(666,'(9(E10.3,1X))') (DULT(I),I=1,NTOT)  		

c23456789012345678901234567890123456789012345678901234567890123456789012


C 7.0 OUTPUTS
C     --------
      USAGr=USAG/FPBM
      UHOGr=UHOG/FPBM
      
      WRITE(666,816) USAG,USAGr,UHOG,UHOGr
      WRITE(66, 816) USAG,USAGr,UHOG,UHOGr
816   FORMAT(/'ULTIMATE BENDING MOMENT (N.m):'/30(1H-)/
     * '- FOR SAGGING:','     Mult. =',E15.7,'     Mult./Mp  =',F10.5/
     * '- FOR HOGGING:','     Mult. =',E15.7,'     Mult./Mp  =',F10.5)

      RETURN
      END
