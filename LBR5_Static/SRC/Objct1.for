      SUBROUTINE OBJCT1(NEL,NETO,N,NN,NXI,NBRXI,OBJ,DFCT,WIDTH,		!restri cout
     *                  IPRINT,SPEC,ITYPE,ISECT,CORRO)				!f�vrier 2004  +!corrosion

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION DFCT(N),NXI(NBRXI)
      DIMENSION ITYPE(1),ISECT(1)								!f�vrier 2004
      DIMENSION CORRO(NETO,3)			!corrosion
      COMMON/PY/PI

C     ENT+COST,HULL,BO1,BO2,HUGUES,OBJEC
C      COMMON/MAT/   E(30),ETA(30),SIGY(30),SIGM(30),SPEC(30)

C     COST(ENT),OPTI,OBJEC

      COMMON/COUT/ Poids,SPoids,
     *        Fmat,Fsou,FMdO,Fmat11,Fmat22,Fmat33,Fsou11,Fsou22,
     *        Fsou33,FMdO11,FMdO22,FMdO33,FMdO44,FMdO55,FMdO66,FMdO77,
     *        REND,EQP,Dref,DrefX,DrefY,C1,C2,C3,DC1,DW2,DW3,P10,DP10,
     *        P4,P5,DP4,DP5,P9X,P9Y,DP9X,DP9Y,BER,BET,P6,P7,C8,DC8,COUT,
     *        ICOUT,IALR,IALT
C **********************************************************************
C     Calcul de la fonction objectif COUT (OBJ) apr�s la subr. Conlin
C
c     16-11-2001 : Sur-�paisseurs de corrosion
c     11-01-2002 : Nouveaux param�tres Co�t (P9X,P9Y,C2,C3,DW2,DW3)
C     23-05-2003 : Correction ds le calcul des sensibilit�s (Errata)
C
C     Version du : 18-11-01   			          Cr�er 18-5-98
c
c     Modif: 25-5-2003
C
C ******************************************************************************
C  Donn�es relatives � la Fonction co�t (FONCTION OBJECTIF)                       
C      ---------------------------------------------------------                        
C     -ICOUT  (0 = Fct Obj. Poids ; 1 = Fct Obj. Co�t)    (INTEGER) 
c
C     - REND, EQP                                         (REAL) 
C       REND  = rendement global du chantier (0 < REND < 10,0)
c               En principe REND=1,0 . 
C               On peut faire varier REND de mani�re � �valuer l'impact d'une variation
C               du rendement sur la fonction objectif mais aussi sur l'optimum
C               (analyse de sensibilit�).
C               REND porte seulement sur la MAIN-D'OEUVRE (P4,P5,P6,P7,P9 et P10).
C       EQP  (0,01 < EQF < 0,1 t/h-h)
c             = Equivalent poids de la main d'oeuvre (t/h-h)
c             = Co�t MdO (Euro/h-h)  / Cout Mat. (Euro/t)  
C             = 0.005 � 0.015 dans les pays � MdO bon march� (100 FB/h-h)
C             = 0.05 � 0.08   dans les pays occidentaux      (1500 � 2000 FB/h-h)
C          
C     - Dref(bord�), DrefX(lisse) et DrefY(Cadre)  (en m)    
C         Dref = Epaisseur de r�f�rence correspondant au co�t C1(Euro/kg) des t�les,
c                    (valeur recommand�e : 0,01m c�d 10 mm)    
C         DrefX= Epaisseur de r�f�rence (ame des lisses) correspondant au co�t C2(Euro/kg)
c                des lisses
C         DrefY= Epaisseur de r�f�rence (ame des cadres) correspondant au co�t C3(Euro/kg) 
c                des cadres,
C               
C               Pour les charges de travail P10, l'�paisseur de r�f�rence  "Dref" 
C               correspond � la t�le du bord�.
C               Pour les charges de travail P4,P5 et P9X, P9Y, et
c               pour le co�t C8(Euro/m) du soudage (�nergie + consommables) cette �paisseur
C               de r�f�rence est DrefX (ou DrefY) qui correspond � la t�le des �mes des membrures
c               que l'on soude, soit sur le bord� (P4 et P5), soit sur la semelle (P9X, P9Y).
C                        
C     - Prix Mat�r.: C1(tole,Euro/kg), C2(long,Euro/kg), C3(trans,Euro/kg), DC1(tole)  (REAL)  
C             C1  = Prix au kg des t�les dont l'�paisseur est "Dref"  (Euro/kg)
C                 = 23 FB/kg  pour acier AE 235) et 26 FB/kg pour AE355)
C             C2  = Prix au kg des mat�riaux des longitudinaux (lisses) dont l'�paisseur 
c                   de l'�me est "Dref" (et pas DrefX)
C                 = Variation en % par rapport au prix C1 pour des membrures longitudinales
C                   constitu�es de profil�s standards (corni�res,T, bulbes, ..) (en %) 
C                 = -10% selon van Frachem (c.�.d 0.90*C1)
C                 = +25% au Bengladesh selon Rahman (c.�.d 1.25*C1).
C             C3  = Prix au kg des mat�riaux des cadres 
c                   dont l'�paisseur de l'�me est "Dref" (et pas DrefY)
C                 = Variation en % par rapport au prix C1 pour des membrures transversales
C                   constitu�es de profil�s standards (corni�res,T, bulbes, ..) (en %)
c                 = -10% selon van Frachem (c.�.d 0.90*C1)
C                 = +40% au Bengladesh selon Rahman (c.�.d 1.40*C1).
C             DC1 = Variation en % du prix C1 entre "Dref" et "Dref + 1mm" (%/mm)
C                 = -0.6% selon van Frachem (c.�.d -0.006)
C                        
C     - MdO Bord�  : P10(h-h/m),     PC10(variation par mm)                   (REAL)  
C             P10 = MdO n�cessaire pour constituer le bord� � partir de t�les d'�paisseur
C                   "Dref" (soudure bout � bout).
C                 = 1.00 h-h/m2 selon van Frachem (bord� constitu� de t�le de 6x3m)
C                 = 0.35 h-h/m2 (selon Winkle et Baird)
C             DP10= Variation en % de la MdO P10 si on passe d'une �paisseur "Dref" �
C                   "Dref + 1mm" (%/mm)
C                 = +7.0% selon van Frachem
C                        
C     - MdO Assembl: P4(Long,h-h/m), P5(Trans,h-h/m), DP4(long),  DP5(trans)  (REAL)                  
C             P4  = MdO n�cessaire pour souder une membrure long. sur le bord�
C                   (si �paisseur �me membrure = "Dref" ).
C                 = +1.65 h-h/m pour Dref=10mm et +0.9 h-h/m pour Dref=5mm selon van Frachem
C                 = +1.10 h-h/m selon Rahman (Bengladesh) 
c                 = +0.90 h-h/m selon Winkle et Baird 
C             DP4 = Variation en % de la MdO P4 si on passe d'une �paisseur "Dref" �
C                   "Dref + 1mm" (%/mm)
C                 = +10% selon van Frachem
C             P5  = MdO n�cessaire pour souder une membrure transv. sur le bord�
C                   (si �paisseur �me membrure = "Dref" ).
C                 = +1.65 h-h/m pour Dref=10mm et +0.9 h-h/m pour Dref=5mm selon van Frachem
C                 = +1.25 h-h/m selon Rahman (Bengladesh) 
c                 = +0.90 h-h/m selon Winkle et Baird 
C             DP5 = Variation en % de la MdO P5 si on passe d'une �paisseur "Dref" �
C                   "Dref + 1mm" (%/mm)
C                 = +10% selon van Frachem 
c
c       NB: TEMPS DE SOUDAGE : DONNEES DU CEWAC-ULg (mai 1998)
C           Pour la soudure semi-automatique (temps de marche 30%): 
C              - si Dref = 5 mm            - si Dref = 10mm
c			 P=4min/m et DP=32%           P=20min/m et DP=25%
c           Pour la soudure manuelle (� la baguette), (temps de marche 25%):
C              - si Dref = 5 mm            - si Dref = 10mm
c			 P=16min/m et DP=29%           P=62min/m et DP=20%
C                        
C     - MdO Membr. :  P9X et P9Y (h-h/m),  DP9X et DP9Y    (4 REAL)
C             Main d'oeuvre n�cessaire � la construction des membrures (profil�s 
C             reconstitu�s en atelier)
c
C             P9X (P9Y)= MdO n�cessaire pour souder l'�me sur la semelle
C                   (si �paisseur �me membrure = "DrefX" pour lisse et "DrefY" pour cadre).
C                 = 1.50 (h-h/m) selon Rahman  
C                 = 0.60 (h-h/m) selon Winkle et Baird 
C                 = 0.50 (h-h/m) selon van Frachem (Dref = 10mm) 
C             DP9X= Variation en % de la MdO P9 si on passe d'une �paisseur "DrefX" �
C                   "DrefX + 1mm" (%/mm)
C             DP9Y= Variation en % de la MdO P9 si on passe d'une �paisseur "DrefY" �
C                   "DrefY + 1mm" (%/mm)
C                 = 10% selon van Frachem (Dref = 10mm)
c
C     - MdO JOINTS : P6(intersect,h-h/joint),P7(gousset,h-h/joint)  
c                      B�ta  R(long.), B�ta  T(transv)                      (REAL)  
C             Main d'oeuvre n�cessaire pour les intersections (P6) et les goussets (P7).
c
C             P6  = Main d'oeuvre n�cessaire pour une intersection (h-h/intersection)
C                 = 0.60 (h-h/m) selon Rahman  
C                 = 0.30 (h-h/m) selon Winkle et Baird 
C             P7  = Main d'oeuvre n�cessaire pour un gousset (h-h/gousset)
C                   Le nombre de gousset = BetaR x BetaT
C               avec BetaR = le nbre relatif de longitudinaux qui ont un gousset 
C                            (0 < BetaR < 1)
C                    BetaT = le nbre relatif de transversaux qui ont un gousset 
C                            (0 < BetaT < 1)
C                 = 1.15 (h-h/m) selon Rahman  
C                 = 0.30 (h-h/m) selon Winkle et Baird 
C                 
C     - Prix Soud. : C8(Euro/m), DC8(variation par mm), 
c                    Alpha R(long.), Alpha T(transv),                 (2 REAL, 2 Int)                  
C             C8  = Prix au m�tre courant de l'�nergie et des consommables pour souder
C                   une t�le d'�me d'�paisseur "Drefx" si lisse et "DrefY" si cadre
C                 = 54 FB/m selon Rahman (0.90 h-h/m avec 1 h-h=60 FB au Bengladesh)
C                 = 40 FB/m selon le CEWAC pour DrefX (DrefY) =10mm (semi automatique ou manuelle)
C                 = 10 FB/m selon le CEWAC pour DrefX (DrefY) = 5mm (semi automatique ou manuelle)
C             DC8 = Variation en % du prix C8 si on passe de "Dref" � "Dref + 1mm" (%/mm)
C                 = 20% entre 3 et 7mm  : DrefX (DrefY) =  5mm (cfr donn�es du CEWAC, mai 1998)     
C                 = 27% entre 8 et 15 mm: DrefX (DrefY) = 10mm (cfr donn�es du CEWAC, mai 1998)     
C                 = 10% si la variation d'�paisseur est importante     
C             Alpha R = 1 : les membrures longitudinales sont des profil�s standards 
C             Alpha T = 1 : les membrures transversales  sont des profil�s standards 
C             Alpha R = 0 : les membrures longitudinales sont des profil�s reconstitu�s 
C             Alpha T = 0 : les membrures transversales  sont des profil�s reconstitu�s 
c             Note: Par Dref (pour C8), il faut entendre DrefX ou DrefY selon le type de renforts consid�r�s
C
C ****************************************************************************************
c
C LES VARIABLES DE CONCEPTION
C ---------------------------
C	1	DELTA =  �paisseur du bordage
C	2	HYA   =  hauteur de l'�me des aiguilles
C	3	DYA   =  �paisseur de l'�me des aiguilles
C	4	WYA   =  largeur des semelles des aiguilles
C	5	EPSA  =  entredistance entre aiguilles
C	6	HXR   =  hauteur de l'�me des raidisseurs
C	7	DXR   =  �paisseur de l'�me des raidisseurs
C	8	WXR   =  largeur des semelles des raidisseurs
C	9	EPSR  =  entredistance entre raidisseurs
c
C Variables Associ�es :
C ---------------------
C	 	TYA = �paisseur semelle aiguilles
C     	TXR = �paisseur semelle raidisseurs 
c
C **************************************************************************************
	
	READ(302) KSA,KSR,KSE,EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,	!extension neto
     *		  HXR,DXR,WXR,TXR,PHIL,Q,EPAIS					!f�vrier 2004

C     DCOR = epaisseur de corrosion pour bord�
c	IF(NEL.EQ.1) THEN
c	  REWIND 57
c	  READ(57,'(////,A1)') Abidon
c	  READ(57,*) IPAN,DCOR,DCOR2,DCOR3
c	ELSE
c	  READ(57,*) IPAN,DCOR,DCOR2,DCOR3
c	ENDIF

	DCOR  = CORRO(NEL,1)							 !corrosion
	DCOR2 = CORRO(NEL,2)							 !corrosion
	DCOR3 = CORRO(NEL,3)							 !corrosion


	IF(ISECT(1).EQ.3) THEN			!�me double T entree=1/2 �me		!f�vrier 2004
	HYB=2*HYA															!f�vrier 2004
	ENDIF																!f�vrier 2004

	IF(ITYPE(1).NE.5) THEN												!f�vrier 2004
	DELTA= DELTA+DCOR
	ELSE																!f�vrier 2004
	DELTA =0.000														!f�vrier 2004
	ENDIF																!f�vrier 2004
c	DCOR2   ! epaisseur de corrosion pour cadres
	DYA  = DYA +DCOR2
	TYA  = TYA +DCOR2
	EPAIS = EPAIS +DCOR2							!f�vrier 2004
c	DCOR3   ! epaisseur de corrosion pour lisses
	DXR  = DXR +DCOR3
	TXR  = TXR +DCOR3

c      IF(IPRINT.GE.1) THEN
c        WRITE(666,*) 'dans OBJCT1,  NBRXI = ',NBRXI
c        WRITE(666,*) 'Bord  = ',DELTA
c        WRITE(666,*) 'Cadre = ',HYA,DYA,WYA,TYA,'   EPSA=',EPSA
c        WRITE(666,*) 'Raid  = ',HXR,DXR,WXR,TXR,'   EPSR=',EPSR
c 	ENDIF

C -------------------------------------------------------------
C     FCP = Poids = bordage + ames + semelles  (du panneau NEL)
	IF(NEL.EQ.1) POIDS=0.

      TEMP = PHIL * Q * WIDTH * PI/180.
	DENS = SPEC/9.81

	IF(ITYPE(1).NE.5) THEN						 !plaque			!f�vrier 2004
      FCP= DELTA 
     *  +  (1.0+DW3)*( DYA * HYA + TYA* WYA ) /EPSA
     *  +  (1.0+DW2)*( DXR * HXR + TXR* WXR ) /EPSR
 	ELSE										 !epontille			!f�vrier 2004
	  IF(ISECT(1).EQ.3)THEN											!f�vrier 2004
      FCP = DELTA +													!f�vrier 2004
c     *      (1.0+DW2)*(DXR*HXR+TXR*WXR)/EPSR +						!+++13.12.05
     *      (1.0+DW3)*(DYA*HYB+2*TYA*WYA)/EPSA    					!f�vrier 2004
	  ELSEIF(ISECT(1).EQ.1)THEN										!f�vrier 2004
	FCP = DELTA +													!f�vrier 2004
c     *      (1.0+DW2)*(DXR*HXR+TXR*WXR)/EPSR +						!+++13.12.05
     *      (1.0+DW3)*(PI*(HYA*HYA-(HYA-2*EPAIS)**2))					!f�vrier 2004
     *	  	/(4*EPSA)												!f�vrier 2004
	  ELSEIF(ISECT(1).EQ.2) THEN									!f�vrier 2004
      FCP = DELTA +													!f�vrier 2004
c     *      (1.0+DW2)*(DXR*HXR+TXR*WXR)/EPSR +						!+++13.12.05
     *      (1.0+DW3)*(HYA*HYA-(HYA-2*EPAIS)**2)/EPSA    				!20.02.04
	  ENDIF
	ENDIF															!f�vrier 2004
      FCP=  FCP * TEMP * SPEC  ! poids du panneau (borde, raid et cadre only)
	POIDS=POIDS+ FCP
C -------------------------------------------------------------
C     Fmat= Co�t des mat�riaux (acier)
C     Fsou= Co�t du soudage (energie + consommables)
C     FMdO= Co�t Main d'oeuvre
C     FCT = Fmat + Fsou + FMdO  (d'un panneau NEL)
C     OBJ = Co�t total          (de la structure complete)

	IF(ITYPE(1).NE.5) THEN									!f�vrier 2004
      Fmat = TEMP * DENS * 
     *  (C1 *(1.+(DELTA-Dref)*1000.*DC1) * DELTA 
     * + C2 *(1.+(DXR  -Dref)*1000.*DC1) * (1.0+DW2)   ! Dref et pas DrefX
     *                                   * (DXR*HXR+TXR*WXR)/EPSR 
     * + C3 *(1.+(DYA  -Dref)*1000.*DC1) * (1.0+DW3)   ! Dref et pas DrefY
     *                                   * (DYA*HYA+TYA*WYA)/EPSA ) 

      Fsou = TEMP * C8 * 
     *    ( (1.+(DXR-DrefX)*1000.*DC8)*(2.-IALR)/EPSR
     *     +(1.+(DYA-DrefY)*1000.*DC8)*(2.-IALT)/EPSA )

      FMdO = TEMP * REND * EQP * 1000. * C1 *  
     *    (  ( P4 *(1.+(DXR-DrefX)*1000.*DP4 ) + 
     *         P9X*(1.+(DXR-DrefX)*1000.*DP9X)   )/EPSR
     *     + ( P5 *(1.+(DYA-DrefY)*1000.*DP5 ) + 
     *         P9Y*(1.+(DYA-DrefY)*1000.*DP9Y)   )/EPSA ! Errata Mars 2004  --> P9Y au lieu de P9X
     *     + (P6+BER*BET*P7) /(EPSA*EPSR)  
     *     +  P10*(1.+(DELTA-Dref)*1000.*DP10)      )
	ELSE							!epontilles				!f�vrier 2004
	IF(ISECT(1).EQ.3) THEN
	Fmat=TEMP * DENS * C3 *(1.+(DYA  -Dref)*1000.*DC1) * (1.0+DW3)   !20.02.04
     *                      * ((2*DYA*HYA+2*TYA*WYA)/EPSA )			!20.02.04
	ELSEIF(ISECT(1).EQ.1) THEN
	Fmat=TEMP * DENS * C3 *(1.+(EPAIS  -Dref)*1000.*DC1) * (1.0+DW3)   !20.02.04
     *                 * ((PI*(HYA*HYA-(HYA-2*EPAIS)**2))/(4*EPSA ))   	!20.02.04
	ELSEIF(ISECT(1).EQ.2) THEN
	Fmat=TEMP * DENS * C3 *(1.+(EPAIS  -Dref)*1000.*DC1) * (1.0+DW3)   !20.02.04
     *                 * ((HYA*HYA-(HYA-2*EPAIS)**2)/EPSA )				!20.02.04
	ENDIF													!12.05.04
	Fsou=0.001												!f�vrier 2004
	FMdo=0.001												!f�vrier 2004
	ENDIF												!20.02.04
      FCT = Fmat + Fsou + FMdO
      OBJ = OBJ + FCT


	IF (NEL==1) THEN
       WRITE(666,*) 
	 WRITE(666,*)'*** COST Objective Function (Subr. OBJCT1)'
	 WRITE(666,*)'    --------------------------------------'
	 WRITE(666,*)'Panneau/Panel (with additional corrosion thickness)'
	ENDIF
      WRITE(666,42) NEL,Fmat,Fsou,FMdO
   42 FORMAT(I3,4x,'Fmat,Fsoud,FMdO=',3(E14.7,1x),'Euro ($ or ...)')
      WRITE(666,43) FCT,FCP
   43 FORMAT(8x,'COST= ',E14.7,'   POIDS= ',E14.7,' N')


      IF (NEL==NETO) THEN
        WRITE(666,*) 
        WRITE(666,*)' TOTAL for the structure (with Gross Thickness)'
        WRITE(666,*)' ----------------------------------------------'
        WRITE(666,*)'  COUT  - COST   = ',OBJ  ,' Euro ($ or...)'
        WRITE(666,*)'  POIDS - WEIGHT = ',POIDS,' N'
	ENDIF

C     TESTS (cout positif ??)
c     -----------------------
	IF ((Fmat.LE.0.0).OR.(Fsou.LE.0.0).OR.(FMdO.LE.0.0)) THEN
       WRITE(666,*) 
       WRITE(666,*) ' CERTAINS COUTS SONT NEGATIFS'
       WRITE(666,*) ' FEW COSTS ARE NEGATIVE!!'
       WRITE(666,*) 
       WRITE(666,*) ' There are some problems in the unitary costs.'
       WRITE(666,*) ' Probably in the variation costs/mm of plate'
       WRITE(*,*) 
       WRITE(*,*) ' CERTAINS COUTS SONT NEGATIFS'
       WRITE(*,*) ' FEW COSTS ARE NEGATIVE!!'
       WRITE(*,*) 
       WRITE(*,*) ' There is some problems in your unitary costs'
       WRITE(*,*) ' Probably in the variation costs/mm of plate'
       WRITE(*,*) 
       WRITE(*,*) ' We will continue but you should check the results.'
       WRITE(*,*) '   Check your ouput file OPT-**.**'
       WRITE(29,*)																!bug
       WRITE(29,*) ' CERTAINS COUTS SONT NEGATIFS'								!bug
       WRITE(29,*) ' FEW COSTS ARE NEGATIVE!!'									!bug
       WRITE(29,*)																!bug
       WRITE(29,*) ' There is some problems in your unitary costs'				!bug
       WRITE(29,*) ' Probably in the variation costs/mm of plate'					!bug
       WRITE(29,*)																!bug
       WRITE(29,*) ' We will continue but you should check the results.'			!bug
       WRITE(29,*) '   Check your ouput file OPT-**.**'							!bug
	 PAUSE 'OK?'
	ENDIF
c234567890123456789012345678901234567890123456789012345678901234567890123456789
C     CALCUL DES SENSIBILITES
c     ------------------------
	DO 101 I=1,NBRXI
	II=NN+I

	IF(NXI(I).EQ.1) THEN
       DFCT(II)=TEMP*C1 * (  (1.+(2.*DELTA-Dref)*1000.*DC1)*DENS
     *                      + REND*EQP*1000.*P10*DP10*1000.    ) 
     
	ELSE IF (NXI(I).EQ.2) THEN
       DFCT(II)= TEMP * DENS * (1.0+(DYA-Dref)*1000.*DC1)
     *                       * (1.+DW3) *C3   *DYA/EPSA
	ELSE IF (NXI(I).EQ.3) THEN
       DFCT(II)= (TEMP/EPSA) * 
     *    (  (1.+(DYA-Dref)*1000.*DC1)*(1.0+DW3)*HYA*C3*DENS   +
     *       (1.0+DW3)*(DYA*HYA+TYA*WYA)*1000.*DC1*C3*DENS     +  !23-05-2003 
     *       1000.0*C8*DC8*(2.-IALT)     +
     *       REND*EQP*1000.0*C1*1000.0*(P5*DP5+P9Y*DP9Y)  )
	ELSE IF (NXI(I).EQ.4) THEN
       DFCT(II)= TEMP  * DENS * (1.0+(DYA-Dref)*1000.0*DC1)
     *                        * (1.0+DW3)  *  C3 * TYA/EPSA
	ELSE IF (NXI(I).EQ.5) THEN
       DFCT(II)= -(TEMP/(EPSA*EPSA)) * 
     *  ( C3*DENS*(1.0+(DYA-Dref)*1000.0*DC1)*(1.+DW3)*(DYA*HYA+TYA*WYA)
     *   + (1.+(DYA-DrefY)*1000.*DC8)*C8*(2.-IALT) 
     *   + REND*EQP*1000.*C1*
     *            (   P5 *(1.+(DYA-Dref)*1000.*DP5)  
     *              + P9Y*(1.+(DYA-Dref)*1000.*DP9Y) 
     *              + (P6+BER*BET*P7)/EPSR           )        )
	
	ELSE IF (NXI(I).EQ.6) THEN
       DFCT(II)= TEMP * DENS * (1.0+(DXR-Dref)*1000.*DC1)
     *                       * (1.0+DW2) * C2 * DXR/EPSR
	ELSE IF (NXI(I).EQ.7) THEN
       DFCT(II)= (TEMP/EPSR) * 
     *    (  (1.+(DXR-Dref)*1000.*DC1)*(1.0+DW2)*HXR*C2*DENS   +   
     *       (1.0+DW2)*(DXR*HXR+TXR*WXR)*1000.*DC1*C2*DENS     +  !23-05-2003 
     *       1000.0* C8*DC8*(2.-IALR)    + 
     *       REND*EQP*1000.0*C1*1000.0*(P4*DP4+P9X*DP9X)  )
	ELSE IF (NXI(I).EQ.8) THEN
       DFCT(II)= TEMP  * DENS * (1.+(DXR-Dref)*1000.0*DC1) 
     *                        * (1.0+DW2)  * C2 * TXR/EPSR
	ELSE IF (NXI(I).EQ.9) THEN
       DFCT(II)= -(TEMP/(EPSR*EPSR)) * 
     *  ( C2*DENS*(1.0+(DXR-Dref)*1000.0*DC1)*(1.+DW2)*(DXR*HXR+TXR*WXR)
     *   + (1.+(DXR-DrefX)*1000.*DC8)*C8*(2.-IALR) 
     *   + REND*EQP*1000.*C1*
     *            (   P4 *(1.+(DXR-Dref)*1000.*DP4)  
     *              + P9X*(1.+(DXR-Dref)*1000.*DP9X) 
     *              + (P6+BER*BET*P7)/EPSA           )        )
	ENDIF

c	WRITE(666,*) '   df/dx(',II,')=',DFCT(II)
  101 CONTINUE
  
      RETURN
      END
