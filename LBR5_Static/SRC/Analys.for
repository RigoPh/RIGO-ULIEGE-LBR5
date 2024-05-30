      SUBROUTINE ANALYS(NETO,INDAIG,INDRAI,IMPR2,AN,IAN,ITYPE,ISECT)   ! février 2004               	

C ********************************************************************************                                                        
C     Subroutine de relecture (FILE 205) des valeurs extrêmes des déplacements
C     et des contraintes sauvées dans les Subr ECRI et ECRI3 (subr EXTREM)

C     Modifié : 4-2-2004                           Créer: 13-3-95 (Ph. Rigo)
C
C      20-11-2000  Impression des resultats en write(67,..):fichier résultat SOL2
C       4-12-2002  19(22) au lieu de 12 variables fournies.
c	 04-02-2004  ajout variables pour épontilles.

C *********************************************************************************
      IMPLICIT REAL *8 (A-H,O-Z) 
	
	DIMENSION AN(1),IAN(1),ISECT(1),ITYPE(1)		! février 2004                                      

c     AN  vecteur (REAL) contenant les valeuts extremes
c        taille de AN :  33 functions x 2 valeurs x 100 panneaux		!février 2004
c                      = 33x2x100										!février 2004
      I1 =1  
	I2 =I1 + 2*NETO
	I3 =I2 + 2*NETO
	I4 =I3 + 2*NETO
	I5 =I4 + 2*NETO
	I6 =I5 + 2*NETO
	I7 =I6 + 2*NETO
	I8 =I7 + 2*NETO
	I9 =I8 + 2*NETO
	I10=I9 + 2*NETO
	I11=I10+ 2*NETO
	I12=I11+ 2*NETO
	I13=I12+ 2*NETO
	I14=I13+ 2*NETO
	I15=I14+ 2*NETO
	I16=I15+ 2*NETO
	I17=I16+ 2*NETO
	I18=I17+ 2*NETO
	I19=I18+ 2*NETO
	I20=I19+ 2*NETO
	I21=I20+ 2*NETO					!février 2004				
	I22=I21+ 2*NETO					!février 2004		
	I23=I22+ 2*NETO					!février 2004	
	I24 =I23 + 2*NETO				!février 2004		
	I25 =I24 + 2*NETO				!février 2004		
	I26 =I25 + 2*NETO				!février 2004			
	I27 =I26 + 2*NETO				!février 2004		
	I28 =I27 + 2*NETO				!février 2004	
c	I29 =I28 + 2*NETO				!février 2004		
c	I30 =I29 + 2*NETO				!février 2004		
c	I31 =I30 + 2*NETO				!février 2004

c     IAN  vecteur (INTEGER*4) contenant les coord. des valeurs extremes
c        taille de IAN :  33 functions x 4 coord x 100 panneaux		!février 2004
c                      = 33x4x100										!février 2004
      J1 =1
	J2 =J1 + 4*NETO
	J3 =J2 + 4*NETO
	J4 =J3 + 4*NETO
	J5 =J4 + 4*NETO
	J6 =J5 + 4*NETO
	J7 =J6 + 4*NETO
	J8 =J7 + 4*NETO
	J9 =J8 + 4*NETO
	J10=J9 + 4*NETO
	J11=J10+ 4*NETO
	J12=J11+ 4*NETO
	J13=J12+ 4*NETO
	J14=J13+ 4*NETO
	J15=J14+ 4*NETO
	J16=J15+ 4*NETO
	J17=J16+ 4*NETO
	J18=J17+ 4*NETO
	J19=J18+ 4*NETO
	J20=J19+ 4*NETO
	J21=J20+ 4*NETO							!février 2004
	J22=J21+ 4*NETO							!février 2004
	J23=J22+ 4*NETO							!février 2004
	J24 =J23 + 4*NETO						!février 2004
	J25 =J24 + 4*NETO						!février 2004
	J26 =J25 + 4*NETO						!février 2004
	J27 =J26 + 4*NETO						!février 2004
	J28 =J27 + 4*NETO						!février 2004
c	J29 =J28 + 4*NETO						!février 2004
c	J30 =J29 + 4*NETO						!février 2004
c	J31 =J30 + 4*NETO						!février 2004

C     Le 4-12-2002, uniquement 19 valeurs sur les 22 possibles sont utilisées.
c     Il reste donc 3 positions de réserve.

      CALL ANAL(NETO,INDAIG,INDRAI,IMPR2,ITYPE,ISECT,					!février 2004	
     *          AN(I1),AN(I2),AN(I3),   IAN(J1), IAN(J2), IAN(J3),
     *          AN(I4),AN(I5),AN(I6),   IAN(J4), IAN(J5), IAN(J6),
     *          AN(I7),AN(I8),AN(I9),   IAN(J7), IAN(J8), IAN(J9),
     *          AN(I10),AN(I11),        IAN(J10),IAN(J11),
     *          AN(I12),AN(I13),        IAN(J12),IAN(J13),
     *          AN(I14),AN(I15),AN(I16),IAN(J14),IAN(J15),IAN(J16),
     *          AN(I17),AN(I18),AN(I19),IAN(J17),IAN(J18),IAN(J19),
     *          AN(I20),AN(I21),        IAN(J20), IAN(J21),			!février 2004
     *          AN(I22),AN(I23),        IAN(J22), IAN(J23),			!février 2004
     *          AN(I24),AN(I25),        IAN(J24), IAN(J25),			!février 2004
     *          AN(I26),AN(I27),AN(I28),IAN(J26),IAN(J27),IAN(J28))	!février 2004


	RETURN
	END
	
C ****************************************************************

      SUBROUTINE ANAL(NETO,INDAIG,INDRAI,IMPR2,ITYPE,ISECT, ! épontille         !février 2004         	
     *                V,U,W,IV,IU,IW,                       ! déplacements
     *                SBORX,SBORY,TAU,ISBORX,ISBORY,ITAU,   ! Sx, Sy et tau
     *                SBOR,SBOR1,SBOR2,ISBOR,ISBOR1,ISBOR2, ! Scomp(z=0),Z=+d/2 et -d/2
     *                SJAS,TJAS,ISJAS,ITJAS,				  !	cadre: Sig et tau JAS	
     *                SJAB,TJAB,ISJAB,ITJAB,			      !	cadre: Sig et tau JAB	
     *                AJAS,AJAB,ASEM,IAJAS,IAJAB,IASEM,     !	cadre: comparaison JAS, JAB et SEM
     *  	            RJAS,RJAB,RSEM,IRJAS,IRJAB,IRSEM,     !	Raid: comparaison
     *                SBORXI,SBORYI,ISBORXI,ISBORYI,		  ! Sx, Sy et tau							!février 2004
     *                SJASi,TJASI,ISJASi,ITJASI,			  !	cadre: Sig et tau JAS					!février 2004
     *                SJABI,TJABI,ISJABI,ITJABI,		      !	cadre: Sig et tau JAB					!février 2004
     *                AJASI,AJABI,ASEMI,IAJASI,IAJABI,IASEMI)   !	cadre: comparaison JAS, JAB et SEM	!février 2004

      IMPLICIT REAL *8 (A-H,O-Z) 
	                                       
      DIMENSION  V(NETO,2), U(NETO,2), W(NETO,2),                             
     *          IV(NETO,4),IU(NETO,4),IW(NETO,4),                             
     *       SBORX(NETO,2), SBORY(NETO,2),   TAU(NETO,2),
     *      ISBORX(NETO,4),ISBORY(NETO,4),  ITAU(NETO,4),					  
     *        SBOR(NETO,2), SBOR1(NETO,2), SBOR2(NETO,2),
     *       ISBOR(NETO,4),ISBOR1(NETO,4),ISBOR2(NETO,4),					  
     *  SJAS(NETO,2), SJAB(NETO,2), TJAS(NETO,2), TJAB(NETO,2), 
     * ISJAS(NETO,2),ISJAB(NETO,2),ITJAS(NETO,2),ITJAB(NETO,2), 
     *  AJAS(NETO,2), AJAB(NETO,2), ASEM(NETO,2),
     * IAJAS(NETO,4),IAJAB(NETO,4),IASEM(NETO,4),                     			
     *  RJAS(NETO,2), RJAB(NETO,2), RSEM(NETO,2),                   		
     * IRJAS(NETO,4),IRJAB(NETO,4),IRSEM(NETO,4),
     * ITYPE(NETO),ISECT(NETO)	!épontille				!février 2004
      DIMENSION                               ! caract pour épontille		!février 2004
     *       SBORXI(NETO,2), SBORYI(NETO,2),						
     *      ISBORXI(NETO,4),ISBORYI(NETO,4),						
     *  SJASi(NETO,2), SJABI(NETO,2), TJASI(NETO,2), TJABI(NETO,2), 
     * ISJASi(NETO,2),ISJABI(NETO,2),ITJASI(NETO,2),ITJABI(NETO,2), 
     *  AJASI(NETO,2), AJABI(NETO,2), ASEMI(NETO,2),		
     * IAJASI(NETO,4),IAJABI(NETO,4),IASEMI(NETO,4)                     			
      DIMENSION I4(150),I5(150),I6(150)									!extension neto
                          		
	COMMON/LANGUE/LANGUE
c      COMMON/BLOKA/BID(25000),
c     *              V,U,W,IV,IU,IW,SBOR,SBOR1,SBOR2,ISBOR,ISBOR1,ISBOR2,
c     *              AJAS,AJAB,ASEM,IAJAS,IAJAB,IASEM,    		
c     *  	          RJAS,RJAB,RSEM,IRJAS,IRJAB,IRSEM

C     Z(I,1) = (VMIN) VALEUR MINIMALE DE Z EN (X,Y)=(IXMIN,IYMIN) pour le panneau I     
C     Z(I,2) = (VMAX) VALEUR MAXIMALE DE Z EN (X,Y)=(IXMAX,IYMAX) pour le panneau I 
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c     VMIN	IXMIN	IYMIN		VMAX	IXMAX	IYMAX
C     Z(I,1)	IZ(I,1)	IZ(I,2)		Z(I,2)	IZ(I,3)	IZ(I,4)
c ------------------------------------------------------------------------------------
	NELO= 0    ! = nombre total d'épontilles dans la structure		!février 2004
	NETA = 0   ! = nombre total de plaques dans la structure		!février 2004
	I1 = 0															!février 2004
	I2 = 0															!février 2004
	I3 = 0															!février 2004
	DO 2 I=1,NETO													!février 2004
	IF(ITYPE(I).NE.5) THEN                                          !février 2004        
	 
	 I1 = I1 + 1													!février 2004

c      DO 1 I=1,NETO 
c	 WRITE(67,*) 'NEL=',I                                                    
  20   READ(304,*,END=1) IVAR   !saved in ECRI	!extension neto
       IF(IVAR.EQ.99) GOTO 30					
c      WRITE(67,*)'ivar=',IVAR
      
      IF(IVAR.EQ.1) THEN											!1  V 
      READ(304,*)V(I,1),IV(I,1),IV(I,2),V(I,2),IV(I,3),IV(I,4)	!extension neto
c      WRITE(67,*)'V',V(I,1),IV(I,1),IV(I,2),V(I,2),IV(I,3),IV(I,4)     
      ELSE IF (IVAR.EQ.2)  THEN									!2  U
      READ(304,*)U(I,1),IU(I,1),IU(I,2),U(I,2),IU(I,3),IU(I,4)	!extension neto
c      WRITE(67,*)'U',U(I,1),IU(I,1),IU(I,2),U(I,2),IU(I,3),IU(I,4)
      ELSE IF (IVAR.EQ.3)  THEN									!3  W
      READ(304,*)W(I,1),IW(I,1),IW(I,2),W(I,2),IW(I,3),IW(I,4)	!extension neto

      ELSE IF (IVAR.EQ.18)  THEN						!18   SX BORDAGE
      READ(304,*)SBORX(I1,1),ISBORX(I1,1),ISBORX(I1,2),			!extension neto
     *           SBORX(I1,2),ISBORX(I1,3),ISBORX(I1,4)
      ELSE IF (IVAR.EQ.17)  THEN						!17   Sy BORDAGE
      READ(304,*)SBORY(I1,1),ISBORY(I1,1),ISBORY(I1,2),	!extension neto
     *           SBORY(I1,2),ISBORY(I1,3),ISBORY(I1,4)
      ELSE IF (IVAR.EQ.19)  THEN						!19   tau BORDAGE
      READ(304,*)TAU(I1,1),ITAU(I1,1),ITAU(I1,2),			!extension neto
     *           TAU(I1,2),ITAU(I1,3),ITAU(I1,4)
      ELSE IF (IVAR.EQ.20)  THEN					    !20 SIGMA Comp BORDAGE
      READ(304,*)SBOR(I1,1),ISBOR(I1,1),ISBOR(I1,2),	!extension neto
     *           SBOR(I1,2),ISBOR(I1,3),ISBOR(I1,4)

      ELSE IF (IVAR.EQ.21) THEN						!21 Sy  JAS (cadre)
      READ(304,*)SJAS(I1,1),ISJAS(I1,1),ISJAS(I1,2),	!extension neto
     *           SJAS(I1,2),ISJAS(I1,3),ISJAS(I1,4)
      ELSE IF (IVAR.EQ.22) THEN						!22 Sy  JAB (cadre)
      READ(304,*)SJAB(I1,1),ISJAB(I1,1),ISJAB(I1,2),	!extension neto
     *           SJAB(I1,2),ISJAB(I1,3),ISJAB(I1,4)
      ELSE IF (IVAR.EQ.23) THEN						!23 TAU  JAS (cadre)
      READ(304,*)TJAS(I1,1),ITJAS(I1,1),ITJAS(I1,2),	!extension neto
     *           TJAS(I1,2),ITJAS(I1,3),ITJAS(I1,4)
      ELSE IF (IVAR.EQ.24) THEN						!24 TAU  JAB (cadre)
      READ(304,*)TJAB(I1,1),ITJAB(I1,1),ITJAB(I1,2),	!extension neto
     *           TJAB(I1,2),ITJAB(I1,3),ITJAB(I1,4)

      ELSE IF (IVAR.EQ.25) THEN						!25 Sig Comp JAS (cadre)
      READ(304,*)AJAS(I1,1),IAJAS(I1,1),IAJAS(I1,2),	!extension neto
     *           AJAS(I1,2),IAJAS(I1,3),IAJAS(I1,4)
      ELSE IF (IVAR.EQ.26) THEN						!26 Sig Comp JAB (cadre)
      READ(304,*)AJAB(I1,1),IAJAB(I1,1),IAJAB(I1,2),	!extension neto
     *           AJAB(I1,2),IAJAB(I1,3),IAJAB(I1,4)
      ELSE IF (IVAR.EQ.29) THEN						!29 Sig Comp sem (cadre)
      READ(304,*)ASEM(I1,1),IASEM(I1,1),IASEM(I1,2),	!extension neto
     *           ASEM(I1,2),IASEM(I1,3),IASEM(I1,4)
      
      ELSE IF (IVAR.EQ.36) THEN						!36 Sig Comp JAS (raid)
      READ(304,*)RJAS(I1,1),IRJAS(I1,1),IRJAS(I1,2),	!extension neto
     *           RJAS(I1,2),IRJAS(I1,3),IRJAS(I1,4)
      ELSE IF (IVAR.EQ.37) THEN						!37 Sig Comp JAB (raid)
      READ(304,*)RJAB(I1,1),IRJAB(I1,1),IRJAB(I1,2),	!extension neto
     *           RJAB(I1,2),IRJAB(I1,3),IRJAB(I1,4)
      ELSE IF (IVAR.EQ.38) THEN						!38 Sig Comp sem (raid)
      READ(304,*)RSEM(I1,1),IRSEM(I1,1),IRSEM(I1,2),	!extension neto
     *           RSEM(I1,2),IRSEM(I1,3),IRSEM(I1,4)
      ELSE
        WRITE(67,*)' ERROR1: Subr. ANALYS; IVAR=',IVAR,' est incorrecte'
        WRITE(*,*) ' ERROR1: Subr. ANALYS; IVAR=',IVAR,' est incorrecte'
        WRITE(29,*)'subr Analysis'													!bug
	  WRITE(29,*)' ERROR1: Subr. ANALYS; IVAR=',IVAR,' est incorrecte'				!bug

        PAUSE 'Stop'
        STOP
      END IF
      GOTO 20
	ELSEIF((ITYPE(I).EQ.5).AND.(ISECT(I).EQ.3)) THEN		!épontille double T		!février 2004
	I2 = I2 + 1	
  	NELO = NELO + 1		
  21    READ(2218,*,END=1) IVAR   !saved in ECRI4				
       IF(IVAR.EQ.99) GOTO 1								
      IF(IVAR.EQ.1) THEN											!1  V	
      READ(2218,*)V(I,1),IV(I,1),IV(I,2),V(I,2),IV(I,3),IV(I,4)		
c      WRITE(67,*)'V',V(I,1),IV(I,1),IV(I,2),
c     *				V(I,2),IV(I,3),IV(I,4)		
      ELSE IF (IVAR.EQ.2)  THEN									!2  U	
      READ(2218,*)U(I,1),IU(I,1),IU(I,2),U(I,2),IU(I,3),IU(I,4)		
c      WRITE(67,*)'U',U(I,1),IU(I,1),IU(I,2),
c     *				U(I,2),IU(I,3),IU(I,4)		
      ELSE IF (IVAR.EQ.3)  THEN									!3  W	
      READ(2218,*)W(I,1),IW(I,1),IW(I,2),W(I,2),IW(I,3),IW(I,4)		
	
	ELSE IF (IVAR.EQ.17) THEN						!17 Sy  JAS SUP(cadre)	
      READ(2218,*)SJASi(I2,1),ISJASi(I2,1),ISJASi(I2,2),							
     *           SJASi(I2,2),ISJASi(I2,3),ISJASi(I2,4)							
      ELSE IF (IVAR.EQ.18) THEN						!18 Sy  JAS INF (cadre)	
      READ(2218,*)SJABI(I2,1),ISJABI(I2,1),ISJABI(I2,2),							
     *           SJABI(I2,2),ISJABI(I2,3),ISJABI(I2,4)							
      ELSE IF (IVAR.EQ.19) THEN						!19 TAU  JAS (cadre)	
      READ(2218,*)TJASI(I2,1),ITJASI(I2,1),ITJASI(I2,2),							
     *           TJASI(I2,2),ITJASI(I2,3),ITJASI(I2,4)							
      ELSE IF (IVAR.EQ.20) THEN						!20 TAU  AN (cadre)		
      READ(2218,*)TJABI(I2,1),ITJABI(I2,1),ITJABI(I2,2),							
     *           TJABI(I2,2),ITJABI(I2,3),ITJABI(I2,4)							
	
      ELSE IF (IVAR.EQ.21) THEN						!21 Sig Comp JAS SUP (cadre) 
      READ(2218,*)AJASI(I2,1),IAJASI(I2,1),IAJASI(I2,2),							
     *           AJASI(I2,2),IAJASI(I2,3),IAJASI(I2,4)							
      ELSE IF (IVAR.EQ.22) THEN						!22 Sig Comp JAS INF (cadre)  
      READ(2218,*)AJABI(I2,1),IAJABI(I2,1),IAJABI(I2,2),							
     *           AJABI(I2,2),IAJABI(I2,3),IAJABI(I2,4)							
      ELSE IF (IVAR.EQ.23) THEN						!23 Sig Comp AN (cadre)	
      READ(2218,*)SBORXI(I2,1),ISBORXI(I2,1),ISBORXI(I2,2),						
     *           SBORXI(I2,2),ISBORXI(I2,3),ISBORXI(I2,4)						
	ELSE IF (IVAR.EQ.27) THEN						!27 Sig Comp sem SUP (cadre) 
      READ(2218,*)ASEMI(I2,1),IASEMI(I2,1),IASEMI(I2,2),							
     *           ASEMI(I2,2),IASEMI(I2,3),IASEMI(I2,4)							
      ELSE IF (IVAR.EQ.28)  THEN						!28 Sig Comp sem INF (cadre) 
      READ(2218,*)SBORYI(I2,1),ISBORYI(I2,1),ISBORYI(I2,2),						
     *           SBORYI(I2,2),ISBORYI(I2,3),ISBORYI(I2,4)						
      ELSE																	
        WRITE(67,*)'ERROR2: Subr. ANALYS; IVAR=',IVAR,' est incorrecte'	
        WRITE(*,*) 'ERROR2: Subr. ANALYS; IVAR=',IVAR,' est incorrecte'	
        	  WRITE(29,*)'subr Analysis'													!bug
	  WRITE(29,*)'ERROR2: Subr. ANALYS; IVAR=',IVAR,' est incorrecte'				!bug

        PAUSE 'Stop'															
        STOP															
	ENDIF
	GOTO 21											
       ELSEIF((ITYPE(I).EQ.5).AND.(ISECT(I).NE.3)) THEN		!épontille parois minces
	 NELO= NELO+1
	 I3 = I3 + 1		
 22   READ(2218,*,END=1) IVAR   !saved in ECRI4				
       IF(IVAR.EQ.99) GOTO 1					

      IF(IVAR.EQ.1) THEN											!1  V 
      READ(2218,*)V(I,1),IV(I,1),IV(I,2),V(I,2),IV(I,3),IV(I,4)   
c      WRITE(67,*)'V',V(I,1),IV(I,1),IV(I,2),V(I,2),IV(I,3),IV(I,4)     
      ELSE IF (IVAR.EQ.2)  THEN									!2  U 
      READ(2218,*)U(I,1),IU(I,1),IU(I,2),U(I,2),IU(I,3),IU(I,4)    
c      WRITE(67,*)'U',U(I,1),IU(I,1),IU(I,2),U(I,2),IU(I,3),IU(I,4)      
      ELSE IF (IVAR.EQ.3)  THEN									!3  W  
      READ(2218,*)W(I,1),IW(I,1),IW(I,2),W(I,2),IW(I,3),IW(I,4)    

	ELSE IF (IVAR.EQ.17) THEN						!17 Sy  JAS SUP(cadre)  
      READ(2218,*)SJASi(I3,1),ISJASi(I3,1),ISJASi(I3,2),                   
     *           SJASi(I3,2),ISJASi(I3,3),ISJASi(I3,4)                     
      ELSE IF (IVAR.EQ.18) THEN						!18 Sy  JAS INF (cadre)  
      READ(2218,*)SJABI(I3,1),ISJABI(I3,1),ISJABI(I3,2),                  
     *           SJABI(I3,2),ISJABI(I3,3),ISJABI(I3,4)                      
      ELSE IF (IVAR.EQ.19) THEN						!19 TAU  JAS (cadre)  
      READ(2218,*)TJASI(I3,1),ITJASI(I3,1),ITJASI(I3,2),                    
     *           TJASI(I3,2),ITJASI(I3,3),ITJASI(I3,4)                      
      ELSE IF (IVAR.EQ.20) THEN						!20 TAU  AN (cadre) 
      READ(2218,*)TJABI(I3,1),ITJABI(I3,1),ITJABI(I3,2),                   
     *           TJABI(I3,2),ITJABI(I3,3),ITJABI(I3,4)                      

      ELSE IF (IVAR.EQ.21) THEN						!21 Sig Comp JAS SUP (cadre)  
      READ(2218,*)AJASI(I3,1),IAJASI(I3,1),IAJASI(I3,2),                      
     *           AJASI(I3,2),IAJASI(I3,3),IAJASI(I3,4)                      
      ELSE IF (IVAR.EQ.22) THEN						!22 Sig Comp JAS INF (cadre)  
      READ(2218,*)AJABI(I3,1),IAJABI(I3,1),IAJABI(I3,2),                  
     *           AJABI(I3,2),IAJABI(I3,3),IAJABI(I3,4)                       
      ELSE IF (IVAR.EQ.23) THEN						!23 Sig Comp AN (cadre)  
      READ(2218,*)SBORXI(I3,1),ISBORXI(I3,1),ISBORXI(I3,2),                
     *           SBORXI(I3,2),ISBORXI(I3,3),ISBORXI(I3,4)                   
      ELSE	                                                            
        WRITE(67,*)'ERROR3: Subr. ANALYS; IVAR=',IVAR,' est incorrecte'	
        WRITE(*,*) 'ERROR3: Subr. ANALYS; IVAR=',IVAR,' est incorrecte'	
	  WRITE(29,*)'subr Analysis'													!bug
	  WRITE(29,*)'ERROR3: Subr. ANALYS; IVAR=',IVAR,' est incorrecte'				!bug
	
        PAUSE 'Stop'		                                              
        STOP	                                                           
	ENDIF                                                              
	goto 22                                                               
	ENDIF													!février 2004

  30  IF(IMPR2.LE.-2) GOTO 1			!15.10.05
  31  READ(304,*,END=1) IVAR !saved in ECRI3					!extension neto
      IF(IVAR.EQ.88) GOTO 1						      
      IF(IVAR.EQ.4) THEN						        ! 4 SIG.Comp z=+d/2 
        READ(304,*)SBOR1(I1,1),ISBOR1(I1,1),ISBOR1(I1,2),	!extension neto
     *             SBOR1(I1,2),ISBOR1(I1,3),ISBOR1(I1,4)
      ELSE IF (IVAR.EQ.8) THEN						! 8 SIG.Comp z=-d/2 
        READ(304,*)SBOR2(I1,1),ISBOR2(I1,1),ISBOR2(I1,2),	!extension neto
     *             SBOR2(I1,2),ISBOR2(I1,3),ISBOR2(I1,4)
      ELSE
        WRITE(67,*)' ERROR4: Subr. ANALYS; IVAR=',IVAR,' est incorrecte'
        WRITE(*,*) ' ERROR4: Subr. ANALYS; IVAR=',IVAR,' est incorrecte'
	  WRITE(29,*)'subr Analysis'													!bug
	  WRITE(29,*)'ERROR4: Subr. ANALYS; IVAR=',IVAR,' est incorrecte'				!bug

        PAUSE 'Stop'
        STOP
      END IF
      GOTO 31

   1   IF(ITYPE(I).NE.5) THEN			!compte des panneaux qui sont épontilles	!février 2004
	  I4(I1) = I1 + I2 + I3						!février 2004
	ELSE IF(ISECT(I).EQ.3) THEN					!février 2004
	  I5(I2) = I1 + I2 + I3						!février 2004
	ELSE										!février 2004
	  I6(I3) = I1 + I2 + I3						!février 2004
	ENDIF										!février 2004

   2  CONTINUE									!février 2004
	 NETA = NETO - NELO							!février 2004
   
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     CLASSEMENT DES MAXIMUM/MINIMUM ENTRE LES PANNEAUX
C
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(LANGUE==1) WRITE(67,10)
      IF(LANGUE==2) WRITE(67,11)

      IF(LANGUE==1) WRITE(67,*)'Les valeurs extrêmes de V sont:'
      IF(LANGUE==2) WRITE(67,*)'Extrem values of V are:'
      CALL CLASSE(V(1,2),IV(1,3),IV(1,4),NETO)
      CALL CLASSE(V(1,1),IV(1,1),IV(1,2),NETO)
      IF(LANGUE==1) WRITE(67,*)'Les valeurs extrêmes de U sont:'
      IF(LANGUE==2) WRITE(67,*)'Extrem values of U are:'
      CALL CLASSE(U(1,2),IU(1,3),IU(1,4),NETO)
      CALL CLASSE(U(1,1),IU(1,1),IU(1,2),NETO)
      IF(LANGUE==1) WRITE(67,*)'Les valeurs extrêmes de W sont:'
      IF(LANGUE==2) WRITE(67,*)'Extrem values of W are:'
      CALL CLASSE(W(1,2),IW(1,3),IW(1,4),NETO)
      CALL CLASSE(W(1,1),IW(1,1),IW(1,2),NETO)
	IF (IMPR2.EQ.-3) GOTO 100					!15.10.05
c -------------------      
      WRITE(67,*)
      IF(LANGUE==1) THEN
	 WRITE(67,*)'Les valeurs extrêmes de Sx BORDAGE (z=0) sont:'
	ELSE
       WRITE(67,*)'Extrem values of Sx STRESS, PLATE (Z=0) are:'
	ENDIF
      CALL CLASSE2(SBORX(1,2),ISBORX(1,3),ISBORX(1,4),NETA,I4)	!février 2004
      CALL CLASSE2(SBORX(1,1),ISBORX(1,1),ISBORX(1,1),NETA,I4)	!février 2004
      
      IF(LANGUE==1) THEN
	 WRITE(67,*)'Les valeurs extrêmes de Sy BORDAGE (z=0) sont:'
	ELSE
       WRITE(67,*)'Extrem values of Sy STRESS, PLATE (Z=0) are:'
	ENDIF
      CALL CLASSE2(SBORY(1,2),ISBORY(1,3),ISBORY(1,4),NETA,I4)	!février 2004
      CALL CLASSE2(SBORY(1,1),ISBORY(1,1),ISBORY(1,1),NETA,I4)	!février 2004
      
      IF(LANGUE==1) THEN
	 WRITE(67,*)'Les valeurs extrêmes de Txy BORDAGE (z=0) sont:'
	ELSE
       WRITE(67,*)'Extrem values of Txy SHEAR STRESS, PLATE (Z=0) are:'
	ENDIF
      CALL CLASSE2(TAU(1,2),ITAU(1,3),ITAU(1,4),NETA,I4)			!février 2004
      CALL CLASSE2(TAU(1,1),ITAU(1,1),ITAU(1,1),NETA,I4)			!février 2004
      
      IF(LANGUE==1) THEN
	 WRITE(67,*)'Les valeurs extrêmes de SIG COMP BORDAGE (z=0) sont:'
	ELSE
       WRITE(67,*)'Extrem values of COMBINED STRESS, PLATE (Z=0) are:'
	ENDIF
      CALL CLASSE2(SBOR(1,2),ISBOR(1,3),ISBOR(1,4),NETA,I4)		!février 2004
      
      IF(IMPR2.GE.0) THEN
      IF(LANGUE==1) THEN
       WRITE(67,*)'Les valeurs extrêmes de SIGMA COMP (z=+d/2) sont:'
	ELSE
       WRITE(67,*)'Extrem values of COMBINED STRESS, PLATE(z=+d/2) are:'
	ENDIF
      CALL CLASSE2(SBOR1(1,2),ISBOR1(1,3),ISBOR1(1,4),NETA,I4)	!février 2004

      IF(LANGUE==1) THEN
       WRITE(67,*)'Les valeurs extrêmes de SIGMA COMP (z=-d/2) sont:'
	ELSE
       WRITE(67,*)'Extrem values of COMBINED STRESS, PLATE(z=-d/2) are:'
	ENDIF
      CALL CLASSE2(SBOR2(1,2),ISBOR2(1,3),ISBOR2(1,4),NETA,I4)	!février 2004
      ENDIF
c ------------------
      WRITE(67,*)
      IF(INDAIG.EQ.1) THEN
   
      IF(LANGUE==1) THEN
	 WRITE(67,*)'Les valeurs extrêmes de Sy JAS Cadre sont:'
	ELSE
       WRITE(67,*)'Extrem values of Sy Stress JWF Frame are:'
	ENDIF
      CALL CLASSE2(SJAS(1,2),ISJAS(1,3),ISJAS(1,4),NETA,I4)		!février 2004
      CALL CLASSE2(SJAS(1,1),ISJAS(1,1),ISJAS(1,1),NETA,I4)		!février 2004
 
      IF(LANGUE==1) THEN
	 WRITE(67,*)'Les valeurs extrêmes de Tau JAS Cadre sont:'
	ELSE
       WRITE(67,*)'Extrem values of Txy Shear Stress JWF Frame are:'
	ENDIF
      CALL CLASSE2(TJAS(1,2),ITJAS(1,3),ITJAS(1,4),NETA,I4)		!février 2004
      CALL CLASSE2(TJAS(1,1),ITJAS(1,1),ITJAS(1,1),NETA,I4)		!février 2004
 
      IF(LANGUE==1) THEN
	 WRITE(67,*)'Les valeurs extrêmes de Sy JAB Cadre sont:'
	ELSE
       WRITE(67,*)'Extrem values of Sy Stress JWP Frame are:'
	ENDIF
      CALL CLASSE2(SJAB(1,2),ISJAB(1,3),ISJAB(1,4),NETA,I4)		!février 2004
      CALL CLASSE2(SJAB(1,1),ISJAB(1,1),ISJAB(1,1),NETA,I4)		!février 2004

      IF(LANGUE==1) THEN
	 WRITE(67,*)'Les valeurs extrêmes de Tau JAB Cadre sont:'
	ELSE
       WRITE(67,*)'Extrem values of Txy Shear Stress JWB Frame are:'
	ENDIF
      CALL CLASSE2(TJAB(1,2),ITJAB(1,3),ITJAB(1,4),NETA,I4)		!février 2004
      CALL CLASSE2(TJAB(1,1),ITJAB(1,1),ITJAB(1,1),NETA,I4)		!février 2004
   
      IF(LANGUE==1) THEN
       WRITE(67,*)'Les valeurs extrêmes de SIGMA COMP JAS CADRE sont:'
	ELSE
       WRITE(67,*)'Extrem values of Sc JWF FRAME (Comb. Stress) are:'
	ENDIF
      CALL CLASSE2(AJAS(1,2),IAJAS(1,3),IAJAS(1,4),NETA,I4)		!février 2004

      IF(LANGUE==1) THEN
       WRITE(67,*)'Les valeurs extrêmes de SIGMA COMP JAB CADRE sont:'
	ELSE
       WRITE(67,*)'Extrem values of Sc JWP FRAME (Comb. Stress) are:'
	ENDIF
      CALL CLASSE2(AJAB(1,2),IAJAB(1,3),IAJAB(1,4),NETA,I4)		!février 2004

      IF(LANGUE==1) THEN
       WRITE(67,*)'Les valeurs extrêmes de SIGMA COMP SEM CADRE sont:'
	ELSE
       WRITE(67,*)'Extrem values of Sc FLANGE FRAME (Comb. Stress) are:'
	ENDIF
      CALL CLASSE2(ASEM(1,2),IASEM(1,3),IASEM(1,4),NETA,I4)		!février 2004
      ENDIF
c -------------------      !épontilles							!février 2004
      IF (I2.GT.0) THEN ! si on a des épontilles en double T      !février 2004       
	IF(LANGUE==1) THEN                                          !février 2004
	WRITE(67,*)'Les valeurs extrêmes de Sy JAS supérieure epon sont:'   
	ELSE                                                                  
       WRITE(67,*)'Extrem  values of Sy Stress JWF top pillar are:'         
	ENDIF                                                             
      CALL CLASSE2(SJASi(1,2),ISJASi(1,3),ISJASi(1,4),NELO,I5)               
      CALL CLASSE2(SJASi(1,1),ISJASi(1,1),ISJASi(1,1),NELO,I5)           
  
      IF(LANGUE==1) THEN                                
	 WRITE(67,*)'Les valeurs extrêmes de Sy JAS inférieur epon sont:'  
	ELSE                                                                 
      WRITE(67,*)'Extrem values of Sy Stress JWF bottom pillar are:'         
	ENDIF                                                                   
      CALL CLASSE2(SJABI(1,2),ISJABI(1,3),ISJABI(1,4),NELO,I5)                    
      CALL CLASSE2(SJABI(1,1),ISJABI(1,1),ISJABI(1,1),NELO,I5)                  
 
      IF(LANGUE==1) THEN                                                     
	 WRITE(67,*)'Les valeurs extrêmes de Tau JAS épontille sont:'         
	ELSE                                                                   
       WRITE(67,*)'Extrem values of Shear Stress JWF pillar are:'             
	ENDIF                                                                  
      CALL CLASSE2(TJASI(1,2),ITJASI(1,3),ITJASI(1,4),NELO,I5)                  
      CALL CLASSE2(TJASI(1,1),ITJASI(1,1),ITJASI(1,1),NELO,I5)                  

      IF(LANGUE==1) THEN                                                   
	 WRITE(67,*)'Les valeurs extrêmes de Tau axe neutre épon sont:'        
	ELSE                                                                    
       WRITE(67,*)'Extrem values of Txy Shear Stress'                          
       WRITE(67,*)'at neutral axis pillar are:'                             
	ENDIF                                                               
      CALL CLASSE2(TJABI(1,2),ITJABI(1,3),ITJABI(1,4),NELO,I5)                
      CALL CLASSE2(TJABI(1,1),ITJABI(1,1),ITJABI(1,1),NELO,I5)                 
   
      IF(LANGUE==1) THEN                                                  
      WRITE(67,*)'Les valeurs extrêmes de SIG COMP JAS sup épon sont:'   
	ELSE                                                                 
      WRITE(67,*)'Extrem values of Comb. Stress JWF top pillar are:'         
	ENDIF                                                               
      CALL CLASSE2(AJASI(1,2),IAJASI(1,3),IAJASI(1,4),NELO,I5)                 
 
       IF(LANGUE==1) THEN                                                
      WRITE(67,*)'Les valeurs extrêmes de SIG COMP JAS inf épon sont:'     
	ELSE                                                                     
      WRITE(67,*)'Extrem values of Sc JWF bottom'                            
      WRITE(67,*)' pillar (Comb. Stress) are:'                                
	ENDIF                                                                    
      CALL CLASSE2(AJABI(1,2),IAJABI(1,3),IAJABI(1,4),NELO,I5)                  

      IF(LANGUE==1) THEN                                                
      WRITE(67,*)'Les valeurs extrêmes de SIG COMP axe n. épon sont:'    
	ELSE                                                                 
      WRITE(67,*)'Extrem values of Sc neutral axis'                          
      WRITE(67,*)' pillar (Comb. Stress) are:'                               
	ENDIF                                                                   
      CALL CLASSE2(SBORXI(1,2),ISBORXI(1,3),ISBORXI(1,4),NELO,I5)             
 
	IF(LANGUE==1) THEN                                                  
       WRITE(67,*)'Les valeurs extrêmes de SIG COMP SEM SUP épon sont:'  
	ELSE                                                                 
       WRITE(67,*)'Extrem values of Sc FLANGE TOP'                         
       WRITE(67,*)' pillar (Comb. Stress) are:'                                
	ENDIF                                                                       
      CALL CLASSE2(ASEMI(1,2),IASEMI(1,3),IASEMI(1,4),NELO,I5)                  

     	IF(LANGUE==1) THEN                                                    
       WRITE(67,*)'Les valeurs extrêmes de SIG COMP SEM INF épon sont:'        
	ELSE                                                                  
       WRITE(67,*)'Extrem values of Sc FLANGE BOTTOM'                        
       WRITE(67,*)' pillar (Comb. Stress) are:'                              
	ENDIF                                                                  
      CALL CLASSE2(SBORYI(1,2),ISBORYI(1,3),ISBORYI(1,4),NELO,I5)                
      ENDIF
c	--------------
      IF (I3.GT.0) THEN  !si on a des épontilles à paroi mince                                              
 	IF(LANGUE==1) THEN                                                   
	WRITE(67,*)'Les valeurs extrêmes de Sy supérieure épon sont:'         
	ELSE                                                                    
       WRITE(67,*)'Extrem  values of Sy Stress top pillar are:'               
	ENDIF                                                                
      CALL CLASSE2(SJASi(1,2),ISJASi(1,3),ISJASi(1,4),NELO,I6)               
      CALL CLASSE2(SJASi(1,1),ISJASi(1,1),ISJASi(1,1),NELO,I6)                 
  
      IF(LANGUE==1) THEN                                           
      WRITE(67,*)'Les valeurs extrêmes de Sy inférieur épon sont:'   
	ELSE                                                        
      WRITE(67,*)'Extrem values of Sy Stress bottom pillar are:'      
	ENDIF                                                        
      CALL CLASSE2(SJABI(1,2),ISJABI(1,3),ISJABI(1,4),NELO,I6)          
      CALL CLASSE2(SJABI(1,1),ISJABI(1,1),ISJABI(1,1),NELO,I6)            
 
      IF(LANGUE==1) THEN                                                  
	 WRITE(67,*)'Les valeurs extrêmes de Tau sommet épon sont:'          
	ELSE                                                                 
       WRITE(67,*)'Extrem values of Shear Stress top pillar are:'         
	ENDIF                                                               
      CALL CLASSE2(TJASI(1,2),ITJASI(1,3),ITJASI(1,4),NELO,I6)                      
      CALL CLASSE2(TJASI(1,1),ITJASI(1,1),ITJASI(1,1),NELO,I6)            
  
      IF(LANGUE==1) THEN                                              
	 WRITE(67,*)'Les valeurs extrêmes de Tau axe neutre épon sont:' 
	ELSE                                                                 
       WRITE(67,*)'Extrem values of Txy Shear Stress'                     
       WRITE(67,*)'at neutral axis pillar are:'                             
	ENDIF                                                               
      CALL CLASSE2(TJABI(1,2),ITJABI(1,3),ITJABI(1,4),NELO,I6)                
      CALL CLASSE2(TJABI(1,1),ITJABI(1,1),ITJABI(1,1),NELO,I6)                
        
      IF(LANGUE==1) THEN                                                   
      WRITE(67,*)'Les valeurs extrêmes de SIG COMP sup épon sont:'             
	ELSE                                                                  
      WRITE(67,*)'Extrem values of Comb. Stress JWF top pillar are:'        
	ENDIF                                                              
      CALL CLASSE2(AJASI(1,2),IAJASI(1,3),IAJASI(1,4),NELO,I6)               
    
      IF(LANGUE==1) THEN                                                
      WRITE(67,*)'Les valeurs extrêmes de SIG COMP inf épon sont:'        
	ELSE                                                                 
      WRITE(67,*)'Extrem values of Sc JWF bottom'                          
      WRITE(67,*)' pillar (Comb. Stress) are:'                           
	ENDIF                                                             
      CALL CLASSE2(AJABI(1,2),IAJABI(1,3),IAJABI(1,4),NELO,I6)              

      IF(LANGUE==1) THEN                                                
      WRITE(67,*)'Les valeurs extrêmes de SIG COMP axe n. épon sont:'  
	ELSE                                                               
      WRITE(67,*)'Extrem values of Sc neutral axis'                     
      WRITE(67,*)' pillar (Comb. Stress) are:'                         
	ENDIF                                                            
      CALL CLASSE2(SBORXI(1,2),ISBORXI(1,3),ISBORXI(1,4),NELO,I6)      !février 2004
	ENDIF															 !février 2004
c -------------------      

      WRITE(67,*)
      IF(INDRAI.EQ.1) THEN
      IF(LANGUE==1) THEN
       WRITE(67,*)'Les valeurs extrêmes de SIGMA COMP JAS RAID sont:'
	ELSE
       WRITE(67,*)'Extrem values of Sc JWF STIFF (Comb. Stress) are:'
	ENDIF
      CALL CLASSE2(RJAS(1,2),IRJAS(1,3),IRJAS(1,4),NETA,I4)				!février 2004

      IF(LANGUE==1) THEN
       WRITE(67,*)'Les valeurs extrêmes de SIGMA COMP JAB RAID sont:'
	ELSE
       WRITE(67,*)'Extrem values of Sc JWP STIFF (Comb. Stress) are:'
	ENDIF
      CALL CLASSE2(RJAB(1,2),IRJAB(1,3),IRJAB(1,4),NETA,I4)				!février 2004

      IF(LANGUE==1) THEN
       WRITE(67,*)'Les valeurs extrêmes de SIGMA COMP SEM RAID sont:'
	ELSE
       WRITE(67,*)'Extrem values of Sc FLANGE STIFF (Comb. Stress) are:'
	ENDIF
      CALL CLASSE2(RSEM(1,2),IRSEM(1,3),IRSEM(1,4),NETA,I4)				!février 2004
      ENDIF
  100 CONTINUE												!15.10.05
  
      
      RETURN                                                            
  10  FORMAT(/'RECAPITULATIFS DE VALEURS EXTREMES CALCULEES (Deplac.',
     * ' et Contraintes de Comparaison'/70(1H-)/
     * '(PAR ORDRE DECROISSANT ; unités = m et N/m2)'/T2,'Valeurs',T18,
     *       'Panneaux',T40,'Positions (X,Y)'/ )
  11  FORMAT(/'SUMMARY OF THE EXTREME VALUES (DISPL & COMBINED STRESS)'
     *  /54(1H*)/
     * '(Ranked by values ; Units = m and N/m2)'/T2,'VALUES',T18,
     *       'PANELS',T40,'LOCATIONS (X,Y)'/ )
      END                                                               

C***********************************************************************
C***********************************************************************
C***********************************************************************

      SUBROUTINE CLASSE(X,IX,IY,NETO)                            	
C ***********************************************************************                                                        
C     subroutine de CLASSEMENT PAR ORDRE DECROISSANT des valeurs extrêmes.
C
C     Modifié : 13-3-95                     Créer: 13-3-95
C
C************************************************************************

      IMPLICIT REAL *8 (A-H,O-Z)                                     
      DIMENSION  X(NETO),IX(NETO),IY(NETO)                    		  

C     X(NEL) avec NEL=1,NETO  : VALEUR à classer     
C     IX et IY  coordonnées des valeurs maximales pour chaque panneau 
c     !! Les valeurs de X ne sont pas conservees. 
C     !! Le vecteur X n'est donc plus utilisable par la suite
  
      DO 3 J=1,NETO 
        X1=-1.E30							! Grande valeur négative
        N1=0
        DO 4 K=1,NETO                                                     
          IF(X1.LT.X(K)) THEN
            X1=X(K)
            N1=K
          ENDIF
   4    CONTINUE

        IF(DABS(X(N1)).GE.1.0E-15) WRITE(67,10)X(N1),N1,IX(N1),IY(N1)
        X(N1)=-1.E30

   3  CONTINUE

c ------------------------------------------------------	                                                           
  10  FORMAT(E11.4,T20,I2,T45,'(',I2,',',I2,')')

      RETURN 
      END    
	                                                           
c************************************************************************
c************************************************************************
c************************************************************************
c
c
      SUBROUTINE CLASSE2(Xi,IXi,IYi,NELO,II)               !février 2004             	
C ***********************************************************************                                                        
C     subroutine de CLASSEMENT PAR ORDRE DECROISSANT des valeurs extrêmes.
C
C     Modifié : 05-02-2004                     Créer: 05-02-2004
C
C************************************************************************

      IMPLICIT REAL *8 (A-H,O-Z)                                     
      DIMENSION  Xi(NELO),IXi(NELO),IYi(NELO),II(NELO)                    		  

C     X(NEL) avec NEL=1,NETO  : VALEUR à classer     
C     IX et IY  coordonnées des valeurs maximales pour chaque panneau 
c     !! Les valeurs de X ne sont pas conservees. 
C     !! Le vecteur X n'est donc plus utilisable par la suite
  
      DO 3 J=1,NELO 
        Xi1=-1.E30							! Grande valeur négative
        Ni1=0
        DO 4 K=1,NELO                                                     
          IF(Xi1.LT.Xi(K)) THEN
            Xi1=Xi(K)
            Ni1=II(K)
	      Ni2=K
          ENDIF
   4    CONTINUE

       IF(DABS(Xi(Ni2)).GE.1.0E-15) then
	 WRITE(67,10)Xi(Ni2),Ni1,IXi(Ni2),IYi(Ni2)
	 endif
        Xi(Ni2)=-1.E30

   3  CONTINUE

c ------------------------------------------------------	                                                           
  10  FORMAT(E11.4,T20,I2,T45,'(',I2,',',I2,')')

      RETURN 
      END                                                               
