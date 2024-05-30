      SUBROUTINE OBJCT2(N,XI,NXI,NBRXI,OBJ,WIDTH,IPRINT,NEL,SPEC,ITT,
     *  itype,isect,CORROEL)			!19.05.04   + !corrosion

      IMPLICIT REAL*8(A-H,O-Z)
	REAL* 8 OBJ,FCT,Fmat,Fsou,FMdO,Fmat11,Fmat22,Fmat33,Fsou11,Fsou22,
     *        Fsou33,FMdO11,FMdO22,FMdO33,FMdO44,FMdO55,FMdO66,FMdO77

c      DIMENSION ITYPE(1),ISECT(1)								!19.05.04
	DIMENSION XI(9),NXI(NBRXI)
	DIMENSION CORROEL(3)			!corrosion ; CORROEL = vecteur de dimension 3 qui ne reprend que les epaiss de corrosion liées au panneau NEL
      COMMON/PY/PI

C     ENT+COST,HULL,BO1,BO2,HUGUES,OBJEC
c      COMMON/MAT/   E(30),ETA(30),SIGY(30),SIGM(30),SPEC(30)

C     COST(ENT),OPTI,OBJEC

      COMMON/COUT/ Poids,SPoids,
     *        Fmat,Fsou,FMdO,Fmat11,Fmat22,Fmat33,Fsou11,Fsou22,
     *        Fsou33,FMdO11,FMdO22,FMdO33,FMdO44,FMdO55,FMdO66,FMdO77,
     *        REND,EQP,Dref,DrefX,DrefY,C1,C2,C3,DC1,DW2,DW3,P10,DP10,
     *        P4,P5,DP4,DP5,P9X,P9Y,DP9X,DP9Y,BER,BET,P6,P7,C8,DC8,COUT,
     *        ICOUT,IALR,IALT
C **********************************************************************
C     Ré-évaluation de la fonction objectif COUT (OBJ) après la subr. CONLIN
C
C     Version du : 16-11-2001    			          Créer 18-5-98
c
c     16-11-2001 : Sur-épaisseurs de corrosion
C
C ****************************************************************************************
c
C LES VARIABLES DE CONCEPTION
C ---------------------------
C	1	DELTA =  épaisseur du bordage
C	2	HYA   =  hauteur de l'âme des aiguilles
C	3	DYA   =  épaisseur de l'âme des aiguilles
C	4	WYA   =  largeur des semelles des aiguilles
C	5	EPSA  =  entredistance entre aiguilles
C	6	HXR   =  hauteur de l'âme des raidisseurs
C	7	DXR   =  épaisseur de l'âme des raidisseurs
C	8	WXR   =  largeur des semelles des raidisseurs
C	9	EPSR  =  entredistance entre raidisseurs
c
C Variables Associées :
C ---------------------
C	 	TYA = épaisseur semelle aiguilles
C     	TXR = épaisseur semelle raidisseurs 
c
C **************************************************************************************
	
	READ(302) KSA,KSR,KSE,EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,	!extension neto
     *		  HXR,DXR,WXR,TXR,PHIL,Q,EPAIS					!14/10/04

 	DO 123 I=1,NBRXI
	GOTO(124,125,120,126,127,128,121,129,130),NXI(I)
  124 DELTA=XI(I)
      GOTO 123
  125 HYA=XI(I)
      GOTO 123
  120 DYA=XI(I)
      GOTO 123
  126 WYA=XI(I)
      GOTO 123
  127 EPSA=XI(I)
      GOTO 123
  128 HXR=XI(I)
      GOTO 123
  121 DXR=XI(I)
      GOTO 123
  129 WXR=XI(I)
      GOTO 123
  130 EPSR=XI(I)
  123 CONTINUE

C
C     Epaisseur de corrosion
c     -----------------------
C     DCOR = epaisseur de corrosion pour bordé
c	IF(NEL.EQ.1) THEN
c	  REWIND 57
c	  READ(57,'(////,A1)') Abidon
c	  READ(57,*) IPAN,DCOR,DCOR2,DCOR3
c	ELSE
c	  READ(57,*) IPAN,DCOR,DCOR2,DCOR3
c	ENDIF

	DCOR  = CORROEL(1)							 !corrosion
	DCOR2 = CORROEL(2)							 !corrosion
	DCOR3 = CORROEL(3)							 !corrosion

	IF(ISECT.EQ.3) THEN			!âme double T entree=1/2 âme		!février 2004	!14/10/04
	HYB=2*HYA															!19.05.04
	ENDIF																!19.05.04

	IF(ITYPE.NE.5) THEN												!19.05.04		!14/10/04
	DELTA= DELTA+DCOR
	ELSE																!19.05.04
	DELTA =0.000														!19.05.04
	ENDIF																!19.05.04
c	DCOR2   ! epaisseur de corrosion pour cadres
	DYA  = DYA +DCOR2
	TYA  = TYA +DCOR2
	EPAIS = EPAIS +DCOR2							!19.05.04
c	DCOR3   ! epaisseur de corrosion pour lisses
	DXR  = DXR +DCOR3
	TXR  = TXR +DCOR3

c	IF(IPRINT.GE.1) THEN
c      WRITE(*,*) 'PANNEAU Nø ',NEL
c      WRITE(*,*) 'NBRXI= ',NBRXI,' et  NXI =',NXI
c      WRITE(*,*) 'Bord = ',DELTA
c      WRITE(*,*) 'Aig  = ',HYA,DYA,WYA,TYA,'   EPSA=',EPSA
c      WRITE(*,*) 'Raid = ',HXR,DXR,WXR,TXR,'   EPSR=',EPSR
c	PAUSE'OBJ2'
c	ENDIF

C     Impressions (avec epaiss corrosion)
C     -----------
      IF(ITT.EQ.1) THEN
      WRITE (666,132)NEL,'Ep. Bord','Hâme Cad','Eâme Cad','Lsem Cad',
     *      	'Tsem Cad','EPSA Cad','Hâme Rai','Eâme Rai','Lsem Rai',
     *        'Tsem Rai','EPSR Rai'  
      WRITE (666,131)DELTA,HYA,DYA,WYA,TYA,EPSA,HXR,DXR,WXR,TXR,EPSR
	ENDIF

C  -------------------------

C     FCT = Poids = bordage + ames + semelles  (du panneau NEL)
C     OBJ = Poids total (structure complete)

C      TEMP=  PHIL * Q * WIDTH * PI/180.
C      FCT= DELTA 
C     *  +  (1.0+DW3)*( DYA * HYA + TYA* WYA ) /EPSA
C     *  +  (1.0+DW2)*( DXR * HXR + TXR* WXR ) /EPSR
C      OBJ= OBJ + FCT * TEMP

C  -------------------------

C     Fmat= Coût des matériaux (acier)
C     Fsou= Coût du soudage (energie + consommables)
C     FMdO= Coût Main d'oeuvre
C     FCT = Fmat + Fsou + FMdO  (d'un panneau NEL)
C     OBJ = Coût total          (de la structure complete)


      TEMP =  PHIL * Q * WIDTH * PI/180.
	DENS = (SPEC/9.81)

	IF(ITYPE.NE.5) THEN						 !plaque			!19.05.04	!14/10/04
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
     *         P9Y*(1.+(DYA-DrefY)*1000.*DP9Y)   )/EPSA
     *     + (P6+BER*BET*P7) /(EPSA*EPSR)  
     *     +  P10*(1.+(DELTA-Dref)*1000.*DP10)      )

	ELSE							!epontilles				!19.05.04
	IF(ISECT.EQ.3) THEN														!14/10/04
	Fmat=TEMP * DENS * C3 *(1.+(DYA  -Dref)*1000.*DC1) * (1.0+DW3)   
     *                      * ((2*DYA*HYA+2*TYA*WYA)/EPSA )			
	ELSEIF(ISECT.EQ.1) THEN													!14/10/04
	Fmat=TEMP * DENS * C3 *(1.+(EPAIS  -Dref)*1000.*DC1) * (1.0+DW3)   
     *                 * ((PI*(HYA*HYA-(HYA-2*EPAIS)**2))/(4*EPSA ))   	
	ELSEIF(ISECT.EQ.2) THEN													!14/10/04
	Fmat=TEMP * DENS * C3 *(1.+(EPAIS  -Dref)*1000.*DC1) * (1.0+DW3)   
     *                 * ((HYA*HYA-(HYA-2*EPAIS)**2)/EPSA )				
	ENDIF													
	Fsou=0.0001													!19.05.04
	FMdo=0.0001													!19.05.04
	ENDIF												!20.02.04

      FCT = Fmat + Fsou + FMdO
 
      OBJ  = OBJ + FCT

c     IF(IPRINT.GE.1) THEN
c       WRITE(*,*) 'Dans OBJCT2,  Panneau nø ', NEL
c       WRITE(*,*) '  Fmat , Fsou , FMdO = ',Fmat , Fsou , FMdO,' FB'
c       WRITE(*,*) '  COUT          = ',COUT,' FB'
c	ENDIF
	        
      RETURN

  131 FORMAT('Variables_:',11(E11.4))
  132 FORMAT('PANEL n°',I2,2X,11(1X,A8,2X))

      STOP

      END
