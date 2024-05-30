      SUBROUTINE OBJPD1(NEL,NETO,N,NN,NXI,NBRXI,OBJ,DFCT,WIDTH,	!03.02.06
     *                  IMPR,SPEC,ITYPE,ISECT,CORRO)			!f�vrier 2004  + !corrosion   !obj inertie

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION DFCT(N),NXI(NBRXI)						!03.02.06
      DIMENSION ITYPE(1),ISECT(1)							!f�vrier 2004
      DIMENSION CORRO(NETO,3)			!corrosion
      COMMON/PY/PI

      COMMON/COUT/ Poids,SPoids,
     *        Fmat,Fsou,FMdO,Fmat11,Fmat22,Fmat33,Fsou11,Fsou22,
     *        Fsou33,FMdO11,FMdO22,FMdO33,FMdO44,FMdO55,FMdO66,FMdO77,
     *        REND,EQP,Dref,DrefX,DrefY,C1,C2,C3,DC1,DW2,DW3,P10,DP10,
     *        P4,P5,DP4,DP5,P9X,P9Y,DP9X,DP9Y,BER,BET,P6,P7,C8,DC8,COUT,
     *        ICOUT,IALR,IALT
C ******************************************************************************
C     Calcul de la fonction objectif POIDS (OBJ) et
C     Calcul des d�riv�es de la fonction objectif (DFCT(i), i=1,NBRXI)
c
C     Version du : nov 2001                                Cr�er 29-4-94
c
C ******************************************************************************

C LES VARIABLES
C -------------
C	1	DELTA =  �paisseur du bordage
C	2	HYA   =  hauteur de l'�me des aiguilles
C	3	DYA   =  �paisseur de l'�me des aiguilles
C	4	WYA   =  largeur des semelles des aiguilles
C	5	EPSA  =  entredistance entre aiguilles
C	6	HXR   =  hauteur de l'�me des raidisseurs
C	7	DXR   =  �paisseur de l'�me des raidisseurs
C	8	WXR   =  largeur des semelles des raidisseurs
C	9	EPSR  =  entredistance entre raidisseurs

C 	Autres param�tres :
C	 	TYA = �paisseur semelle aiguilles
C     	TXR = �paisseur semelle raidisseurs 
c
C  LES VARIABLES DE CONCEPTION POUR EPONTILLE					!f�vrier 2004
C  ------------------------------------------					
C	1	HYA		=	hauteur demi �me ou diam ou c�t� ext.	
c	2	DYA		=	�paisseur d'�me							
c	3	WYA		=	largeur de semelle						
c	4	EPAIS	=	�paisseur de paroi mince				
c	5	EPSA	=	entredistance entre �pontilles			
C

C ******************************************************************************

	
	READ(302) KSA,KSR,KSE,EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,	!extension neto
     *		 HXR,DXR,WXR,TXR,PHIL,Q,EPAIS					!f�vrier 2004

C     DCOR = epaisseur de corrosion pour bord�
c	IF(NEL.EQ.1) THEN
c	  REWIND 57
c	  READ(57,'(////,A1)') Abidon
c	  READ(57,*) IPAN,DCOR,DCOR2,DCOR3
c	ELSE
c	  READ(57,*) IPAN,DCOR,DCOR2,DCOR3
c	ENDIF

	DCOR  = CORRO(NEL,1)							!corrosion
	DCOR2 = CORRO(NEL,2)							!corrosion
	DCOR3 = CORRO(NEL,3)							!corrosion

	IF(ISECT(1).EQ.3) THEN				!�me double T entree=1/2 �me	!f�vrier 2004
	HYB=2*HYA															!f�vrier 2004
	ENDIF																!f�vrier 2004

	IF(ITYPE(1).NE.5) THEN								!f�vrier 2004
	DELTA= DELTA+DCOR
	ELSE											!f�vrier 2004
	DELTA =0.000									!f�vrier 2004
	ENDIF											!f�vrier 2004
c	DCOR2   ! epaisseur de corrosion pour cadres
	DYA  = DYA +DCOR2
	TYA  = TYA +DCOR2
	EPAIS = EPAIS +DCOR2							!f�vrier 2004
c	DCOR3   ! epaisseur de corrosion pour lisses
	DXR  = DXR +DCOR3
	TXR  = TXR +DCOR3

c      IF(IPRINT.GE.1) THEN
c      WRITE(*,*) 'dans OBJECT,  NBRXI = ',NBRXI
c      WRITE(*,*) 'Bord = ',DELTA
c      WRITE(*,*) 'cadre= ',HYA,DYA,WYA,TYA,'   EPSA=',EPSA
c      WRITE(*,*) 'Raid = ',HXR,DXR,WXR,TXR,'   EPSR=',EPSR
c	 ENDIF

C     FCT = Poids = bordage + ames + semelles  (du panneau NEL)
C     OBJ = Poids total (structure complete)

	DENS = SPEC/9.81
      TEMP=  PHIL * Q * WIDTH * PI/180.
c     TEMP=  TEMP * DENS  !si poids en Kg
      TEMP=  TEMP * SPEC  !si poids en N

	IF(ITYPE(1).NE.5) THEN			  !plaque		!f�vrier 2004
      FCT= DELTA 
     *  + (1.0+DW3)* ( DYA * HYA + TYA* WYA ) /EPSA
     *  + (1.0+DW2)* ( DXR * HXR + TXR* WXR ) /EPSR
	ELSE						    	! epontille						!f�vrier 2004
	  IF(ISECT(1).EQ.3)THEN												!f�vrier 2004
      FCT = DELTA +														!f�vrier 2004
c     *      (1.0+DW2)*(DXR*HXR+TXR*WXR)/EPSR +							!05.12.05
     *      (1.0+DW3)*(DYA*HYB+2*TYA*WYA)/EPSA    						!f�vrier 2004
	  ELSEIF(ISECT(1).EQ.1)THEN											!f�vrier 2004
	FCT = DELTA +														!f�vrier 2004
c     *      (1.0+DW2)*(DXR*HXR+TXR*WXR)/EPSR +							!05.12.05
     *      (1.0+DW3)*(PI*(HYA*HYA-(HYA-2*EPAIS)**2))						!f�vrier 2004
     *	  	/(4*EPSA)													!f�vrier 2004
	  ELSEIF(ISECT(1).EQ.2) THEN										!f�vrier 2004
      FCT = DELTA +														!f�vrier 2004
c     *      (1.0+DW2)*(DXR*HXR+TXR*WXR)/EPSR +							!05.12.05
     *      (1.0+DW3)*(HYA*HYA-(HYA-2*EPAIS)**2)/EPSA						!20.02.04
	  ENDIF
	ENDIF																!f�vrier 2004
      PPT= FCT * TEMP ! poids du panneau
      OBJ= OBJ + PPT  ! poids des panneaux (cumule)
	      
	IF ((IMPR.NE.0).AND.(NEL.EQ.1)) THEN								!obj inertie
	  WRITE(666,*)
	  WRITE(666,*)'*** WEIGHT Objective Function (Subr. OBJPD1)'
	  WRITE(666,*)'    ----------------------------------------'
	  WRITE(666,*)'Panneau/Panel   POIDS-WEIGHT (N)'						!03.02.06
	  WRITE(666,42) NEL,PPT
   42   FORMAT(5x,I3,15x,E14.7)
	ENDIF

      IF ((IMPR.NE.0).AND.(NEL.EQ.NETO)) THEN								!obj inertie
	  WRITE(666,*)
        WRITE(666,*)' POIDS TOTAL pour toute la structure (Subr.OBJPD1)'
        WRITE(666,*)' -------------------------------------------------'
        WRITE(666,*)'  POIDS - WEIGHT = ',OBJ,' N'						!03.02.06
	ENDIF

C     CALCUL DES SENSITIVITES
	DO 101 I=1,NBRXI
	II=NN+I
	IF(NXI(I).EQ.1) THEN
      DFCT(II)= TEMP
	ELSE IF (NXI(I).EQ.2) THEN
      DFCT(II)= TEMP  * (1.0+DW3)*DYA/EPSA
	ELSE IF (NXI(I).EQ.3) THEN
      DFCT(II)= TEMP  * (1.0+DW3)*HYA/EPSA
	ELSE IF (NXI(I).EQ.4) THEN
      DFCT(II)= TEMP  * (1.0+DW3)*TYA/EPSA
	ELSE IF (NXI(I).EQ.5) THEN
      DFCT(II)= -TEMP * (1.0+DW3)*(DYA*HYA+TYA*WYA)/(EPSA*EPSA) 
	ELSE IF (NXI(I).EQ.6) THEN
      DFCT(II)= TEMP  * (1.0+DW2)*DXR/EPSR
	ELSE IF (NXI(I).EQ.7) THEN
      DFCT(II)= TEMP  * (1.0+DW2)*HXR/EPSR
	ELSE IF (NXI(I).EQ.8) THEN
      DFCT(II)= TEMP  * (1.0+DW2)*TXR/EPSR
	ELSE IF (NXI(I).EQ.9) THEN
      DFCT(II)= -TEMP * (1.0+DW2)*(DXR*HXR+TXR*WXR)/(EPSR*EPSR) 
	ENDIF
  101 CONTINUE
  
      RETURN
      END
