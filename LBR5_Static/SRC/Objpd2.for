      SUBROUTINE OBJPD2(N,XI,NXI,NBRXI,OBJ,WIDTH,IPRINT,NEL,SPEC,ITT,
     * itype,isect,CORROEL)										!19.05.04 + !corrosion

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XI(9),NXI(NBRXI)
c      DIMENSION ITYPE(1),ISECT(1)								!19.05.04	!14/10/04
      DIMENSION CORROEL(3)			!corrosion ; CORROEL = vecteur de dimension 3 qui ne reprend que les epaiss de corrosion liées au panneau NEL

      COMMON/PY/PI

      COMMON/COUT/ Poids,SPoids,
     *        Fmat,Fsou,FMdO,Fmat11,Fmat22,Fmat33,Fsou11,Fsou22,
     *        Fsou33,FMdO11,FMdO22,FMdO33,FMdO44,FMdO55,FMdO66,FMdO77,
     *        REND,EQP,Dref,DrefX,DrefY,C1,C2,C3,DC1,DW2,DW3,P10,DP10,
     *        P4,P5,DP4,DP5,P9X,P9Y,DP9X,DP9Y,BER,BET,P6,P7,C8,DC8,COUT,
     *        ICOUT,IALR,IALT

C ******************************************************************************
C     Calcul de la fonction objectif POIDS (OBJ) après la subr. Conlin
C
C     Version du : Nov 2001                                Créer 11-5-95

C    - suppression de la lecture sur file 201 (10-6-96)
C    - Extra épaisseur de corrosion (nov 2001)
C ******************************************************************************

C LES VARIABLES
C -------------
C	1	DELTA =  épaisseur du bordage
C	2	HYA   =  hauteur de l'âme des aiguilles
C	3	DYA   =  épaisseur de l'âme des aiguilles
C	4	WYA   =  largeur des semelles des aiguilles
C	5	EPSA  =  entredistance entre aiguilles
C	6	HXR   =  hauteur de l'âme des raidisseurs
C	7	DXR   =  épaisseur de l'âme des raidisseurs
C	8	WXR   =  largeur des semelles des raidisseurs
C	9	EPSR  =  entredistance entre raidisseurs

C 	Autres paramètres :
C	 	TYA = épaisseur semelle aiguilles
C     	TXR = épaisseur semelle raidisseurs 

C ******************************************************************************
	
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

C     DCOR = epaisseur de corrosion pour bordé
c	IF(NEL.EQ.1) THEN
c	  REWIND 57
c	  READ(57,'(////,A1)') Abidon
c	  READ(57,*) IPAN,DCOR,DCOR2,DCOR3
c	ELSE
c	  READ(57,*) IPAN,DCOR,DCOR2,DCOR3
c	ENDIF

	DCOR  = CORROEL(1)							!corrosion
	DCOR2 = CORROEL(2)							!corrosion
	DCOR3 = CORROEL(3)							!corrosion

	IF(ISECT.EQ.3) THEN				!âme double T entree=1/2 âme	!19.05.04	!14/10/04
	HYB=2*HYA															!19.05.04
	ENDIF																!19.05.04

	IF(ITYPE.NE.5) THEN								!19.05.04		!14/10/04
	DELTA= DELTA+DCOR
	ELSE											!19.05.04
	DELTA =0.000									!19.05.04
	ENDIF											!19.05.04
c	DCOR2   ! epaisseur de corrosion pour cadres
	DYA  = DYA +DCOR2
	TYA  = TYA +DCOR2
	EPAIS = EPAIS +DCOR2							!19.05.04
c	DCOR3   ! epaisseur de corrosion pour lisses
	DXR  = DXR +DCOR3
	TXR  = TXR +DCOR3


c	IF(IPRINT.GE.1) THEN
c      WRITE(*,*) 'PANNEAU N° ',NEL
c      WRITE(*,*) 'NBRXI= ',NBRXI,' et  NXI =',NXI
c      WRITE(*,*) 'Bord = ',DELTA
c      WRITE(*,*) 'Aig  = ',HYA,DYA,WYA,TYA,'   EPSA=',EPSA
c      WRITE(*,*) 'Raid = ',HXR,DXR,WXR,TXR,'   EPSR=',EPSR
c	 PAUSE'OBJ2'
c	ENDIF

C     Impressions (avec epaiss corrosion)
C     -----------
      IF(ITT.EQ.1) THEN
      WRITE (666,132)NEL,'Ep. Bord','Hâme Cad','Eâme Cad','Lsem Cad',
     *      	'Tsem Cad','EPSA Cad','Hâme Rai','Eâme Rai','Lsem Rai',
     *        'Tsem Rai','EPSR Rai'  
      WRITE (666,131)DELTA,HYA,DYA,WYA,TYA,EPSA,HXR,DXR,WXR,TXR,EPSR
	ENDIF

C     FCT = Poids = bordage + ames + semelles  (du panneau NEL)
C     OBJ = Poids total (structure complete)


	DENS = SPEC/9.81
      TEMP=  PHIL * Q * WIDTH * PI/180.
c     TEMP=  TEMP * DENS  !si poids en Kg
      TEMP=  TEMP * SPEC  !si poids en N

	IF(ITYPE.NE.5) THEN			  !plaque		!19.05.04		!14/10/04
      FCT= DELTA 
     *  + (1.0+DW3)* ( DYA * HYA + TYA* WYA ) /EPSA
     *  + (1.0+DW2)* ( DXR * HXR + TXR* WXR ) /EPSR
c      OBJ= OBJ + FCT * TEMP															!14/10/04
	ELSE						    	! epontille						!19.05.04
	  IF(ISECT.EQ.3)THEN												!19.05.04	!14/10/04
      FCT = DELTA +														!19.05.04
c     *      (1.0+DW2)*(DXR*HXR+TXR*WXR)/EPSR +							!05.12.05
     *      (1.0+DW3)*(DYA*HYB+2*TYA*WYA)/EPSA    						!19.05.04
	  ELSEIF(ISECT.EQ.1)THEN											!19.05.04	!14/10/04
	FCT = DELTA +														!19.05.04
c     *      (1.0+DW2)*(DXR*HXR+TXR*WXR)/EPSR +							!05.12.05
     *      (1.0+DW3)*(PI*(HYA*HYA-(HYA-2*EPAIS)**2))						!19.05.04
     *	  	/(4*EPSA)													!19.05.04
	  ELSEIF(ISECT.EQ.2) THEN											!19.05.04	!14/10/04
      FCT = DELTA +														!19.05.04
c     *      (1.0+DW2)*(DXR*HXR+TXR*WXR)/EPSR +							!05.12.05
     *      (1.0+DW3)*(HYA*HYA-(HYA-2*EPAIS)**2)/EPSA						!19.05.04
	  ENDIF
	ENDIF																!19.05.04

	OBJ= OBJ + FCT * TEMP	!14/10/04
	        
      RETURN

  131 FORMAT('Variables_:',11(E11.4))
  132 FORMAT('PANEL n°',I2,2X,11(1X,A8,2X))

      STOP

      END
