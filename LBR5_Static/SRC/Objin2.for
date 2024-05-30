      SUBROUTINE OBJIN2(NETO,OBJ,XI,NVAR,NXI,DELT,PHILN,TETAS,Z,			
     *				  ITT,SYMX,SYM,YNEUTPART,PART)					!r&d13
      IMPLICIT REAL*8(A-H,O-Z)
	INTEGER SYMX,SYM
	REAL *8 IXX
	DIMENSION NVAR(NETO)
      DIMENSION NXI(9,NETO),XI(1)						
      DIMENSION PHILN(NETO),TETAS(NETO),DELT(NETO),Z(NETO,4)
	DIMENSION PART(NETO)
      COMMON/PY/PI

C ******************************************************************************
C     Calcul de la fonction objectif INERTIE (OBJ)
c
C     Version du : février 2006                                Créer 14-2-06
c
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
c
C  LES VARIABLES DE CONCEPTION POUR EPONTILLE					
C  ------------------------------------------					
C	1	HYA		=	hauteur demi âme ou diam ou côté ext.	
c	2	DYA		=	épaisseur d'âme							
c	3	WYA		=	largeur de semelle						
c	4	EPAIS	=	épaisseur de paroi mince				
c	5	EPSA	=	entredistance entre épontilles			
C

C ******************************************************************************

      NC=0	
	OMESYPART=0.
	OMETPART=0.

      DO NEL=1,NETO
	  
	  NBRXI=NVAR(NEL)
	  
	  READ(302) KSA,KSR,KSE,EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,
     *            HXR,DXR,WXR,TXR,PHIL,Q,EPAIS
     
        DELT(NEL)=DELT(NEL)-(HXR*DXR+WXR*TXR)/EPSR-DELTA		
     
        DO 123 I=1,NBRXI
	    GOTO(124,125,120,126,127,128,121,129,130),NXI(I,NEL)
  124     DELTA=XI(NC+I)
          GOTO 123
  125     HYA=XI(NC+I)
          GOTO 123
  120     DYA=XI(NC+I)
          GOTO 123
  126     WYA=XI(NC+I)
          GOTO 123
  127     EPSA=XI(NC+I)
          GOTO 123
  128     HXR=XI(NC+I)
          GOTO 123
  121     DXR=XI(NC+I)
          GOTO 123
  129     WXR=XI(NC+I)
          GOTO 123
  130     EPSR=XI(NC+I)
  123   CONTINUE

        NC=NC+NBRXI

	  DELT(NEL)=DELT(NEL)+(HXR*DXR+WXR*TXR)/EPSR+DELTA
  
        SPH2=VSIN(-PHILN(NEL)/2.,0.D00)
        S1=VSIN(TETAS(NEL),0.D00)
        S2=VSIN(TETAS(NEL),-PHILN(NEL)/2.)
        PHIL=-PHILN(NEL)*PI/180.
        D2=Q*(S1-2.*SPH2*S2/PHIL)	
        OMEGA=DABS(DELT(NEL)*Q*PHIL)	
        OMETPART=OMETPART+PART(NEL)*OMEGA				! section totale
        YNEU=Z(NEL,3)+D2
	  OMESYPART=OMESYPART+PART(NEL)*OMEGA*YNEU		! moment statique vertical
	  IF(SYMX.NE.1) YNEUTPART=OMESYPART/OMETPART	! axe neutre horizontale

	  IF(ITT.EQ.1) THEN					!impression
          WRITE (666,132)NEL,'Ep. Bord','Hâme Cad','Eâme Cad',
     *       	   'Lsem Cad','Tsem Cad','EPSA Cad','Hâme Rai',
     *           'Eâme Rai','Lsem Rai','Tsem Rai','EPSR Rai'  
          WRITE (666,131)DELTA,HYA,DYA,WYA,TYA,EPSA,HXR,DXR,WXR,TXR,EPSR
	  ENDIF

	ENDDO


	REWIND 302
	
	
	DO NEL=1,NETO

	  READ(302) KSA,KSR,KSE,EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,
     *            HXR,DXR,WXR,TXR,PHIL,Q,EPAIS
     
        C1=VCOS(TETAS(NEL),0.D00)
        C3=VCOS(2.*TETAS(NEL),-PHILN(NEL))
	  SPH1=VSIN(-PHILN(NEL),0.D00)
	  SPH2=VSIN(-PHILN(NEL)/2.,0.D00)
        S1=VSIN(TETAS(NEL),0.D00)
        S2=VSIN(TETAS(NEL),-PHILN(NEL)/2.)
	  PHIL=-PHILN(NEL)*PI/180.
        D2=Q*(S1-2.*SPH2*S2/PHIL)
	  OMEGA=DABS(DELT(NEL)*Q*PHIL)
	  YNEU=Z(NEL,3)+D2
	  IXX=((C1*Q*PHIL)**2+(S1*PART(NEL)*DELT(NEL))**2)*
     *		PART(NEL)*DELT(NEL)*Q*DABS(PHIL)/12.			! ** Cas des plaques
	  FCT=IXX+PART(NEL)*OMEGA*(YNEU-YNEUTPART)**2

	  IF(SYM.EQ.1) FCT=2*FCT				
	  IF(SYM.EQ.2) FCT=4*FCT				
	
	  OBJ=OBJ+FCT
	
	ENDDO

      RETURN

  131 FORMAT('Variables_:',11(E11.4))
  132 FORMAT('PANEL n°',I2,2X,11(1X,A8,2X))

      STOP

      END
