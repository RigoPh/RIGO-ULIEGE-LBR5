      SUBROUTINE OBJIN1(NETO,N,NXI,NVAR,OBJ,DFCT,ITYPE,PHILN,TETAS,
     *                  Z,DELT,YNEUTPART,SYM,SYMX,PART)				!r&d13			

      IMPLICIT REAL*8(A-H,O-Z)
	INTEGER SYMX,SYM
	REAL *8 IXX
	DIMENSION NVAR(NETO)
      DIMENSION DFCT(N),NXI(9,NETO)						
      DIMENSION ITYPE(NETO)
	DIMENSION PHILN(NETO),TETAS(NETO),DELT(NETO),Z(NETO,4)
	DIMENSION DIXX(9),DS(9)
	DIMENSION PART(NETO)							
      COMMON/PY/PI

C ******************************************************************************
C     Calcul de la fonction objectif INERTIE (OBJ) et
C     Calcul des dérivées de la fonction objectif (DFCT(i), i=1,NBRXI)
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

      OMETPART=0.
	OBJ=0.
	
	DO NEL=1,NETO
	
	  READ(302) KSA,KSR,KSE,EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,
     *            HXR,DXR,WXR,TXR,PHIL,Q,EPAIS
     
        SPH2=VSIN(-PHILN(NEL)/2.,0.D00)
        C1=VCOS(TETAS(NEL),0.D00)
	  S1=VSIN(TETAS(NEL),0.D00)
        S2=VSIN(TETAS(NEL),-PHILN(NEL)/2.)
	  PHIL=-PHILN(NEL)*PI/180.
        D2=Q*(S1-2.*SPH2*S2/PHIL)
	  OMEGA=DABS(DELT(NEL)*Q*PHIL)
	  OMETPART=OMETPART+PART(NEL)*OMEGA						!r&d13
	  YNEU=Z(NEL,3)+D2
	  IXX=((C1*Q*PHIL)**2+(S1*PART(NEL)*DELT(NEL))**2)*
     *      PART(NEL)*DELT(NEL)*Q*DABS(PHIL)/12.				! ** Cas des plaques
	  FCT=IXX+PART(NEL)*OMEGA*(YNEU-YNEUTPART)**2

	  IF(SYM.EQ.1) FCT=2*FCT				
	  IF(SYM.EQ.2) FCT=4*FCT				
	
	  OBJ=OBJ+FCT

	ENDDO
	
	WRITE(666,*)
      WRITE(666,*)' Inertie autour axe neutre hor. (Subr.OBJIN1)'
      WRITE(666,*)' --------------------------------------------'
      WRITE(666,*)' INERTIE - INERTIA IYY = ',OBJ,' m**4'	

	REWIND 302

C	CALCUL DES SENSIBILITES
	NN=0
	DO 1 NEL=1,NETO
	  
	  READ(302) KSA,KSR,KSE,EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,
     *            HXR,DXR,WXR,TXR,PHIL,Q,EPAIS
        
	  NBRXI=NVAR(NEL)
	  IF (NBRXI.EQ.0) GOTO 1
        IF(ITYPE(NEL).NE.5) THEN
	    SPH2=VSIN(-PHILN(NEL)/2.,0.D00)
	    C1=VCOS(TETAS(NEL),0.D00)
	    S1=VSIN(TETAS(NEL),0.D00)
	    S2=VSIN(TETAS(NEL),-PHILN(NEL)/2.)
	    PHIL=-PHILN(NEL)*PI/180.
	    D2=Q*(S1-2.*SPH2*S2/PHIL)
	    OMEGA=DABS(DELT(NEL)*Q*PHIL)
	    YNEU=Z(NEL,3)+D2
	    DIXX(1)=Q*DABS(PHIL)/12.*
     *	        (PART(NEL)*((C1*Q*PHIL)**2+
     *		    (S1*PART(NEL)*DELT(NEL))**2)+
     *		    PART(NEL)*DELT(NEL)*
     *			(2.*(S1*PART(NEL)*DELT(NEL))*S1*PART(NEL)))			! XI=1 épaisseur borde
		DIXX(6)=Q*DABS(PHIL)/12.*				
     *	        (PART(NEL)*DXR/EPSR*((C1*Q*PHIL)**2+
     *			(S1*PART(NEL)*DELT(NEL))**2)+		
     *			PART(NEL)*DELT(NEL)*
     *			(2.*(S1*PART(NEL)*DELT(NEL))*S1*PART(NEL)*DXR/EPSR))	! XI=6 hauteur ame raidisseur
          DIXX(7)=Q*DABS(PHIL)/12.*				
     *	        (PART(NEL)*HXR/EPSR*((C1*Q*PHIL)**2+		
     *			(S1*PART(NEL)*DELT(NEL))**2)+
     *			PART(NEL)*DELT(NEL)*
     *			(2.*(S1*PART(NEL)*DELT(NEL))*S1*PART(NEL)*HXR/EPSR))	! XI=7 epaiss ame raidisseur
          DIXX(8)=Q*DABS(PHIL)/12.*				
     *	        (PART(NEL)*TXR/EPSR*((C1*Q*PHIL)**2		
     *			+(S1*PART(NEL)*DELT(NEL))**2)+
     *			PART(NEL)*DELT(NEL)*
     *			(2.*(S1*PART(NEL)*DELT(NEL))*S1*PART(NEL)*TXR/EPSR))	! XI=8 larg sem raidisseur
		DIXX(9)=Q*DABS(PHIL)/12.*				
     *	        ((-1.)*PART(NEL)*(HXR*DXR+WXR*TXR)/(EPSR**2)*
     *			((C1*Q*PHIL)**2+(S1*PART(NEL)*DELT(NEL))**2)+		
     *			PART(NEL)*DELT(NEL)*(2.*(S1*PART(NEL)*DELT(NEL))*S1*
     *			(-1.)*PART(NEL)*(HXR*DXR+WXR*TXR)/(EPSR**2)))			! XI=9 larg sem raidisseur
	    DIXX(2)=DIXX(6)/1.e3											! XI=2 hauteur ame cadre
	    DIXX(3)=DIXX(7)/1.e3											! XI=3 epaiss ame cadre
	    DIXX(4)=DIXX(8)/1.e3											! XI=4 largeur sem cadre
	    DIXX(5)=DIXX(9)/1.e3											! XI=5 entredistance cadre (EPSA)
		DS(1)=DABS(Q*PHIL)												! XI=1 epaisseur borde
	    DS(6)=DXR/EPSR*DABS(Q*PHIL)										! XI=6 haut ame raid
	    DS(7)=HXR/EPSR*DABS(Q*PHIL)										! XI=7 epaiss ame raid
	    DS(8)=TXR/EPSR*DABS(Q*PHIL)										! XI=8 larg sem raid
	    DS(9)=-(HXR*DXR+WXR*TXR)/(EPSR*EPSR)*DABS(Q*PHIL)				! XI=9 entredistance raid (EPSR)
	    DS(2)=DS(6)/1.e3												! XI=2 hauteur ame cadre
	    DS(3)=DS(7)/1.e3												! XI=3 epaiss ame cadre
	    DS(4)=DS(8)/1.e3												! XI=4 largeur sem cadre
	    DS(5)=DS(9)/1.e3												! XI=5 entredistance cadre (EPSA)
		DO I=1,NBRXI
	      J=NXI(I,NEL)		! No de ref de chaque var. de conception
		  IF(SYMX.EQ.0) THEN
	        DFCT(NN+I)=(-1.)*(DIXX(J)+PART(NEL)*DS(J)*
     *				   (YNEU-YNEUTPART)**2+
     *			       PART(NEL)*OMEGA*(2.*(YNEU-YNEUTPART)*
     *			       (-1.)*PART(NEL)*DS(J)*(YNEU-YNEUTPART)/
     *				   OMETPART))
		  ELSE
	        DFCT(NN+I)=(-1.)*(DIXX(J)+PART(NEL)*DS(J)*
     *				   ((YNEU-YNEUTPART)**2))
	      ENDIF
		  IF(SYM.EQ.1) DFCT(NN+I)=2.*DFCT(NN+I)
	      IF(SYM.EQ.2) DFCT(NN+I)=4.*DFCT(NN+I)
	    ENDDO
	  ELSE
	    DO I=1,NBRXI
	      DFCT(NN+I)=1.e-10
	    ENDDO
	  ENDIF
	  NN=NN+NBRXI
    1	CONTINUE
  
      RETURN
      END
