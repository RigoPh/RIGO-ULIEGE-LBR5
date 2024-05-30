      SUBROUTINE SHEAR(NETO,NOEUD,NNO,Z,PHILN,QN,ANGLE,E,ETA,ITYPE,
     *				 PART,NVAR,NXIT,IOPTI,THICKTAU,XNEUT,
     *				 YNEUTTAU,dYNEUTTAU,ITAU,dITAU,
     *				 A,B,KLI,BLOKA,TRAV,TAU,dTAU)

	IMPLICIT REAL*8 (A-H,O-Z)

	DIMENSION TRAV(1)
	COMMON/NOEUD/NTN

	I1=1				!WQ(NETO,2)
	I2=I1+2*NETO		!TMEAN(NETO)	
	I3=I2+NETO			!LIST(NTN)
	I4=I3+NTN			!ARECIP(NTN,NTN)
	I5=I4+NTN*NTN		!COEF(NETO)
	I6=I5+NETO			!dWQ(NETO,2,NETO)
	I7=I6+2*NETO*NETO	

	IMAX=I7-1	!300+150+151+151*151+150+2*150*150=68552	!extension neto

	CALL ANNULD(TRAV,IMAX)

	CALL SSHEAR(NETO,NOEUD,NNO,Z,PHILN,QN,ANGLE,E,ETA,ITYPE,PART,
     *			NVAR,NXIT,IOPTI,THICKTAU,XNEUT,YNEUTTAU,dYNEUTTAU,
     *			ITAU,dITAU,A,B,KLI,BLOKA,TRAV(I1),TRAV(I2),
     *			TRAV(I3),TRAV(I4),TRAV(I5),TRAV(I6),TAU,dTAU)

	RETURN
      END

c	******************************************************************************************************
c	******************************************************************************************************
c	******************************************************************************************************

	SUBROUTINE SSHEAR(NETO,NOEUD,NNO,Z,PHILN,QN,ANGLE,E,ETA,ITYPE,
     *				  PART,NVAR,NXIT,IOPTI,THICKTAU,XNEUT,
     *				  YNEUTTAU,dYNEUTTAU,ITAU,dITAU,
     *				  A,B,KLI,BLOKA,WQ,TMEAN,LIST,ARECIP,
     *				  COEF,dWQ,TAU,dTAU)	

	IMPLICIT REAL*8 (A-H,O-Z)
	REAL*8 ITAU
		
	COMMON/NOEUD/NTN
	
	DIMENSION NOEUD(NETO,2),NNO(NETO+1,2),Z(NETO,4),PHILN(NETO),
     *		  QN(NETO),ANGLE(NETO),E(NETO),ETA(NETO),ITYPE(NETO),
     *		  PART(NETO)

	DIMENSION NVAR(NETO),NXIT(9,NETO)
	
	DIMENSION A(NTN,NTN),B(NTN)

	DIMENSION THICKTAU(NETO)
	DIMENSION dYNEUTTAU(9,NETO),dITAU(9,NETO)

	DIMENSION WQ(NETO,2),TMEAN(NETO)
	DIMENSION LIST(NTN)
	DIMENSION COEF(NETO)
	DIMENSION dWQ(NETO,2,NETO)

	DIMENSION TAU(NETO,3),dTAU(NETO,3,NETO)

      COMMON /PY/PI       

C*********************************************************************************

	CALL ANNULD(TAU,3*NETO)
	CALL ANNULD(dTAU,3*NETO*NETO)

	PI1=PI/180.


c	Détermination des valeurs nodales de warping pour les noeuds situés au-dessus de l'axe neutre 
c	---------------------------------------------------------------------------------------------
	NP=0
	DO 1 N=1,NTN
	  DO NEL=1,NETO
	    IF(N.EQ.NOEUD(NEL,1)) THEN
	      IF(((Z(NEL,3)-YNEUTTAU).GE.0.).AND.(PART(NEL).NE.0.)) THEN
	        NP=NP+1
			LIST(NP)=N
			GOTO 1
		  ENDIF
	    ENDIF
		IF(N.EQ.NOEUD(NEL,2)) THEN
	      IF(((Z(NEL,4)-YNEUTTAU).GE.0.).AND.(PART(NEL).NE.0.)) THEN
	        NP=NP+1
			LIST(NP)=N
			GOTO 1
		  ENDIF
	    ENDIF
   	  ENDDO
    1 CONTINUE

	CALL WARPING(NETO,NOEUD,Z,PHILN,QN,ANGLE,ITYPE,PART,NVAR,NXIT,
     *			 IOPTI,THICKTAU,YNEUTTAU,dYNEUTTAU,A,B,KLI,BLOKA,
     *			 NP,LIST,ARECIP,WQ,dWQ)


c	Détermination des valeurs nodales de warping pour les noeuds situés en-dessous de l'axe neutre 
c	----------------------------------------------------------------------------------------------
	CALL ANNULD(LIST,NP)
	NP=0
	DO 2 N=1,NTN
	  DO NEL=1,NETO
	    IF(N.EQ.NOEUD(NEL,1)) THEN
	      IF(((Z(NEL,3)-YNEUTTAU).LT.0.).AND.(PART(NEL).NE.0.)) THEN
	        NP=NP+1
			LIST(NP)=N
			GOTO 2
		  ENDIF
	    ENDIF
		IF(N.EQ.NOEUD(NEL,2)) THEN
	      IF(((Z(NEL,4)-YNEUTTAU).LT.0.).AND.(PART(NEL).NE.0.)) THEN
	        NP=NP+1
			LIST(NP)=N
			GOTO 2
		  ENDIF
	    ENDIF
   	  ENDDO
    2 CONTINUE

	CALL WARPING(NETO,NOEUD,Z,PHILN,QN,ANGLE,ITYPE,PART,NVAR,NXIT,
     *			 IOPTI,THICKTAU,YNEUTTAU,dYNEUTTAU,A,B,KLI,BLOKA,
     *			 NP,LIST,ARECIP,WQ,dWQ)

c	Détermination des contraintes de cisaillement (élément) en 3 points de l'axe local y 
c	------------------------------------------------------------------------------------
	DO NEL=1,NETO
	  IF((ITYPE(NEL).NE.5).AND.(PART(NEL).NE.0.)) THEN

c		Détermination du sens du flux de cisaillement
		Y1=Z(NEL,3)-YNEUTTAU
		Y2=Z(NEL,4)-YNEUTTAU
		COEF(NEL)=1.
		IF((Y1*Y2).LT.0.) GOTO 3						!extrémités de l'élément de part et d'autre de l'axe neutre
		IF(DABS(Y1).LT.DABS(Y2)) COEF(NEL)=-1.			!distance à l'axe neutre augmente suivant le sens de l'élément => sens de l'élément opposé à celui du flux de cisaillement
		IF(DABS(Y1-Y2).LT.0.001) THEN					!élément horizontal
	      K=0
		  DO IPAN=1,NETO								!flux de cisaillement provient du noeud avec condition de bord (libre ou axe symétrie) le plus proche 
		    IF(DABS(Z(IPAN,3)-Z(NEL,3)).LT.0.001) THEN
	          IF(NNO(NOEUD(IPAN,1),2).NE.0) THEN
				K=K+1
				IF(K.GE.1) THEN
				  IF(DABS(Z(NEL,1)-XCONDI).GT.
     *				 DABS(Z(NEL,1)-Z(IPAN,1))) THEN
				    XCONDI=Z(IPAN,1)
				  ENDIF
				ELSE
	              XCONDI=Z(IPAN,1)
				ENDIF
	          ENDIF
			  IF(NNO(NOEUD(IPAN,2),2).NE.0) THEN
			    K=K+1
				IF(K.GE.1) THEN
				  IF(DABS(Z(NEL,1)-XCONDI).GT.
     *				 DABS(Z(NEL,1)-Z(IPAN,2))) THEN
				    XCONDI=Z(IPAN,2)
				  ENDIF
				ELSE
	              XCONDI=Z(IPAN,2)
				ENDIF
	          ENDIF
			ENDIF
		  ENDDO
		  IF(K.EQ.0) XCONDI=XNEUT						!si pas de condition de bord à la même ordonnée Z, flux de cisaillement provient de l'axe neutre vertical
		  X1=Z(NEL,1)-XCONDI
		  X2=Z(NEL,2)-XCONDI
	      IF(DABS(X1).GT.DABS(X2)) THEN
	        COEF(NEL)=-1.
	      ELSE
	        COEF(NEL)=1.
	      ENDIF
	    ENDIF
   		IF(Y1.LT.0.) COEF(NEL)=-COEF(NEL)				!valeur opposée de coef pour les éléments situés sous l'axe neutre vu expression de tau(nel,i)

    3	    HIGHT=DABS(PHILN(NEL)*QN(NEL)*PI1)
		TMEAN(NEL)=DABS(WQ(NEL,2)-WQ(NEL,1))/HIGHT
		TETA=-ANGLE(NEL)*PI/180.
   		DO I=1,3
	      S=(I-1)*HIGHT/2.
		  IF((Y1*Y2).LT.0.) THEN						!extrémités de l'élément de part et d'autre de l'axe neutre
	        IF((Y1*(Y1+S*SIN(TETA))).GE.0.) THEN
			  COEF(NEL)=1.
	          IF(Y1.LT.0.) COEF(NEL)=-1.
			  BREADTH=DABS(Y1/SIN(TETA))
			  TMEAN(NEL)=DABS(0.-WQ(NEL,1))/BREADTH
	          TAU(NEL,I)=TMEAN(NEL)+COEF(NEL)*(Y1*(S-BREADTH/2.)+
     *			         SIN(TETA)/2.*(S**2-BREADTH**2/3.))
			ELSE
	          COEF(NEL)=1.
	          IF(Y1.LT.0.) COEF(NEL)=-1.
			  BREADTH=DABS(Y2/SIN(TETA))
	          TMEAN(NEL)=DABS(WQ(NEL,2)-0.)/BREADTH
	          SS=S-DABS(Y1/SIN(TETA))
	          TAU(NEL,I)=TMEAN(NEL)+COEF(NEL)*(0.*(SS-BREADTH/2.)+
     *			         SIN(TETA)/2.*(SS**2-BREADTH**2/3.))
			ENDIF
		  ELSE
		    TAU(NEL,I)=TMEAN(NEL)+COEF(NEL)*(Y1*(S-HIGHT/2.)+
     *			       SIN(TETA)/2.*(S**2-HIGHT**2/3.))
		  ENDIF
		ENDDO
	  ENDIF
	ENDDO


c	Vérification de la conservation du flux de cisaillement à la jonction de 2 éléments horizontaux (modification éventuelle de coef)
c	-----------------------------------------------------------------------------------------------
	DO NEL=1,NETO
	  IF((ITYPE(NEL).NE.5).AND.(PART(NEL).NE.0.)) THEN
	    Y1=Z(NEL,3)-YNEUTTAU
	    Y2=Z(NEL,4)-YNEUTTAU
	    IF(DABS(Y1-Y2).LT.0.001) THEN
		  FLW=0.
	      K=0
		  DO IPAN=1,NETO
	        IF((ITYPE(IPAN).NE.5).AND.(IPAN.NE.NEL)) THEN
			  IF(NOEUD(NEL,1).EQ.NOEUD(IPAN,1)) THEN
	            FLW=FLW+TAU(IPAN,1)*PART(IPAN)*THICKTAU(IPAN)
				K=K+1
	          ELSEIF(NOEUD(NEL,1).EQ.NOEUD(IPAN,2)) THEN
	            FLW=FLW+TAU(IPAN,3)*PART(IPAN)*THICKTAU(IPAN)
	            K=K+1
	          ENDIF
	        ENDIF
	      ENDDO
		  IF(K.EQ.1) THEN
	        IF(DABS(PART(NEL)*THICKTAU(NEL)*TAU(NEL,1)-FLW).
     *		GT.0.001) THEN
	          COEF(NEL)=-COEF(NEL)
	          GOTO 4
	        ENDIF
	      ENDIF
	      FLW=0.
	      K=0
		  DO IPAN=1,NETO
	        IF((ITYPE(IPAN).NE.5).AND.(IPAN.NE.NEL)) THEN
			  IF(NOEUD(NEL,2).EQ.NOEUD(IPAN,1)) THEN
	            FLW=FLW+TAU(IPAN,1)*PART(IPAN)*THICKTAU(IPAN)
				K=K+1
	          ELSEIF(NOEUD(NEL,2).EQ.NOEUD(IPAN,2)) THEN
	            FLW=FLW+TAU(IPAN,3)*PART(IPAN)*THICKTAU(IPAN)
	            K=K+1
	          ENDIF
	        ENDIF
	      ENDDO
		  IF(K.EQ.1) THEN
	        IF(DABS(PART(NEL)*THICKTAU(NEL)*TAU(NEL,3)-FLW).
     *		GT.0.001) THEN
	          COEF(NEL)=-COEF(NEL)
	          GOTO 4
	        ENDIF
	      ENDIF
    4	      DO I=1,3
	        HIGHT=DABS(PHILN(NEL)*QN(NEL)*PI1)
			TETA=-ANGLE(NEL)*PI/180.
		    S=(I-1)*HIGHT/2
		    TAU(NEL,I)=TMEAN(NEL)+COEF(NEL)*(Y1*(S-HIGHT/2.)+
     *			       SIN(TETA)/2.*(S**2-HIGHT**2/3.))
		  ENDDO
		ENDIF
	  ENDIF
	ENDDO 

	DO NEL=1,NETO
	  IF((ITYPE(NEL).NE.5).AND.(PART(NEL).NE.0)) THEN
	    DO I=1,3
	      TAU(NEL,I)=PART(NEL)*TAU(NEL,I)/ITAU
		ENDDO
	  ENDIF
	ENDDO


c	Détermination des sensibilités des contraintes de cisaillement par rapport aux variables de conception (ép. de tôle)
c	--------------------------------------------------------------------------------------------------------------------
	IF(IOPTI.GE.1) THEN
	  DO 5 IPAN=1,NETO
	    NBRXI=NVAR(IPAN)
	    IF(NBRXI.EQ.0) GOTO 5
	    DO L=1,NBRXI
	      IF(NXIT(L,IPAN).EQ.1) GOTO 6
	    ENDDO
	    GOTO 5
    6	    DO NEL=1,NETO
	      IF((ITYPE(NEL).NE.5).AND.(PART(NEL).NE.0.)) THEN
	        HIGHT=DABS(PHILN(NEL)*QN(NEL)*PI1)
		    IF((WQ(NEL,2)-WQ(NEL,1)).GE.0.) THEN
			  dTMEAN=(dWQ(NEL,2,IPAN)-dWQ(NEL,1,IPAN))/HIGHT
	        ELSE
			  dTMEAN=(dWQ(NEL,1,IPAN)-dWQ(NEL,2,IPAN))/HIGHT
	        ENDIF
		    TETA=-ANGLE(NEL)*PI/180.
   		    Y1=Z(NEL,3)-YNEUTTAU
		    dY1=-dYNEUTTAU(1,IPAN)
		    Y2=Z(NEL,4)-YNEUTTAU
		    dY2=-dYNEUTTAU(1,IPAN)
		    DO I=1,3
	          S=(I-1)*HIGHT/2.
		      IF((Y1*Y2).LT.0.) THEN						!extrémités de l'élément de part et d'autre de l'axe neutre
	            IF((Y1*(Y1+S*SIN(TETA))).GE.0.) THEN
			      IF(Y1.GE.0.) THEN
				    BREADTH=Y1/DABS(SIN(TETA))
				    dBREADTH=dY1/DABS(SIN(TETA))
				  ELSE
	                BREADTH=-Y1/DABS(SIN(TETA))
				    dBREADTH=-dY1/DABS(SIN(TETA))
				  ENDIF
				  IF(WQ(NEL,1).GE.0.) THEN
				    dTMEAN=(dWQ(NEL,1,IPAN)*BREADTH-WQ(NEL,1)*
     *				       dBREADTH)/BREADTH**2
				  ELSE
				    dTMEAN=-(dWQ(NEL,1,IPAN)*BREADTH-WQ(NEL,1)*
     *				       dBREADTH)/BREADTH**2
				  ENDIF
				  dTAU(NEL,I,IPAN)=dTMEAN+COEF(NEL)*(dY1*
     *							   (S-BREADTH/2.)+Y1*(S-dBREADTH/2.)+
     *							   SIN(TETA)/2.*(S**2-2./3.*BREADTH*
     *							   dBREADTH))
			    ELSE
	              IF(Y2.GE.0.) THEN
				    BREADTH=Y2/DABS(SIN(TETA))
				    dBREADTH=dY2/DABS(SIN(TETA))
				  ELSE
	                BREADTH=-Y2/DABS(SIN(TETA))
				    dBREADTH=-dY2/DABS(SIN(TETA))
				  ENDIF
	              IF(WQ(NEL,2).GE.0.) THEN
				    dTMEAN=(dWQ(NEL,2,IPAN)*BREADTH-WQ(NEL,2)*
     *				       dBREADTH)/BREADTH**2
				  ELSE
				    dTMEAN=-(dWQ(NEL,2,IPAN)*BREADTH-WQ(NEL,2)*
     *				       dBREADTH)/BREADTH**2
				  ENDIF
				  SS=S-DABS(Y1/SIN(TETA))
				  IF(Y1.GE.0.) THEN
				    SS=S-Y1/DABS(SIN(TETA))
				    dSS=-dY1/DABS(SIN(TETA))
				  ELSE
				    SS=S+Y1/DABS(SIN(TETA))
				    dSS=dY1/DABS(SIN(TETA))
				  ENDIF
				  dTAU(NEL,I,IPAN)=dTMEAN+COEF(NEL)*(SIN(TETA)/2.*
     *						       (2.*SS*dSS-2./3.*BREADTH*dBREADTH))
			    ENDIF
		      ELSE
			    dTAU(NEL,I,IPAN)=dTMEAN+COEF(NEL)*dY1*(S-HIGHT/2.)				   
		      ENDIF
			  dTAU(NEL,I,IPAN)=PART(NEL)*(dTAU(NEL,I,IPAN)*ITAU-
     *						   TAU(NEL,I)*ITAU/PART(NEL)*
     *						   dITAU(1,IPAN))/ITAU**2
		    ENDDO
		  ENDIF
	    ENDDO
    5	  CONTINUE
      ENDIF	

      RETURN
      END


C	****************************************************************************
C	****************************************************************************

	SUBROUTINE WARPING(NETO,NOEUD,Z,PHILN,QN,ANGLE,ITYPE,PART,
     *				   NVAR,NXIT,IOPTI,THICKTAU,YNEUTTAU,dYNEUTTAU,
     *				   A,B,KLI,BLOKA,NP,LIST,ARECIP,WQ,dWQ)

	IMPLICIT REAL*8 (A-H,O-Z)
	INTEGER*2 KLI

      DIMENSION NOEUD(NETO,2),Z(NETO,4),PHILN(NETO),QN(NETO),
     *		  ANGLE(NETO),ITYPE(NETO),PART(NETO),THICKTAU(NETO)

	DIMENSION NVAR(NETO),NXIT(9,NETO)

	DIMENSION WQ(NETO,2),dWQ(NETO,2,NETO)

	DIMENSION dYNEUTTAU(9,NETO)

	DIMENSION A(NP,NP),B(NP),KLI(NP,2),BLOKA(NP,NP)

	DIMENSION LIST(NP),ARECIP(NP,NP)

	COMMON /PY/PI 

C*********************************************************************************
C*********************************************************************************

	CALL ANNULD(A,NP*NP)
	CALL ANNULD(B,NP)

	CALL ANNULD(ARECIP,NP*NP)

	PI1=PI/180.


c	Assemblage des matrices pour la résolution du système d'équations (dPI/dwq=0, PI=U-W: total potential energy of the system) 
c	---------------------------------------------------------------------------------------------------------------------------
	DO I=1,NP
	  DO NEL=1,NETO
		IF(ITYPE(NEL).NE.5) THEN
	      HIGHT=DABS(PHILN(NEL)*QN(NEL)*PI1)
		  TETA=-ANGLE(NEL)*PI/180.
		  IF(LIST(I).EQ.NOEUD(NEL,1)) THEN
	        FACT=1.
			Y1=Z(NEL,3)-YNEUTTAU
			IF(((Z(NEL,3)-YNEUTTAU)*(Z(NEL,4)-YNEUTTAU)).LT.0.) THEN	!extrémités de l'élément de part et d'autre de l'axe neutre
			  HIGHT=DABS((Z(NEL,3)-YNEUTTAU)/SIN(TETA))
			  GOTO 1
			ENDIF
		    DO J=1,NP
			  IF(NOEUD(NEL,2).EQ.LIST(J)) THEN
			    A(I,J)=-PART(NEL)*THICKTAU(NEL)/HIGHT
	          ENDIF
			ENDDO
    1			A(I,I)=A(I,I)+PART(NEL)*THICKTAU(NEL)/HIGHT
		    B(I)=B(I)+0.5*PART(NEL)*THICKTAU(NEL)*HIGHT*
     *		     (Y1+FACT/3.*HIGHT*SIN(TETA))			
		  ELSEIF(LIST(I).EQ.NOEUD(NEL,2)) THEN
	        FACT=2.
			Y1=Z(NEL,3)-YNEUTTAU
			IF(((Z(NEL,3)-YNEUTTAU)*(Z(NEL,4)-YNEUTTAU)).LT.0.) THEN	!extrémités de l'élément de part et d'autre de l'axe neutre
	          HIGHT=DABS((Z(NEL,4)-YNEUTTAU)/SIN(TETA))
	          Y1=0.
			  GOTO 2
			ENDIF
		    DO J=1,NP
			  IF(NOEUD(NEL,1).EQ.LIST(J)) THEN
			    A(I,J)=-PART(NEL)*THICKTAU(NEL)/HIGHT
	          ENDIF
			ENDDO
    2			A(I,I)=A(I,I)+PART(NEL)*THICKTAU(NEL)/HIGHT
		    B(I)=B(I)+0.5*PART(NEL)*THICKTAU(NEL)*HIGHT*
     *		     (Y1+FACT/3.*HIGHT*SIN(TETA))
		  ENDIF
	    ENDIF
	  ENDDO
	ENDDO


c	Résolution du système d'équations (dPI/dwq=0, PI=U-W: total potential energy of the system) 
c	-------------------------------------------------------------------------------------------
	DO I=1,NP
	  DO J=1,NP
	    ARECIP(I,J)=A(I,J)
	  ENDDO
	ENDDO

	CALL RECIP(NP,ARECIP,BLOKA,KLI(1,1),KLI(1,2))

	DO I=1,NP
	  DO NEL=1,NETO
	    DO J=1,NP
	      IF(ITYPE(NEL).NE.5) THEN
	        IF(LIST(I).EQ.NOEUD(NEL,1)) THEN
	          WQ(NEL,1)=WQ(NEL,1)+ARECIP(I,J)*B(J)
			ELSEIF(LIST(I).EQ.NOEUD(NEL,2)) THEN
	          WQ(NEL,2)=WQ(NEL,2)+ARECIP(I,J)*B(J)
			ENDIF
	      ENDIF
		ENDDO
	  ENDDO
	ENDDO


c	Sensibilités des valeurs nodales de warping WQ par rapport aux variables de conception (ép. de tôle)
c	----------------------------------------------------------------------------------------------------
	IF(IOPTI.GE.1) THEN
	  DO 6 IPAN=1,NETO
	    NBRXI=NVAR(IPAN)
	    IF(NBRXI.EQ.0) GOTO 6
	    DO L=1,NBRXI
	      IF(NXIT(L,IPAN).EQ.1) GOTO 7
	    ENDDO
	    GOTO 6
    7	    CALL ANNULD(A,NP*NP)
	    CALL ANNULD(B,NP)
	    DO I=1,NP
	      DO NEL=1,NETO
		    IF(ITYPE(NEL).NE.5) THEN
	          HIGHT=DABS(PHILN(NEL)*QN(NEL)*PI1)
		      dHIGHT=0.
		      TETA=-ANGLE(NEL)*PI/180.
			  IF(NEL.EQ.IPAN) THEN
			    dTHICKTAU=1.
			  ELSE
			    dTHICKTAU=0.
			  ENDIF
		      IF(LIST(I).EQ.NOEUD(NEL,1)) THEN
	            FACT=1.
			    Y1=Z(NEL,3)-YNEUTTAU
			    dY1=-dYNEUTTAU(1,IPAN)
			    IF(((Z(NEL,3)-YNEUTTAU)*(Z(NEL,4)-YNEUTTAU)).LT.0.) 		!extrémités de l'élément de part et d'autre de l'axe neutre
     *		    THEN
				  IF((Z(NEL,3)-YNEUTTAU).GE.0.) THEN
				    HIGHT=(Z(NEL,3)-YNEUTTAU)/DABS(SIN(TETA))
				    dHIGHT=-dYNEUTTAU(1,IPAN)/DABS(SIN(TETA))
				  ELSE
	                HIGHT=-(Z(NEL,3)-YNEUTTAU)/DABS(SIN(TETA))
				    dHIGHT=dYNEUTTAU(1,IPAN)/DABS(SIN(TETA))
				  ENDIF
			      GOTO 3
			    ENDIF
		        DO J=1,NP
			      IF(NOEUD(NEL,2).EQ.LIST(J)) THEN
			        A(I,J)=-PART(NEL)*(dTHICKTAU*HIGHT-THICKTAU(NEL)*
     *					   dHIGHT)/HIGHT**2
	              ENDIF
			    ENDDO
    3			    A(I,I)=A(I,I)+PART(NEL)*(dTHICKTAU*HIGHT-
     *			       THICKTAU(NEL)*dHIGHT)/HIGHT**2
			    B(I)=B(I)+0.5*PART(NEL)*(dTHICKTAU*HIGHT*
     *			     (Y1+FACT/3.*HIGHT*SIN(TETA))+THICKTAU(NEL)*
     *			     ((dHIGHT*Y1+FACT/3.*HIGHT*SIN(TETA))+HIGHT*(dY1+
     *			     FACT/3.*dHIGHT*SIN(TETA))))	
		      ELSEIF(LIST(I).EQ.NOEUD(NEL,2)) THEN
	            FACT=2.
			    Y1=Z(NEL,3)-YNEUTTAU
	            dY1=-dYNEUTTAU(1,IPAN)
			    IF(((Z(NEL,3)-YNEUTTAU)*(Z(NEL,4)-YNEUTTAU)).LT.0.) 		!extrémités de l'élément de part et d'autre de l'axe neutre
     *            THEN
				  IF((Z(NEL,4)-YNEUTTAU).GE.0.) THEN
				    HIGHT=(Z(NEL,4)-YNEUTTAU)/DABS(SIN(TETA))
				    dHIGHT=-dYNEUTTAU(1,IPAN)/DABS(SIN(TETA))
				  ELSE
	                HIGHT=-(Z(NEL,4)-YNEUTTAU)/DABS(SIN(TETA))
				    dHIGHT=dYNEUTTAU(1,IPAN)/DABS(SIN(TETA))
				  ENDIF
	              Y1=0.
				  dY1=0.
			      GOTO 4
			    ENDIF
		        DO J=1,NP
			      IF(NOEUD(NEL,1).EQ.LIST(J)) THEN
			        A(I,J)=-PART(NEL)*(dTHICKTAU*HIGHT-THICKTAU(NEL)*
     *					   dHIGHT)/HIGHT**2
	              ENDIF
			    ENDDO
    4			    A(I,I)=A(I,I)+PART(NEL)*(dTHICKTAU*HIGHT-
     *			       THICKTAU(NEL)*dHIGHT)/HIGHT**2
			    B(I)=B(I)+0.5*PART(NEL)*(dTHICKTAU*HIGHT*
     *			     (Y1+FACT/3.*HIGHT*SIN(TETA))+THICKTAU(NEL)*
     *			     (dHIGHT*(Y1+FACT/3.*HIGHT*SIN(TETA))+HIGHT*(dY1+
     *			     FACT/3.*dHIGHT*SIN(TETA))))	
		      ENDIF
	        ENDIF
	      ENDDO
	    ENDDO
	    DO I=1,NP
	      DO J=1,NP
	        DO NEL=1,NETO
	          IF(ITYPE(NEL).NE.5) THEN
	            IF(LIST(J).EQ.NOEUD(NEL,1)) THEN
	              WARP=WQ(NEL,1)
			      GOTO 5
			    ELSEIF(LIST(J).EQ.NOEUD(NEL,2)) THEN
	              WARP=WQ(NEL,2)
			      GOTO 5
			    ENDIF
			  ENDIF
	        ENDDO
    5	        B(I)=B(I)-A(I,J)*WARP
	      ENDDO
	    ENDDO
	    DO I=1,NP
		  DO NEL=1,NETO
	        DO J=1,NP
	          IF(ITYPE(NEL).NE.5) THEN
	            IF(LIST(I).EQ.NOEUD(NEL,1)) THEN
			      dWQ(NEL,1,IPAN)=dWQ(NEL,1,IPAN)+ARECIP(I,J)*B(J)
	            ELSEIF(LIST(I).EQ.NOEUD(NEL,2)) THEN
	              dWQ(NEL,2,IPAN)=dWQ(NEL,2,IPAN)+ARECIP(I,J)*B(J)
	            ENDIF
	          ENDIF
		    ENDDO
	      ENDDO
	    ENDDO
    6	  CONTINUE
      ENDIF

      RETURN
      END