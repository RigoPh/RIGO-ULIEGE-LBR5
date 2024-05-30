	SUBROUTINE INERTIA(NETO,Z,PHILN,TETAS,ITYPE,PART,DELT,CORRO,SYM,
     *				   NVAR,NXIT,IOPTI,YNEUT,dYNEUT,INERT,dINERT,
     *				   ICOR)					


      IMPLICIT REAL*8 (A-H,O-Z)

	REAL *8 INERT,IPANEL

	INTEGER SYM

	DIMENSION Z(NETO,4),PHILN(NETO),TETAS(NETO),ITYPE(NETO),
     *		  PART(NETO),DELT(NETO),CORRO(NETO,3)

	DIMENSION NVAR(NETO),NXIT(9,NETO)

	DIMENSION dYNEUT(9,NETO)
	DIMENSION dINERT(9,NETO)
	
	DIMENSION dIPANEL(9),dSPANEL(9)
	
	COMMON/PY/PI

c	*****************************************************************************************************

	INERT=0.
	OMET=0.
	OMESY=0.
	CALL ANNULD(dYNEUT,9*NETO)
	CALL ANNULD(dINERT,9*NETO)

c	Position de l'axe neutre
c	------------------------
	REWIND 302
	DO IPAN=1,NETO
	  READ(302) KSA,KSR,KSE,EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,		
     *            HXR,DXR,WXR,TXR,PHIL,Q,EPAIS
	  IF(ITYPE(IPAN).NE.5) THEN
	    IF(ICOR.NE.0) THEN
		  DELTPAN=DELT(IPAN)-(HXR*DXR+WXR*TXR)/EPSR-DELTA		
	      DELTA=DELTA+CORRO(IPAN,1)									
	      DXR=DXR+CORRO(IPAN,3)
		  TXR=TXR+CORRO(IPAN,3)									
		  DELTPAN=DELTPAN+(HXR*DXR+WXR*TXR)/EPSR+DELTA
	    ELSE
	      DELTPAN=DELT(IPAN)
		ENDIF		

		SPH2=VSIN(-PHILN(IPAN)/2.,0.D00)
 	    S1=VSIN(TETAS(IPAN),0.D00)
          S2=VSIN(TETAS(IPAN),-PHILN(IPAN)/2.)
		PHIL=-PHILN(IPAN)*PI/180.
		D2=Q*(S1-2.*SPH2*S2/PHIL)
	    
		OMEGA=DABS(DELTPAN*Q*PHIL)
	    OMET=OMET+PART(IPAN)*OMEGA
	    YNEU=Z(IPAN,3)+D2
	    OMESY=OMESY+PART(IPAN)*OMEGA*YNEU
	  ENDIF
	ENDDO
	YNEUT=OMESY/OMET

c	Moment d'inertie
c	----------------
	REWIND 302
	DO IPAN=1,NETO
	  READ(302) KSA,KSR,KSE,EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,		
     *	        HXR,DXR,WXR,TXR,PHIL,Q,EPAIS
	  IF(ITYPE(IPAN).NE.5) THEN
	    IF(ICOR.NE.0) THEN
		  DELTPAN=DELT(IPAN)-(HXR*DXR+WXR*TXR)/EPSR-DELTA		
	      DELTA=DELTA+CORRO(IPAN,1)									
		  DXR=DXR+CORRO(IPAN,3)
		  TXR=TXR+CORRO(IPAN,3)										
		  DELTPAN=DELTPAN+(HXR*DXR+WXR*TXR)/EPSR+DELTA
	    ELSE
	      DELTPAN=DELT(IPAN)
		ENDIF		

		SPH2=VSIN(-PHILN(IPAN)/2.,0.D00)
          C1=VCOS(TETAS(IPAN),0.D00)
	    S1=VSIN(TETAS(IPAN),0.D00)
          S2=VSIN(TETAS(IPAN),-PHILN(IPAN)/2.)
		PHIL=-PHILN(IPAN)*PI/180.
		D2=Q*(S1-2.*SPH2*S2/PHIL)
	    
		OMEGA=DABS(DELTPAN*Q*PHIL)
	    YNEU=Z(IPAN,3)+D2
	    IPANEL=((C1*Q*PHIL)**2+(S1*PART(IPAN)*DELTPAN)**2)*
     *           PART(IPAN)*DELTPAN*Q*DABS(PHIL)/12.
	    INERT=INERT+IPANEL+PART(IPAN)*OMEGA*(YNEU-YNEUT)**2
	  ENDIF
	ENDDO 
	IF(SYM.EQ.1) INERT=2*INERT				
	IF(SYM.EQ.2) INERT=4*INERT

	REWIND 302

	IF(IOPTI.GE.1) THEN

c	Sensibilités pour la position de l'axe neutre	
c	---------------------------------------------
	  DO 1 IPAN=1,NETO
	    NBRXI=NVAR(IPAN)																	
	    READ(302) KSA,KSR,KSE,EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,		
     *              HXR,DXR,WXR,TXR,PHIL,Q,EPAIS
          IF(NBRXI.EQ.0) GOTO 1
	    IF(ITYPE(IPAN).NE.5) THEN
	      IF(ICOR.NE.0) THEN
		    DELTA=DELTA+CORRO(IPAN,1)									
	        DXR=DXR+CORRO(IPAN,3)
		    TXR=TXR+CORRO(IPAN,3)
		  ENDIF										
		
		  SPH2=VSIN(-PHILN(IPAN)/2.,0.D00)
            C1=VCOS(TETAS(IPAN),0.D00)
	      S1=VSIN(TETAS(IPAN),0.D00)
            S2=VSIN(TETAS(IPAN),-PHILN(IPAN)/2.)
		  PHIL=-PHILN(IPAN)*PI/180.
		  D2=Q*(S1-2.*SPH2*S2/PHIL)
	    
	      YNEU=Z(IPAN,3)+D2

		  dSPANEL(1)=DABS(Q*PHIL)										! XI=1 epaisseur borde
		  dSPANEL(6)=DXR/EPSR*DABS(Q*PHIL)							! XI=6 haut ame raid
		  dSPANEL(7)=HXR/EPSR*DABS(Q*PHIL)							! XI=7 epaiss ame raid
		  dSPANEL(8)=TXR/EPSR*DABS(Q*PHIL)							! XI=8 larg sem raid
		  dSPANEL(9)=-(HXR*DXR+WXR*TXR)/(EPSR*EPSR)*DABS(Q*PHIL)		! XI=9 entredistance raid (EPSR)

		  DO L=1,NBRXI
	        LL=NXIT(L,IPAN)		
		    dYNEUT(LL,IPAN)=PART(IPAN)*dSPANEL(LL)*
     *						(YNEU-YNEUT)/OMET
	      ENDDO
	    ENDIF
    1	  CONTINUE

	  REWIND 302

c	Sensibilités pour le moment d'inertie	
c	-------------------------------------
	  DO 2 IPAN=1,NETO
	    NBRXI=NVAR(IPAN)																	
	    IF(NBRXI.EQ.0) GOTO 2
	    DO NEL=1,NETO
	      READ(302) KSA,KSR,KSE,EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,		
     *                HXR,DXR,WXR,TXR,PHIL,Q,EPAIS
     	      IF(ITYPE(NEL).NE.5) THEN
	        IF(ICOR.NE.0) THEN
		      DELTPAN=DELT(NEL)-(HXR*DXR+WXR*TXR)/EPSR-DELTA		
	          DELTA=DELTA+CORRO(NEL,1)									
	          DXR=DXR+CORRO(NEL,3)
		      TXR=TXR+CORRO(NEL,3)										
		      DELTPAN=DELTPAN+(HXR*DXR+WXR*TXR)/EPSR+DELTA
	        ELSE
	          DELTPAN=DELT(NEL)
		    ENDIF		
		
		    SPH2=VSIN(-PHILN(NEL)/2.,0.D00)
              C1=VCOS(TETAS(NEL),0.D00)
	        S1=VSIN(TETAS(NEL),0.D00)
              S2=VSIN(TETAS(NEL),-PHILN(NEL)/2.)
		    PHIL=-PHILN(NEL)*PI/180.
		    D2=Q*(S1-2.*SPH2*S2/PHIL)
	    
		    OMEGA=DABS(DELTPAN*Q*PHIL)
	        YNEU=Z(NEL,3)+D2

		    CALL ANNULD(dIPANEL,9)
		    CALL ANNULD(dSPANEL,9)
		    IF(NEL.EQ.IPAN) THEN
		      dIPANEL(1)=Q*DABS(PHIL)/12.*
     *	                 (PART(NEL)*((C1*Q*PHIL)**2+
     *	  	  	         (S1*PART(NEL)*DELTPAN)**2)+
     *			         PART(NEL)*DELTPAN*
     *			         (2.*(S1*PART(NEL)*DELTPAN)*S1*PART(NEL)))			! XI=1 épaisseur borde
		      dIPANEL(6)=Q*DABS(PHIL)/12.*				
     *	    	         (PART(NEL)*DXR/EPSR*((C1*Q*PHIL)**2+
     *			         (S1*PART(NEL)*DELTPAN)**2)+		
     *			         PART(NEL)*DELTPAN*
     *		             (2.*(S1*PART(NEL)*DELTPAN)*S1*
     *			         PART(NEL)*DXR/EPSR))								! XI=6 hauteur ame raidisseur
                dIPANEL(7)=Q*DABS(PHIL)/12.*				
     *	  		         (PART(NEL)*HXR/EPSR*((C1*Q*PHIL)**2+		
     *			         (S1*PART(NEL)*DELTPAN)**2)+
     *			         PART(NEL)*DELTPAN*
     *			         (2.*(S1*PART(NEL)*DELTPAN)*S1*
     *			         PART(NEL)*HXR/EPSR))								! XI=7 epaiss ame raidisseur
                dIPANEL(8)=Q*DABS(PHIL)/12.*				
     *	  		         (PART(NEL)*TXR/EPSR*((C1*Q*PHIL)**2		
     *			         +(S1*PART(NEL)*DELTPAN)**2)+
     *			         PART(NEL)*DELTPAN*
     *			         (2.*(S1*PART(NEL)*DELTPAN)*S1*
     *			         PART(NEL)*TXR/EPSR))								! XI=8 larg sem raidisseur
		      dIPANEL(9)=Q*DABS(PHIL)/12.*				
     *	  		         ((-1.)*PART(NEL)*(HXR*DXR+WXR*TXR)/
     *			         (EPSR**2)*((C1*Q*PHIL)**2+(S1*PART(NEL)*		
     *			         DELTPAN)**2)+PART(NEL)*DELTPAN*
     *			         (2.*(S1*PART(NEL)*DELTPAN)*S1*(-1.)*
     *			         PART(NEL)*(HXR*DXR+WXR*TXR)/(EPSR**2)))				! XI=9 entredistance raidisseur
		      dSPANEL(1)=DABS(Q*PHIL)											! XI=1 epaisseur borde
		      dSPANEL(6)=DXR/EPSR*DABS(Q*PHIL)								! XI=6 haut ame raid
		      dSPANEL(7)=HXR/EPSR*DABS(Q*PHIL)								! XI=7 epaiss ame raid
		      dSPANEL(8)=TXR/EPSR*DABS(Q*PHIL)								! XI=8 larg sem raid
		      dSPANEL(9)=-(HXR*DXR+WXR*TXR)/(EPSR*EPSR)*DABS(Q*PHIL)			! XI=9 entredistance raid (EPSR)
	        ENDIF

		    DO L=1,NBRXI
	          LL=NXIT(L,IPAN)		
		      dINERT(LL,IPAN)=dINERT(LL,IPAN)+dIPANEL(LL)+PART(NEL)*
     *					      dSPANEL(LL)*(YNEU-YNEUT)**2+
     *					      PART(NEL)*OMEGA*(2.*(YNEU-YNEUT)*
     *					      (-1.)*dYNEUT(LL,IPAN))
	        ENDDO
	      ENDIF
	    ENDDO
	    DO L=1,NBRXI
	      LL=NXIT(L,IPAN)
	      IF(SYM.EQ.1) dINERT(LL,IPAN)=2.*dINERT(LL,IPAN)
		  IF(SYM.EQ.2) dINERT(LL,IPAN)=4.*dINERT(LL,IPAN)
	    ENDDO
	    REWIND 302
    2	  CONTINUE

	ENDIF

      RETURN
      END
