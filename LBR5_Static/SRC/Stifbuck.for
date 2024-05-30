      SUBROUTINE STIFBUCK(NEL,NETO,E,SIGY,COEFK,PHILN,NVAR,NXIT,		!r&d15
     *					EPSA,EPSR,DELTA,HXR,DXR,WXR,TXR,Q,
     *					EPSA2,WYA2,SIG,dSIG,TRAV,
     *					NTOT,DSIGSTIF,dDSIGSTIF)


      IMPLICIT REAL*8 (A-H,O-Z)
	DIMENSION TRAV(1)

	DIMENSION SIG(2)
	
	I1=1			!dPSI(9,NETO)
	I2=I1+9*NETO	!dM(9,NETO)
	I3=I2+9*NETO	!dSIGEPL(9,NETO)
	I4=I3+9*NETO	!dKP(9,NETO)
	I5=I4+9*NETO	!dC(9,NETO)
	I6=I5+9*NETO	!dK(9,NETO)
	I7=I6+9*NETO	!dSIGESTIF(9,NETO)
	I8=I7+9*NETO	!dDSIGPL(9,NETO)
	I9=I8+9*NETO		

	IMAX=I9-1	!72*NETO

	CALL SSTIFBUCK(NEL,NETO,E,SIGY,COEFK,PHILN,NVAR,NXIT,			!r&d15
     *			   EPSA,EPSR,DELTA,HXR,DXR,WXR,TXR,Q,
     *			   EPSA2,WYA2,SIG,dSIG,
     *		       TRAV(I1),TRAV(I2),TRAV(I3),TRAV(I4),
     *			   TRAV(I5),TRAV(I6),TRAV(I7),TRAV(I8),
     *			   NTOT,DSIGSTIF,dDSIGSTIF)

	RETURN
	END

c	******************************************************************************************************
c	******************************************************************************************************
c	******************************************************************************************************

	SUBROUTINE SSTIFBUCK(NEL,NETO,E,SIGY,COEFK,PHILN,NVAR,NXIT,		!r&d15
     *					 EPSA,EPSR,DELTA,HXR,DXR,WXR,TXR,Q,
     *					 EPSA2,WYA2,SIG,dSIG,
     *					 dPSI,dM,dSIGEPL,dKP,dC,dK,dSIGESTIF,
     *					 dDSIGPL,NTOT,DSIGSTIF,dDSIGSTIF)


      IMPLICIT REAL*8 (A-H,O-Z)

	REAL *8 M,IA,IT,IP,IW,MSTAT,KP,K

	INTEGER FIN

	DIMENSION SIG(2),dSIG(2,9,NETO)
	
	DIMENSION NVAR(NETO),NXIT(9,NETO)
	DIMENSION PHILN(NETO)

	DIMENSION dDSIGPL(9,NETO),dDSIGSTIF(NTOT)
	
	DIMENSION dPSI(9,NETO),dM(9,NETO),dSIGEPL(9,NETO)					!plates			
     
      DIMENSION dAIRE(9),dMSTAT(9),dAXEN(9),dIA(9),dIT(9),dIP(9),dIW(9)	!longitudinals
	DIMENSION dSIGESTIF(9,NETO),dKP(9,NETO),dC(9,NETO),dK(9,NETO)

	COMMON/PY/PI

c	*****************************************************************************************************

c	Elastic buckling of plates
c	--------------------------

	CALL ANNULD(dPSI,9*NETO)
	CALL ANNULD(dM,9*NETO)
	CALL ANNULD(dSIGEPL,9*NETO)
	CALL ANNULD(dDSIGPL,9*NETO)
	
	IF((SIG(1).GE.0.).AND.(SIG(2).GE.0.)) THEN							!panneau en traction
	  DSIGPL=0.
	  GOTO 30											
	ELSE
	  IF((-SIG(1).GE.0.).AND.(-SIG(2).GE.0.)) THEN						!panneau totalement en compression
	    IF((-SIG(1)).LE.(-SIG(2))) THEN
	      SIGA=-SIG(2)
	      PSI=SIG(1)/SIG(2)
	    ELSE
	      SIGA=-SIG(1)
	      PSI=SIG(2)/SIG(1)
	    ENDIF
	    IF(SIGA.LT.(30.0E+06/COEFK)) SIGA=30.0E+06/COEFK
		DO IPAN=1,NETO
	      NBRXI=NVAR(IPAN)
	      DO L=1,NBRXI
	        LL=NXIT(L,IPAN)
		    IF((-SIG(1)).LE.(-SIG(2))) THEN
      	      dPSI(LL,IPAN)=(dSIG(1,LL,IPAN)*SIG(2)-
     *				        SIG(1)*dSIG(2,LL,IPAN))/SIG(2)**2
		    ELSE
      	      dPSI(LL,IPAN)=(dSIG(2,LL,IPAN)*SIG(1)-
     *				        SIG(2)*dSIG(1,LL,IPAN))/SIG(1)**2
			ENDIF
		  ENDDO
	    ENDDO
	  ELSE																!panneau partiellement en compression
	    IF((-SIG(1)).GE.(-SIG(2))) THEN
	      SIGA=-SIG(1)
	    ELSE
	      SIGA=-SIG(2)
	    ENDIF
		IF(SIGA.LT.(30.0E+06/COEFK)) SIGA=30.0E+06/COEFK
	    PSI=0.
	    DO IPAN=1,NETO
	      NBRXI=NVAR(IPAN)
	      DO L=1,NBRXI
	        LL=NXIT(L,IPAN)
		    dPSI(LL,IPAN)=0.
	      ENDDO
	    ENDDO
	  ENDIF
	  IF(HXR.GT.0.010) THEN												!Plating with longitudinal stiffeners
          M=8.4/(PSI+1.1)
	    SIGEPL=0.9*M*E*(DELTA/EPSR)**2
	  ELSE																!Plating with transverse stiffeners
		IF(EPSA2.GE.(0.00001)) THEN
		  PHIL=-PHILN(NEL)*PI/180.
		  IF(EPSA2.LT.EPSA) THEN		
	        IF(WYA2.LT.0.010) THEN										!Flat bars
	          C=1.05
			ELSE														!Angles or t-sections
	          C=1.21
			ENDIF
			IF(EPSA2.LT.DABS(Q*PHIL)) THEN
			  M=C*(1+(EPSA2/DABS(Q*PHIL))**2)**2*2.1/(PSI+1.1)
	          SIGEPL=0.9*M*E*(DELTA/EPSA2)**2
	        ELSE
	          M=C*(1+(DABS(Q*PHIL)/EPSA2)**2)**2*2.1/(PSI+1.1)
			  SIGEPL=0.9*M*E*(DELTA/DABS(Q*PHIL))**2
			ENDIF
		  ELSE
	        IF(WYA.LT.0.010) THEN										!Flat bars
	          C=1.05
			ELSE														!Angles or t-sections
	          C=1.21
			ENDIF
			IF(EPSA.LT.DABS(Q*PHIL)) THEN
			  M=C*(1+(EPSA/DABS(Q*PHIL))**2)**2*2.1/(PSI+1.1)
	          SIGEPL=0.9*M*E*(DELTA/EPSA)**2
	        ELSE
	          M=C*(1+(DABS(Q*PHIL)/EPSA)**2)**2*2.1/(PSI+1.1)
	          SIGEPL=0.9*M*E*(DELTA/DABS(Q*PHIL))**2
			ENDIF
	      ENDIF
		ELSE
	      PHIL=-PHILN(NEL)*PI/180.
		  C=1.3															!Floors or deep girders (primary supporting members)
		  IF(EPSA.LT.DABS(Q*PHIL)) THEN
			M=C*(1+(EPSA/DABS(Q*PHIL))**2)**2*2.1/(PSI+1.1)
			SIGEPL=0.9*M*E*(DELTA/EPSA)**2
	      ELSE
	        M=C*(1+(DABS(Q*PHIL)/EPSA)**2)**2*2.1/(PSI+1.1)
			SIGEPL=0.9*M*E*(DELTA/DABS(Q*PHIL))**2
		  ENDIF
		ENDIF
	  ENDIF
	  IF(SIGEPL.LE.SIGY/2.) THEN
	    SIGC=SIGEPL
	  ELSE
	    SIGC=SIGY*(1.-0.25*SIGY/SIGEPL)
	  ENDIF
	  DSIGPL=SIGA/SIGC
	  DO IPAN=1,NETO
	    NBRXI=NVAR(IPAN)
	    DO L=1,NBRXI
	      LL=NXIT(L,IPAN)
	      IF(HXR.GT.0.010) THEN											!Plating with longitudinal stiffeners
		    dM(LL,IPAN)=-8.4*dPSI(LL,IPAN)/((PSI+1.1)*(PSI+1.1))
			IF((NEL.EQ.IPAN).AND.(LL.EQ.1)) THEN
     	          dSIGEPL(LL,IPAN)=0.9*E*(dM(LL,IPAN)*
     *					       (DELTA/EPSR)**2+2*M*DELTA/EPSR**2)
		    ELSE
		      IF((NEL.EQ.IPAN).AND.(LL.EQ.9)) THEN
     			    dSIGEPL(LL,IPAN)=0.9*E*(dM(LL,IPAN)*
     *					         (DELTA/EPSR)**2+2*M*(DELTA/EPSR)*
     *					         (-DELTA/EPSR**2))
			  ELSE
			    dSIGEPL(LL,IPAN)=0.9*E*(DELTA/EPSR)**2*
     *						     dM(LL,IPAN)
			  ENDIF
		    ENDIF
	      ELSE															!Plating with transverse stiffeners
	        IF(EPSA2.GE.(0.00001)) THEN
	          IF(EPSA2.LT.EPSA) THEN		
			    IF(EPSA2.LT.DABS(Q*PHIL)) THEN
			      dM(LL,IPAN)=-2.1*C*(1+(EPSA2/DABS(Q*PHIL))**2)**2*
     *						  dPSI(LL,IPAN)/(PSI+1.1)**2
				  IF((NEL.EQ.IPAN).AND.(LL.EQ.1)) THEN
     	                dSIGEPL(LL,IPAN)=0.9*E*(dM(LL,IPAN)*
     *					             (DELTA/EPSA2)**2+
     *						         2*M*DELTA/EPSA2**2)
				  ELSE
	                dSIGEPL(LL,IPAN)=0.9*E*(DELTA/EPSA2)**2*
     *						         dM(LL,IPAN)
				  ENDIF
	            ELSE
	              dM(LL,IPAN)=-2.1*C*(1+(DABS(Q*PHIL)/EPSA2)**2)**2*
     *						  dPSI(LL,IPAN)/(PSI+1.1)**2
				  IF((NEL.EQ.IPAN).AND.(LL.EQ.1)) THEN
     	                dSIGEPL(LL,IPAN)=0.9*E*(dM(LL,IPAN)*
     *					             (DELTA/DABS(Q*PHIL))**2+
     *								 2*M*DELTA/DABS(Q*PHIL)**2)
				  ELSE
	                dSIGEPL(LL,IPAN)=0.9*E*(DELTA/DABS(Q*PHIL))**2*
     *						         dM(LL,IPAN)
				  ENDIF
			    ENDIF
	          ELSE
			    IF(EPSA.LT.DABS(Q*PHIL)) THEN
			      IF((NEL.EQ.IPAN).AND.(LL.EQ.1)) THEN
     	                dM(LL,IPAN)=-2.1*C*(1+(EPSA/DABS(Q*PHIL))**2)**2*
     *						    dPSI(LL,IPAN)/(PSI+1.1)**2
                      dSIGEPL(LL,IPAN)=0.9*E*(dM(LL,IPAN)*
     *					            (DELTA/EPSA)**2+
     *						        2*M*DELTA/EPSA**2)
				  ELSE
				    IF((NEL.EQ.IPAN).AND.(LL.EQ.5)) THEN
				      dM(LL,IPAN)=(2*C*(1+(EPSA/DABS(Q*PHIL))**2)*2*
     *							  (EPSA/DABS(Q*PHIL))*1/DABS(Q*PHIL))*
     *							  (2.1/(PSI+1.1))+
     *							  (C*(1+(EPSA/DABS(Q*PHIL))**2)**2)*
     *							  (-2.1*dPSI(LL,IPAN)/(PSI+1.1)**2)
					  dSIGEPL(LL,IPAN)=0.9*E*(dM(LL,IPAN)*
     *					         (DELTA/EPSA)**2+2*M*(DELTA/EPSA)*
     *					         (-DELTA/EPSA**2))
	                ELSE
				      dM(LL,IPAN)=-2.1*C*(1+(EPSA/DABS(Q*PHIL))**2)**2
     *						      *dPSI(LL,IPAN)/(PSI+1.1)**2
					  dSIGEPL(LL,IPAN)=0.9*E*(DELTA/EPSA)**2*
     *						           dM(LL,IPAN)
					ENDIF
				  ENDIF
	            ELSE
	              IF((NEL.EQ.IPAN).AND.(LL.EQ.1)) THEN
     	                dM(LL,IPAN)=-2.1*C*(1+(DABS(Q*PHIL)/EPSA)**2)**2*
     *						    dPSI(LL,IPAN)/(PSI+1.1)**2
                      dSIGEPL(LL,IPAN)=0.9*E*(dM(LL,IPAN)*
     *					            (DELTA/DABS(Q*PHIL))**2+
     *						        2*M*DELTA/DABS(Q*PHIL)**2)
				  ELSE
				    IF((NEL.EQ.IPAN).AND.(LL.EQ.5)) THEN
				      dM(LL,IPAN)=(2*C*(1+(DABS(Q*PHIL)/EPSA)**2)*2*
     *							  (DABS(Q*PHIL)/EPSA)*
     *							  (-DABS(Q*PHIL)/EPSA**2)*
     *						      (2.1/(PSI+1.1))+
     *							  (C*(1+(EPSA/DABS(Q*PHIL))**2)**2))*
     *							  (-2.1*dPSI(LL,IPAN)/(PSI+1.1)**2)
					  dSIGEPL(LL,IPAN)=0.9*E*(dM(LL,IPAN)*
     *					               (DELTA/DABS(Q*PHIL))**2+
     *								   2*M*(DELTA/DABS(Q*PHIL))*
     *					               (-DELTA/DABS(Q*PHIL)**2))
	                ELSE
				      dM(LL,IPAN)=-2.1*C*(1+(DABS(Q*PHIL)/EPSA)**2)**2
     *						      *dPSI(LL,IPAN)/(PSI+1.1)**2
					  dSIGEPL(LL,IPAN)=0.9*E*(DELTA/DABS(Q*PHIL))**2*
     *						           dM(LL,IPAN)
					ENDIF
				  ENDIF
			    ENDIF
	          ENDIF
			ELSE
	          IF(EPSA.LT.DABS(Q*PHIL)) THEN
			    IF((NEL.EQ.IPAN).AND.(LL.EQ.1)) THEN
     	              dM(LL,IPAN)=-2.1*C*(1+(EPSA/DABS(Q*PHIL))**2)**2*
     *					      dPSI(LL,IPAN)/(PSI+1.1)**2
                    dSIGEPL(LL,IPAN)=0.9*E*(dM(LL,IPAN)*
     *				               (DELTA/EPSA)**2+
     *						       2*M*DELTA/EPSA**2)
				ELSE
				  IF((NEL.EQ.IPAN).AND.(LL.EQ.5)) THEN
				    dM(LL,IPAN)=(2*C*(1+(EPSA/DABS(Q*PHIL))**2)*2*
     *						    (EPSA/DABS(Q*PHIL))*1/DABS(Q*PHIL))*
     *							(2.1/(PSI+1.1))+
     *							(C*(1+(EPSA/DABS(Q*PHIL))**2)**2)*
     *							(-2.1*dPSI(LL,IPAN)/(PSI+1.1)**2)
					dSIGEPL(LL,IPAN)=0.9*E*(dM(LL,IPAN)*
     *					             (DELTA/EPSA)**2+2*M*(DELTA/EPSA)*
     *					             (-DELTA/EPSA**2))
	              ELSE
				    dM(LL,IPAN)=-2.1*C*(1+(EPSA/DABS(Q*PHIL))**2)**2*
     *					        dPSI(LL,IPAN)/(PSI+1.1)**2
					dSIGEPL(LL,IPAN)=0.9*E*(DELTA/EPSA)**2*
     *					             dM(LL,IPAN)
				  ENDIF
				ENDIF
	          ELSE
	            IF((NEL.EQ.IPAN).AND.(LL.EQ.1)) THEN
     	              dM(LL,IPAN)=-2.1*C*(1+(DABS(Q*PHIL)/EPSA)**2)**2*
     *					      dPSI(LL,IPAN)/(PSI+1.1)**2
				  dSIGEPL(LL,IPAN)=0.9*E*(dM(LL,IPAN)*
     *				               (DELTA/DABS(Q*PHIL))**2+
     *						       2*M*DELTA/DABS(Q*PHIL)**2)
				ELSE
				  IF((NEL.EQ.IPAN).AND.(LL.EQ.5)) THEN
				    dM(LL,IPAN)=(2*C*(1+(DABS(Q*PHIL)/EPSA)**2)*2*
     *						    (DABS(Q*PHIL)/EPSA)*
     *							(-DABS(Q*PHIL)/EPSA**2)*
     *						    (2.1/(PSI+1.1))+
     *							(C*(1+(EPSA/DABS(Q*PHIL))**2)**2))*
     *							(-2.1*dPSI(LL,IPAN)/(PSI+1.1)**2)
					dSIGEPL(LL,IPAN)=0.9*E*(dM(LL,IPAN)*
     *				                 (DELTA/DABS(Q*PHIL))**2+
     *								 2*M*(DELTA/DABS(Q*PHIL))*
     *					             (-DELTA/DABS(Q*PHIL)**2))
	              ELSE
				    dM(LL,IPAN)=-2.1*C*(1+(DABS(Q*PHIL)/EPSA)**2)**2*
     *					        dPSI(LL,IPAN)/(PSI+1.1)**2
					dSIGEPL(LL,IPAN)=0.9*E*(DELTA/DABS(Q*PHIL))**2*
     *					             dM(LL,IPAN)
			      ENDIF
				ENDIF
	          ENDIF
			ENDIF
		  ENDIF
		  IF(SIGEPL.LE.SIGY/2.) THEN
	        IF(SIGA.LT.(30.0E+06/COEFK)) THEN
	          dDSIGPL(LL,IPAN)=(-SIGA*dSIGEPL(LL,IPAN))/SIGC**2
	        ELSE
			  IF((-SIG(1)).GE.(-SIG(2))) THEN
	            dDSIGPL(LL,IPAN)=(-dSIG(1,LL,IPAN)*SIGC-
     *						     SIGA*dSIGEPL(LL,IPAN))/SIGC**2
	          ELSE
	            dDSIGPL(LL,IPAN)=(-dSIG(2,LL,IPAN)*SIGC-
     *						     SIGA*dSIGEPL(LL,IPAN))/SIGC**2
			  ENDIF
	        ENDIF
	      ELSE
	        IF(SIGA.LT.(30.0E+06/COEFK)) THEN
	          dDSIGPL(LL,IPAN)=(-SIGA*SIGY*(0.25*SIGY*
     *						   dSIGEPL(LL,IPAN)/SIGEPL**2))/SIGC**2
			ELSE
			  IF((-SIG(1)).GE.(-SIG(2))) THEN
			    dDSIGPL(LL,IPAN)=(-dSIG(1,LL,IPAN)*SIGC-
     *						     SIGA*SIGY*(0.25*SIGY*
     *						     dSIGEPL(LL,IPAN)/SIGEPL**2))/SIGC**2
			  ELSE
	            dDSIGPL(LL,IPAN)=(-dSIG(2,LL,IPAN)*SIGC-
     *						     SIGA*SIGY*(0.25*SIGY*
     *						     dSIGEPL(LL,IPAN)/SIGEPL**2))/SIGC**2
			  ENDIF
			ENDIF
		  ENDIF
		ENDDO
	  ENDDO
	ENDIF

    1 FORMAT(I3,39x,F8.3,3x,F6.3,6x,'plate')
    2 FORMAT(I3,15x,F8.3,16x,F8.3,3x,F6.3,6x,'plate')


c	Elastic buckling of longitudinals
c	---------------------------------
	
   30	CALL ANNULD(dKP,9*NETO)
	CALL ANNULD(dC,9*NETO)
	CALL ANNULD(dK,9*NETO)
	CALL ANNULD(dSIGESTIF,9*NETO)
	CALL ANNULD(dDSIGSTIF,NTOT)
	
	IF((SIG(1).GE.0).AND.(SIG(2).GE.0)) THEN
	  DSIGSTIF=0.
	  RETURN		
	ELSE
	  IF((-SIG(1)).GE.(-SIG(2))) THEN
	    SIGA=-SIG(1)
	  ELSE
	    SIGA=-SIG(2)
	  ENDIF
	  IF(SIGA.LT.(30.0E+06/COEFK)) SIGA=30.0E+06/COEFK


c	Column buckling
c	---------------

	  AIRE=DELTA*EPSR+HXR*DXR+WXR*TXR

	  MSTAT=DELTA*EPSR*DELTA/2.+HXR*DXR*(HXR/2.+DELTA)+
     *	    WXR*TXR*(TXR/2.+HXR+DELTA)

	  AXEN=MSTAT/AIRE

	  IA=EPSR*DELTA**3/12.+DELTA*EPSR*(DELTA/2.-AXEN)**2+
     *	 DXR*HXR**3/12.+HXR*DXR*(HXR/2.+DELTA-AXEN)**2+
     *	 WXR*TXR**3/12.+WXR*TXR*(TXR/2.+HXR+DELTA-AXEN)**2

	  SIGESTIFC=10.*E*IA/(AIRE*EPSA**2)

	  BETA=1.1
	  IF(SIGESTIFC.LE.SIGY/2.) THEN
	    SIGCC=SIGESTIFC/BETA
	  ELSE
	    SIGCC=SIGY*(1-0.25*SIGY/SIGESTIFC)/BETA
	  ENDIF

	  GOTO 10 

c	*************************************************************************************
   13	  CALL ANNULD(dAIRE,9)
	  CALL ANNULD(dMSTAT,9)
	  CALL ANNULD(dAXEN,9)
	  CALL ANNULD(dIA,9)
	  
	  dAIRE(1)=EPSR
	  dAIRE(6)=DXR
	  dAIRE(7)=HXR
	  dAIRE(8)=TXR
	  dAIRE(9)=DELTA
	  
	  dMSTAT(1)=EPSR*DELTA+HXR*DXR+WXR*TXR
	  dMSTAT(6)=DXR*(HXR/2.+DELTA)+0.5*HXR*DXR+WXR*TXR
	  dMSTAT(7)=HXR*(HXR/2.+DELTA)
	  dMSTAT(8)=TXR*(TXR/2.+HXR+DELTA)
	  dMSTAT(9)=DELTA**2/2.

	  DO I=1,9
	    dAXEN(I)=(dMSTAT(I)*AIRE-MSTAT*dAIRE(I))/AIRE**2
	  ENDDO

	  dIA(1)=EPSR*DELTA**2/4.+EPSR*(DELTA/2.-AXEN)**2+
     *		 2.*DELTA*EPSR*(DELTA/2.-AXEN)*(0.5-dAXEN(1))+
     *		 2.*HXR*DXR*(HXR/2.+DELTA-AXEN)*(1.-dAXEN(1))+
     *		 2.*WXR*TXR*(TXR/2.+HXR+DELTA-AXEN)*(1.-dAXEN(1))
	  dIA(6)=2.*DELTA*EPSR*(DELTA/2.-AXEN)*(-dAXEN(6))+
     *		 DXR*HXR**2/4.+DXR*(HXR/2.+DELTA-AXEN)**2+
     *		 2.*HXR*DXR*(HXR/2.+DELTA-AXEN)*(0.5-dAXEN(6))+
     *		 2.*WXR*TXR*(TXR/2.+HXR+DELTA-AXEN)*(1.-dAXEN(6))
	  dIA(7)=2.*DELTA*EPSR*(DELTA/2.-AXEN)*(-dAXEN(7))+
     *		 HXR**3/12.+HXR*(HXR/2.+DELTA-AXEN)**2+
     *		 2.*HXR*DXR*(HXR/2.+DELTA-AXEN)*(-dAXEN(7))+
     *		 2.*WXR*TXR*(TXR/2.+HXR+DELTA-AXEN)*(-dAXEN(7))
	  dIA(8)=2.*DELTA*EPSR*(DELTA/2.-AXEN)*(-dAXEN(8))+
     *		 2.*HXR*DXR*(HXR/2.+DELTA-AXEN)*(-dAXEN(8))+
     *		 TXR**3/12.+TXR*(TXR/2.+HXR+DELTA-AXEN)**2+
     *		 WXR*TXR*(TXR/2.+HXR+DELTA-AXEN)*(-dAXEN(8))
	  dIA(9)=DELTA**3/12.+DELTA*(DELTA/2.-AXEN)**2+
     *		 2.*DELTA*EPSR*(DELTA/2.-AXEN)*(-dAXEN(9))+
     *		 2.*HXR*DXR*(HXR/2.+DELTA-AXEN)*(-dAXEN(9))+
     *		 2.*WXR*TXR*(TXR/2.+HXR+DELTA-AXEN)*(-dAXEN(9))
	  
	  DO I=1,9
	    dSIGESTIF(I,NEL)=10.*E/EPSA**2*(dIA(I)*AIRE-IA*dAIRE(I))/
     *				     AIRE**2
	  ENDDO
	  dSIGESTIF(5,NEL)=-2.*10.*E*IA/AIRE*(1./EPSA**3)

	  GOTO 16
c	*************************************************************************************

c	Torsional buckling
c	------------------

   10	  IT=(1./3.)*(HXR*DXR**3+WXR*TXR**3*(1.-0.63*TXR/WXR))

	  IP=(HXR**3*DXR)/3.+HXR**2*WXR*TXR

	  IW=TXR*WXR**3*HXR**2/12.

	  KP=1.-SIGA/SIGEPL
	  IF(KP.LE.0.1) THEN
	    KP=0.1
	  ENDIF

	  C=KP*E*DELTA**3/(3*EPSR*(1.+1.33*KP*HXR*DELTA**3/
     *    (EPSR*DXR**3)))

	  K=C*EPSA**4/(PI**4*E*IW)

	  M=1.
	  FIN=0
	  DO WHILE(FIN.EQ.0)
		IF((K.GT.((M-1)**2*M**2)).AND.(K.LE.(M**2*(M+1)**2))) THEN
		  FIN=1
		ELSE 
	      M=M+1.
	    ENDIF
	  ENDDO 
	  
	  SIGESTIFT=PI**2*E*IW/(IP*EPSA**2)*(M**2+K/M**2)+0.385*E*IT/IP

	  BETA=1.1
	  IF(SIGESTIFT.LE.SIGY/2.) THEN
	    SIGCT=SIGESTIFT/BETA
	  ELSE
	    SIGCT=SIGY*(1-0.25*SIGY/SIGESTIFT)/BETA
	  ENDIF
	  
	  GOTO 11

c	*************************************************************************************
   14   CALL ANNULD(dIT,9)
	  CALL ANNULD(dIP,9)
	  CALL ANNULD(dIW,9)

	  dIT(6)=(1./3.)*DXR**3
	  dIT(7)=HXR*DXR**2
	  dIT(8)=(1./3.)*(TXR**3*(1.-0.63*TXR/WXR)+
     *	      WXR*TXR**3*(0.63*TXR/WXR**2))

	  dIP(6)=DXR*HXR**2+2*HXR*WXR*TXR
	  dIP(7)=HXR**3/3.
	  dIP(8)=TXR*HXR**2

	  dIW(6)=TXR*WXR**3*HXR/6.
	  dIW(8)=TXR*WXR**2*HXR**2/4.

	  DO IPAN=1,NETO
	    NBRXI=NVAR(IPAN)
	    DO L=1,NBRXI
	      LL=NXIT(L,IPAN)
		  IF(SIGA.LT.(30.0E+06/COEFK)) THEN
		    dKP(LL,IPAN)=-(-SIGA*dSIGEPL(LL,IPAN))/SIGEPL**2
	      ELSE
	        IF((-SIG(1)).GE.(-SIG(2))) THEN
			  dKP(LL,IPAN)=-(-dSIG(1,LL,IPAN)*SIGEPL-
     *			           SIGA*dSIGEPL(LL,IPAN))/SIGEPL**2
	        ELSE
			  dKP(LL,IPAN)=-(-dSIG(2,LL,IPAN)*SIGEPL-
     *			           SIGA*dSIGEPL(LL,IPAN))/SIGEPL**2
			ENDIF
		  ENDIF
		  IF(KP.EQ.0.1) dKP(LL,IPAN)=0.
		ENDDO
	  ENDDO

	  DO IPAN=1,NETO
	    NBRXI=NVAR(IPAN)
	    DO L=1,NBRXI
	      LL=NXIT(L,IPAN)
		  dC(LL,IPAN)=(((E*DELTA**3/(3*EPSR))*dKP(LL,IPAN))*(1.+
     *		          (1.33*HXR*DELTA**3/(EPSR*DXR**3))*KP)-
     *				  ((E*DELTA**3/(3*EPSR))*KP)*((1.33*HXR*DELTA**3/
     *				  (EPSR*DXR**3))*dKP(LL,IPAN)))/(1.+
     *				  (1.33*HXR*DELTA**3/(EPSR*DXR**3))*KP)**2				  	
		ENDDO
	  ENDDO
	  dC(1,NEL)=(((E*DELTA**3/(3*EPSR))*dKP(1,NEL)+
     *			(E*DELTA**2/EPSR)*KP)*
     *		    (1.+(1.33*HXR*DELTA**3/(EPSR*DXR**3))*KP)-
     *			((E*DELTA**3/(3*EPSR))*KP)*((1.33*HXR*DELTA**3/
     *			(EPSR*DXR**3))*dKP(1,NEL)+(3.99*HXR*DELTA**2/
     *			(EPSR*DXR**3))*KP))/(1.+
     *			(1.33*HXR*DELTA**3/(EPSR*DXR**3))*KP)**2
	  dC(6,NEL)=(((E*DELTA**3/(3*EPSR))*dKP(6,NEL))*(1.+
     *		    (1.33*HXR*DELTA**3/(EPSR*DXR**3))*KP)-
     *			((E*DELTA**3/(3*EPSR))*KP)*((1.33*HXR*DELTA**3/
     *			(EPSR*DXR**3))*dKP(6,NEL)+(1.33*DELTA**3/
     *			(EPSR*DXR**3))*KP))/(1.+
     *			(1.33*HXR*DELTA**3/(EPSR*DXR**3))*KP)**2
	  dC(7,NEL)=(((E*DELTA**3/(3*EPSR))*dKP(7,NEL))*(1.+
     *		    (1.33*HXR*DELTA**3/(EPSR*DXR**3))*KP)-
     *			((E*DELTA**3/(3*EPSR))*KP)*((1.33*HXR*DELTA**3/
     *			(EPSR*DXR**3))*dKP(7,NEL)+(-3.99*HXR*DELTA**3*
     *			EPSR*DXR**2/(EPSR*DXR**3)**2)*KP))/(1.+
     *			(1.33*HXR*DELTA**3/(EPSR*DXR**3))*KP)**2
	  dC(9,NEL)=(((E*DELTA**3/(3*EPSR))*dKP(9,NEL)+
     *			(-3*E*DELTA**3/(3*EPSR)**2)*KP)*(1.+
     *		    (1.33*HXR*DELTA**3/(EPSR*DXR**3))*KP)-
     *			((E*DELTA**3/(3*EPSR))*KP)*((1.33*HXR*DELTA**3/
     *			(EPSR*DXR**3))*dKP(9,NEL)+(-1.33*HXR*DELTA**3*
     *			DXR**3/(EPSR*DXR**3)**2)))/(1.+
     *			(1.33*HXR*DELTA**3/(EPSR*DXR**3))*KP)**2

	  DO IPAN=1,NETO
	    NBRXI=NVAR(IPAN)
	    DO L=1,NBRXI
	      LL=NXIT(L,IPAN)
		  IF(IPAN.EQ.NEL) THEN
		    dK(LL,IPAN)=EPSA**4/(PI**4*E)*(dC(LL,IPAN)*IW-C*dIW(LL))/
     *					IW**2
		  ELSE
	        dK(LL,IPAN)=EPSA**4/(PI**4*E)*dC(LL,IPAN)/IW
	      ENDIF
		ENDDO
	  ENDDO
	  dK(5,NEL)=dK(5,NEL)+4*EPSA**3/(PI**4*E)*C/IW

	  DO IPAN=1,NETO
	  	NBRXI=NVAR(IPAN)
	    DO L=1,NBRXI
	      LL=NXIT(L,IPAN)
	      IF(IPAN.EQ.NEL) THEN
		    dSIGESTIF(LL,IPAN)=PI**2*E/EPSA**2*
     *						   ((dIW(LL)*IP-IW*dIP(LL))/IP**2*
     *					       (M**2+K/M**2)+IW/IP*dK(LL,IPAN)/M**2)+
     *						   0.385*E*(dIT(LL)*IP-IT*dIP(LL))/IP**2
		  ELSE
	        dSIGESTIF(LL,IPAN)=PI**2*E/EPSA**2*IW/IP*dK(LL,IPAN)/M**2
	      ENDIF
	    ENDDO
	  ENDDO
	  dSIGESTIF(5,NEL)=dSIGESTIF(5,NEL)+-2*PI**2*E/EPSA**3*IW/IP*
     *				  (M**2+K/M**2)

	  GOTO 16
c	**********************************************************************************

c	Web buckling
c	------------
   11	  SIGESTIFL=3.8*E*(DXR/HXR)**2
        BETA=1.
        IF(SIGESTIFL.LE.SIGY/2.) THEN
	    SIGCL=SIGESTIFL/BETA
	  ELSE
	    SIGCL=SIGY*(1-0.25*SIGY/SIGESTIFL)/BETA
	  ENDIF
	  GOTO 12

c	**********************************************************************************
   15   dSIGESTIF(6,NEL)=7.6*E*(DXR/HXR)*(-DXR/HXR**2)
	  dSIGESTIF(7,NEL)=7.6*E*(DXR/HXR)*(1./HXR)
	
	  GOTO 16
c	**********************************************************************************

   12	  SIGC=MIN(SIGCC,SIGCT,SIGCL)
	  BETA=1.1
	  IF(SIGCC.EQ.SIGC) THEN
	    SIGESTIF=SIGESTIFC
		GOTO 13
	  ENDIF
	  IF(SIGCT.EQ.SIGC) THEN
	    SIGESTIF=SIGESTIFT
		GOTO 14
	  ENDIF
	  IF(SIGCL.EQ.SIGC) THEN
	    SIGESTIF=SIGESTIFL
		BETA=1.
		GOTO 15
	  ENDIF
   

   16	  DSIGSTIF=SIGA/SIGC
	  L1=0
	  DO IPAN=1,NETO
	    NBRXI=NVAR(IPAN)
		DO L=1,NBRXI
	      LL=NXIT(L,IPAN)
		  IF(SIGESTIF.LE.SIGY/2.) THEN
			IF(SIGA.LT.(30.0E+06/COEFK)) THEN
	          dDSIGSTIF(L1+L)=(-SIGA*dSIGESTIF(LL,IPAN)/BETA)/
     *						   SIGC**2
			ELSE
			  IF((-SIG(1)).GE.(-SIG(2))) THEN
			    dDSIGSTIF(L1+L)=(-dSIG(1,LL,IPAN)*SIGC-SIGA*
     *						     dSIGESTIF(LL,IPAN)/BETA)/SIGC**2
	          ELSE
			    dDSIGSTIF(L1+L)=(-dSIG(2,LL,IPAN)*SIGC-SIGA*
     *						     dSIGESTIF(LL,IPAN)/BETA)/SIGC**2
			  ENDIF
	        ENDIF
	      ELSE
	        IF(SIGA.LT.(30.0E+06/COEFK)) THEN
	          dDSIGSTIF(L1+L)=(-SIGA*SIGY*(0.25*SIGY*
     *						   dSIGESTIF(LL,IPAN)/SIGESTIF**2)/BETA)/
     *						   SIGC**2
			ELSE
			  IF((-SIG(1)).GE.(-SIG(2))) THEN
		        dDSIGSTIF(L1+L)=(-dSIG(1,LL,IPAN)*SIGC-SIGA*
     *						     SIGY*(0.25*SIGY*dSIGESTIF(LL,IPAN)/
     *						     SIGESTIF**2)/BETA)/SIGC**2							   
			  ELSE
			    dDSIGSTIF(L1+L)=(-dSIG(2,LL,IPAN)*SIGC-SIGA*
     *						     SIGY*(0.25*SIGY*dSIGESTIF(LL,IPAN)/
     *						     SIGESTIF**2)/BETA)/SIGC**2							   
			  ENDIF
			ENDIF
		  ENDIF
		ENDDO
		L1=L1+NBRXI
	  ENDDO

	ENDIF


    3 FORMAT(I3,39x,F8.3,3x,F6.3,6x,'stiffener')
    4 FORMAT(I3,4x,F8.3,3x,F8.3,3x,F8.3,5x,F8.3,3x,F6.3,6x,'stiffener')      


      RETURN
      END
