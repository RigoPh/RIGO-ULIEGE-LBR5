      SUBROUTINE COMPBUCK(NEL,NETO,E,SIGY,COEFK,PHILN,NVAR,NXIT,			!r&d15
     *				    EPSA,EPSR,DELTA,HXR,DXR,WXR,TXR,Q,
     *				    EPSA2,WYA2,SIG,dSIG,TRAV,NTOT,DSIGPL,dDSIGPL)


      IMPLICIT REAL*8 (A-H,O-Z)
	DIMENSION TRAV(1)

	DIMENSION SIG(2)
	
	I1=1			!dPSI(9,NETO)
	I2=I1+9*NETO	!dM(9,NETO)
	I3=I2+9*NETO	!dSIGEPL(9,NETO)
	I4=I3+9*NETO	
	IMAX=I4-1		!27*NETO

	CALL CCOMPBUCK(NEL,NETO,E,SIGY,COEFK,PHILN,NVAR,NXIT,			!r&d15
     *			   EPSA,EPSR,DELTA,HXR,DXR,WXR,TXR,Q,
     *			   EPSA2,WYA2,SIG,dSIG,
     *		       TRAV(I1),TRAV(I2),TRAV(I3),NTOT,DSIGPL,dDSIGPL)

	RETURN
	END

c	******************************************************************************************************
c	******************************************************************************************************
c	******************************************************************************************************

	SUBROUTINE CCOMPBUCK(NEL,NETO,E,SIGY,COEFK,PHILN,NVAR,NXIT,		!r&d15
     *				     EPSA,EPSR,DELTA,HXR,DXR,WXR,TXR,Q,
     *				     EPSA2,WYA2,SIG,dSIG,
     *				     dPSI,dM,dSIGEPL,NTOT,DSIGPL,dDSIGPL)


      IMPLICIT REAL*8 (A-H,O-Z)

	REAL *8 M

	DIMENSION SIG(2),dSIG(2,9,NETO)
	
	DIMENSION NVAR(NETO),NXIT(9,NETO)
	DIMENSION PHILN(NETO)

	DIMENSION dDSIGPL(NTOT)
	
	DIMENSION dPSI(9,NETO),dM(9,NETO),dSIGEPL(9,NETO)					!plates			
     
	COMMON/PY/PI

c	*****************************************************************************************************

c	Elastic buckling of plates
c	--------------------------

	CALL ANNULD(dPSI,9*NETO)
	CALL ANNULD(dM,9*NETO)
	CALL ANNULD(dSIGEPL,9*NETO)
	CALL ANNULD(dDSIGPL,NTOT)
	
	IF((SIG(1).GE.0.).AND.(SIG(2).GE.0.)) THEN							!panneau en traction
	  DSIGPL=0.
	  RETURN											
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
	  L1=0
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
	          dDSIGPL(L1+L)=(-SIGA*dSIGEPL(LL,IPAN))/SIGC**2
	        ELSE
			  IF((-SIG(1)).GE.(-SIG(2))) THEN
	            dDSIGPL(L1+L)=(-dSIG(1,LL,IPAN)*SIGC-
     *						   SIGA*dSIGEPL(LL,IPAN))/SIGC**2
	          ELSE
	            dDSIGPL(L1+L)=(-dSIG(2,LL,IPAN)*SIGC-
     *						   SIGA*dSIGEPL(LL,IPAN))/SIGC**2
			  ENDIF
	        ENDIF
	      ELSE
	        IF(SIGA.LT.(30.0E+06/COEFK)) THEN
	          dDSIGPL(L1+L)=(-SIGA*SIGY*(0.25*SIGY*
     *						 dSIGEPL(LL,IPAN)/SIGEPL**2))/SIGC**2
			ELSE
			  IF((-SIG(1)).GE.(-SIG(2))) THEN
			    dDSIGPL(L1+L)=(-dSIG(1,LL,IPAN)*SIGC-
     *						   SIGA*SIGY*(0.25*SIGY*
     *						   dSIGEPL(LL,IPAN)/SIGEPL**2))/SIGC**2
			  ELSE
	            dDSIGPL(L1+L)=(-dSIG(2,LL,IPAN)*SIGC-
     *						   SIGA*SIGY*(0.25*SIGY*
     *						   dSIGEPL(LL,IPAN)/SIGEPL**2))/SIGC**2
			  ENDIF
			ENDIF
		  ENDIF
		ENDDO
		L1=L1+NBRXI
	  ENDDO
	ENDIF

    1 FORMAT(I3,39x,F8.3,3x,F6.3,6x,'plate')
    2 FORMAT(I3,15x,F8.3,16x,F8.3,3x,F6.3,6x,'plate')

      RETURN
      END
