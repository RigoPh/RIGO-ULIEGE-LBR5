      SUBROUTINE BUCKDCN(NEL,E,SIGY,SIGM,DELTA,HXR,DXR,WXR,TXR,EPSR,
     *				   EPSA,WYA,EFF,SENS2,SENS3,NVAR,NXIT,NETO,
     *				   DSIGPL,dDSIGPL,DSIGSTIF,dDSIGSTIF,WORK,
     *				   ITERA,Z,PHILN,TETAS,ITYPE,PART,DELT,
     *				   YNEUTPART,SYM,SYMX,BM1,IBUCK)

      IMPLICIT REAL*8 (A-H,O-Z)
	DIMENSION WORK(1)

	I1=1			!dPSI(9,NETO)
	I2=I1+9*NETO	!dSIG1(9,NETO)
	I3=I2+9*NETO	!dSIG2(9,NETO)
	I4=I3+9*NETO	!dM(9,NETO)
	I5=I4+9*NETO	!dSIGEPL(9,NETO)
	I6=I5+9*NETO	!dKP(9,NETO)
	I7=I6+9*NETO	!dC(9,NETO)
	I8=I7+9*NETO	!dK(9,NETO)
	I9=I8+9*NETO	!dSIGESTIF(9,NETO)
	I10=I9+9*NETO	!IPART
	I11=I10+1		!OMETPART
	I12=I11+1		!dIPART(9,NETO)
	I13=I12+9*NETO	

	IMAX=I13-1	!90*NETO+2

	CALL BBUCKDCN(NEL,E,SIGY,SIGM,DELTA,HXR,DXR,WXR,TXR,EPSR,EPSA,
     *			  WYA,EFF,SENS2,SENS3,NVAR,NXIT,NETO,DSIGPL,
     *			  dDSIGPL,DSIGSTIF,dDSIGSTIF,WORK(I1),WORK(I2),
     *			  WORK(I3),WORK(I4),WORK(I5),WORK(I6),WORK(I7),
     *			  WORK(I8),WORK(I9),WORK(I10),WORK(I11),WORK(I12),
     *			  ITERA,Z,PHILN,TETAS,ITYPE,PART,DELT,
     *			  YNEUTPART,SYM,SYMX,BM1,IBUCK)

	RETURN
	END

c	******************************************************************************************************
c	******************************************************************************************************
c	******************************************************************************************************

	SUBROUTINE BBUCKDCN(NEL,E,SIGY,SIGM,DELTA,HXR,DXR,WXR,TXR,
     *					EPSR,EPSA,WYA,EFF,SENS2,SENS3,NVAR,NXIT,
     *					NETO,DSIGPL,dDSIGPL,DSIGSTIF,dDSIGSTIF,
     *					dPSI,dSIG1,dSIG2,dM,dSIGEPL,dKP,dC,dK,
     *					dSIGESTIF,IPART,OMETPART,dIPART,ITERA,				  
     *					Z,PHILN,TETAS,ITYPE,PART,DELT,
     *					YNEUTPART,SYM,SYMX,BM1,IBUCK)


      IMPLICIT REAL*8 (A-H,O-Z)

	REAL *8 M,IA,IT,IP,IW,MSTAT,KP,K,IPART,IPANEL

	INTEGER FIN
	INTEGER SYM,SYMX

	DIMENSION EFF(9690),SENS2(16,IPTmax,9,NETO),
     *		  SENS3(21,IPTmax,9,NETO)

	DIMENSION NVAR(NETO),NXIT(9,NETO)
	DIMENSION Z(NETO,4),PHILN(NETO),TETAS(NETO),ITYPE(NETO),
     *		  PART(NETO),DELT(NETO)

	DIMENSION dDSIGPL(9,NETO),dDSIGSTIF(9,NETO)
	
	DIMENSION dPSI(9,NETO),dSIG1(9,NETO),dSIG2(9,NETO),
     *		  dM(9,NETO),dSIGEPL(9,NETO)								!plates			
     
      DIMENSION dAIRE(9),dMSTAT(9),dAXEN(9),dIA(9),dIT(9),dIP(9),dIW(9)	!longitudinals
	DIMENSION dSIGESTIF(9,NETO),dKP(9,NETO),dC(9,NETO),dK(9,NETO)

	DIMENSION dIPART(9,NETO)
	DIMENSION dIPANEL(9),dSPANEL(9)
	
      COMMON/DIM1/Nmax,NEmax,NVmax,
     *            M1max,M1Tmax,M2max,M2Tmax,Mmax,MGmax,ISmax,IPTmax		! dimension max des vecteurs
	COMMON/PY/PI
	COMMON/OPTI/IOPTI,NTOT,M1T,M2T

c	*****************************************************************************************************

c	Working Stress
c	--------------

c	IF(NBUCK.EQ.0) THEN
c	  CALL ANNULD(dIPART,9*NETO)
c	  CALL ANNULD(dIPANEL,9)
c	  CALL ANNULD(dSPANEL,9)

c	  IPART=0.
c	  OMETPART=0.
c        DO IPAN=1,NETO
c	    READ(302) KSA,KSR,KSE,EPSAP,EPSRP,DELTAP,HYAP,DYAP,WYAP,TYAP,		
c     *		      HXRP,DXRP,WXRP,TXRP,PHIL,Q,EPAIS
c		IF(ITYPE(IPAN).NE.5) THEN
c	      SPH2=VSIN(-PHILN(IPAN)/2.,0.D00)
c            C1=VCOS(TETAS(IPAN),0.D00)
c	      S1=VSIN(TETAS(IPAN),0.D00)
c            S2=VSIN(TETAS(IPAN),-PHILN(IPAN)/2.)
c		  PHIL=-PHILN(IPAN)*PI/180.
c		  D2=Q*(S1-2.*SPH2*S2/PHIL)
	    
c		  OMEGA=DABS(DELT(IPAN)*Q*PHIL)
c	      OMETPART=OMETPART+PART(IPAN)*OMEGA
c	      YNEU=Z(IPAN,3)+D2
c	      IPANEL=((C1*Q*PHIL)**2+(S1*PART(IPAN)*DELT(IPAN))**2)*
c     *             PART(IPAN)*DELT(IPAN)*Q*DABS(PHIL)/12.
c	      IPART=IPART+IPANEL+PART(IPAN)*OMEGA*(YNEU-YNEUTPART)**2
c	    ENDIF
c	  ENDDO
c	  IF(SYM.EQ.1) IPART=2*IPART				
c	  IF(SYM.EQ.2) IPART=4*IPART
c	  REWIND 302
	  
c	  DO 20 IPAN=1,NETO
c	    NBRXI=NVAR(IPAN)																	
c		READ(302) KSA,KSR,KSE,EPSAP,EPSRP,DELTAP,HYAP,DYAP,WYAP,TYAP,		
c     *		      HXRP,DXRP,WXRP,TXRP,PHIL,Q,EPAIS
c		IF(NBRXI.EQ.0) GOTO 20
c		IF(ITYPE(IPAN).NE.5) THEN
c		  SPH2=VSIN(-PHILN(IPAN)/2.,0.D00)
c            C1=VCOS(TETAS(IPAN),0.D00)
c	      S1=VSIN(TETAS(IPAN),0.D00)
c            S2=VSIN(TETAS(IPAN),-PHILN(IPAN)/2.)
c		  PHIL=-PHILN(IPAN)*PI/180.
c		  D2=Q*(S1-2.*SPH2*S2/PHIL)
	    
c		  OMEGA=DABS(DELT(IPAN)*Q*PHIL)
c	      YNEU=Z(IPAN,3)+D2

c	      dIPANEL(1)=Q*DABS(PHIL)/12.*
c     *	             (PART(IPAN)*((C1*Q*PHIL)**2+
c     *				 (S1*PART(IPAN)*DELT(IPAN))**2)+
c     *				 PART(IPAN)*DELT(IPAN)*
c     *				(2.*(S1*PART(IPAN)*DELT(IPAN))*S1*PART(IPAN)))			! XI=1 épaisseur borde
c		  dIPANEL(6)=Q*DABS(PHIL)/12.*				
c     *				 (PART(IPAN)*DXRP/EPSRP*((C1*Q*PHIL)**2+
c     *				 (S1*PART(IPAN)*DELT(IPAN))**2)+		
c     *				 PART(IPAN)*DELT(IPAN)*
c     *		         (2.*(S1*PART(IPAN)*DELT(IPAN))*S1*
c     *				 PART(IPAN)*DXRP/EPSRP))								! XI=6 hauteur ame raidisseur
c            dIPANEL(7)=Q*DABS(PHIL)/12.*				
c     *				 (PART(IPAN)*HXRP/EPSRP*((C1*Q*PHIL)**2+		
c     *				 (S1*PART(IPAN)*DELT(IPAN))**2)+
c     *				 PART(IPAN)*DELT(IPAN)*
c     *				 (2.*(S1*PART(IPAN)*DELT(IPAN))*S1*
c     *				 PART(IPAN)*HXRP/EPSRP))								! XI=7 epaiss ame raidisseur
c            dIPANEL(8)=Q*DABS(PHIL)/12.*				
c     *				 (PART(IPAN)*TXRP/EPSRP*((C1*Q*PHIL)**2		
c     *				 +(S1*PART(IPAN)*DELT(IPAN))**2)+
c     *				 PART(IPAN)*DELT(IPAN)*
c     *				 (2.*(S1*PART(IPAN)*DELT(IPAN))*S1*
c     *				 PART(IPAN)*TXRP/EPSRP))								! XI=8 larg sem raidisseur
c		  dIPANEL(9)=Q*DABS(PHIL)/12.*				
c     *				 ((-1.)*PART(IPAN)*(HXRP*DXRP+WXRP*TXRP)/
c     *				 (EPSRP**2)*((C1*Q*PHIL)**2+(S1*PART(IPAN)*		
c     *				 DELT(IPAN))**2)+PART(IPAN)*DELT(IPAN)*
c     *				 (2.*(S1*PART(IPAN)*DELT(IPAN))*S1*(-1.)*
c     *				 PART(IPAN)*(HXRP*DXRP+WXRP*TXRP)/(EPSRP**2)))			! XI=9 larg sem raidisseur
c		  dSPANEL(1)=DABS(Q*PHIL)											! XI=1 epaisseur borde
c		  dSPANEL(6)=DXRP/EPSRP*DABS(Q*PHIL)								! XI=6 haut ame raid
c		  dSPANEL(7)=HXRP/EPSRP*DABS(Q*PHIL)								! XI=7 epaiss ame raid
c		  dSPANEL(8)=TXRP/EPSRP*DABS(Q*PHIL)								! XI=8 larg sem raid
c		  dSPANEL(9)=-(HXRP*DXRP+WXRP*TXRP)/(EPSRP*EPSRP)*DABS(Q*PHIL)		! XI=9 entredistance raid (EPSRP)

c		  DO L=1,NBRXI
c	        LL=NXIT(L,IPAN)		
c		    IF(SYMX.EQ.0) THEN
c			  dIPART(LL,IPAN)=dIPANEL(LL)+PART(IPAN)*dSPANEL(LL)*
c     *			    (YNEU-YNEUTPART)**2+
c     *			    PART(IPAN)*OMEGA*(2.*(YNEU-YNEUTPART)*
c     *			    (-1.)*PART(IPAN)*dSPANEL(LL)*
c     *			    (YNEU-YNEUTPART)/OMETPART)
c			  IF(SYM.EQ.1) dIPART(LL,IPAN)=2.*dIPART(LL,IPAN)
c	          IF(SYM.EQ.2) dIPART(LL,IPAN)=4.*dIPART(LL,IPAN)
c		    ELSE
c	          dIPART(LL,IPAN)=dIPANEL(LL)+PART(IPAN)*dSPANEL(LL)*
c     *				          ((YNEU-YNEUTPART)**2)
c			  IF(SYM.EQ.1) dIPART(LL,IPAN)=2.*dIPART(LL,IPAN)
c	          IF(SYM.EQ.2) dIPART(LL,IPAN)=4.*dIPART(LL,IPAN)
c			ENDIF
c	      ENDDO
c	    ENDIF
c  20	  CONTINUE
c	  REWIND 302	  
c	ENDIF

c      V1=Z(NEL,3)-YNEUTPART
c      V2=Z(NEL,4)-YNEUTPART

c	SIG1=PART(NEL)*BM1*V1/IPART
c	SIG2=PART(NEL)*BM1*V2/IPART

c	CALL ANNULD(dSPANEL,9)
c	CALL ANNULD(dSIG1,9*NETO)
c	CALL ANNULD(dSIG2,9*NETO)
c	DO 21 IPAN=1,NETO
c	  NBRXI=NVAR(IPAN)																	
c	  READ(302) KSA,KSR,KSE,EPSAP,EPSRP,DELTAP,HYAP,DYAP,WYAP,TYAP,		
c     *			HXRP,DXRP,WXRP,TXRP,PHIL,Q,EPAIS
c	  IF(NBRXI.EQ.0) GOTO 21
c	  IF(ITYPE(IPAN).NE.5) THEN
c	    SPH2=VSIN(-PHILN(IPAN)/2.,0.D00)
c          C1=VCOS(TETAS(IPAN),0.D00)
c	    S1=VSIN(TETAS(IPAN),0.D00)
c          S2=VSIN(TETAS(IPAN),-PHILN(IPAN)/2.)
c		PHIL=-PHILN(IPAN)*PI/180.
c		D2=Q*(S1-2.*SPH2*S2/PHIL)
	    
c	    YNEU=Z(IPAN,3)+D2

c	    dSPANEL(1)=DABS(Q*PHIL)											! XI=1 epaisseur borde
c		dSPANEL(6)=DXRP/EPSRP*DABS(Q*PHIL)								! XI=6 haut ame raid
c		dSPANEL(7)=HXRP/EPSRP*DABS(Q*PHIL)								! XI=7 epaiss ame raid
c		dSPANEL(8)=TXRP/EPSRP*DABS(Q*PHIL)								! XI=8 larg sem raid
c		dSPANEL(9)=-(HXRP*DXRP+WXRP*TXRP)/(EPSRP*EPSRP)*DABS(Q*PHIL)	! XI=9 entredistance raid (EPSRP)

c		DO L=1,NBRXI
c		  LL=NXIT(L,IPAN)
c		  IF(SYMX.EQ.0) THEN
c	        dSIG1(LL,IPAN)=PART(NEL)*BM1*
c     *					   ((-1.)*PART(IPAN)*dSPANEL(LL)*
c     *				       (YNEU-YNEUTPART)/OMETPART*IPART-
c     *					   V1*dIPART(LL,IPAN))/IPART**2
c			dSIG2(LL,IPAN)=PART(NEL)*BM1*
c     *					   ((-1.)*PART(IPAN)*dSPANEL(LL)*
c     *				       (YNEU-YNEUTPART)/OMETPART*IPART-
c     *					   V2*dIPART(LL,IPAN))/IPART**2
c	      ELSE
c	        dSIG1(LL,IPAN)=PART(NEL)*BM1*
c     *					   V1*(-dIPART(LL,IPAN)/IPART**2)
c		    dSIG2(LL,IPAN)=PART(NEL)*BM1*
c     *					   V2*(-dIPART(LL,IPAN)/IPART**2)
c	      ENDIF
c	    ENDDO
c	  ENDIF
c   21	CONTINUE
c	REWIND 302

c	COEFK=175.0E+06/SIGM


c	Elastic buckling of plates
c	--------------------------

	CALL ANNULD(dPSI,9*NETO)
	CALL ANNULD(dM,9*NETO)
	CALL ANNULD(dSIGEPL,9*NETO)
	CALL ANNULD(dDSIGPL,9*NETO)
	
	IF((EFF(4540).GE.0.).AND.(EFF(4570).GE.0.)) THEN	!panneau en traction
c	IF((SIG1.GE.0.).AND.(SIG2.GE.0.)) THEN				!panneau en traction
	  DSIGPL=0.
	  GOTO 30											
	ELSE
	  SIG1=-EFF(4540)
	  SIG2=-EFF(4570)
	  IF((SIG1.GE.0.).AND.(SIG2.GE.0.)) THEN	
	    IF(SIG1.LE.SIG2) THEN
		  SIGA=SIG2
	      PSI=SIG1/SIG2
	    ELSE
	      SIGA=SIG1
	      PSI=SIG2/SIG1
	    ENDIF
		DO IPAN=1,NETO
	      NBRXI=NVAR(IPAN)
	      DO L=1,NBRXI
	        LL=NXIT(L,IPAN)
	        dSIG1(LL,IPAN)=-SENS3(1,1,LL,IPAN)			!IPT=1 (y = 0)
		    dSIG2(LL,IPAN)=-SENS3(1,3,LL,IPAN)			!IPT=3 (y = b, largeur du panneau)
			IF(SIG1.LE.SIG2) THEN
      	      dPSI(LL,IPAN)=(dSIG1(LL,IPAN)*SIG2-SIG1*dSIG2(LL,IPAN))/
     *				        (SIG2*SIG2)
		    ELSE
      	      dPSI(LL,IPAN)=(dSIG2(LL,IPAN)*SIG1-SIG2*dSIG1(LL,IPAN))/
     *				        (SIG1*SIG1)
			ENDIF
		  ENDDO
	    ENDDO
	  ELSE												!panneau partiellement en compression
		IF(SIG1.GE.SIG2) THEN
		  SIGA=SIG1
	    ELSE
	      SIGA=SIG2
	    ENDIF
	    PSI=0.
	    DO IPAN=1,NETO
	      NBRXI=NVAR(IPAN)
	      DO L=1,NBRXI
	        LL=NXIT(L,IPAN)
	        dSIG1(LL,IPAN)=-SENS3(1,1,LL,IPAN)			!IPT=1 (y = 0)
		    dSIG2(LL,IPAN)=-SENS3(1,3,LL,IPAN)			!IPT=3 (y = b, largeur du panneau)
		    dPSI(LL,IPAN)=0.
	      ENDDO
	    ENDDO
	  ENDIF
	  IF(HXR.GT.0.010) THEN							!Plating with longitudinal stiffeners
          M=8.4/(PSI+1.1)
	    SIGEPL=0.9*M*E*(DELTA/EPSR)**2
	  ELSE											!Plating with transverse stiffeners
	    DO IPAN=1,NETO
	      IF(IPAN.EQ.NEL) THEN
	        READ(302) KSA,KSR,KSE,EPSAP,EPSRP,DELTAP,HYAP,DYAP,WYAP,		
     *		          TYAP,HXRP,DXRP,WXRP,TXRP,PHIL,Q,EPAIS,EPSA2
		    IF(EPSA2.GE.(0.00001)) THEN
			  BACKSPACE(302)
			  READ(302) KSA,KSR,KSE,EPSAP,EPSRP,DELTAP,HYAP,DYAP,		
     *		            WYAP,TYAP,HXRP,DXRP,WXRP,TXRP,PHIL,Q,EPAIS,
     *				    EPSA2,HYA2,DYA2,WYA2,TYA2
              ENDIF
	        GOTO 40
	      ELSE
	        READ(302)
	      ENDIF
	    ENDDO
   40	    REWIND 302
          PHIL=-PHILN(NEL)*PI/180.
		IF(EPSA2.GE.(0.00001)) THEN
	      IF(EPSA2.LT.EPSA) THEN		
c	        IF(WYA2.LT.0.010) THEN		!Flat bars
c	          C=1.05
c			ELSE						!Angles or t-sections
c	          C=1.21
c			ENDIF
			C=1.0
			IF(EPSA2.LT.DABS(Q*PHIL)) THEN
			  M=C*(1+(EPSA2/DABS(Q*PHIL))**2)**2*2.1/(PSI+1.1)
	          SIGEPL=0.9*M*E*(DELTA/EPSA2)**2
	        ELSE
	          M=C*(1+(DABS(Q*PHIL)/EPSA2)**2)**2*2.1/(PSI+1.1)
			  SIGEPL=0.9*M*E*(DELTA/DABS(Q*PHIL))**2
			ENDIF
		  ELSE
c	        IF(WYA.LT.0.010) THEN		!Flat bars
c	          C=1.05
c			ELSE						!Angles or t-sections
c	          C=1.21
c			ENDIF
			C=1.0
			IF(EPSA.LT.DABS(Q*PHIL)) THEN
			  M=C*(1+(EPSA/DABS(Q*PHIL))**2)**2*2.1/(PSI+1.1)
	          SIGEPL=0.9*M*E*(DELTA/EPSA)**2
	        ELSE
	          M=C*(1+(DABS(Q*PHIL)/EPSA)**2)**2*2.1/(PSI+1.1)
	          SIGEPL=0.9*M*E*(DELTA/DABS(Q*PHIL))**2
			ENDIF
	      ENDIF
		ELSE
c	      C=1.3												!Floors or deep girders (primary supporting members)
		  C=1.0
		  IF(EPSA.LT.DABS(Q*PHIL)) THEN
			M=C*(1+(EPSA/DABS(Q*PHIL))**2)**2*2.1/(PSI+1.1)
			SIGEPL=0.9*M*E*(DELTA/EPSA)**2
	      ELSE
	        M=C*(1+(DABS(Q*PHIL)/EPSA)**2)**2*2.1/(PSI+1.1)
			SIGEPL=0.9*M*E*(DELTA/DABS(Q*PHIL))**2
		  ENDIF
		ENDIF
	  ENDIF
        SIGC=SIGEPL
	  DSIGPL=SIGA/SIGC
	  DO IPAN=1,NETO
	    NBRXI=NVAR(IPAN)
	    DO L=1,NBRXI
	      LL=NXIT(L,IPAN)
	      IF(HXR.GT.0.010) THEN										!Plating with longitudinal stiffeners
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
	      ELSE														!Plating with transverse stiffeners
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
		  IF(SIG1.GE.SIG2) THEN
	        dDSIGPL(LL,IPAN)=(-dSIG1(LL,IPAN)*SIGC-
     *		 			     SIGA*dSIGEPL(LL,IPAN))/SIGC**2
	      ELSE
	        dDSIGPL(LL,IPAN)=(-dSIG2(LL,IPAN)*SIGC-
     *						 SIGA*dSIGEPL(LL,IPAN))/SIGC**2
	      ENDIF
		ENDDO
	  ENDDO
	  IF(ITERA.GE.1) WRITE(666,2) NEL,1.e-6*SIGEPL,1.e-6*SIGC,DSIGPL
	ENDIF

    1 FORMAT(I3,39x,F8.3,3x,F6.3,6x,'plate')
    2 FORMAT(I3,15x,F8.3,16x,F8.3,3x,F6.3,6x,'plate')

   30 IF(IBUCK.EQ.1) RETURN


c	Elastic buckling of longitudinals
c	---------------------------------
	
	CALL ANNULD(dKP,9*NETO)
	CALL ANNULD(dC,9*NETO)
	CALL ANNULD(dK,9*NETO)
	CALL ANNULD(dSIGESTIF,9*NETO)
	CALL ANNULD(dDSIGSTIF,9*NETO)
	
c	IF((EFF(7855).GE.0).AND.(EFF(7885).GE.0)) THEN
	IF(DSIGPL.EQ.0.) THEN
	  DSIGSTIF=0.
	  RETURN		
	ELSE
c	  SIG1=-EFF(7855)
c	  SIG2=-EFF(7885)
	  IF(SIG1.GE.SIG2) THEN
		SIGA=SIG1
	  ELSE
		SIGA=SIG2
	  ENDIF

	  AIRE=DELTA*EPSR+HXR*DXR+WXR*TXR
	  dAIRE(1)=EPSR
	  dAIRE(6)=DXR
	  dAIRE(7)=HXR
	  dAIRE(8)=TXR
	  dAIRE(9)=DELTA

	  MSTAT=DELTA*EPSR*DELTA/2.+HXR*DXR*(HXR/2.+DELTA)+
     *	    WXR*TXR*(TXR/2.+HXR+DELTA)
	  dMSTAT(1)=EPSR*DELTA+HXR*DXR+WXR*TXR
	  dMSTAT(6)=DXR*(HXR/2.+DELTA)+0.5*HXR*DXR+WXR*TXR
	  dMSTAT(7)=HXR*(HXR/2.+DELTA)
	  dMSTAT(8)=TXR*(TXR/2.+HXR+DELTA)
	  dMSTAT(9)=DELTA*DELTA/2.

	  AXEN=MSTAT/AIRE
	  DO I=1,9
	    dAXEN(I)=(dMSTAT(I)*AIRE-MSTAT*dAIRE(I))/(AIRE*AIRE)
	  ENDDO

	  IA=EPSR*DELTA**3/12.+DELTA*EPSR*(DELTA/2.-AXEN)**2+
     *	 DXR*HXR**3/12.+HXR*DXR*(HXR/2.+DELTA-AXEN)**2+
     *	 WXR*TXR**3/12.+WXR*TXR*(TXR/2.+HXR+DELTA-AXEN)**2
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

	  SIGESTIF=10.*E*IA/(AIRE*EPSA*EPSA)
	  DO I=1,9
	    dSIGESTIF(I,NEL)=10.*E/(EPSA*EPSA)*(dIA(I)*AIRE-IA*dAIRE(I))/
     *				     AIRE**2
	  ENDDO
	  dSIGESTIF(5,NEL)=-2.*10.*E*IA/AIRE*(1./EPSA**3)

	  BETA=1.1
	  IF(SIGESTIF.LE.SIGY/2.) THEN											!critère de Johnson
	    SIGC=SIGESTIF/BETA
	  ELSE
	    SIGC=SIGY*(1-0.25*SIGY/SIGESTIF)/BETA
	  ENDIF
	  DSIGSTIF=SIGA/SIGC
	  DO IPAN=1,NETO
	    NBRXI=NVAR(IPAN)
		DO L=1,NBRXI
	      LL=NXIT(L,IPAN)
		  IF(SIGESTIF.LE.SIGY/2.) THEN
			IF(SIG1.GE.SIG2) THEN
			  dDSIGSTIF(LL,IPAN)=(dSIG1(LL,IPAN)*SIGC-SIGA*
     *						      dSIGESTIF(LL,IPAN)/BETA)/SIGC**2
	        ELSE
			  dDSIGSTIF(LL,IPAN)=(dSIG2(LL,IPAN)*SIGC-SIGA*
     *						      dSIGESTIF(LL,IPAN)/BETA)/SIGC**2
			ENDIF
	      ELSE
			IF(SIG1.GE.SIG2) THEN
		      dDSIGSTIF(LL,IPAN)=(dSIG1(LL,IPAN)*SIGC-SIGA*
     *						      SIGY*(0.25*SIGY*dSIGESTIF(LL,IPAN)
     *						      /SIGESTIF**2)/BETA)/SIGC**2							   
			ELSE
			  dDSIGSTIF(LL,IPAN)=(dSIG2(LL,IPAN)*SIGC-SIGA*
     *					          SIGY*(0.25*SIGY*dSIGESTIF(LL,IPAN)
     *						      /SIGESTIF**2)/BETA)/SIGC**2							   
			ENDIF
		  ENDIF
		ENDDO
	  ENDDO

	  IF(ITERA.GE.1) THEN
	    WRITE(666,4) NEL,1.e-6*SIGESTIF,1.e-6*SIGC,DSIGSTIF
	  ENDIF

	ENDIF


    3 FORMAT(I3,39x,F8.3,3x,F6.3,6x,'stiffener')
    4 FORMAT(I3,4x,F8.3,3x,F6.3,6x,'stiffener')      


      RETURN
      END
