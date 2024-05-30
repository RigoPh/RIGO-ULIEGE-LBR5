      SUBROUTINE SHEARBUCK(NEL,NETO,E,SIGY,Q,PHILN,NVAR,NXIT,EPSA,EPSR,
     *				     DELTA,HXR,EPSA2,TAU,dTAU,SF,TRAV,NTOT,
     *					 DTAUPL,dDTAUPL)


      IMPLICIT REAL*8 (A-H,O-Z)
	DIMENSION TRAV(1)

	I1=1			!dKT(9*NETO)
	I2=I1+9*NETO	!dTAUE(9*NETO)
	I3=I2+9*NETO	!dTAUA(9*NETO)
	I4=I3+9*NETO		

	IMAX=I4-1		!27*NETO

	CALL SSHEARBUCK(NEL,NETO,E,SIGY,Q,PHILN,NVAR,NXIT,EPSA,EPSR,DELTA,
     *		        HXR,EPSA2,TAU,dTAU,SF,TRAV(I1),TRAV(I2),
     *				TRAV(I3),NTOT,DTAUPL,dDTAUPL)

	RETURN
	END

c	******************************************************************************************************
c	******************************************************************************************************
c	******************************************************************************************************

	SUBROUTINE SSHEARBUCK(NEL,NETO,E,SIGY,Q,PHILN,NVAR,NXIT,EPSA,EPSR,
     *					  DELTA,HXR,EPSA2,TAU,dTAU,SF,dKT,dTAUE,
     *					  dTAUA,NTOT,DTAUPL,dDTAUPL)


      IMPLICIT REAL*8 (A-H,O-Z)

	REAL *8 KT

	DIMENSION TAU(NETO,3),dTAU(NETO,3,NETO)
	
	DIMENSION PHILN(NETO),NVAR(NETO),NXIT(9,NETO)

	DIMENSION dDTAUPL(NTOT)
	
	DIMENSION dKT(NTOT),dTAUE(NTOT),dTAUA(NTOT)		
     
	COMMON/PY/PI

c	*****************************************************************************************************

c	Elastic buckling of plates
c	--------------------------

	CALL ANNULD(dKT,9*NETO)
	CALL ANNULD(dTAUE,9*NETO)
	CALL ANNULD(dTAUA,NTOT)
	
	COEF=1.
	TAUMAX=MAX(DABS(TAU(NEL,1)),DABS(TAU(NEL,2)),DABS(TAU(NEL,3)))
	IF(DABS(TAU(NEL,1)).EQ.TAUMAX) THEN
	  TAUA=DABS(SF*TAU(NEL,1))
	  IF(TAUA.NE.(SF*TAU(NEL,1))) COEF=-1.
	  L1=0
	  DO IPAN=1,NETO
	    NBRXI=NVAR(IPAN)
		DO L=1,NBRXI
	      LL=NXIT(L,IPAN)
		  IF(LL.EQ.1) THEN
		    dTAUA(L1+L)=COEF*SF*dTAU(NEL,1,IPAN)
			GOTO 1
	      ENDIF
		ENDDO
    1	    L1=L1+NBRXI
	  ENDDO
	ELSEIF(DABS(TAU(NEL,2)).EQ.TAUMAX) THEN
	  TAUA=DABS(SF*TAU(NEL,2))
	  IF(TAUA.NE.(SF*TAU(NEL,2))) COEF=-1.
	  L1=0
	  DO IPAN=1,NETO
	    NBRXI=NVAR(IPAN)
		DO L=1,NBRXI
	      LL=NXIT(L,IPAN)
		  IF(LL.EQ.1) THEN
		    dTAUA(L1+L)=COEF*SF*dTAU(NEL,2,IPAN)
			GOTO 2
	      ENDIF
		ENDDO
    2	    L1=L1+NBRXI
	  ENDDO
	ELSEIF(DABS(TAU(NEL,3)).EQ.TAUMAX) THEN
	  TAUA=DABS(SF*TAU(NEL,3))
	  IF(TAUA.NE.(SF*TAU(NEL,3))) COEF=-1.
	  L1=0
	  DO IPAN=1,NETO
	    NBRXI=NVAR(IPAN)
		DO L=1,NBRXI
	      LL=NXIT(L,IPAN)
		  IF(LL.EQ.1) THEN
		    dTAUA(L1+L)=COEF*SF*dTAU(NEL,3,IPAN)
			GOTO 3
	      ENDIF
		ENDDO
    3	    L1=L1+NBRXI
	  ENDDO
	ENDIF	  

      IF(HXR.GT.0.010) THEN												!Plating with longitudinal stiffeners
	  IF(EPSA2.GE.(0.00001)) THEN
	    IF(EPSA2.LT.EPSA) THEN
		  KT=5.34+4.*(EPSR/EPSA2)**2
	      L1=0
	      DO IPAN=1,NETO
	        NBRXI=NVAR(IPAN)
	        DO L=1,NBRXI
	          LL=NXIT(L,IPAN)
	          IF(NEL.EQ.IPAN) THEN
	            IF(LL.EQ.9) THEN
	              dKT(L1+L)=4.*2.*(EPSR/EPSA2)*(1./EPSA2)
	            ENDIF
	          ENDIF
	        ENDDO
     	        L1=L1+NBRXI
            ENDDO
	      GOTO 4
	    ENDIF
	  ENDIF
	  KT=5.34+4.*(EPSR/EPSA)**2
	  L1=0
	  DO IPAN=1,NETO
	    NBRXI=NVAR(IPAN)
	    DO L=1,NBRXI
	      LL=NXIT(L,IPAN)
	      IF(NEL.EQ.IPAN) THEN
		    IF(LL.EQ.5) THEN
	          dKT(L1+L)=4.*2.*(EPSR/EPSA)*(-EPSR/EPSA**2)
	        ELSEIF(LL.EQ.9) THEN
	          dKT(L1+L)=4.*2.*(EPSR/EPSA)*(1./EPSA)
	        ENDIF
	      ENDIF
	    ENDDO
     	    L1=L1+NBRXI
        ENDDO
    4	  TAUE=0.9*KT*E*(DELTA/EPSR)**2
	  L1=0
	  DO IPAN=1,NETO
	    NBRXI=NVAR(IPAN)
	    DO L=1,NBRXI
	      LL=NXIT(L,IPAN)
	      IF(NEL.EQ.IPAN) THEN
	        IF(LL.EQ.1) THEN
	          dTAUE(L1+L)=0.9*E*KT*2.*(DELTA/EPSR)*(1./EPSR)
		    ELSEIF(LL.EQ.5) THEN
	          dTAUE(L1+L)=0.9*E*(dKT(L1+L)*(DELTA/EPSR)**2)
	        ELSEIF(LL.EQ.9) THEN
	          dTAUE(L1+L)=0.9*E*(dKT(L1+L)*(DELTA/EPSR)**2+
     *					  KT*2.*(DELTA/EPSR)*(-DELTA/EPSR**2))
	        ENDIF
	      ENDIF
	    ENDDO
     	    L1=L1+NBRXI
        ENDDO
	ELSE																!Plating with transverse stiffeners
	  PHIL=-PHILN(NEL)*PI/180.
	  IF(EPSA2.GE.(0.00001)) THEN
		IF(EPSA2.LT.EPSA) THEN
	      IF(EPSA2.LT.DABS(Q*PHIL)) THEN
		    KT=5.34+4.*(EPSA2/DABS(Q*PHIL))**2
	        TAUE=0.9*KT*E*(DELTA/EPSA2)**2
	        L1=0
	        DO IPAN=1,NETO
	          NBRXI=NVAR(IPAN)
	          DO L=1,NBRXI
	            LL=NXIT(L,IPAN)
	            IF(NEL.EQ.IPAN) THEN
	              IF(LL.EQ.1) THEN
	                dTAUE(L1+L)=0.9*E*KT*2.*(DELTA/EPSA2)*(1./EPSA2)
		          ENDIF
	            ENDIF
	          ENDDO
     	          L1=L1+NBRXI
              ENDDO
	      ELSE
	        KT=5.34+4.*(DABS(Q*PHIL)/EPSA2)**2
			TAUE=0.9*KT*E*(DELTA/DABS(Q*PHIL))**2
			L1=0
	        DO IPAN=1,NETO
	          NBRXI=NVAR(IPAN)
	          DO L=1,NBRXI
	            LL=NXIT(L,IPAN)
	            IF(NEL.EQ.IPAN) THEN
	              IF(LL.EQ.1) THEN
	                dTAUE(L1+L)=0.9*E*KT*2.*(DELTA/DABS(Q*PHIL))*
     *							(1./DABS(Q*PHIL))
		          ENDIF
	            ENDIF
	          ENDDO
     	          L1=L1+NBRXI
              ENDDO
	      ENDIF
	    ELSE
	      IF(EPSA.LT.DABS(Q*PHIL)) THEN
		    KT=5.34+4.*(EPSA/DABS(Q*PHIL))**2
	        TAUE=0.9*KT*E*(DELTA/EPSA)**2
	        L1=0
	        DO IPAN=1,NETO
	          NBRXI=NVAR(IPAN)
	          DO L=1,NBRXI
	            LL=NXIT(L,IPAN)
	            IF(NEL.EQ.IPAN) THEN
	              IF(LL.EQ.1) THEN
	                dTAUE(L1+L)=0.9*E*KT*2.*(DELTA/EPSA)*(1./EPSA)
	              ELSEIF(LL.EQ.5) THEN
	                dKT(L1+L)=4.*2.*(EPSA/DABS(Q*PHIL))
	                dTAUE(L1+L)=0.9*E*(dKT(L1+L)*(DELTA/EPSA)**2+
     *					        KT*2.*(DELTA/EPSA)*(-DELTA/EPSA**2))
		          ENDIF
	            ENDIF
	          ENDDO
     	          L1=L1+NBRXI
              ENDDO
	      ELSE
	        KT=5.34+4.*(DABS(Q*PHIL)/EPSA)**2
		    TAUE=0.9*KT*E*(DELTA/DABS(Q*PHIL))**2
		    L1=0
	        DO IPAN=1,NETO
	          NBRXI=NVAR(IPAN)
	          DO L=1,NBRXI
	            LL=NXIT(L,IPAN)
	            IF(NEL.EQ.IPAN) THEN
	              IF(LL.EQ.1) THEN
	                dTAUE(L1+L)=0.9*E*KT*2.*(DELTA/DABS(Q*PHIL))*
     *						    (1./DABS(Q*PHIL))
				  ELSEIF(LL.EQ.5) THEN
				    dKT(L1+L)=4.*2.*(DABS(Q*PHIL)/EPSA)*
     *					      (-DABS(Q*PHIL)/EPSA**2)
				    dTAUE(L1+L)=0.9*E*(dKT(L1+L)*(DELTA/
     *						    DABS(Q*PHIL))**2)
		          ENDIF
	            ENDIF
	          ENDDO
     	          L1=L1+NBRXI
              ENDDO
	      ENDIF
	    ENDIF
	  ELSE
    	    IF(EPSA.LT.DABS(Q*PHIL)) THEN
		  KT=5.34+4.*(EPSA/DABS(Q*PHIL))**2
	      TAUE=0.9*KT*E*(DELTA/EPSA)**2
	      L1=0
	      DO IPAN=1,NETO
	        NBRXI=NVAR(IPAN)
	        DO L=1,NBRXI
	          LL=NXIT(L,IPAN)
	          IF(NEL.EQ.IPAN) THEN
	            IF(LL.EQ.1) THEN
	              dTAUE(L1+L)=0.9*E*KT*2.*(DELTA/EPSA)*(1./EPSA)
	            ELSEIF(LL.EQ.5) THEN
	              dKT(L1+L)=4.*2.*(EPSA/DABS(Q*PHIL))
	              dTAUE(L1+L)=0.9*E*(dKT(L1+L)*(DELTA/EPSA)**2+
     *					      KT*2.*(DELTA/EPSA)*(-DELTA/EPSA**2))
		        ENDIF
	          ENDIF
	        ENDDO
     	        L1=L1+NBRXI
            ENDDO
	    ELSE
	      KT=5.34+4.*(DABS(Q*PHIL)/EPSA)**2
		  TAUE=0.9*KT*E*(DELTA/DABS(Q*PHIL))**2
		  L1=0
	      DO IPAN=1,NETO
	        NBRXI=NVAR(IPAN)
	        DO L=1,NBRXI
	          LL=NXIT(L,IPAN)
	          IF(NEL.EQ.IPAN) THEN
	            IF(LL.EQ.1) THEN
	              dTAUE(L1+L)=0.9*E*KT*2.*(DELTA/DABS(Q*PHIL))*
     *						  (1./DABS(Q*PHIL))
				ELSEIF(LL.EQ.5) THEN
				  dKT(L1+L)=4.*2.*(DABS(Q*PHIL)/EPSA)*
     *					    (-DABS(Q*PHIL)/EPSA**2)
				  dTAUE(L1+L)=0.9*E*(dKT(L1+L)*(DELTA/
     *						  DABS(Q*PHIL))**2)
		        ENDIF
	          ENDIF
	        ENDDO
     	        L1=L1+NBRXI
            ENDDO
	    ENDIF
	  ENDIF
	ENDIF
	
	IF(TAUE.LE.((SIGY/SQRT(3.))/2.)) THEN
	  DTAUPL=TAUA/TAUE
	  L1=0
	  DO IPAN=1,NETO
	    NBRXI=NVAR(IPAN)
	    DO L=1,NBRXI
	      LL=NXIT(L,IPAN)
	      dDTAUPL(L1+L)=(dTAUA(L1+L)*TAUE-TAUA*dTAUE(L1+L))/TAUE**2
	    ENDDO
     	    L1=L1+NBRXI
        ENDDO
	ELSE
	  DTAUPL=TAUA/((SIGY/SQRT(3.))*(1.-0.25*(SIGY/SQRT(3.))/TAUE))
	  L1=0
	  DO IPAN=1,NETO
	    NBRXI=NVAR(IPAN)
	    DO L=1,NBRXI
	      LL=NXIT(L,IPAN)
	      dDTAUPL(L1+L)=1./(SIGY/SQRT(3.))*(dTAUA(L1+L)*
     *				    (1.-0.25*(SIGY/SQRT(3.))/TAUE)-TAUA*
     *				    (0.25*(SIGY/SQRT(3.))*dTAUE(L1+L)/TAUE**2))
     *			        /(1.-0.25*(SIGY/SQRT(3.))/TAUE)**2
	    ENDDO
     	    L1=L1+NBRXI
        ENDDO
	ENDIF

      RETURN
      END
