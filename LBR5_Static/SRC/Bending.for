	SUBROUTINE BENDING(NEL,NETO,Z,PHILN,TETAS,ITYPE,PART,DELT,
     *				   BM,NVAR,NXIT,IOPTI,YNEUT,dYNEUT,INERT,dINERT,
     *				   SIG,dSIG)


	IMPLICIT REAL*8 (A-H,O-Z)

	REAL*8 INERT
		
	DIMENSION Z(NETO,4),PHILN(NETO),TETAS(NETO),ITYPE(NETO),
     *		  PART(NETO),DELT(NETO)

	DIMENSION NVAR(NETO),NXIT(9,NETO)

	DIMENSION dYNEUT(9,NETO),dINERT(9,NETO)
	
	DIMENSION SIG(2),dSIG(2,9,NETO)									!résultats

      COMMON /PY/PI
C*********************************************************************************

c	Contraintes normales d'ensemble
c	-------------------------------
	V1=Z(NEL,3)-YNEUT
      V2=Z(NEL,4)-YNEUT

	SIG(1)=PART(NEL)*BM*V1/INERT
	SIG(2)=PART(NEL)*BM*V2/INERT
	

c	Sensibilités des contraintes normales d'ensemble
c	------------------------------------------------
	IF(IOPTI.GE.1) THEN

	  V1=Z(NEL,3)-YNEUT
        V2=Z(NEL,4)-YNEUT
		  
	  DO 2 IPAN=1,NETO
	    NBRXI=NVAR(IPAN)																	
	    IF(NBRXI.EQ.0) GOTO 2
	    IF(ITYPE(IPAN).NE.5) THEN
		  DO L=1,NBRXI
		    LL=NXIT(L,IPAN)
	        dSIG(1,LL,IPAN)=PART(NEL)*BM*((-1.)*dYNEUT(LL,IPAN)*INERT-
     *					    V1*dINERT(LL,IPAN))/INERT**2
		    dSIG(2,LL,IPAN)=PART(NEL)*BM*((-1.)*dYNEUT(LL,IPAN)*INERT-
     *					    V2*dINERT(LL,IPAN))/INERT**2
	      ENDDO
	    ENDIF
    2	  CONTINUE

      ENDIF

      RETURN
      END