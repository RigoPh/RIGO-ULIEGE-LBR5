      SUBROUTINE BENDYIELD(NEL,NETO,NVAR,NXIT,SIG,dSIG,NTOT,SIGA,dSIGA)


      IMPLICIT REAL*8 (A-H,O-Z)

	DIMENSION NVAR(NETO),NXIT(9,NETO)

	DIMENSION dSIGA(NTOT)
	
	DIMENSION SIG(2),dSIG(2,9,NETO)	

c	*****************************************************************************************************

	CALL ANNULD(dSIGA,NTOT)
	
	COEF=1.
	IF(DABS(SIG(1)).GE.DABS(SIG(2))) THEN
	  SIGA=DABS(SIG(1))
	  IF(SIGA.NE.SIG(1)) COEF=-1.
	  L1=0
	  DO IPAN=1,NETO
	    NBRXI=NVAR(IPAN)
		DO L=1,NBRXI
	      LL=NXIT(L,IPAN)
		  dSIGA(L1+L)=COEF*dSIG(1,LL,IPAN)
		ENDDO
	    L1=L1+NBRXI
	  ENDDO
	ELSE
	  SIGA=DABS(SIG(2))
	  IF(SIGA.NE.SIG(2)) COEF=-1.
	  L1=0
	  DO IPAN=1,NETO
	    NBRXI=NVAR(IPAN)
		DO L=1,NBRXI
	      LL=NXIT(L,IPAN)
		  dSIGA(L1+L)=COEF*dSIG(2,LL,IPAN)
		ENDDO
	    L1=L1+NBRXI
	  ENDDO
	ENDIF	  

      RETURN
      END