	SUBROUTINE IMPRTAU(TAUNET,TAUGRO,NEL,NETO,SF)

	IMPLICIT REAL*8 (A-H,O-Z)

	DIMENSION TAUNET(NETO,3),TAUGRO(NETO,3),TEMP(6)

C*********************************************************************************

	WRITE(68,'(1x,I3,T15,A,T31,F8.3,T44,F8.3,T57,
     *	  F8.3,T73,F8.3,T86,F8.3,T99,F8.3)') NEL,'/Txy/',
     *	  (SF*DABS(TAUNET(NEL,1))/1.E+06),
     *	  (SF*DABS(TAUNET(NEL,2))/1.E+06),
     *	  (SF*DABS(TAUNET(NEL,3))/1.E+06),
     *	  (SF*DABS(TAUGRO(NEL,1))/1.E+06),
     *	  (SF*DABS(TAUGRO(NEL,2))/1.E+06),
     *	  (SF*DABS(TAUGRO(NEL,3))/1.E+06)

      RETURN
      END