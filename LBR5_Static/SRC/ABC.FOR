						
      SUBROUTINE ANNULI(MN,NL)
C     *****************************
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION MN(NL)
      DATA IZERO/0/,IUN/1/
      DO  10  I=IUN,NL
           MN(I)=IZERO  
10    CONTINUE
      RETURN
      END



      SUBROUTINE ANNULD (S,N)
C     *****************************
      REAL*8 S(N)
      ZERO=0.D0
      DO    10     I=1,N
           S(I)= ZERO
   10 CONTINUE
      RETURN
      END




      REAL*8 FUNCTION EXPO(AL)
C=======================================================================
C "EXPO" CALCULE LA VALEUR DE " EXP(-AL) "
C        et VERIFIE SI ON NE CREE PAS UN "UNDERFLOW"
C
C     Créer : 1992 (thèse), Ph. RIGO
C=======================================================================

      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/PY/PI

      IF(AL.GE.150.) THEN
        EXPO=0.
      ELSE
        EXPO=DEXP(-AL)
      ENDIF

      RETURN
      END                                                               
