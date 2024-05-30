      SUBROUTINE RECIP(N,A,BL,M1,M2)
C     ******************************

      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER*2 M1,M2
      DIMENSION A(N,N),BL(N,N),M1(N),M2(N)

C***********************************************************************
C
C     SUBROUTINE RECIP
C     *****************
C     CALCUL DE L'INVERSE D'UNE MATRICE A(N,N) DE DIMENSION N x N (INPUT/OUTPUT)

C     TYPE : INVERSION D'UNE MATRICE COMPLETE EN MEMOIRE CENTRALE.
C     *****
C     Les vecteurs BL(N,N) et M1(N),M2(N) sont des vecteurs de travail.
C     La matrice inverse est stokée dans A (matrice de départ est donc perdue)

C     Modifié : 18-8-94 			             Créer: 10-8-94
C     (Ph. Rigo pour MODULE OPTIMISATION)		  (par Wang. Hengfeng)
C***********************************************************************

CC Assign original value to BL and M2.
      DO I=1,N
        DO J=1,N
          BL(I,J)=0.0
        END DO
        BL(I,I)=1.0
        M2(I)=0
      END DO
CC Normalize former MATRIX A and B.
      DO I=1,N
        AM=A(I,1)
        DO J=1,N
          IF (DABS(A(I,J)).GT.DABS(AM)) AM=A(I,J)
        END DO
        IF (AM.EQ.0.0) THEN
          WRITE(*,*) ' ERROR 1 IN SUBROUTINE RECIP, STOP'
          WRITE(*,*) '  In Equation n0',I,' Amax=',AM,'!!'
          WRITE(66,*)' ERROR 1 IN SUBROUTINE RECIP, STOP'
          WRITE(66,*)'  In Equation n0',I,' Amax=',AM,'!!'
          WRITE(29,*)' ERROR 1 IN SUBROUTINE RECIP, STOP'							!bug
          WRITE(29,*)'  In Equation n0',I,' Amax=',AM,'!!'						!bug
c	    STOP
        END IF
        DO J=1,N
          A(I,J)=A(I,J)/AM
        END DO
        BL(I,I)=BL(I,I)/AM
      END DO
CC Make A into unit I, AND BL into reciprocal matrix of A at the same time
      DO I=1,N
C       Find large element
        AM=0.0
        DO J=1,N
          IF (M2(J).EQ.0) THEN
            IF (DABS(A(J,I)).GT.DABS(AM)) THEN
              AM=A(J,I)
              M1(I)=J
            END IF
          END IF
        END DO
C       If large element is too small, print error and stop.
        IF (DABS(AM).LT.1.0E-20) THEN												
          WRITE(*,*) ' ERROR 2 IN SUBROUTINE RECIP, STOP'
          WRITE(*,*) '  In Equation n0',I,' Pivot=',AM,'!!'
          WRITE(66,*)' ERROR 2 IN SUBROUTINE RECIP, STOP'
          WRITE(66,*)'  In Equation n0',I,' Pivot=',AM,'!!'
          WRITE(29,*)' ERROR 2 IN SUBROUTINE RECIP, STOP'							!bug
          WRITE(29,*)'  In Equation n0',I,' Pivot=',AM,'!!'						!bug
          STOP
        END IF
C       Normalize the large element row.
        IM=M1(I)
        M2(IM)=1
        DO J=1,N
          BL(IM,J)=BL(IM,J)/AM
          IF (J.GE.I) THEN
            A(IM,J)=A(IM,J)/AM
          END IF
        END DO
C       Eliminate elements on the large element column.
        DO K=1,N
          IF (K.NE.IM) THEN
            IF (A(K,I).NE.0.0) THEN
              AK=A(K,I)
              DO J=1,N
                BL(K,J)=BL(K,J)-BL(IM,J)*AK
                IF (J.GE.I) THEN
                  A(K,J)=A(K,J)-A(IM,J)*AK
                END IF
              END DO
            END IF
          END IF
        END DO
      END DO
CC Restore BL, the reciprocal matrix of A, into A.
      DO I=1,N
        DO J=1,N
          A(I,J)=BL(M1(I),J)
        END DO
      END DO
CC Terminate normally.
      RETURN
      END
