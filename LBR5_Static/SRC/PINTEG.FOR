
      SUBROUTINE PINTEG(PHIL,TETA,L,LL,SOM,Z2,MT,Q,PI,ABTR,
     *                  C1,C2,S1,S2,IT,VMAX)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Z2(2295),ABTR(10)
C***********************************************************************
C
C      SUBROUTINE INTEGR
C      SUBROUTINE D'INTEGRATION D'UNE FONCTION SUR UN ESPACE DONNE PAR
C      CUMULATION.
C
C******************************************************************+****
      SM=PHIL/30.
      IJ=(L-1)*51
      JI=(LL-1)*51
      SOM=0.
C
      VMAX=0.
      IF(MT.EQ.0) THEN
C
      DO 15 I=1,31
      GOTO(20,21,22),IT
C 20  WRITE(6,*)SOM
  20  SOM=SOM+Z2(IJ+I)*DCOS((TETA+SM*(I-1))*PI/180.)
     *       +Z2(JI+I)*DSIN((TETA+SM*(I-1))*PI/180.)
C     TEMP1=Z2(IJ+I)*DCOS((TETA+SM*(I-1))*PI/180.)
C     TEMP2=Z2(JI+I)*DSIN((TETA+SM*(I-1))*PI/180.)
C     WRITE(6,*)Z2(IJ+I),DCOS((TETA+SM*(I-1))*PI/180.),TEMP1
C     WRITE(6,*)Z2(JI+I),DSIN((TETA+SM*(I-1))*PI/180.),TEMP2,SOM
      GOTO 15
  21  SOM=SOM+Z2(IJ+I)
      VVAL=DABS(Z2(IJ+I))
      IF(VVAL.GT.VMAX)VMAX=VVAL
      GOTO 15
C     WRITE(6,*) SOM
  22  SOM=SOM+Z2(IJ+I)*DCOS((TETA+SM*(I-1))*PI/180.)
     *       -Z2(JI+I)*DSIN((TETA+SM*(I-1))*PI/180.)
C     TEMP1=Z2(IJ+I)*DCOS((TETA+SM*(I-1))*PI/180.)
C     TEMP2=-Z2(JI+I)*DSIN((TETA+SM*(I-1))*PI/180.)
C     WRITE(6,*)Z2(IJ+I),DCOS((TETA+SM*(I-1))*PI/180.),TEMP1
C     WRITE(6,*)Z2(JI+I),DSIN((TETA+SM*(I-1))*PI/180.),TEMP2,SOM
  15  CONTINUE
C
      GOTO(23,24,25),IT
  23  SOM=SOM-(Z2(IJ+1)*C1+Z2(IJ+31)*C2)*0.5
     *       -(Z2(JI+1)*S1+Z2(JI+31)*S2)*0.5
      GOTO 26
  24  SOM=SOM-(Z2(IJ+1)+Z2(IJ+31))*0.5
      GOTO 26
  25  SOM=SOM-(Z2(IJ+1)*C1+Z2(IJ+31)*C2)*0.5
     *       +(Z2(JI+1)*S1+Z2(JI+31)*S2)*0.5
C
  26  CONTINUE
C     IF(MT.EQ.0) GOTO 16
C     IJ=IJ+31
C     JI=JI+31
C     DO 16 I=1,MT
C     GOTO(27,28,29),IT
C 27  SOM=SOM-(Z2(IJ+2*I)-Z2(IJ+2*I-1))*0.5*DCOS((TETA+ABTR(I))*PI/180.)
C    *       -(Z2(JI+2*I)-Z2(JI+2*I-1))*0.5*DSIN((TETA+ABTR(I))*PI/180.)
C     GOTO 16
C 28  SOM=SOM-(Z2(IJ+2*I)-Z2(IJ+2*I-1))*0.5
C     GOTO 16
C 29  SOM=SOM-(Z2(IJ+2*I)-Z2(IJ+2*I-1))*0.5*DCOS((TETA+ABTR(I))*PI/180.)
C    *       +(Z2(JI+2*I)-Z2(JI+2*I-1))*0.5*DSIN((TETA+ABTR(I))*PI/180.)
  16  CONTINUE
      ELSE
C
      DO 115 I=1,30
      IMT=0
C     WRITE(6,*) I,IMT
C     WRITE(6,*) SOM
      DO 130 J=1,MT
      N=ABTR(J)*0.9999/SM
      IF(N.NE.(I-1)) GOTO 130
      IMT=1
      A=DMOD((ABTR(J)*0.9999),SM)/SM
      B=1.-A
C     WRITE(6,*) N,J,I
C     WRITE(6,*) A,B
      GOTO(127,128,129),IT
 127  SOM=SOM+0.5*(A*(Z2(IJ+I)+Z2(IJ+31+2*J-1))
     *    +B*(Z2(IJ+I+1)+Z2(IJ+31+2*J)))*DCOS((TETA+SM*(I-0.5))*PI/180.)
     *       +0.5*(A*(Z2(JI+I)+Z2(JI+31+2*J-1))
     *    +B*(Z2(JI+I+1)+Z2(JI+31+2*J)))*DSIN((TETA+SM*(I-0.5))*PI/180.)
C     WRITE(6,*) SOM
      GOTO 130
 128  SOM=SOM+0.5*(A*(Z2(IJ+I)+Z2(IJ+31+2*J-1))
     *            +B*(Z2(IJ+I+1)+Z2(IJ+31+2*J)))
C     WRITE(6,*) SOM
      GOTO 130
 129  SOM=SOM+0.5*(A*(Z2(IJ+I)+Z2(IJ+31+2*J-1))
     *    +B*(Z2(IJ+I+1)+Z2(IJ+31+2*J)))*DCOS((TETA+SM*(I-0.5))*PI/180.)
     *       -0.5*(A*(Z2(JI+I)+Z2(JI+31+2*J-1))
     *    +B*(Z2(JI+I+1)+Z2(JI+31+2*J)))*DSIN((TETA+SM*(I-0.5))*PI/180.)
C     WRITE(6,*) SOM
 130  CONTINUE
      IF(IMT.EQ.1) GOTO 115
      GOTO(117,118,119),IT
 117  SOM=SOM+0.5*(Z2(IJ+I)+Z2(IJ+I+1))*DCOS((TETA+SM*(I-0.5))*PI/180.)
     *       +0.5*(Z2(JI+I)+Z2(JI+I+1))*DSIN((TETA+SM*(I-0.5))*PI/180.)
      GOTO 115
 118  SOM=SOM+0.5*(Z2(IJ+I)+Z2(IJ+I+1))
      VVAL=DABS(Z2(IJ+I))
      IF(VVAL.GT.VMAX)VMAX=VVAL
      GOTO 115
 119  SOM=SOM+0.5*(Z2(IJ+I)+Z2(IJ+I+1))*DCOS((TETA+SM*(I-0.5))*PI/180.)
     *       -0.5*(Z2(JI+I)+Z2(JI+I+1))*DSIN((TETA+SM*(I-0.5))*PI/180.)
 115  CONTINUE
      ENDIF
      SOM=SOM*SM*Q*PI/180.
      RETURN
      END
