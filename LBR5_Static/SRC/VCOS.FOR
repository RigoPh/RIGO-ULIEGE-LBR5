      REAL*8 FUNCTION VCOS(A,B)                                        
      IMPLICIT REAL*8(A-H,O-Z)                                          VCO00020
      COMMON/PY/PI                                                      VCO00030
c       WRITE(66,*) A,B
      X=A                                                               VCO00040
      Y=B                                                               VCO00050
      IF(DABS(Y).LE.0.5)GOTO 1                                          VCO00060
      X=X+Y                                                             VCO00070
      Y=0.D00                                                           VCO00080
  1   CONTINUE                                                          VCO00090
C                                                                       VCO00100
c       WRITE(66,*) X,Y
  3   IF(X.GE.0.) GOTO 2                                                VCO00110
      X=X+360.                                                          VCO00120
      GOTO 3                                                            VCO00130
  2   CONTINUE                                                          VCO00140
C                                                                       VCO00150
      NN=1                                                              VCO00160
      N=0                                                               VCO00170
  5   IF(X.LT.180.) GOTO 4                                              VCO00180
      X=X-180.                                                          VCO00190
      N=N+1                                                             VCO00200
      GOTO 5                                                            VCO00210
 4    IF(N.NE.0) NN=(-1)**N                                             VCO00220
C                                                                       VCO00230
c       WRITE(66,*) X,Y
      IF((X+Y).GE.90.) THEN                                             VCO00240
      X=180.-X                                                          VCO00250
      Y=-Y                                                              VCO00260
      VCOS=-NN*DCOS((X+Y)*PI/180.)                                      VCO00270
      ELSE                                                              VCO00280
      VCOS=NN*DCOS((X+Y)*PI/180.)                                       VCO00290
      ENDIF                                                             VCO00300
      IF((X.LE.90.).AND.(X.GT.89.99)) VCOS=NN*DSIN(Y*PI/180.)           VCO00310
      RETURN                                                            VCO00320
      END                                                               VCO00330
