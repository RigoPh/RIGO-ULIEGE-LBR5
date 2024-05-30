      REAL*8 FUNCTION VSIN(A,B)                                         VSI00010
      IMPLICIT REAL*8(A-H,O-Z)                                          VSI00020
      COMMON/PY/PI                                                      VSI00030
c      WRITE(66,*)A,B,PI
      X=A                                                               VSI00040
      Y=B                                                               VSI00050
      IF(DABS(Y).LE.0.5)GOTO 1                                          VSI00060
      X=X+Y                                                             VSI00070
      Y=0.D00                                                           VSI00080
  1   CONTINUE                                                          VSI00090
c      WRITE(66,*)A,B,PI
C                                                                       VSI00100
  3   IF(X.GE.0.) GOTO 2                                                VSI00110
      X=X+360.                                                          VSI00120
      GOTO 3                                                            VSI00130
  2   CONTINUE                                                          VSI00140
C                                                                       VSI00150
      NN=1                                                              VSI00160
      N=0                                                               VSI00170
  5   IF(X.LT.180.) GOTO 4                                              VSI00180
      X=X-180.                                                          VSI00190
      N=N+1                                                             VSI00200
      GOTO 5                                                            VSI00210
 4    IF(N.NE.0) NN=(-1)**N                                             VSI00220
c      WRITE(66,*)A,B,PI,NN
C                                                                       VSI00230
      IF((X+Y).LT.90.) GOTO 6                                           VSI00240
      X=180.-X                                                          VSI00250
      Y=-Y                                                              VSI00260
  6   VSIN=NN*DSIN((X+Y)*PI/180.)                                       VSI00270
      IF((X.LE.90.).AND.(X.GT.89.99)) VSIN=NN*DCOS(Y*PI/180.)           VSI00280
      RETURN                                                            VSI00290
      END                                                               VSI00300
